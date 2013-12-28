-module(gus_worker).
-export([start_link/1,
         init/2,
         system_code_change/4,
         system_continue/3,
         system_terminate/4,
         write_debug/3]).

-include_lib("common.hrl").

-record(state, {
    tab :: ets:tab(),
    id :: non_neg_integer(),
    ref :: any(),
    addr :: {inet:ip_address(), inet:port_number()},
    module :: atom()
}).

start_link(Opts) ->
    Tab = proplists:get_value(tab, Opts),
    Id = proplists:get_value(id, Opts),
    Ref = proplists:get_value(ref, Opts),
    Addr = {proplists:get_value(ip, Opts), proplists:get_value(port, Opts)},
    Module = proplists:get_value(module, Opts),
    State = #state{tab = Tab, id = Id, ref = Ref, addr = Addr, module = Module},
    proc_lib:start_link(?MODULE, init, [self(), State]).

init(Parent, #state{tab = Tab, id = Id} = State) ->
    true = ets:insert(Tab, #gus_tab{id = Id, pid = self(), count = 0}),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, State, []).

loop(Parent, #state{} = State, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {udp, Socket, Addr, Port, Msg} ->
            ok = dispatch(State, Socket, {Addr, Port}, Msg),
            ok = decrement_count(State),
            loop(Parent, State, Debug)
    end.

dispatch(#state{ref = Ref, addr = LocalAddr, module = Module},
         Socket, RemoteAddr, Msg) ->
    ok = Module:gus_handle_msg(Ref, Socket, LocalAddr, RemoteAddr, Msg).

decrement_count(#state{tab = Tab, id = Id}) ->
    _ = ets:update_counter(Tab, Id, {#gus_tab.count, -1}),
    ok.

system_code_change(State, _Moduleule, _OldVsn, _Extra) -> {ok, State}.

system_continue(Parent, Debug, State) -> loop(Parent, State, Debug).

system_terminate(Reason, _Parent, _Debug, _State) -> exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, ?MODULE_STRING ": ~p event = ~p~n", [Name, Event]).
