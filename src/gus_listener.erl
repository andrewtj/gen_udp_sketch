-module(gus_listener).

-export([start_link/1,
         init/2,
         system_code_change/4,
         system_continue/3,
         system_terminate/4,
         write_debug/3]).

-include_lib("common.hrl").

-record(state, {
    tab :: ets:tab(),
    id :: neg_integer(),
    ref :: any(),
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    kernel_balance :: boolean(),
    active :: non_neg_integer() | 'once' | boolean(),
    socket :: gen_udp:socket(),
    payload_min :: non_neg_integer(),
    payload_max :: pos_integer(),
    worker_count :: pos_integer(),
    worker_queue_max :: pos_integer()
}).

start_link(Opts) ->
    Tab = proplists:get_value(tab, Opts),
    Id = proplists:get_value(id, Opts),
    Ref = proplists:get_value(ref, Opts),
    IP = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    KernelBalance = proplists:get_value(listener_count, Opts) =/= 1,
    Active = proplists:get_value(active, Opts),
    PayloadMin = proplists:get_value(payload_min, Opts),
    PayloadMax = proplists:get_value(payload_max, Opts),
    WorkerCount = proplists:get_value(worker_count, Opts),
    WorkerQueueMax = proplists:get_value(worker_queue_max, Opts),
    State = #state{tab = Tab,
                   id = Id,
                   ref = Ref,
                   ip = IP,
                   port = Port,
                   kernel_balance = KernelBalance,
                   active = Active,
                   payload_min = PayloadMin,
                   payload_max = PayloadMax,
                   worker_count = WorkerCount,
                   worker_queue_max = WorkerQueueMax},
    proc_lib:start_link(?MODULE, init, [self(), State]).    

init(Parent, #state{} = State) ->
    State0 = open_socket(State),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, State0, []).

loop(Parent, #state{active = once, socket = Socket} = State, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {udp, Socket, Addr, Port, Msg} ->
            State0 = dispatch(State, Socket, Addr, Port, Msg),
            ok = inet:setopts(Socket, [{active, once}]),
            loop(Parent, State0, Debug)
    end;
loop(Parent, #state{active = false, socket = Socket} = State, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {udp, Socket, Addr, Port, Msg} ->
            State0 = dispatch(State, Socket, Addr, Port, Msg),
            loop(Parent, State0, Debug)
    after 0 ->
        ok = inet:setopts(Socket, [{active, true}]),
        State0 = State#state{active = true},
        loop(Parent, State0, Debug)
    end;
loop(Parent, #state{active = true, socket = Socket} = State, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {udp, Socket, Addr, Port, Msg} ->
            ok = inet:setopts(Socket, [{active, false}]),
            State0 = State#state{active = false},
            State1 = dispatch(State0, Socket, Addr, Port, Msg),
            loop(Parent, State1, Debug)
    end;
loop(Parent, #state{active = N, socket = Socket} = State, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        {udp, Socket, Addr, Port, Msg} ->
            State0 = dispatch(State, Socket, Addr, Port, Msg),
            loop(Parent, State0, Debug);
        {udp_passive, Socket} ->
            ok = inet:setopts(Socket, [{active, N}]),
            loop(Parent, State, Debug)
    end.


system_code_change(State, _Module, _OldVsn, _Extra) -> {ok, State}.

system_continue(Parent, Debug, State) -> loop(Parent, State, Debug).

system_terminate(Reason, _Parent, _Debug, _State) -> exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, ?MODULE_STRING ": ~p event = ~p~n", [Name, Event]).

open_socket(#state{kernel_balance = false} = State) ->
    open_socket(State, []);
open_socket(#state{kernel_balance = true} = State) ->
    Opt = gus:inet_opt_kernel_balance(),
    true = Opt =/= undefined,
    open_socket(State, [Opt]).

open_socket(#state{ip = IP,
                   port = Port,
                   active = Active,
                   payload_max = PayloadMax} = State, Opts) ->
    Buf = PayloadMax + 1,
    Opts0 = [{ip, IP}, {active, Active}, {sndbuf, Buf}, {recbuf, Buf}, binary|Opts],
    {ok, Socket} = gen_udp:open(Port, Opts0),
    State#state{socket = Socket}.

dispatch(#state{payload_min = Min, payload_max = Max} = State,
         _Socket, _Addr, _Port, Msg) when byte_size(Msg) < Min orelse byte_size(Msg) > Max ->
    State; 
dispatch(#state{tab = Tab,
                worker_count = WorkerCount,
                worker_queue_max = WorkerQueueMax} = State,
         Socket, Addr, Port, Msg) ->
    WorkerNum = ets:update_counter(Tab, 0, {#gus_tab.count, 1, WorkerCount, 1}),
    case ets:update_counter(Tab, WorkerNum, {#gus_tab.count, 1}) of
        N when N > WorkerQueueMax ->
            _ = ets:update_counter(Tab, WorkerNum, {#gus_tab.count, -1}),
            State;
        _ ->
            WorkerPid = ets:lookup_element(Tab, WorkerNum, #gus_tab.pid),
            WorkerPid ! {udp, Socket, Addr, Port, Msg},
            State
    end.
