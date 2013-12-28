-module(gus_sup).

-behaviour(supervisor).

-include_lib("common.hrl").

-export([start_link/1, init/1]).

start_link(Opts) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(Opts) ->
    Tab = ets:new(?MODULE, [set,
                            public,
                            {keypos, #gus_tab.id},
                            {write_concurrency, true}]),
    Opts0 = [{tab, Tab}|Opts],
    WorkerCount = proplists:get_value(worker_count, Opts),
    ListenerCount = proplists:get_value(listener_count, Opts),
    true = ets:insert(Tab, #gus_tab{id = 0, pid = self(), count = 0}),
    Specs = worker_specs(WorkerCount, Opts0) ++ listener_specs(ListenerCount, Opts0),
    {ok, { {one_for_one, 5, 10}, Specs} }.

worker_specs(Count, Opts) -> worker_specs(Count, Opts, []).

worker_specs(0, _Opts, Specs) -> lists:reverse(Specs);
worker_specs(N, Opts, Specs) ->
    Spec = worker_spec(N, Opts),
    worker_specs(N - 1, Opts, [Spec|Specs]).

worker_spec(N, Opts) when N > 0 ->
    Id = {gus_worker, N},
    StartFunc = {gus_worker, start_link, [[{id, N}|Opts]]},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [gus_worker],
    {Id, StartFunc, Restart, Shutdown, Type, Modules}.

listener_specs(Count, Opts) -> listener_specs(Count, Opts, []).

listener_specs(0, _Opts, Specs) -> Specs;
listener_specs(N, Opts, Specs) ->
    Spec = listener_spec(-N, Opts),
    listener_specs(N - 1, Opts, [Spec|Specs]).

listener_spec(N, Opts) when N < 0 ->
    Id = {gus_listener, N},
    StartFunc = {gus_listener, start_link, [[{id, N}|Opts]]},
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    Modules = [gus_listener],
    {Id, StartFunc, Restart, Shutdown, Type, Modules}.
