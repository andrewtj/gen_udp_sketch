-module(gus).

-export([start_link/1, inet_opt_kernel_balance/0]).

start_link(Opts) ->
    case check_opts(Opts) of
        {ok, Opts0} -> gus_sup:start_link(Opts0);
        Other -> {error, Other}
    end.

check_opts(Opts) ->
    Specs = [{module, module, undefined},
             {ref, any, undefined},
             {payload_min, {integer, 0, undefined}, 0},
             {payload_max, {integer, 1, undefined}, 1280},
             {worker_count, {integer, 1, undefined}, 100},
             {worker_queue_max, {integer, 1, undefined}, 10},
             {listener_count, {integer, 1, undefined}, 4},
             {port, {integer, 1, 65535}, undefined},
             {active, active, {100, once}},
             {ip, ip, undefined}],
    Keys = [ Key || {Key, _, _} <- Specs ],
    case check_opt_keys(Opts, Keys) of
        ok -> check_opt_values(Specs, Opts, []);
        Other -> {error, Other}
    end.

check_opt_keys(Opts, Keys) -> check_opt_keys(Opts, Keys, Keys).

check_opt_keys([], _, _) -> ok;
check_opt_keys([{Key, _}|Opts], Keys, AllKeys) ->
    case {lists:member(Key, Keys), lists:member(Key, AllKeys)} of
        {true, true} -> check_opt_keys(Opts, lists:delete(Key, Keys), AllKeys);
        {false, true} -> {duparg, Key};
        {false, false} -> {badarg, Key}
    end;
check_opt_keys(_, _, _) -> badarg.

check_opt_values([], _Raw, Opts) ->
    case have_feature(kernel_balance) of
        true -> {ok, Opts};
        false ->
            Opts0 = proplists:delete(listener_count, Opts),
            Opts1 = [{listener_count, 1}|Opts0],
            {ok, Opts1}
    end;
check_opt_values([{Name, module, Default}|Specs], Raw, Checked) ->
    Mod = proplists:get_value(Name, Raw, Default),
    ModExports = try Mod:module_info(exports) catch _:_ -> [] end,
    case lists:member({gus_handle_msg, 5}, ModExports) of
        true -> check_opt_values(Specs, Raw, [{Name, Mod}|Checked]);
        false -> {badarg, Name}
    end;
check_opt_values([{Name, any, Default}|Specs], Raw, Checked) ->
    Value = proplists:get_value(Name, Raw, Default),
    check_opt_values(Specs, Raw, [{Name, Value}|Checked]);
check_opt_values([{Name, {integer, Min, Max}, Default}|Specs], Raw, Checked) ->
    Value = proplists:get_value(Name, Raw, Default),
    case valid_integer(Value, Min, Max) of
        true -> check_opt_values(Specs, Raw, [{Name, Value}|Checked]);
        false -> {badarg, Name}
    end;
check_opt_values([{Name, ip, Default}|Specs], Raw, Checked) ->
    Value = proplists:get_value(Name, Raw, Default),
    case inet:ntoa(Value) of
        {error, einval} -> {badarg, Name};
        _ -> check_opt_values(Specs, Raw, [{Name, Value}|Checked])
    end;
check_opt_values([{Name, active, Default}|Specs], Raw, Checked) ->
    Value = proplists:get_value(Name, Raw, Default),
    HaveActiveN = have_feature(inet_activen),
    case Value of
        _ when Value =:= true orelse Value =:= once ->
            check_opt_values(Specs, Raw, [{Name, Value}|Checked]);
        {N, X} when N > 0 andalso (X =:= true orelse X =:= once) ->
            case HaveActiveN of
                true -> check_opt_values(Specs, Raw, [{Name, N}|Checked]);
                false -> check_opt_values(Specs, Raw, [{Name, X}|Checked])
            end;
        N when HaveActiveN andalso N > 0 ->
            check_opt_values(Specs, Raw, [{Name, N}|Checked]);
        _ -> {badarg, Name}
    end.

valid_integer(Value, _Min, _Max) when not is_integer(Value) -> false;
valid_integer(Value, Min, undefined) -> Value >= Min;
valid_integer(Value, Min, Max) -> Value >= Min andalso Value =< Max.

have_feature(inet_activen) ->
    try gen_udp:open(0,[{active,42}]) of
        {ok, S} ->
            ok = gen_udp:close(S),
            true
    catch
        exit:badarg -> false
    end;
have_feature(kernel_balance) ->
    case inet_opt_kernel_balance() of
        {raw, Prot, OptNum, ValueBin} = Opt ->
            {ok, S} = gen_udp:open(0, [{reuseaddr, true}, Opt]),
            {ok, L} = inet:getopts(S, [{raw, Prot, OptNum, byte_size(ValueBin)}]),
            ok = gen_udp:close(S),
            [Opt] =:= L;
        undefined -> false
    end.

inet_opt_kernel_balance() ->
    case os:type() of
        {unix, linux} -> {raw, 1, 15, <<1:32/native>>};
        _ -> undefined
    end.
