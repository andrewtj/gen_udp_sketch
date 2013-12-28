-module(gus_echo).

-export([gus_handle_msg/5]).

gus_handle_msg(_Ref, Socket, _LocalAddr, {RemoteIP, RemotePort}, Msg) ->
    ok = gen_udp:send(Socket, RemoteIP, RemotePort, Msg).
