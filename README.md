# gen_udp_sketch

[License: MIT](LICENSE)

This is a sketch of a simple generic Erlang UDP listener. It's used as follows:

```
{ok, Pid} = gus:start_link(Opts).
```

Where `Opts` are:

* `{module, Module}` - A module implementing `gus_handle_msg/5` that accepts
  the following arguments:
  - `Ref` - detailed below
  - `Socket` - gen_udp socket
  - `LocalAddr` - a tuple pair of the local address and port
  - `RemoteAddr` - a tuple pair of the remote address and port
  - `Msg` - a binary payload
* `{ref, Ref}` - Ref will be passed verbatim to the callback outlined above
* `{payload_min, N}` - Minimum useful payload size
* `{payload_max, N}` - Maximum useful payload size
* `{worker_count, N}` - Number of workers to process requests
* `{worker_queue_max, N}` - Maximum number of messages to queue with a worker
* `{listener_count, N}` - Number of sockets to open if the kernel supports[1]
  balancing between multiple sockets.
* `{ip, IP}` - IP to listen on 
* `{port, N}` - Port to listen on
* `{active, once | true | {N, once | true} | N}`
  - `once` - listener(s) should accept one datagram at a time.
    (`inet:setopts(S,[{active,once}])`.)
  - `true` - listener(s) will set the socket into active mode until a message
    is received, then put it into passive mode until the mailbox is drained of
    datagrams.
    (`inet:setopts(S,[{active,true}])` -> `inet:setopts(S,[{active,once}])`.)
  - `{N, once | true}` - if `{active,N}` is available[2], set the socket to
    receive up to `N` datagrams, process those datagrams, then set the option
    again. Falls back to `once` or `true` if `{active,N}` is unavailable.
    (`inet:setopts(S,[{active,N}])`.)
  - `N` - same behaviour as previous option with no fallback.

See gus.erl for defaults.

Example echo server:

```
gus:start_link([{module,gus_echo},{ip,{127,0,0,1}},{port,5030}]).
```

1: At the time of writing, that's Linux 3.9 or later only and Dragonfly, but
   I've only added support for Linux. The FreeBSD networking folks seem to
   think it's a good idea but have not implemented it yet.

2: https://github.com/erlang/otp/pull/66
