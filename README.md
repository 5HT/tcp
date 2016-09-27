TCP Server
==========

Simple Erlang/OTP non-blocking TCP Server in 100 LOC dedicated to all Erlang newcomers.

Build
-----

``
$ mad dep com pla rep
```

Run
---

```
> {ok,S2} = gen_tcp:connect({127,0,0,1},8100,[{packet,2}]),
            gen_tcp:send(S2,<<"hello">>),
            flush().
Cli Socket: #Port<0.16165>
Shell got {tcp,#Port<0.16164>,"hello"}
ok
```

Credits
-------

* Maxim Sokhatsky

OM A HUM
