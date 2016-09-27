-module(tcp_tables).
-compile(export_all).

tables()   -> [ cache ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
init()     -> [ ets:new(T,opt()) || T <- tables() ].

cache(Key, undefined) -> ets:delete(cache,Key);
cache(Key, Value) -> ets:insert(cache,{Key,Value}), Value.
cache(Key) ->
    Res = ets:lookup(cache,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined;
                {_,X} -> X;
                _ -> Val end.
