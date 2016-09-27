-module(tcp).
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1]).

client()    -> supervisor:start_child(con_sup, []).
con(M)      -> {con_sup,{supervisor, start_link,[{local,con_sup},?MODULE,[M]]},permanent,infinity,supervisor,[]}.
lsn(P,M)    -> {tcp_lsn,{tcp_lsn,    start_link,[P,M]}, permanent,2000,worker, [tcp_lsn]}.
cli(M)      -> {none,   {M,          start_link,[]},    temporary,2000,worker, []}.

port()      -> application:get_env(tcp,port,8100).
init([M])   -> {ok,{{simple_one_for_one,5,60},[cli(M)]}};
init([P,M]) -> tcp_tables:init(),{ok,{{one_for_one,5,60},[lsn(P,M),con(M)]}}.

stop(_)     -> ok.
start(_,_)  -> supervisor:start_link({local,?MODULE},?MODULE,[port(),tcp_conn]).
main(A)     -> mad:main(A).
