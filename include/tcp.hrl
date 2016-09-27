-ifndef(TCP_HRL).
-define(TCP_HRL, true).

-define(GEN_SERVER, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3).
-record(client, {socket,addr,fsm}).
-record(server, {listener,acceptor,module}).

-endif.
