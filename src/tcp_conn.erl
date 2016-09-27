-module(tcp_conn).
-include("tcp.hrl").
-compile(export_all).
-export([?GEN_SERVER,start_link/0]).

timeout() -> 120000.
start_link() -> gen_server:start_link(?MODULE, [], []).
set_socket(Pid, Socket) -> Pid ! {socket_ready, Socket}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #client{fsm='socket'}}.

handle_info({socket_ready, Socket}, #client{fsm='socket'} = State)  ->
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {noreply, State#client{fsm='data', socket=Socket, addr=IP}, timeout()};


handle_info({data, Data}, #client{socket=S,fsm='data'} = State) ->
    ok = gen_tcp:send(S, Data),
    {noreply, State, timeout()};

handle_info({tcp, Socket, Bin}, #client{socket=Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    ok = gen_tcp:send(Socket, Bin),
    {noreply, State};

handle_info(timeout, #client{fsm='data'} = State) ->
    io:format("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

handle_info({tcp_closed, Socket}, #client{socket=Socket, addr=Addr} = State) ->
    io:format("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, State};

handle_info(_, #client{fsm='data'} = State) ->
    {noreply, State, timeout()};

handle_info(_, State) ->
    {noreply, State#client{fsm='socket'}}.

handle_call(_, _, S)                 -> {noreply, S}.
handle_cast(_, S)                    -> {noreply, S}.
code_change(_, S, _)                 -> {ok, S}.
terminate(_, #client{socket=Socket}) -> (catch gen_tcp:close(Socket)), ok.
