-module(tcp_lsn).
-include("tcp.hrl").
-compile(export_all).
-export([?GEN_SERVER,start_link/2]).

start_link(Port, Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

init([Port, Module]) ->
    Opts = [binary, {packet, 2},       {reuseaddr, true},
                    {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
    {ok, Listen_socket} ->
        {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
        {ok, #server{listener = Listen_socket,
                    acceptor = Ref,
                    module   = Module}};
    {error, Reason} ->
        {stop, Reason}
    end.

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #server{listener=ListSock, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
        ok              -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
        end,

        {ok, Pid} = tcp:client(),
        io:format("Cli Socket: ~p~n",[CliSocket]),
        gen_tcp:controlling_process(CliSocket, Pid),
        Module:set_socket(Pid, CliSocket),

        case prim_inet:async_accept(ListSock, -1) of
        {ok,    NewRef} -> ok;
        {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#server{acceptor=NewRef}}
    catch exit:Why ->
        io:format("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListSock, Ref, Error}, #server{listener=ListSock, acceptor=Ref} = State) ->
    io:format("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) -> {noreply, State}.

handle_call(Request, _, State) -> {stop, {unknown_call, Request}, State}.
handle_cast(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} -> case prim_inet:setopts(CliSocket, Opts) of
                           ok -> ok;
                           Error -> gen_tcp:close(CliSocket), Error end;
             Error -> gen_tcp:close(CliSocket), Error end.
