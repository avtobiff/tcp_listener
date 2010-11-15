-module(echo_server).

%-behaviour(gen_tcp_listener).
-behaviour(gen_tcp_acceptor).
-behaviour(gen_server).
-behaviour(application).

-include("../src/gen_tcp_listener.hrl").

-export([start/2, stop/1]). % application exports
-export([start_link/0]).
-export([init/1, handle_accept/2]).
-export([handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start(_, _) ->
    gen_tcp_listener:start_link({local, echo_listener}, ?MODULE, [], []).

stop(_) ->
    ok.

start_link() ->
    ?DEBUGP("start_link/0~n"),
    %{ok, self()}.
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    ?DEBUGP("init/1~n"),
    {ok, []}.

handle_accept(_Pid, Socket) ->
    ?DEBUGP("accepted~n"),
    Pid = spawn(fun() -> echo(Socket) end),
    ok = gen_tcp:controlling_process(Socket, Pid).

echo(Socket) ->
    ?DEBUGP("echo...~n"),
    ok = inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    receive
        {tcp, Socket, Bin} ->
            gen_tcp:send(Socket, Bin),
            echo(Socket);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket)
    end.

handle_cast(_,_) -> ok.
handle_call(_,_,_) -> ok.
handle_info(_,_) -> ok.
terminate(_,_) -> ok.
code_change(_,_,_) -> ok.

