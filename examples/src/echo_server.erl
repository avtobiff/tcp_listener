-module(echo_server).

-behaviour(gen_tcp_acceptor).

-include("echo_common.hrl").

% API
-export([start/0]).
% gen_tcp_acceptor
-export([start_link/0, init/1, handle_accept/2]).
% internal
-export([echo/1]).


%% API
start() ->
    tcp_listener:start_link({local, echo_server}, ?MODULE, [], []).


%% GEN_TCP_ACCEPTOR
start_link() ->
    ?PRINT("start_link/0~n"),
    gen_tcp_acceptor:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, []}.

handle_accept(Socket, State) ->
    ?PRINT("accepted~n"),
    ?MODULE:echo(Socket),
    {noreply, State}.


%% INTERNAL
echo(Socket) ->
    ?PRINT("echo~n"),
    % TCP flow control, receive one message at the time
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} ->
            gen_tcp:send(Socket, Bin),
            ?MODULE:echo(Socket);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket)
    end.
