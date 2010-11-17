-module(echo_app).

-behaviour(application).
-behaviour(gen_tcp_acceptor).

-include("echo_common.hrl").

% application
-export([start/2, stop/1]).
% gen_tcp_acceptor
-export([start_link/0, init/1, handle_accept/2]).



%% APPLICATION
start(_, _) ->
    tcp_listener:start_link({local, echo_listener}, ?MODULE, [], []).

stop(_) ->
    ok.


%% GEN_TCP_ACCEPTOR
start_link() ->
    ?PRINT("start_link/0~n"),
    % use echo_fsm to handle the socket once it is ready
    {ok, Pid} = gen_fsm:start_link(echo_fsm, [], []),
    gen_tcp_acceptor:start_link(?MODULE, [{handler, Pid}], []).

init(Args) ->
    HandlerPid = proplists:get_value(handler, Args),
    {ok, [{handler, HandlerPid}]}.

handle_accept(Socket, State = [{handler, HandlerPid}]) ->
    ?PRINT("accepted~n"),
    gen_fsm:send_event(HandlerPid, {socket_ready, Socket}),
    gen_tcp:controlling_process(Socket, HandlerPid),
    {noreply, State}.
