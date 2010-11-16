-module(echo_server).

-behaviour(application).
-behaviour(gen_tcp_acceptor).

-include("../include/gen_tcp_acceptor.hrl").
-include("../src/gen_tcp_listener.hrl").

% application
-export([start/2, stop/1]).
% gen_tcp_acceptor
-export([start_link/0, init/1, handle_accept/2]).


%% APPLICATION
start(_, _) ->
    gen_tcp_listener:start_link({local, echo_listener}, ?MODULE, [], []).

stop(_) ->
    ok.


%% GEN_TCP_ACCEPTOR
start_link() ->
    ?DEBUGP("start_link/0~n"),
    {ok, Pid} = gen_fsm:start_link(echo_fsm, [], []),
    gen_tcp_acceptor:start_link(?MODULE, [{handler, Pid}], []).

init(Args) ->
    HandlerPid = proplists:get_value(handler, Args),
    {ok, #acceptor_state{handler = HandlerPid}}.

handle_accept(Socket, State = #acceptor_state{handler = Pid}) ->
    ?DEBUGP("accepted~n"),
    gen_fsm:send_event(Pid, {socket_ready, Socket}),
    gen_tcp:controlling_process(Socket, Pid),
    {noreply, State}.
