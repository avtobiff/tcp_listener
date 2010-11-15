-module(gen_tcp_acceptor).
-author('Per Andersson <avtobiff@gmail.com>').

-behaviour(gen_server).

-include("gen_tcp_listener.hrl").

-export([behaviour_info/1]).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([accept/2]).

-record(acceptor_state, {module,
                         state,
                         client_socket}).


behaviour_info(callbacks) ->
    [{start_link, 0},
     {handle_accept, 2}];
behaviour_info(_) ->
    undefined.


start_link(Mod, Args, Opts) ->
    gen_server:start_link(Mod, [{'$gen_tcp_acceptor_module', Mod}|Args], Opts).

init(Args) ->
    process_flag(trap_exit, true),
    ?DEBUGP("init/1~n"),
    Module = proplists:get_value('$gen_tcp_acceptor_module', Args),
    AcceptorState =
        case Module:init(Args) of
            {ok, S} -> S;
            _ -> exit(bad_return)
        end,
    {ok, #acceptor_state{module = Module, state = AcceptorState}}.

accept(ServerRef, ClientSocket) ->
    ?DEBUGP("accept/2~n"),
    %gen_server:call(self(), {accept, ClientSocket}).
    undefined:handle_accept(ServerRef, ClientSocket).


handle_call(_From, {accept, ClientSocket},
            State = #acceptor_state{module = Module}) ->
    ?DEBUGP("handle_call/3~n"),
    Pid = spawn_link(fun() -> Module:handle_accept(ClientSocket) end),
    ok = gen_tcp:controlling_process(ClientSocket, Pid),
    {reply, ok, State#acceptor_state{client_socket = ClientSocket}}.



handle_cast(_,_) -> ok.
handle_info(_,_) -> ok.
terminate(Reason, State = #acceptor_state{client_socket = ClientSocket}) ->
    gen_tcp:close(ClientSocket),
    gen_server:terminate(Reason, State).
code_change(_,_,_) -> ok.
