-module(gen_tcp_acceptor).
-author('Per Andersson <avtobiff@gmail.com>').

-behaviour(gen_server).

-include("gen_tcp_listener.hrl").
-include("../include/gen_tcp_acceptor.hrl").

%% API
-export([start_link/3, accept/2]).
-export([behaviour_info/1]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(tcp_acceptor_state, {module,
                             client_state}).


%% =============================================================================
%% API
%% =============================================================================
behaviour_info(callbacks) ->
    [{start_link, 0},
     {init, 1},
     {handle_accept, 2}];
behaviour_info(_) ->
    undefined.


%start_link(Mod, Args, Opts) ->
%    gen_server:start_link(Mod, [{'$gen_tcp_acceptor_module', Mod}|Args], Opts).

start_link(Module, Args, Opts) ->
    gen_server:start_link(?MODULE,
                          [{'$gen_tcp_acceptor_module', Module}|Args], Opts).


accept(ServerRef, ClientSocket) ->
    ?DEBUGP("accept/2~n"),
    gen_server:cast(ServerRef, {accept, ClientSocket}).



%% =============================================================================
%% GEN_SERVER EXPORTS
%% =============================================================================
init(Args) ->
    process_flag(trap_exit, true),
    ?DEBUGP("init/1~n"),
    Module = proplists:get_value('$gen_tcp_acceptor_module', Args),
    {ok, ClientState} = Module:init(Args),
    {ok, #tcp_acceptor_state{module = Module, client_state = ClientState}}.


handle_cast({accept, ClientSocket},
            State = #tcp_acceptor_state{module = Module,
                                        client_state = ClientState =
                                                       #acceptor_state{}}) ->
    ?DEBUGP("handle_cast/2~n"),
    case Module:handle_accept(ClientSocket, ClientState) of
        {noreply, ClientState} ->
            {noreply, State#tcp_acceptor_state{client_state = ClientState}};
        {noreply, ClientState, Timeout} ->
            {noreply, State#tcp_acceptor_state{client_state = ClientState},
             Timeout};
        {noreply, ClientState, hibernate} ->
            {noreply, State#tcp_acceptor_state{client_state = ClientState},
             hibernate};
        {stop, Reason, ClientState} ->
            {stop, Reason,
             State#tcp_acceptor_state{client_state = ClientState}}
    end.


handle_call(_, State, _) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
