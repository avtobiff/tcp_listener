%% -----------------------------------------------------------------------------
%% Copyright @ 2010 Per Andersson
%%
%% gen_tcp_listener is free software: you can redistribute it and/or
%% modify it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%%
%% gen_tcp_listener is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with gen_tcp_listener.  If not, see
%% <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%%
%% @author Per Andersson <avtobiff@gmail.com>
%% @doc
%% Generic TCP Listener
%%
%% This is a behaviour that implements an asynchronous TCP listener server.
%% The server implementation then only needs to implement init/1 and
%% handle_accept/2.
%%
%% See gen_server(3), gen_tcp(3) for more information.
%%
%% Inspired by
%% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
%% http://github.com/erlware/gen_socket
%% http://github.com/kaos/gen_listener_tcp
%% @end
%%
%% -----------------------------------------------------------------------------
-module(gen_tcp_listener).
-author('Per Andersson <avtobiff@gmail.com>').

-behaviour(gen_server).


%% API
-export([start_link/3, start_link/4]).
-export([behaviour_info/1]).

%% gen_server exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include("gen_tcp_listener.hrl").



%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec behaviour_info(atom()) -> undefined | [{atom(), arity()}].
%% @doc
%% Behaviour exports.
%% @end
%% -----------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{start_link, 0},
     %{init, 1},
     {handle_accept, 1}];
behaviour_info(_) ->
    undefined.



%% -----------------------------------------------------------------------------
-spec start_link(Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.
-spec start_link(Name :: atom(), Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.
%% @doc
%% Starts the gen_tcp_listener as part of a supervisor tree.
%%
%% Parses options from Args and uses defaults if an argument is not supplied.
%% @end
%% -----------------------------------------------------------------------------
start_link(Mod, Args, Opts) ->
    start_link({local, Mod}, Mod, Args, Opts).

start_link(Name, Mod, Args, Opts) ->
    ?DEBUGP("~p:start_link/4~n", [?MODULE]),
    %gen_server:start_link(Name, Mode, Args, Opts).
    supervisor:start_link(Name, gen_tcp_listener_sup,
                          [{'$gen_tcp_listener_server_ref', Name},
                           {'$gen_tcp_listener_opts', Opts},
                           {module, Mod}|Args]).



%% =============================================================================
%% GEN_SERVER EXPORTS
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec init(Args :: args()) -> {ok, record(listener_state)}.
%% @doc
%% Creates listen socket and starts an asynchronous accept.
%% @end
%% -----------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    ?DEBUGP("~p:init/1~n", [?MODULE]),

    %% arguments
    Port      = proplists:get_value(port, Args, ?DEFAULT_PORT),
    TcpOpts   = proplists:get_value(tcp_opts, Args, ?TCP_OPTS),
    Module    = proplists:get_value(module, Args),
    ServerRef = proplists:get_value('$gen_tcp_listener_server_ref', Args),

    %% fire off the asynchronous acceptor
    case gen_tcp:listen(Port, TcpOpts) of
        {ok, ListenSocket} ->
            {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
            {ok, #listener_state{listener   = ListenSocket,
                                 acceptor   = Ref,
                                 server_ref = ServerRef,
                                 module     = Module}};
        {error, Reason} ->
            {stop, Reason}
    end.



%% ----------------------------------------------------------------------------
-spec terminate(_, record(listener_state)) -> ok.
%% @doc
%% Shutdown the server, close the listening socket.
%% @end
%% ----------------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#listener_state.listener),
    ok.



%% ----------------------------------------------------------------------------
-spec handle_cast({accept, ClientSocket :: port()},
                  State :: record(listener_state)) -> {noreply, term()};
      ({shutdown, Reason :: term()},
       State :: record(listener_state)) -> ok.
%% @doc
%% Handles asynchronous calls to gen_tcp_listener.
%%
%% Messages handled are accept and shutdown.
%% @end
%% ----------------------------------------------------------------------------
handle_cast({accept, ClientSocket},
            State = #listener_state{module = Module,
                                    server_ref = ServerRef}) ->
    ?DEBUGP("handle_cast/2, accept~n"),
    {ok, Pid} = gen_tcp_listener_sup:start_client(ServerRef, ClientSocket),
    {hum, ok} = {hum, gen_tcp:controlling_process(ClientSocket, Pid)},
    Module:handle_accept(Pid, ClientSocket),
    %gen_tcp_acceptor:accept(Pid, ClientSocket),
    {noreply, State};

handle_cast({shutdown, Reason}, State) ->
    gen_server:terminate({shutdown, Reason}, State);

handle_cast(Msg, State = #listener_state{module = Module}) ->
    Module:handle_cast(Msg, State).



%% ----------------------------------------------------------------------------
-spec handle_info({inet_async, ListenSocket :: port(), Ref :: port(),
                  {ok, ClientSocket :: port()}}, record(listener_state)) -> ok.
%% @doc
%% Spawns a new connection handler (server) for accepted asynchronous
%% connection.
%% @end
%% ----------------------------------------------------------------------------
handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}},
            State = #listener_state{module     = Module,
                                    listener   = ListenSocket,
                                    acceptor   = Ref,
                                    server_ref = ServerRef}) ->
    ?DEBUGP("inet_async~n"),
    try
        case transfer_sockopt(ListenSocket, ClientSocket) of
            ok              -> ok;
            {error, Reason} -> exit({transfer_sockopt, Reason})
        end,

        ?DEBUGP("handle (hand over) incoming connection~n"),
        %gen_tcp_listener:accept(self(), ClientSocket),
        %gen_server:cast(self(), {accept, ClientSocket}),
        %ok = gen_tcp:controlling_process(ClientSocket, self()),
        {ok, Pid} = gen_tcp_listener_sup:start_client(ServerRef, ClientSocket),
        Module:handle_accept(self(), ClientSocket),
        ok = gen_tcp:controlling_process(ClientSocket, Pid),
        ?DEBUGP("new async acceptor~n"),

        case prim_inet:async_accept(ListenSocket, -1) of
            {ok, NewRef}    -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#listener_state{acceptor = NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.~n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error},
            State = #listener_state{listener = ListenSocket, acceptor = Ref}) ->
    error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.



%% ----------------------------------------------------------------------------
%%
%% INTERNAL
%%
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
-spec transfer_sockopt(ListenSocket :: port(), ClientSocket :: port()) ->
    ok | term().
%% @doc
%% Transfer socket options from listen socket to client socket.
%% @end
%% ----------------------------------------------------------------------------
transfer_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case {getopts, prim_inet:getopts(ListenSocket,
                                     [active, nodelay, keepalive,
                                      delay_send, priority, tos])} of
        {getopts, {ok, TcpOpts}} ->
            case {setopts, prim_inet:setopts(ClientSocket, TcpOpts)} of
                {setopts, ok}    -> ok;
                {setopts, Error} ->
                    gen_tcp:close(ClientSocket),
                    Error
            end;
        {getopts, Error} ->
            gen_tcp:close(ClientSocket),
            Error
    end.


%% ----------------------------------------------------------------------------
%%
%% UNUSED GEN_SERVER EXPORTS
%%
%% ----------------------------------------------------------------------------
handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
