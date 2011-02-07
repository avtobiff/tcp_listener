%% -----------------------------------------------------------------------------
%% Copyright @ 2010 Per Andersson
%%
%% This file is part of tcp_listener.
%%
%% tcp_listener is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%%
%% tcp_listener is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with tcp_listener.  If not, see
%% <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%%
%% @author Per Andersson <avtobiff@gmail.com>
%% @doc
%% TCP Listener
%%
%% This software implements an asynchronous TCP listener.
%%
%% The server implementation then only needs to implement the gen_tcp_acceptor
%% behaviour.
%%
%% See gen_tcp_acceptor, gen_server(3), and gen_tcp(3) for more information.
%%
%% Inspired by
%% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
%% http://github.com/erlware/gen_socket
%% http://github.com/kaos/gen_listener_tcp
%% @end
%%
%% -----------------------------------------------------------------------------
-module(tcp_listener).
-author('Per Andersson <avtobiff@gmail.com>').

-behaviour(gen_server).

-include("tcp_listener.hrl").

%% API
-export([start_link/3, start_link/4]).

%% gen_server exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec start_link(Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.
-spec start_link(Name :: atom(), Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.
%% @doc
%% Starts the tcp_listener as part of a supervisor tree.
%%
%% Parses options from Args and uses defaults if an argument is not supplied.
%% @end
%% -----------------------------------------------------------------------------
start_link(Mod, Args, Opts) ->
    start_link({local, Mod}, Mod, Args, Opts).

start_link(Name, Mod, Args, Opts) ->
    ?DEBUGP("start_link/4~n"),
    supervisor:start_link(Name, tcp_listener_sup,
                          [{'$tcp_listener_server_ref', Name},
                           {'$tcp_listener_opts', Opts},
                           {'$tcp_listener_module', Mod}|Args]).



%% =============================================================================
%% GEN_SERVER EXPORTS
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec init(Args :: args()) -> {ok, record(listener_state)}.
%% @private
%% @doc
%% Creates listen socket and starts an asynchronous accept.
%% @end
%% -----------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    ?DEBUGP("init/1~n"),

    %% arguments
    Port         = proplists:get_value(port, Args),
    ListenSocket = proplists:get_value(listen_socket, Args),
    TcpOpts      = proplists:get_value(tcp_opts, Args, ?TCP_OPTS),
    Module       = proplists:get_value('$tcp_listener_module', Args),
    ServerRef    = proplists:get_value('$tcp_listener_server_ref', Args),

    %% create listener state
    State0 = #listener_state{server_ref = ServerRef,
                             module     = Module},

    %% fire off the asynchronous acceptor
    case {Port, ListenSocket} of
        %% establish new listen socket
        {Port, undefined} when is_integer(Port) ->
            case gen_tcp:listen(Port, TcpOpts) of
                {ok, NewListenSocket} ->
                    State1 = State0#listener_state{listener = NewListenSocket},
                    {ok, create_async_acceptor(State1)};
                {error, Reason} ->
                    {stop, Reason}
            end;
        %% use supplied listen socket
        {undefined, ListenSocket} when is_port(ListenSocket) ->
            State1 = State0#listener_state{listener = ListenSocket},
            {ok, create_async_acceptor(State1)};
        {Port, ListenSocket} when is_integer(Port), is_port(ListenSocket) ->
            {stop, ambiguos_listen_socket};
        {_, _} ->
            {stop, bad_listen_socket}
    end.



%% ----------------------------------------------------------------------------
-spec terminate(_, record(listener_state)) -> ok.
%% @private
%% @doc
%% Shutdown the server, close the listening socket.
%% @end
%% ----------------------------------------------------------------------------
terminate(_Reason, #listener_state{listener = ListenSocket}) ->
    (catch gen_tcp:close(ListenSocket)),
    ok.



%% ----------------------------------------------------------------------------
-spec code_change(_, State :: record(listener_state), _) ->
          Result :: {ok, record(listener_state)}.
%% @private
%% @doc
%% Transforms servers internal state upon code change.
%% @end
%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ----------------------------------------------------------------------------
-spec handle_cast({shutdown, Reason :: term()},
                  State :: record(listener_state)) -> ok.
%% @private
%% @doc
%% Handles asynchronous calls to tcp_listener.
%%
%% Messages handled are shutdown.
%% @end
%% ----------------------------------------------------------------------------
handle_cast({shutdown, Reason}, State) ->
    gen_server:terminate({shutdown, Reason}, State).



%% ----------------------------------------------------------------------------
-spec handle_info({inet_async, ListenSocket :: port(), Ref :: port(),
                   {ok, ClientSocket :: port()}},
                  State :: record(listener_state)) ->
          Result :: {noreply, record(listener_state)};

      ({inet_async, ListenSocket :: port(), Ref :: port(), Error :: term()},
                  State :: record(listener_state)) ->
          Result :: {stop, Error :: term(), State :: record(listener_state)};

      ({'EXIT', _Pid :: pid(), normal}, State :: record(listener_state)) ->
          Result :: {stop, normal, State :: record(listener_state)};

      (Info :: term(), State :: record(listener_state)) ->
          Result :: {stop, {unknown_info, Info :: term()},
                     State :: record(listener_state)}.
%% @private
%% @doc
%% Spawns a new connection handler (server) for accepted asynchronous
%% connection.
%% @end
%% ----------------------------------------------------------------------------
handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}},
            State = #listener_state{listener   = ListenSocket,
                                    acceptor   = Ref,
                                    server_ref = ServerRef}) ->
    ?DEBUGP("inet_async~n"),
    try
        case transfer_sockopt(ListenSocket, ClientSocket) of
            ok              -> ok;
            {error, Reason} -> exit({transfer_sockopt, Reason})
        end,

        ?DEBUGP("handle (hand over) incoming connection~n"),
        {ok, Pid} = tcp_listener_sup:start_acceptor(ServerRef),
        gen_tcp_acceptor:accept(Pid, ClientSocket),
        ok = gen_tcp:controlling_process(ClientSocket, Pid),

        {noreply, create_async_acceptor(State)}
    catch exit:Error ->
        error_logger:error_msg("Error in async accept: ~p.~n", [Error]),
        {stop, Error, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error},
            State = #listener_state{listener = ListenSocket, acceptor = Ref}) ->
    error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
    {stop, Error, State};

handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.


%% ----------------------------------------------------------------------------
-spec handle_call(Request :: term(), _, State :: record(listener_state)) ->
          Result :: {stop,
                     {unknown_call, Request :: term()},
                     State :: record(listener_state)}.
%% @private
%% @doc
%% Unused gen_server function for handling synchronous calls.
%% @end
%% ----------------------------------------------------------------------------
handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.



%% ----------------------------------------------------------------------------
%% INTERNAL
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
-spec create_async_acceptor(State :: record(listener_state)) ->
          Result :: record(listener_state).
%% @private
%% @doc
%% Create asynchrounous acceptor.
%% @end
%% ----------------------------------------------------------------------------
create_async_acceptor(State = #listener_state{listener = ListenSocket}) ->
    ?DEBUGP("new async acceptor~n"),
    Ref =
        case prim_inet:async_accept(ListenSocket, -1) of
            {ok, NewRef}    -> NewRef;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,
    State#listener_state{acceptor = Ref}.


%% ----------------------------------------------------------------------------
-spec transfer_sockopt(ListenSocket :: port(), ClientSocket :: port()) ->
    ok | term().
%% @private
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
