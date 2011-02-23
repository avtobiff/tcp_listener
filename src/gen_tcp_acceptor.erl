%% -----------------------------------------------------------------------------
%% Copyright © 2010 Per Andersson
%%
%% This file is part of tcp_listener.
%%
%% tcp_listener is free software: you can redistribute it and/or
%% modify it under the terms of the GNU General Public License as published by
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
%% Generic TCP Acceptor
%%
%% This is a behaviour that implements an asynchronous TCP acceptor server.
%% The server implementation then only needs to implement start_link/0, init/1,
%% and handle_accept/2.
%%
%% See tcp_listener, gen_server(3), and gen_tcp(3) for more information.
%%
%% Inspired by
%% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
%% http://github.com/erlware/gen_socket
%% http://github.com/kaos/gen_listener_tcp
%% @end
%%
%% -----------------------------------------------------------------------------
-module(gen_tcp_acceptor).
-author('Per Andersson <avtobiff@gmail.com>').

-behaviour(gen_server).

-include("tcp_listener.hrl").

%% API
-export([behaviour_info/1]).
-export([start_link/3, accept/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record('$gen_tcp_acceptor_state', {module,
                                    client_socket,
                                    client_state}).



%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec behaviour_info(callbacks) -> list(tuple());
      (_) -> undefined.
%% @doc
%% The gen_tcp_acceptor behaviour.
%% @end
%% -----------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{start_link, 0},
     {init, 1},
     {handle_accept, 2}];
behaviour_info(_) ->
    undefined.


%% -----------------------------------------------------------------------------
-spec start_link(Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.
%% @doc
%% Starts a new gen_tcp_acceptor as part of a supervisor tree.
%%
%% Mod is the behaviour implementation module, Args and Opts are passed to the
%% behaviour.
%% @end
%% -----------------------------------------------------------------------------
start_link(Mod, Args, Opts) ->
    gen_server:start_link(?MODULE,
                          [{'$gen_tcp_acceptor_module', Mod}|Args], Opts).


%% -----------------------------------------------------------------------------
-spec accept(ServerRef :: {local, string()} | {global, string()} | pid(),
             ClientSocket :: port()) -> ok.
%% @doc
%% Makes the gen_tcp_acceptor accept incoming connections on ClientSocket.
%% @end
%% -----------------------------------------------------------------------------
accept(ServerRef, ClientSocket) ->
    ?DEBUGP("accept/2 ~p~n", [ServerRef]),
    gen_server:cast(ServerRef, {accept, ClientSocket}).



%% =============================================================================
%% GEN_SERVER EXPORTS
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec init(Args :: list(term())) ->
          Result :: {ok, record('$gen_tcp_acceptor_state')}.
%% @private
%% @doc
%% Initializes the gen_tcp_acceptor behaviour.
%% @end
%% -----------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    ?DEBUGP("init/1~n"),
    Module = proplists:get_value('$gen_tcp_acceptor_module', Args),
    {ok, ClientState} = Module:init(Args),
    {ok, #'$gen_tcp_acceptor_state'{module = Module, client_state = ClientState}}.


%% -----------------------------------------------------------------------------
-spec handle_cast({accept, ClientSocket :: port()},
                  State :: record('$gen_tcp_acceptor_state')) ->
          Result :: {noreply, record('$gen_tcp_acceptor_state')}
                  | {noreply, record('$gen_tcp_acceptor_state'),
                     Timeout :: pos_integer()}
                  | {noreply, record('$gen_tcp_acceptor_state'), hibernate}
                  | {stop, Reason :: term(), record('$gen_tcp_acceptor_state')}.
%% @private
%% @doc
%% Handles the asynchronous accept by calling the behaviour implementation
%% module's handle_accept function.
%% @end
%% -----------------------------------------------------------------------------
handle_cast({accept, ClientSocket},
            State0 = #'$gen_tcp_acceptor_state'{module = Module,
                                                client_state = ClientState}) ->
    ?DEBUGP("handle_cast/2~n"),
    State1 = State0#'$gen_tcp_acceptor_state'{client_socket = ClientSocket},
    case Module:handle_accept(ClientSocket, ClientState) of
        {noreply, ClientState} ->
            {noreply, State1#'$gen_tcp_acceptor_state'{client_state = ClientState}};
        {noreply, ClientState, Timeout} ->
            {noreply, State1#'$gen_tcp_acceptor_state'{client_state = ClientState},
             Timeout};
        {noreply, ClientState, hibernate} ->
            {noreply, State1#'$gen_tcp_acceptor_state'{client_state = ClientState},
             hibernate};
        {stop, Reason, ClientState} ->
            {stop, Reason,
             State1#'$gen_tcp_acceptor_state'{client_state = ClientState}}
    end.


%% -----------------------------------------------------------------------------
-spec terminate(_, record('$gen_tcp_acceptor_state')) -> ok.
%% @private
%% @doc
%% Shutdown the behaviour and cleanup sockets that might not have been closed.
%% @end
%% -----------------------------------------------------------------------------
terminate(_, #'$gen_tcp_acceptor_state'{client_socket = ClientSocket}) ->
    (catch gen_tcp:close(ClientSocket)),
    ok.


%% -----------------------------------------------------------------------------
-spec code_change(_, State :: record('$gen_tcp_acceptor_state'), _) ->
          {ok, State :: record('$gen_tcp_acceptor_state')}.
%% @private
%% @doc
%% Handles upgrade of internal state upon code change.
%% @end
%% -----------------------------------------------------------------------------
code_change(_, State, _) -> {ok, State}.


%% -----------------------------------------------------------------------------
-spec handle_call(Request :: term(), _,
                  State :: record('$gen_tcp_acceptor_state')) ->
          Result :: {stop, {unknown_call, Request :: term()},
                     State :: record('$gen_tcp_acceptor_state')}.
%% @private
%% @doc
%% Unhandled call.
%% @end
%% -----------------------------------------------------------------------------
handle_call(Request, _, State) -> {stop, {unknown_call, Request}, State}.


%% -----------------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: record('$gen_tcp_acceptor_state')) ->
          Result :: {stop, {unknown_call, Request :: term()},
                     State :: record('$gen_tcp_acceptor_state')}.
%% @private
%% @doc
%% Unhandled info.
%% @end
%% -----------------------------------------------------------------------------
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.
