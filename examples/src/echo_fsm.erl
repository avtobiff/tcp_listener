-module(echo_fsm).

-behaviour(gen_fsm).

-include("echo_common.hrl").

%% gen_fsm
-export([init/1, code_change/4, terminate/3,
         handle_event/3, handle_info/3, handle_sync_event/4]).
-export(['WAIT_FOR_SOCKET'/2, 'WAIT_FOR_DATA'/2]).

-record(state_data, {socket}).

-define(TIMEOUT, 60000).



%% GEN_FSM
init(_) ->
    process_flag(trap_exit, true),
    ?PRINT("init/1~n"),
    {ok, 'WAIT_FOR_SOCKET', #state_data{}}.

terminate(_, _, StateData = #state_data{socket = Socket}) ->
    (catch gen_tcp:close(Socket)),
    {stop, normal, StateData}.

code_change(_, StateName, StateData, _) -> {ok, StateName, StateData}.
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
handle_sync_event(Event, _, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.


handle_info({tcp, Socket, Bin}, StateName,
            StateData = #state_data{socket = Socket}) ->
    % flow control: enable forwarding of next TCP packet
    ok = inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _, StateData = #state_data{socket = Socket}) ->
    {stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
    {stop, {StateName, unknown_info, Info}, StateData}.


%% STATES
'WAIT_FOR_SOCKET'({socket_ready, Socket}, StateData) when is_port(Socket) ->
    ?PRINT("WAIT_FOR_SOCKET/2...~n"),
    % set socket options: flow control, enable forwarding of next
    % TCP packet; receive raw packets as binaries
    ok = inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    {next_state,
     'WAIT_FOR_DATA', StateData#state_data{socket = Socket},
     ?TIMEOUT};

'WAIT_FOR_SOCKET'(_, StateData) ->
    {next_state, 'WAIT_FOR_SOCKET', StateData}.


'WAIT_FOR_DATA'({data, Bin}, StateData = #state_data{socket = Socket}) ->
    ?PRINT("WAIT_FOR_DATA/2 echo data~n"),
    % flow control, enable forwarding of next TCP packet
    ok = inet:setopts(Socket, [{active, once}]),
    % echo data
    gen_tcp:send(Socket, Bin),
    {next_state, 'WAIT_FOR_DATA', StateData, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, StateData) ->
    ?PRINT("WAIT_FOR_DATA/2 timeout~n"),
    % client timeout
    {stop, normal, StateData};

'WAIT_FOR_DATA'(_, StateData) ->
    % ignoring unknown input
    {next_state, 'WAIT_FOR_DATA', StateData, ?TIMEOUT}.
