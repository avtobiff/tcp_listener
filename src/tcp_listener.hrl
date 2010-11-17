%% DEBUG
-define(DEBUG, true).
-ifdef(DEBUG).
-define(DEBUGP(Format), error_logger:info_msg("~w ~w (~w) " ++ Format,
                                              [self(), ?MODULE, ?LINE])).
-define(DEBUGP(Format, Args), error_logger:info_msg("~w ~w (~w) " ++ Format,
                                                    [self(), ?MODULE,
                                                     ?LINE|Args])).
-else.
-define(DEBUGP(_Format), ok).
-define(DEBUGP(_Format, _Args), ok).
-endif.


%% listener state
-record(listener_state, {listener   :: port(),    % listen socket
                         acceptor   :: port(),    % accepted socket reference
                         server_ref :: atom(),    % socket handler server ref
                         module     :: atom()}).  % socket handler module


%% listener defaults
-define(DEFAULT_PORT, 8000).
-define(TCP_OPTS, [binary,
                   {packet, raw},
                   {active, false},
                   {reuseaddr, true},
                   {keepalive, true},
                   {backlog, 30}]).


%% listener types
-type arg() :: {port, pos_integer()}            % listener port
             | {tcp_opts, list(term())}         % TCP options
             | {module, atom()}.                % server callback module

-type args() :: list(arg()).


%% supervisor definitions
-define(SUP_MAX_RESTART, 60).
-define(SUP_MAX_TIME,     5).
-define(SUP_TIMEOUT,   2000).
