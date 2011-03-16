-module(dummy_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(dummy_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting dummy_app application...~n"),
    dummy_app_sup:start_link().

stop(_State) ->
    ok.
