-module(helper_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(helper_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting helper_app application...~n"),
    helper_app_sup:start_link().

stop(_State) ->
    ok.
