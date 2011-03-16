-module(helper_app_server).

-behaviour(gen_server).

%% API
%% API
-export([ start_link/0
        , num_to_comment/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {num_pokes = 0, num_prods = 0}).

%% API
start_link()      -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

num_to_comment(N) -> gen_server:call(?MODULE, {num_to_comment, N}).

%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({num_to_comment, N}, _From, State) ->
    Reply = comment(N),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Upgrade from 2
code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code_change in helper app server~n"),
    {ok, State}.

%% Note downgrade code_change not implemented
       

%%% Internal functions

comment(N) when is_integer(N), N rem 2 =:= 0 -> "An even number!";
comment(_) -> "An odd number!".