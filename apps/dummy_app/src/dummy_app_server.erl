-module(dummy_app_server).

-behaviour(gen_server).

%% API
-export([start_link/0, poke/0, num_pokes/0, poke_twice/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {num_pokes = 0}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

poke() ->
    gen_server:call(?MODULE, poke).

poke_twice() ->
    gen_server:call(?MODULE, poke_twice).

num_pokes() ->
    gen_server:call(?MODULE, num_pokes).


%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call(num_pokes, _From, State = #state{ num_pokes = PokeCount }) ->
    {reply, PokeCount, State};

handle_call(poke_twice, _From, State) ->
    NewPokeCount = State#state.num_pokes + 2,
    NewState     = State#state{num_pokes = NewPokeCount},
    Reply        = {ok, NewPokeCount},
    {reply, Reply, NewState};

handle_call(poke, _From, State) ->
    NewPokeCount = State#state.num_pokes + 1,
    NewState     = State#state{num_pokes = NewPokeCount},
    Reply        = {ok, NewPokeCount},
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    error_logger:info_msg("code_change, oldvsn:~p state:~p extra:~p~n",
                          [OldVsn, State, Extra]),
    {ok, State}.

%%% Internal functions
