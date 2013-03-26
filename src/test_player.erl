%%%%%%%%%
%%
%%   testplayer.erl
%%   Example of a "game" build on textgames. This game just increments a counter.
%%   Pretty fun game!!
%%%%%%%%%%
-module(test_player).
-author("chadrs").

-behaviour(gen_fsm).
-behaviour(textgames_player).

%% textgames_player interface
-export([start_link/1, make_move/2]).

%% gen_fsm interface
-export([init/1, counting/2, counting/3, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {player, counter = 0}).

start_link(PlayerRef) ->
  gen_fsm:start_link(?MODULE, #state{player=PlayerRef}, []).

make_move(Pid, MoveStr) ->
  gen_fsm:sync_send_event(Pid, {move, MoveStr}).

init(State) ->
  {ok, counting, State}.

counting(_Event, State) ->
  NewCount = State#state.counter + 1,
  {next_state, counting, State#state{counter=NewCount}}.

counting(Event, _From, State) ->
  {next_state, StateName, StateData} = counting(Event, State),
  Reply = io_lib:format("Counter: ~p", [State#state.counter]),
  {reply, Reply, StateName, StateData}.

handle_event(backdoor, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
