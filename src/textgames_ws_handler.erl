%% Copyright
-module(textgames_ws_handler).
-author("chadrs").

-behaviour(cowboy_websocket_handler).

%% API
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-record(state, {player_ref, player_pid, module}).


init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, PlayerModule) ->
  Player = {websocket, self()},
  %% I suppose I don't really need to use textgame_store here.
  %% maybe if I had a cookie or something for disconnected users
  {ok, Pid} = textgames_player_sup:start_child([Player]),
  textgames_store:add_player([Player], Pid, undefined),
  {ok, Req, #state{player_ref=Player, player_pid=Pid, module=PlayerModule}}.

websocket_handle({text, Msg}, Req, State=#state{module=Mod, player_pid=Pid}) ->
  Resp = Mod:make_move(Pid, Msg),
  {reply, {text, Resp}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

