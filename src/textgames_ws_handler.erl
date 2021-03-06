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
  {ok, Pid} = textgames_player_sup:start_child([Player]),
  %% I suppose I don't really need to use textgame_store here.
  %% maybe if I had a cookie or something for disconnected users
  %textgames_store:add_player([Player], Pid, undefined),
  {ok, Req, #state{player_ref=Player, player_pid=Pid, module=PlayerModule}}.

websocket_handle({text, Msg}, Req, State=#state{module=Mod, player_pid=Pid}) ->
  case Mod:make_move(Pid, Msg) of
    ok -> {ok, Req, State};
    X when is_binary(X) -> {repy, {text, X}, Req, State};
    X when is_list(X) ->
      Reply = unicode:characters_to_binary(X, unicode, utf8),
      {reply, {text, Reply}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(Sent, Req, State) ->
  {reply, {text, Sent}, Req, State}.

websocket_terminate(_Reason, _Req, #state{player_ref=_Ref}) ->
  %textgames_score:delete_player(Ref),
  ok.

