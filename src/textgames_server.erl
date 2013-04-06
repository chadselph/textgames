-module(textgames_server).
-behaviour(application).
-export([start/2, stop/1, start/0]).
-define(DEFAULT_PORT, 8084).

start() ->
  application:start(ranch),
  application:start(crypto),
  application:start(cowboy),
  application:start(xmerl),
  application:start(mnesia),
  application:start(mimetypes),
  application:start(sasl),
  application:start(textgames_server).

start(_Type, _Args) ->
  PlayerModule = get_env_or_default(player_module, undefined),
  textgames_store:init(),
  Dispatch = [
    {'_', [
      {[<<"twiml">>, <<"sms">>], textgames_sms_handler, PlayerModule},
      {[<<"ws">>], textgames_ws_handler, PlayerModule},
      {[], cowboy_static, [
        {directory, "priv/static/"},
        {file, <<"index.html">>},
        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
      ]},
      {['...'], cowboy_static, [
        {directory, "priv/static/"},
        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
      ]}
    ]}
  ],
  cowboy:start_http(http, 10, [{port, get_env_or_default(http_port, ?DEFAULT_PORT)}], [{dispatch, Dispatch}]),
  textgames_player_sup:start_link(PlayerModule).


stop(_State) ->
  ok.

get_env_or_default(Opt, Default) ->
  case application:get_env(?MODULE, Opt) of
    {ok, Value} -> Value;
    _ -> Default
  end.
