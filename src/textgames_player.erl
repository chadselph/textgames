%% Copyright
-module(textgames_player).
-author("chadrs").

%% API
-export([behaviour_info/1, async_send/2]).

behaviour_info(callbacks) ->
  [{start_link, 1}, {make_move, 2}];
behaviour_info(_) -> undefined.

async_send(PlayerSpec, Message) ->
  case PlayerSpec of
    {websocket, Pid} ->
      % cowboy websokets...
      Pid ! Message;
    {phone, PhoneNumber} ->
      % twilio
      io:format("send sms here...", []);
    {_Other, Pid} ->
      % if it's an adapter we don't know about yet
      % just assume we have a pid and try sending
      % the message.
      Pid ! Message
  end.
