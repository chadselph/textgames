-module(textgames_sms_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, Options) ->
    {ok, Req, Options}.

handle(Req, PlayerModule) ->
  {ok, PostData, _} = cowboy_req:body_qs(Req),
  Input = string_param(<<"Body">>, PostData, fun string:to_lower/1),
  Player = {phone, string_param(<<"From">>, PostData)},
  {ok, Req2} = case Input of
    undefined ->
      cowboy_req:reply(400, [], "Bad request.", Req);
    Move ->
      do_move(Req, PlayerModule, Player, Move)
  end,
  {ok, Req2, PlayerModule}.

string_param(Key, PostData) ->
  string_param(Key, PostData, fun (X) -> X end).

string_param(Key, PostData, Filter) ->
  case proplists:get_value(Key, PostData) of
    undefined ->
      undefined;
    Other when is_binary(Other) ->
      Filter(binary_to_list(Other));
    Else ->
      Filter(Else)
  end.

do_move(Req, PlayerModule, PlayerRef, Move) ->
  GPid = case textgames_store:get_player(PlayerRef) of
    {ok, Pid} ->
      Pid;
    {error, nogame} ->
      {ok, Pid} = textgames_player_sup:start_child([PlayerRef]),
      textgames_store:add_player([PlayerRef], Pid, undefined),
      Pid
  end,
  % XXX: make this a cast?
  Resp = PlayerModule:make_move(GPid, Move),
  twiml_cowboy_response(Req, Resp).

twimlsms(Texts) ->
% Texts is a list of messages that should each be surrounded by <Sms></Sms>
  Children = [{'Sms', [], [[Text]]} || Text <- Texts],
  xmerl:export_simple([{'Response', [], Children}], xmerl_xml).

% twiml_cowboy_response works when the 2nd arg is a string or a list of strings
twiml_cowboy_response(Req, [First|_Rest]=Messages) when is_list(First) ->
  Twiml = twimlsms(Messages),
  cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}], Twiml, Req);

twiml_cowboy_response(Req, Resp) ->
  twiml_cowboy_response(Req, [Resp]).


terminate(_Req, _State) ->
  ok.


