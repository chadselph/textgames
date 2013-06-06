%% Run this example with this command...
%%
%%  erl -pa ebin deps/*/ebin -s textgames_server -textgames_server http_port 7000 -textgames_server player_module dice_example
%%
-module(dice_example).
-author("chadrs").

%% API
-export([start_link/1, make_move/2]).
-behaviour(textgames_player).

% Don't really need to spawn anything for a stateless "game"
start_link(_Player_Ref) -> {ok, self()}.

make_move(_Pid, MoveStr) ->
  {Style, Count, Size} = read_input(binary_to_list(MoveStr)),
  print_roll(Style, roll_dice(Count, Size), 2).

roll_dice(HowMany, Sides) ->
  [random:uniform(Sides) || _ <- lists:seq(1, HowMany)].

print_roll(Style, Numbers, Spaces) ->
  Chars = lists:map( fun (N) -> unicode_die(Style, N) end, Numbers),
  io_lib:format("~ts~n", [space_out(Chars, Spaces)]).

unicode_die(dots, N) when N > 0 andalso N =< 6-> 16#267f + N;
unicode_die(digits, N) when N > 0 andalso N =< 50 -> 16#245f + N.

% This function is like string:to_integer but fast-forwards
% though non-digits, and returns a Default if nothing found.
read_number_or(Default, []) -> {Default, []};
read_number_or(Default, String) ->
  case string:to_integer(String) of
    {error, _} ->
      read_number_or(Default, lists:dropwhile(fun not_digit/1, String));
    X -> X
  end.

read_input(String) ->
  {Number, Rest} = read_number_or(5, String),
  case read_number_or(6, Rest) of
    {6, _} -> {dots, Number, 6};
    {Size, _} -> {digits, Number, Size}
  end.

% put spaces between all the characters in a string
space_out([], _) -> [];
space_out([X], _) -> [X];
space_out([X|Rest], NSpaces) -> [X|string:copies(" ", NSpaces)] ++ space_out(Rest, NSpaces).


not_digit(D) when D >= $0 andalso D =< $9 -> false;
not_digit(_) -> true.
