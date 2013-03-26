%% Copyright
-module(textgames_player).
-author("chadrs").

%% API
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{start_link, 1}, {make_move, 2}];
behaviour_info(_) -> undefined.
