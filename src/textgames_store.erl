%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-05-23 10:50:26.467234
%%%-------------------------------------------------------------------
-module(textgames_store).


%% gen_server callbacks
-export([init/0, get_player/1, add_player/3, delete_player/1]).

-record(player_to_pid, {player, pid, game_id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connect to mnesia database for game data!
%%
%% @end
%%--------------------------------------------------------------------
init() ->
    mnesia:start(),
    mnesia:create_table(player_to_pid, [
        {attributes, record_info(fields, player_to_pid)}
    ]).

%%--------------------------------------------------------------------
%% @doc
%% Fetches the game by Player (phone number or other id)
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_player(Player) ->
    case mnesia:dirty_read(player_to_pid, Player) of
        [#player_to_pid{pid=Pid}] ->
            case is_pid_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, nogame}
            end;
        [] ->
            {error, nogame}
    end.

add_player(Players, Pid, GameId) ->
    Trans = fun() ->
        lists:map(fun(Player) -> mnesia:write(#player_to_pid{player=Player, pid=Pid, game_id=GameId}) end, Players)
    end,
    mnesia:transaction(Trans),
    {ok, Pid}.

delete_player(Player) ->
    case mnesia:dirty_read(player_to_pid, Player) of
        [#player_to_pid{} = Record] ->
            mnesia:dirty_delete_object(Record);
        _ ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
        false ->
            false;
        true ->
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                true ->
                    true;
                false ->
                    false;
                {badrpc, _Reason} ->
                    false
            end
    end.
