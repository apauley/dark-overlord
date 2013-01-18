%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 17 Jan 2013 by Andreas Pauley <>

-module(minion).

%% This function is used to spawn a minion
-export([aye_dark_overlord/1]).

%% These exports are for direct minion commands
-export([sing/0, sing/1]).

aye_dark_overlord(MasterPID) ->
  Text = lists:flatten(io_lib:format("~p (~p) Aye, Dark Overlord who lives at ~p!~n",[node(), self(), MasterPID])),
  io:format(Text),
  erlang:display(Text),
  minion_wait(MasterPID).

minion_wait(MasterPID) ->
  receive
    Message ->
      minion_message_handler(Message, MasterPID, minion_wait)
  after 5 ->
      minion_wait(MasterPID)
  end.

minion_message_handler(_Message=sing, MasterPID, FunctionName) ->
  minion_utils:sing(),
  ?MODULE:FunctionName(MasterPID);
minion_message_handler(Message, MasterPID, FunctionName) ->
  io:format("[~p] ~p unknown message: ~p~n",[?MODULE, FunctionName, Message]),
  ?MODULE:FunctionName(MasterPID).

sing() ->
  Line1 = "My node now belongs to you",
  Line2 = "it is yours",
  Line3 = "I am screwed",
  Line4 = Line1 ++ " yes you shall cause despair!!!",

  Lines = [Line1, Line2, Line3, Line4],
  sing(Lines).

sing(Lines) ->
  display_song(Lines),
  sing(Lines, os:type()).

sing(Lines, _OSType={unix,darwin}) ->
  Song = string:join(Lines, " "),
  Cmd = "osascript -e 'set Volume 5'; say -v cello " ++ Song,
  os:cmd(Cmd);
sing(_Lines, _OSType) ->
  ok.

display_song(Lines) ->
  [erlang:display(Line) || Line <- Lines].
