%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 17 Jan 2013 by Andreas Pauley <>

-module(minion).

-export([aye_dark_overlord/1]).
-export([sing/0]).

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
  sing(),
  ?MODULE:FunctionName(MasterPID);
minion_message_handler(Message, MasterPID, FunctionName) ->
  io:format("[~p] ~p unknown message: ~p~n",[?MODULE, FunctionName, Message]),
  ?MODULE:FunctionName(MasterPID).


sing() ->
  Line1 = "Your node now belongs to me",
  Line2 = "it is mine",
  Line3 = "you are screwed",
  Line4 = Line1 ++ " yes I will make it mine!!!",

  Lines = [Line1, Line2, Line3, Line4],

  display_song(Lines),

  Song = string:join(Lines, " "),
  Cmd = "osascript -e 'set Volume 5'; say -v cello " ++ Song,
  os:cmd(Cmd).

display_song(Lines) ->
  [erlang:display(Line) || Line <- Lines].
