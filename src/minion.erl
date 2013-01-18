%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 17 Jan 2013 by Andreas Pauley <>

-module(minion).

%% This function is used to spawn a minion
-export([aye_dark_overlord/1]).

%% These exports are for direct minion commands
-export([sing/0, sing/1, minion_info/0]).

aye_dark_overlord(MasterPID) ->
  %% By Overlord decree, in the unlikely event that a master dies, all minions must commit suicide out of respect.
  _Ref = erlang:monitor(process, MasterPID),

  Text = lists:flatten(io_lib:format("~p (~p) Aye, Dark Overlord who lives at ~p~n",[node(), self(), MasterPID])),
  io:format(Text),
  erlang:display_string(Text),
  minion_wait(MasterPID).

minion_wait(MasterPID) ->
  receive
    Message ->
      minion_message_handler(Message, MasterPID)
  after 5 ->
      minion_wait(MasterPID)
  end.

minion_message_handler(_Message=minion_info, MasterPID) ->
  Info = minion_info(),
  MasterPID ! Info,
  minion_wait(MasterPID);
minion_message_handler(_Message=sing, MasterPID) ->
  sing(),
  minion_wait(MasterPID);
minion_message_handler(_Message={'DOWN', _Ref, process, DownMasterPid, _Reason}, _MasterPid) ->
  %% If my parent dies, I too see no reason to live.
  SuicideNote = lists:flatten(io_lib:format("~p ~p My master (~p) died of reason '~p' ;-(. I too see no reason to live...~n",
                                            [node(), self(), DownMasterPid, _Reason])),
  erlang:display_string(SuicideNote),
  erlang:exit(shutdown);
minion_message_handler(Message, MasterPID) ->
  io:format("[~p] ~p ~p unknown message: ~p~n",[?MODULE, node(), self(), Message]),
  MasterPID ! {unknown_message, Message, minion_info()},
  minion_wait(MasterPID).

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
  Cmd = "osascript -e 'set Volume 6'; say -v cello " ++ Song,
  os:cmd(Cmd);
sing(_Lines, _OSType) ->
  ok.

display_song(Lines) ->
  [erlang:display_string(Line++"\n") || Line <- Lines].

minion_info() ->
  OTPVersion = erlang:system_info(otp_release),
  OS = os:type(),
  {node(), self(), OTPVersion, OS}.
