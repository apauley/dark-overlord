%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 17 Jan 2013 by Andreas Pauley <>

-module(minion).

-export([start_link/1,
         init/1]).

%% These exports are for direct minion commands
-export([sing/0, sing/1, minion_info/0]).

-define(DERANGEDLAUGH_RANDOM_MAX, 30000).


%% ------------------------------------------------------------------
%% Startup Functions
%% ------------------------------------------------------------------

start_link(HypnoSpongePid) ->
  Pid = proc_lib:spawn_link(?MODULE, init, [HypnoSpongePid]),
  {ok, Pid}.

init(HypnoSpongePid) ->
  %% By Overlord decree, in the unlikely event that a hypnosponge dies, all minions must commit suicide out of respect.
  _Ref = erlang:monitor(process, HypnoSpongePid),

  ok = report_for_duty(HypnoSpongePid),

  random:seed(now()),
  minion_wait(HypnoSpongePid).

%% ------------------------------------------------------------------
%% API Functions
%% ------------------------------------------------------------------

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


minion_info() ->
  OTPVersion = erlang:system_info(otp_release),
  OS = os:type(),
  {node(), self(), OTPVersion, OS}.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

report_for_duty(HypnoSpongePid) ->
  log("Aye, Dark Overlord whith PID ~p~n", [HypnoSpongePid]),
  HypnoSpongePid ! {aye_dark_overlord, self(), node()},
  ok.

minion_wait(HypnoSpongePid) ->
  random_deranged_laugh(),
  receive
    Message ->
      minion_message_handler(Message, HypnoSpongePid)
  after 5 ->
      minion_wait(HypnoSpongePid)
  end.

minion_message_handler(_Message=minion_info, HypnoSpongePid) ->
  Info = minion_info(),
  HypnoSpongePid ! Info,
  minion_wait(HypnoSpongePid);
minion_message_handler(_Message=crash_you_worthless_fool, HypnoSpongePid) ->
  %% Cause a badmatch
  HypnoSpongePid = yes_master_my_life_is_in_your_hands;
minion_message_handler(_Message={exit, Reason}, _HypnoSpongePid) ->
  erlang:exit(Reason);
minion_message_handler(_Message=sing, HypnoSpongePid) ->
  sing(),
  minion_wait(HypnoSpongePid);
minion_message_handler(_Message={'DOWN', _Ref, process, DownPid, _Reason}, _HypnoSpongePid) ->
  %% If my parent dies, I too see no reason to live.
  log("My sponge (~p) died of reason '~p' ;-(. I too see no reason to live...~n",
      [DownPid, _Reason]),
  erlang:exit(shutdown);
minion_message_handler(Message, HypnoSpongePid) ->
  log("~p ~p unknown message: ~p~n",[node(), self(), Message]),
  HypnoSpongePid ! {unknown_message, Message, minion_info()},
  minion_wait(HypnoSpongePid).

random_deranged_laugh() ->
  random_deranged_laugh(random:uniform(?DERANGEDLAUGH_RANDOM_MAX)).

random_deranged_laugh(?DERANGEDLAUGH_RANDOM_MAX) ->
  deranged_laugh();
random_deranged_laugh(_OtherRandomInt) ->
  ok.

deranged_laugh() ->
  deranged_laugh(os:type()).

deranged_laugh(_OSType={unix,darwin}) ->
  Cmd = "osascript -e 'set Volume 2'; say -v Deranged hahahaha",
  os:cmd(Cmd);
deranged_laugh(_OSType) ->
  ok.

display_song(Lines) ->
  [erlang:display_string(Line++"\n") || Line <- Lines].

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
