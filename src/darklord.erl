%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 18 Jan 2013 by Andreas Pauley <>

%% The dark lord can has to give each command directly, unlike the hypnosponge.

-module(darklord).

-export([minion_info/0, sing/0]).

minion_info() ->
  darklord_utils:multicall(minion_info).

sing() ->
  Line1 = "Your node now belongs to me",
  Line2 = "it is mine",
  Line3 = "you are screwed",
  Line4 = Line1 ++ " yes I will make it mine!!!",

  Lines = [Line1, Line2, Line3, Line4],
  darklord_utils:multicall(sing, [Lines]).
