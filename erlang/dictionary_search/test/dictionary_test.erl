%%%-------------------------------------------------------------------
%%% @author dharan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2019 10:23 PM
%%%-------------------------------------------------------------------
-module(dictionary_test).
-author("dharan").

-include_lib("eunit/include/eunit.hrl").


dictionary_one_test() ->
  {ok,Dict} = dict_store_unit:load("dictionary.txt"),


  {Time2,WordList2} = timer:tc(dictionary,search,[6686787825,Dict]),
  ?assert(Time2< 1000), %% checking time taken should be less than 1000ms
  ?assertMatch(["MOTORTRUCK"],hd(lists:reverse(WordList2))),
  ?assert(lists:member(["NOUNS", "USUAL"],WordList2)),
  ?assert(lists:member(["ONTO", "STRUCK"],WordList2)),
  ?assert(lists:member(["MOTOR", "USUAL"],WordList2)),


  {Time,WordList} = timer:tc(dictionary,search,[2282668687,Dict]),
  ?assert(Time< 1000), %% checking time taken should be less than 1000ms
  ?assertMatch(["CATAMOUNTS"],hd(lists:reverse(WordList))),
  ?assert(lists:member(["ACTA", "MOUNTS"],WordList)),
  ?assert(lists:member(["BAT", "CONTOUR"],WordList)),
  ?assert(lists:member(["ACT", "AMOUNTS"],WordList)).






