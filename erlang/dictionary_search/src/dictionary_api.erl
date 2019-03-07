%%%-------------------------------------------------------------------
%%% @author regupathy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Mar 2019 7:12 AM
%%%-------------------------------------------------------------------
-module(dictionary_api).
-author("regupathy").

%% API
-export([conversation/1]).

-define(SERVER,dict_store_unit).

%%%===================================================================
%%% API
%%%===================================================================

-spec conversation(PhoneNumber::integer()) -> {TimeInMilliSeconds::timer:time(),dictionary:word()}.
conversation(PhoneNumber) ->
  {TimeInMicroSeconds,Result} = timer:tc(gen_server,call,[?SERVER,{process,PhoneNumber}]),
  {TimeInMicroSeconds/1000,Result}.


%%
%%-spec load_and_search(FileName::dict_store_unit:filePath(),PhoneNumber::integer())->{MilliSeconds::timer:time(),dictionary:word()}.
%%load_and_search(FileName,PhoneNumber) ->
%%  gen_server:cast(?SERVER,clear_dictionary),
%%  {TimeInMicroSeconds,Result} = timer:tc(fun() -> gen_server:call(?SERVER,{load_and_search,FileName,PhoneNumber}) end),
%%  {TimeInMicroSeconds/1000,Result}.
