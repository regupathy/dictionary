%%%-------------------------------------------------------------------
%%% @author regupathy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2019 6:34 AM
%%%-------------------------------------------------------------------
-module(dict_store_unit).
-author("regupathy").

-behaviour(gen_server).

%% API
-export([start_link/1,slice/3,load/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {dictionary = []}).

-type filePath() ::string().

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(FileName::filePath()) ->  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(FileName) ->  gen_server:start_link({local, ?SERVER}, ?MODULE, [FileName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FilePath]) ->
%%  Loading Dictionary words from File say dictionary.txt
  io:format("Loading Dictionary is started......~n"),
  {TimeInMicroSeconds,Dict} = timer:tc(fun() -> load(FilePath) end),
  case Dict of
    {ok,[]} -> io:format("Loading Dictionary is not completed~n"),{ok, #state{dictionary = []}};
    {ok,Dictionary} -> io:format("Loading Dictionary is completed with in ~p ms~n",[TimeInMicroSeconds/1000]),
      {ok, #state{dictionary = Dictionary}}
  end.

handle_call({process,PhoneNumber}, _From, #state{dictionary = Dict}= State) ->
  {reply, dictionary:search(PhoneNumber,Dict), State};

handle_call({load_and_search,FileName,PhoneNumber}, _From, #state{dictionary = Dictionary} = State) ->
  NewDictionary = custom_read(FileName,100,Dictionary),
  {reply, dictionary:search(PhoneNumber,Dictionary), State#state{dictionary = NewDictionary}};

handle_call(_Request, _From, State) ->  {reply, not_handled, State}.

handle_cast(clear_dictionary, State) ->  {noreply, State#state{dictionary = []}};

handle_cast(_Request, State) ->  {noreply, State}.

handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, _State) ->  ok.

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc
%% Need to change the approach due the following reason
%% 1. load/2 method reads the content from file after that it will added into dictionary
%% 2. Not possible to accomplish both task 'loading words in dictionary' and 'conversation of phone number' within
%%    the given time bound(1000millisecond), The file:read_line/2 method itself taking time the average of 1050 ms
%%    to complete their task.
%%
%% Resolution I made:-
%%  1. create two processes one for reading and other for adding entry to Dictionary. both should happen concurrently.
%%  2. create a custom steaming file reader to send portion of data to the Dictionary add operation.
%%

-spec load(FilePath::filePath()) -> {ok,Dictionary::dictionary:dictionary()}.
load(FilePath) ->
  case file:open(FilePath, [read]) of
    {ok, Device}  -> {ok,get_all_lines(Device,[])};
    {error,Reason} -> io:format("Failed to read file : ~p due to :~p ~n",[FilePath,Reason]),{ok,[]}
  end.

get_all_lines(Device,Dict) ->
  case catch file:read_line(Device) of
    eof  -> file:close(Device),dictionary:remove_duplicate(Dict);
    {ok,Word} -> get_all_lines(Device,dictionary:add(string:trim(Word,trailing),Dict));
    {Type,Reason} -> io:format("Problem occured while Reading file that is ~p~n",[{Type,Reason}])
  end.

%%%-------------------------------------------------------------------
%%% Controlling Two process
%%%-------------------------------------------------------------------
%% This approach got failed due to sending message to other process mailbox.
%% it will occupy more memory of processes.
-spec custom_read(FilePath::filePath(),Count::non_neg_integer(),Dict::dictionary:dictionary()) ->
      {ok,Dictionary::dictionary:dictionary()}.
custom_read(FilePath,Count,Dictionary) ->
  case file:read_file(FilePath) of
    {ok,Bin} ->
      {LoopPid, Ref1} = spawn_monitor(fun()-> loop(Dictionary) end),
      {SlicePid,Ref2} = spawn_monitor(fun() -> slice(Bin,Count,LoopPid) end),
      receive
        {ok,LoopPid,Dict} -> {ok,Dict};
        {'DOWN', Ref1, process, LoopPid, Why} when Why /= normal ->
          io:format("Not able to complete the Dictionary loading operation due to : ~p~n ",[Why]),
          exit(SlicePid),
          {ok,Dictionary};
        {'DOWN', Ref2, process, SlicePid, Why} when Why /= normal ->
          io:format("Not able to complete the Dictionary loading operation due to : ~p~n ",[Why]),
          if Why =/= normal -> exit(LoopPid); true -> ok end,
          {ok,Dictionary}
      end
  end.

%%%-------------------------------------------------------------------
%%%  Adding Words to Dictionary
%%%-------------------------------------------------------------------

loop(Dictionary) -> loop(look(),Dictionary).

loop({eof,[]},Dict) -> erlang:send(?SERVER, {ok,self(),Dict});
loop({eof,Words},Dict) -> lists:foldr(fun(Word,D) -> dictionary:add(Word,D) end,Dict,Words);
loop({continue,Words},Dict) -> loop(look(),lists:foldr(fun(Word,D) -> dictionary:add(Word,D) end,Dict,Words)).

look() -> receive X -> X after 1000 -> {eof,[]} end.

%%%-------------------------------------------------------------------
%%% Slice the Words from File Binary content
%%%-------------------------------------------------------------------

slice(Bin,Count,Reporter) -> slice(Bin,<<>>,[],0,{Count,Reporter}).

slice(<<"">>,<<>>,L,_,{_,Reporter}) ->  Reporter ! {eof,L};
slice(<<"">>,Temp,L,_,{_,Reporter}) ->  Reporter ! {eof,[Temp|L]};
slice(<<"\n",Rest/binary>>,<<>>,L,Count,Repo) -> slice(Rest,<<>>,L,Count,Repo);
slice(<<"\n",Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,<<>>,[binary_to_list(Temp)|L],Count+1,Repo);
slice(<<" ",Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,Temp,L,Count,Repo);
slice(<<"\r",Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,Temp,L,Count,Repo);
slice(Binary,Temp,L,Count,{Count,Repo}) -> erlang:send(Repo,{continue,L}),slice(Binary,Temp,[],0,{Count,Repo});
slice(<<H/utf8,Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,<<Temp/binary,H/utf8>>,L,Count,Repo).







