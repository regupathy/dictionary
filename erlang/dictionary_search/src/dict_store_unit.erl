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
-export([start_link/1,conversation/1,load_and_search/2]).

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
-spec conversation(PhoneNumber::integer()) -> {TimeInMilliSeconds::timer:time(),dictionary:word()}.
conversation(PhoneNumber) ->
  {TimeInMicroSeconds,Result} = timer:tc(gen_server,call,[?SERVER,{process,PhoneNumber}]),
  {TimeInMicroSeconds/1000,Result}.

-spec load_and_search(FileName::filePath(),PhoneNumber::integer())->{MilliSeconds::timer:time(),dictionary:word()}.
load_and_search(FileName,PhoneNumber) ->
  gen_server:cast(?SERVER,clear_dictionary),
  {TimeInMicroSeconds,Result} = timer:tc(fun() -> gen_server:call(?SERVER,{load_and_search,FileName,PhoneNumber}) end),
  {TimeInMicroSeconds/1000,Result}.

-spec(start_link(FileName::filePath()) ->  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(FileName) ->  gen_server:start_link({local, ?SERVER}, ?MODULE, [FileName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FilePath]) ->
%%  Loading Dictionary words from File say dictionary.txt
  io:format("Loading Dictionary is started......"),
  {TimeInMicroSeconds,Dict} = timer:tc(fun() -> load(FilePath) end),
  io:format("Loading Dictionary is conmpleted with in ~p ms......",[TimeInMicroSeconds/1000]),
  {ok, #state{dictionary = Dict}}.

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
%% Resolution :-
%%  1. create two processes one for reading and other for adding entry to Dictionary. both should happen concurrently.
%%  2. create a custom steaming file reader to send portion of data to the Dictionary add operation.
%%
-spec load(FilePath::filePath()) -> {ok,Dictionary::dictionary:dictionary()}.
load(FilePath) ->
  case file:open(FilePath, [read]) of
    {ok, Device}  -> get_all_lines(Device,[]);
    {error,enoent} -> io:format("File : ~p is not found ~n",[FilePath])
  end.

get_all_lines(Device,Dict) ->
  case catch file:read_line(Device) of
    eof  -> io:format("Loading Dictionary is done"),file:close(Device),Dict;
    {ok,Word} -> get_all_lines(Device,dictionary:add(string:trim(Word,trailing),Dict));
    {Type,Reason} -> io:format("Problem occured while Reading file that is ~p",[{Type,Reason}])
  end.

%%%-------------------------------------------------------------------
%%% Controlling Two process
%%%-------------------------------------------------------------------

-spec custom_read(FilePath::filePath(),Count::non_neg_integer(),Dict::dictionary:dictionary()) ->
      {ok,Dictionary::dictionary:dictionary()}.
custom_read(FilePath,Count,Dictionary) ->
  case file:read_file(FilePath) of
    {ok,Bin} ->
      {LoopPid, Ref1} = spawn_monitor(fun()-> loop(Dictionary) end),
      {SlicePid,Ref2} = spawn(fun() -> slice(Bin,Count,LoopPid) end),
      receive
        {ok,LoopPid,Dict} -> {ok,Dict};
        {'DOWN', Ref1, process, LoopPid, Why} ->
          io:format("Not able to complete the Dictionary loading operation due to : ~p~n ",[Why]),
          exit(SlicePid),
          {ok,Dictionary};
        {'DOWN', Ref2, process, SlicePid, Why} ->
          io:format("Not able to complete the Dictionary loading operation due to : ~p~n ",[Why]),
          exit(LoopPid),
          {ok,Dictionary}
      end
  end.

%%%-------------------------------------------------------------------
%%%  Adding Words to Dictionary
%%%-------------------------------------------------------------------

loop(Dictionary) -> loop(look(),Dictionary).

loop({eof,[]},Dict) -> erlang:send(?SERVER, {ok,self(),Dict});
loop({eof,Word},Dict) -> dictionary:add(Word,Dict);
loop({continue,Word},Dict) -> loop(look(),dictionary:add(Word,Dict)).

look() -> receive X -> X after 100 -> {eof,[]} end.

%%%-------------------------------------------------------------------
%%% Slice the Words from File Binary content
%%%-------------------------------------------------------------------

slice(Bin,Count,Reporter) -> slice(Bin,<<>>,[],0,{Count,Reporter}).

slice(<<"">>,<<>>,L,_,{_,Reporter}) ->  Reporter ! {eof,L};
slice(<<"">>,Temp,L,_,{_,Reporter}) ->  Reporter ! {eof,[Temp|L]};
slice(<<"\r\n",Rest/binary>>,<<>>,L,Count,Repo) -> slice(Rest,<<>>,L,Count,Repo);
slice(<<"\r\n",Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,<<>>,[binary_to_list(Temp)|L],Count+1,Repo);
slice(<<" ",Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,Temp,L,Count,Repo);
slice(Binary,Temp,L,Count,{Count,Repo}) -> erlang:send(Repo,{continue,L}),slice(Binary,Temp,[],0,{Count,Repo});
slice(<<H/utf8,Rest/binary>>,Temp,L,Count,Repo) -> slice(Rest,<<Temp/binary,H/utf8>>,L,Count,Repo).








