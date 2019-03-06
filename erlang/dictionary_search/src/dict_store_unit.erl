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
-export([start_link/0,conversation/1,load_and_search/2]).

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
-spec conversation(PhoneNumber::integer()) -> dictionary:word().
conversation(PhoneNumber) -> gen_server:call(?SERVER,{process,PhoneNumber}).

load_and_search(FileName,PhoneNumber) ->
  gen_server:cast(?SERVER,clear_dictionary),
  {TimeInMicroSeconds,Result} = gen_server:call(?SERVER,{load_and_search,FileName,PhoneNumber}),
  {TimeInMicroSeconds/1000,Result}.

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([FilePath]) ->
%%  Loading Dictionary words from File say dictionary.txt
  io:format("Loading Dictionary is started......"),
  {TimeInMicroSeconds,Dict} = timer:tc(load,[FilePath]),
  io:format("Loading Dictionary is conmpleted with in ~p ms......",[TimeInMicroSeconds/1000]),
  {ok, #state{dictionary = Dict}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({process,PhoneNumber}, _From, #state{dictionary = Dict}= State) ->
  {reply, dictionary:search(PhoneNumber,Dict), State};
handle_call({load_and_search,FileName,PhoneNumber}, _From, State) ->


  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, not_handled, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(clear_dictionary, State) ->
  {noreply, State#state{dictionary = []}};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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

-spec load(FilePath::filePath()) -> {ok,Dictionary::dictionary:dictionary()}.
custom_read(FilePath,Reporter,Count) -> ok.





loop(Bin,Count) -> loop(Bin,<<>>,[]).

loop(<<"">>,<<>>,L) -> L;
loop(<<"">>,Temp,L) -> [Temp|L];
loop(<<"\r\n",Rest/binary>>,<<>>,L) -> loop(Rest,<<>>,L);
loop(<<"\r\n",Rest/binary>>,Temp,L) -> loop(Rest,<<>>,[binary_to_list(Temp)|L]);
loop(<<" ",Rest/binary>>,Temp,L) -> loop(Rest,Temp,L);
loop(<<H/utf8,Rest/binary>>,Temp,L) -> loop(Rest,<<Temp/binary,H/utf8>>,L).








