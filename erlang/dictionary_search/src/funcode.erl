%%%-------------------------------------------------------------------
%%% @author regupathy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%   I have taken 3 hours of time to complete the full functionality of this module. Unfortunately i forget to add
%%%   this in repository. After two days, I was too busy with office work. Now i have created repository for both Erlang
%%%   and Elixir.
%%%
%%% @end
%%% Created : 05. Mar 2019 4:31 AM
%%%-------------------------------------------------------------------
-module(funcode).
-author("regupathy").

%% API
-export([test/0,zip/2,search/2,readlines/2,int_to_int_list/1]).


test() ->
  A = funcode:readlines("dictionary.txt",[]),
  funcode:search(76239678446377,A).


search(Number,Dict)when is_number(Number) ->   search(int_to_int_list(Number),Dict,Dict,[]).

search([Key|[]],SubTree,_Dict,Acc) ->
  case dictionary_take(Key,SubTree) of

    not_found -> []; %% Simply Ignore because of No Entry in Dictionary for Key

    %% No need to check the NestedTree
    {match,{Key,[],_}} ->  []; %% Simply Ignore because of Empty word list in Dictionary for Key

    {match,{Key,Words,_}} -> zip([Words|Acc],[])

  end;

search([Key|Rest],SubTree,Dict,Acc) ->

  case dictionary_take(Key,SubTree) of

    not_found ->  []; %% No Entry in Dictionary

    {match,{Key,[],[]}} -> []; %% Empty entry in Dictionary

    {match,{Key,[],NestedTree}} -> search(Rest,NestedTree,Dict,Acc); %% Incomplete Word

    {match,{Key,Words,[]}} -> search(Rest,Dict,Dict,[Words|Acc]); %% No further continuation

    {match,{Key,Words,NestedTree}} -> %% Has further continuation with set words

      search(Rest,Dict,Dict,[Words|Acc]) %% carry forward with word list and start search from original Dict
      ++
      search(Rest,NestedTree,Dict,Acc)  %% search will carry forward with Nested Tree and not considering the word list

  end.


dictionary_take(_,[]) -> not_found;
dictionary_take(Char,[{Char,Words,NestedTree}|_]) -> {match,{Char,Words,NestedTree}};
dictionary_take(Char,[_|Rest]) -> dictionary_take(Char,Rest).


zip([],Acc) -> Acc;
zip([H|Rest],[]) -> zip(Rest,lists:map(fun(X) -> [X] end,H));
zip([H|Rest],Acc) -> zip(Rest,[[Y|X] || X <- Acc,Y <- H]).


readlines(FileName,Dict) ->
  case file:open(FileName, [read]) of
    {ok, Device}  -> get_all_lines(Device,Dict);

    {error,enoent} -> io:format("File : ~p is not found ~n",[FileName])
end.

get_all_lines(Device,Dict) ->
  case catch file:read_line(Device) of
    eof  -> io:format("Loading Dictionary is done"),file:close(Device),Dict;

    {ok,Word} -> get_all_lines(Device,load(string:trim(Word,trailing),Dict));

    {Type,Reason} -> io:format("Problem occured while Reading file that is ~p",[{Type,Reason}])

  end.

load(Word,Dict) -> add(word_to_number(Word),Dict,Word).

add([],Dict,_) -> Dict;
add(_,Dict,Word) when length(Word) < 3 -> Dict; %% Words size should be greater than or equal to 3
add(Keys,Dict,Word) -> add(Keys,Dict,Word,[]).

add([H],[],Word,Acc) -> [{H,[Word],[]}|Acc];
add([H],[{H,Words,NestedTree}|Rest],Word,Acc) -> Acc ++ [{H,[Word|Words],NestedTree}|Rest];
add([H|Rest],[],Word,Acc) -> [{H,[],add(Rest,[],Word,[])}|Acc];
add([H|Rest],[{H,Words,NestedTree}|Rest2],Word,Acc) -> Acc ++ [{H,Words,add(Rest,NestedTree,Word,[])}|Rest2];
add(List,[UnMatch|Rest],Word,Acc) -> add(List,Rest,Word,[UnMatch|Acc]).


word_to_number(Words) -> lists:map(fun(X) -> char_to_number(X) end,Words).


char_to_number(Char) when Char >= $A, Char =< $C -> 2;
char_to_number(Char) when Char >= $a, Char =< $c -> 2;
char_to_number(Char) when Char >= $d, Char =< $f -> 3;
char_to_number(Char) when Char >= $D, Char =< $F -> 3;
char_to_number(Char) when Char >= $g, Char =< $i -> 4;
char_to_number(Char) when Char >= $G, Char =< $I -> 4;
char_to_number(Char) when Char >= $j, Char =< $l -> 5;
char_to_number(Char) when Char >= $J, Char =< $L -> 5;
char_to_number(Char) when Char >= $m, Char =< $o -> 6;
char_to_number(Char) when Char >= $M, Char =< $O -> 6;
char_to_number(Char) when Char >= $p, Char =< $s -> 7;
char_to_number(Char) when Char >= $P, Char =< $S -> 7;
char_to_number(Char) when Char >= $t, Char =< $v -> 8;
char_to_number(Char) when Char >= $T, Char =< $V -> 8;
char_to_number(Char) when Char >= $w, Char =< $z -> 9;
char_to_number(Char) when Char >= $W, Char =< $Z -> 9;
char_to_number(Char) -> throw({invalid_char,Char}).

int_to_int_list(Int) -> int_to_int_list(Int,[]).
int_to_int_list(0,Acc) -> Acc;
int_to_int_list(N,Acc) -> int_to_int_list(N div 10,[N rem 10 | Acc]).
