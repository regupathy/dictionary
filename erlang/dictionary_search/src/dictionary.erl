%%%-------------------------------------------------------------------
%%% @author regupathy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2019 6:36 AM
%%%-------------------------------------------------------------------
-module(dictionary).
-author("regupathy").

%% API
-export([add/2,search/2, remove_duplicate/1]).

-type key() :: number() | string() | atom().

-type keys() :: list(key()).

-type word() :: string() | list(string()).

-type node() :: {Key::term(),Data::list(word()),ChildTree::node()}.

-type dictionary() :: [node()].

-export_type([dictionary/0,keys/0]).

%%========================================================================
%%                   Adding Element to Dictionary
%%========================================================================
%% @doc
%%
%% As per Instruction the Dictionary should add the word only if size of word is more than or equal 3.
%%

-spec add(Word::string(),Dictionary::dictionary()) -> NewDictionary::dictionary().
add(Word,Dictionary) ->
  case catch word_to_key(Word) of
    {throw,Error} -> io:format("Not able to add word : ~p bz of : ~p",[Word,Error]),Dictionary;
    Keys -> add(Keys,Dictionary,Word)
  end.

add([],Dict,_) -> Dict;
add(_,Dict,Word) when length(Word) < 3 -> Dict; %% Words size should be greater than or equal to 3
add(Keys,Dict,Word) -> add(Keys,Dict,Word,[]).

add([H],[],Word,Acc) -> [{H,[Word],[]}|Acc];
add([H],[{H,Words,NestedTree}|Rest],Word,Acc) -> Acc ++ [{H,[Word|Words],NestedTree}|Rest];
add([H|Rest],[],Word,Acc) -> [{H,[],add(Rest,[],Word,[])}|Acc];
add([H|Rest],[{H,Words,NestedTree}|Rest2],Word,Acc) -> Acc ++ [{H,Words,add(Rest,NestedTree,Word,[])}|Rest2];
add(List,[UnMatch|Rest],Word,Acc) -> add(List,Rest,Word,[UnMatch|Acc]).

%%========================================================================
%%                   Search Words in Dictionary
%%========================================================================
-spec search(Keys::keys(),Dictionary::dictionary()) -> {ok,Result:: list(word())} | {error,Reason::string()|term()}.
search(Number,Dictionary)when is_number(Number) ->
  case catch number_to_list_of_int(Number) of
    {throw,Reason} -> {error,Reason};
    Keys ->  search(Keys,Dictionary,Dictionary,[])
  end.

search([Key|[]],SubTree,_Dict,Acc) ->
%% No need to check the NestedTree
  case dict_take(Key,SubTree) of
    not_found -> []; %% Simply Ignore because of No Entry in Dictionary for Key
    {match,{Key,[],_}} ->  []; %% Simply Ignore because of Empty word list in Dictionary for Key
    {match,{Key,Words,_}} -> zip([Words|Acc],[])
  end;

search([Key|Rest],SubTree,Dict,Acc) ->
  case dict_take(Key,SubTree) of
    not_found ->  []; %% No Entry in Dictionary
    {match,{Key,[],[]}} -> []; %% Empty entry in Dictionary
    {match,{Key,[],NestedTree}} -> search(Rest,NestedTree,Dict,Acc); %% Incomplete Word
    {match,{Key,Words,[]}} -> search(Rest,Dict,Dict,[Words|Acc]); %% No further continuation
    {match,{Key,Words,NestedTree}} -> %% Has further continuation with set words
      search(Rest,Dict,Dict,[Words|Acc]) %% carry forward with word list and start search from original Dict
      ++
      search(Rest,NestedTree,Dict,Acc)  %% search will carry forward with Nested Tree and not considering the word list
  end.

%%========================================================================
%%                   Remove the Duplicate word in Dictionary
%%========================================================================
-spec remove_duplicate(Dictionary::dictionary()) -> NewDicitonary::dictionary().
remove_duplicate(Dict) -> remove_duplicate(Dict,[]).

remove_duplicate([],Dict) -> Dict;
remove_duplicate([{Key,[],[]}|Rest],Acc) -> remove_duplicate(Rest,[{Key,[],[]}|Acc]);
remove_duplicate([{Key,Words,[]}|Rest],Acc) -> remove_duplicate(Rest,[{Key,rm_dup(Words),[]}|Acc]);
remove_duplicate([{Key,[],SubTree}|Rest],Acc) -> remove_duplicate(Rest,[{Key,[], remove_duplicate(SubTree,[])}|Acc]);
remove_duplicate([{Key,Words,SubTree}|Rest],Acc) ->
  remove_duplicate(Rest,[{Key,rm_dup(Words), remove_duplicate(SubTree,[])}|Acc]).

rm_dup(List) -> sets:to_list(sets:from_list(List)).
%%========================================================================
%%                   Local conversion
%%========================================================================
%% @doc
%% convert phone number into list of digit
%% Limitation :
%%           phone number shouldn't contains 0 and 1.
-spec number_to_list_of_int(Number) -> list(integer()).
number_to_list_of_int(Number) -> number_to_list_of_int(Number,[]).
number_to_list_of_int(0,Acc) -> Acc;
number_to_list_of_int(N,Acc) -> number_to_list_of_int(N div 10,add_only_if(N rem 10 , Acc)).

%% @doc
%% convert word into keys
%% Limitation :
%%          Word should be formed only by English characters in both upper and lower cases.

-spec word_to_key(Word::string()) -> Key::key().
word_to_key(Word) -> lists:map(fun(X) -> char_to_number(X) end,Word).

%%========================================================================
%%                   Local Functions
%%========================================================================

%% As per Assignment Instruction Each Character has mapped with a corresponding number
-spec char_to_number(Char::char()) -> N::non_neg_integer().
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


%% avoid adding number 1 and 0 to the list
-spec add_only_if(Digit::non_neg_integer(),List::list()) -> NewList::list().
add_only_if(0,_) -> throw({invalid_digit,0});
add_only_if(1,_) -> throw({invalid_digit,1});
add_only_if(Digit,List) -> [Digit|List].


%% take Dictionary node by Key
-spec dict_take(Key::key(),Dict::dictionary()) -> node().
dict_take(_,[]) -> not_found;
dict_take(Key,[{Key,Words,NestedTree}|_]) -> {match,{Key,Words,NestedTree}};
dict_take(Key,[_|Rest]) -> dict_take(Key,Rest).


%% Concatenating the 'list of list of String' into  'list of String'
%% For Making output even more understandable
-spec zip(Words::list(word()),Acc::list()) -> word().
zip([],Acc) -> Acc;
zip([H|Rest],[]) -> zip(Rest,lists:map(fun(X) -> [X] end,H));
zip([H|Rest],Acc) -> zip(Rest,[[Y|X] || X <- Acc,Y <- H]).
