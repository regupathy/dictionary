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
-export([add/2,search/2]).

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
add(Word,Dictionary) -> ok.




%%========================================================================
%%                   Search Words in Dictionary
%%========================================================================
-spec search(Keys::keys(),Dictionary::dictionary()) -> Result:: list(word()).
search(Keys,Dictionary) -> ok.




%%========================================================================
%%                   Remove the Duplicate word in Dictionary
%%========================================================================
-spec avoid_duplicate(Dictionary::dictionary()) -> NewDicitonary::dictionary().
avoid_duplicate(Dictionary) -> ok.



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
%%          Word's should be a English characters in both upper and lower cases.

-spec word_to_key(Word::string()) -> Key::key().
word_to_key(Word) -> lists:map(fun(X) -> char_to_number(X) end,Word).


%%========================================================================
%%                   Local Functions
%%========================================================================


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



add_only_if(0,_) -> throw({invalid_digit,0});
add_only_if(1,_) -> throw({invalid_digit,1});
add_only_if(Digit,List) -> [Digit|List].


