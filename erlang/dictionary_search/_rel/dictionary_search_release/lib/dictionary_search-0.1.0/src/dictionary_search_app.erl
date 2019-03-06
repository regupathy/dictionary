-module(dictionary_search_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	dictionary_search_sup:start_link().

stop(_State) ->
	ok.
