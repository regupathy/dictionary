-module(dictionary_search_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	FileName = application:get_env(dictionary_search,dictionary_file,"../dictionary.txt"),

	ChildSpec = {dict_store_unit, {dict_store_unit, start_link, [FileName]},
		permanent, brutal_kill, worker, [dict_store_unit]},

	{ok, {{one_for_one, 1, 5}, [ChildSpec]}}.
