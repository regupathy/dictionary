{application, 'dictionary_search', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['dictionary_search_app','dictionary_search_sup']},
	{registered, [dictionary_search_sup]},
	{applications, [kernel,stdlib]},
	{mod, {dictionary_search_app, []}},
	{env, []}
]}.