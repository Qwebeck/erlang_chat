{application, 'erlang_chat', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['chat_room_handler','erlang_chat_app','erlang_chat_sup','user_account_handler','welcome_page_handler']},
	{registered, [erlang_chat_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {erlang_chat_app, []}},
	{env, []}
]}.