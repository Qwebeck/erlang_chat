{application, 'erlang_chat', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['chat_room_handler','cors_header_middleware','erlang_chat_app','erlang_chat_sup','file_utils','room_manager','user_account_handler','user_file_manager','user_manager']},
	{registered, [erlang_chat_sup]},
	{applications, [kernel,stdlib,cowboy,jsx]},
	{mod, {erlang_chat_app, []}},
	{env, []}
]}.