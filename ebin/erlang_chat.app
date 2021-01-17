{application, 'erlang_chat', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['chat_room_handler','erlang_chat_app','erlang_chat_sup','room_manager','supervisor_process_storage','user_account_handler','user_file_manager','user_process','user_supervisor','utils']},
	{registered, [erlang_chat_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {erlang_chat_app, []}},
	{env, []}
]}.