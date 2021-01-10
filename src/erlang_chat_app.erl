-module(erlang_chat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	UserManager = spawn(user_manager, user_manager, []),
	RoomManager = spawn(room_manager, room_manager, []),

	InitialState = #{user_manager_pid => UserManager, room_manager_pid=>RoomManager},
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/room", chat_room_handler, InitialState},
			{"/user", user_account_handler, InitialState},
			{"/", welcome_page_handler, InitialState}
			]
			}]),
	{ok, _} = cowboy:start_clear(http_listener,
		[{port, 8080}],
		#{env =>#{dispatch => Dispatch}}),
	erlang_chat_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http_listener).
