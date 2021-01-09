-module(erlang_chat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/room", char_room_handler, []},
			{"/user", user_account_handler, []},
			{"/", welcome_page_handler, []}
			]
			}]),
	{ok, _} = cowboy:start_clear(http_listener,
		[{port, 8080}],
		#{env =>#{dispatch => Dispatch}}),
	erlang_chat_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http_listener).
