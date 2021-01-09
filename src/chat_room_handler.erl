-module(chat_room_handler).
-export([init/2]).

init(Req0, State) -> 
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello from chat room!\r\n">>,
        Req0),
    {ok, Req, State}.