-module(welcome_page_handler).
-export([init/2]).

init(Req0, State) -> 
	io:fwrite("Hello world\n"),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello from welcome!">>,
        Req0),
    {ok, Req, State}.