-module(user_account_handler).
-export([init/2]).

init(Req, State = #{user_supervisor_pid := UserManagerPID}) ->
    #{name := User, password := Password } = cowboy_req:match_qs([name, password], Req),
    {Status, Data} = get_user_info(User, Password),
    reply(Status, Data, Req, State).


get_user_info(User, Password) ->
    {Status, UserData} = user_supervisor:get_user_data(User, Password),
    case Status of
        ok -> {ok, UserData};
        wrong_credentials -> {wrong_credentials, []}
    end.


reply(ok, _UserData=#{user := UserName, rooms := Rooms}, Req, State) -> 
    erlang:display(Rooms),
    Content = jsx:encode(#{<<"user">> =>UserName,<<"loggedIn">> => true,<<"rooms">> => Rooms}),
    Res = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json">>},
    Content,
    Req),
    {ok, Res, State};
reply(ok, _UserData=#{user := UserName}, Req, State) -> 
    UserDataWithRooms = maps:merge(_UserData, #{rooms => []}),
    reply(ok, UserDataWithRooms, Req, State);

reply(wrong_credentials, _,Req, State) -> 
    Content = jsx:encode(#{<<"loggedIn">> => false}),
    Res = cowboy_req:reply(403,
    #{<<"content-type">> => <<"application/json">>},
    <<"Wrong password!\n">>,
    Content),
    {ok, Res, State}.
