-module(user_account_handler).
-export([init/2]).

init(Req, State) -> 
    #{name := User, pass := Password } = cowboy_req:match_qs([name, pass], Req),
    {Status, Data} = get_user_info(User, Password),
    reply(Status, Data, Req, State).


get_user_info(User, Password) ->
    {Status, UserData} = parse_user_file(User),
    case Status of
        new_user -> {ok, UserData};
        existing_user -> get_user_info(User, Password, UserData)
    end.
% get_user_info(User, Password, UserData) -> 
%     #{ password := RequiredPassword } = UserData,
%     get_user_info(User, Password, UserData, RequiredPassword).

get_user_info(_User, Password, UserData=#{ password := Password}) -> 
    {ok, maps:remove(password, UserData)};
get_user_info(_, _, _) ->
    {wrong_credentials, []}.
    
     

parse_user_file(User) -> 
    io:format("User: ~s~n", [User]),
    {Status, Device} = file:open(create_filename(User), [read]),
    case Status of 
        ok -> try parse_user_file(user_object, Device)
                after file:close(Device)
            end;
        error -> create_new_user(User)
    end.
parse_user_file(user_object, Device) -> 
    [UserName, Password] = io:get_line(Device, ""),
    Rooms = parse_user_file(user_rooms, Device),
    {ok, #{user => UserName, password => Password, rooms => Rooms}};
parse_user_file(user_rooms, Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ parse_user_file(user_rooms, Device)
    end.

create_new_user(UserData) ->
    #{user := UserName} = UserData,
    io:format("User: ~s~n", [UserName]),
    Device = file:open(create_filename(UserName), [write]),
    file:write(Device, UserData),
    UserData.

create_filename(_User) ->
    "File.txt.".


reply(ok, _UserData, Req, State) -> 
    Res = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello from user account!\n">>,
    Req),
    {ok, Res, State};
reply(wrong_credentials, _,Req, State) -> 
    Res = cowboy_req:reply(403,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello from user account!\n">>,
    Req),
    {ok, Res, State}.
