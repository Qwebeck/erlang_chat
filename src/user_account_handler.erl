-module(user_account_handler).
-export([init/2]).

init(Req, State) -> 
    #{name := User, pass := Password } = cowboy_req:match_qs([name, pass], Req),
    {Status, Data} = get_user_info(User, Password),
    reply(Status, Data, Req, State).


get_user_info(User, Password) ->
    {Status, UserData} = parse_user_file(User, Password),
    case Status of
        ok -> {ok, UserData};
        wrong_credentials -> {wrong_credentials, []}
    end.

parse_user_file(User, Password) -> 
    {Status, File} = file:open(create_filename(User), [read]),
    case Status of 
        ok -> try parse_user_file(user_object, File, Password)
                after file:close(File)
            end;
        error -> create_new_user(User, Password)
    end.

parse_user_file(user_object, File, RequiredPassword) -> 
    UserName = string:strip(io:get_line(File, ""), both, $\n),
    Password = string:strip(io:get_line(File, ""), both, $\n),
    Rooms = parse_user_rooms(File),
    IsPasswodMatch = string:equal(Password,RequiredPassword),
    if IsPasswodMatch ->
        {ok, #{user => UserName, password => Password, rooms => Rooms}};
    true -> 
        {wrong_credentials, []}
    end.

parse_user_rooms(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ parse_user_rooms(Device)
    end.



create_new_user(UserName, Password) ->    
    Filename = create_filename(UserName),
    filelib:ensure_dir(Filename),
    {ok, File} = file:open(Filename, [write]),
    io:format(File, "~s~n~s~n", [UserName, Password]),
    {ok, #{user=>UserName}}.

create_filename(User) ->
    io_lib:format("./users/~s.txt", [User]).


reply(ok, _UserData=#{user := UserName }, Req, State) -> 
    Content = io_lib:format("Hello ~s!~n", [UserName]), 
    Res = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    list_to_binary(Content),
    Req),
    {ok, Res, State};
reply(wrong_credentials, _,Req, State) -> 
    Res = cowboy_req:reply(403,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Wrong password!\n">>,
    Req),
    {ok, Res, State}.
