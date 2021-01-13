-module(user_manager).
-export([create_user/2, get_user_data/3, user_manager/0]).

create_user(UserManagerPID, UserData) ->
    UserManagerPID ! {self(), {create_user, UserData}},
    receive
        {UserManagerPID, Msg} -> Msg
    end.

get_user_data(UserManagerPID, UserName, UserPassword) -> 
    UserManagerPID ! {self(), {get_user_data, UserName, UserPassword}},
    receive
        {UserManagerPID, UserData} -> UserData
    end. 

user_manager() ->
    receive
        {From, {create_user, UserData}} ->
            NewUser = create_new_user(UserData),
            From ! {self(), NewUser},
            user_manager();
        {From, {get_user_data, UserName, UserPassword}} -> 
            ParsedUserData = read_user(from_file, UserName, UserPassword),
            From ! { self(), ParsedUserData },
            user_manager();
        terminate ->
            ok
        end.


create_filename(User) ->
    io_lib:format("./users/~s.txt", [User]).

create_new_user(UserData = #{user := User, pass := Password}) -> 
    save_user(to_file, User, Password).

save_user(to_file, UserName, Password) -> 
    Filename = create_filename(UserName),
    filelib:ensure_dir(Filename),
    {ok, File} = file:open(Filename, [write]),
    io:format(File, "~s~n~s~n", [UserName, Password]), 
    {ok, #{user => UserName}}.     


read_user(from_file, UserName, UserPassword) -> 
    Filename = create_filename(UserName),
    {Status, File} = file:open(Filename, [read]),
    case Status of 
        ok -> try parse(user_object, File, UserPassword)
                after file:close(File)
            end;
        error -> create_new_user(#{user => UserName, pass => UserPassword})
    end.

parse(user_object, File, RequiredPassword) -> 
    UserName = string:strip(io:get_line(File, ""), both, $\n),
    Password = string:strip(io:get_line(File, ""), both, $\n),
    Rooms = parse(user_rooms, File),
    IsPasswodMatch = string:equal(Password,RequiredPassword),
    if IsPasswodMatch ->
        {ok, #{user => UserName, password => Password, rooms => Rooms}};
    true -> 
        {wrong_credentials, []}
    end.

parse(user_rooms, File) ->
    case io:get_line(File, "") of
        eof  -> [];
        Line -> Line ++ parse(user_rooms, File)
    end.
