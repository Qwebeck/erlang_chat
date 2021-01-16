-module(user_file_manager).
-export([save_user/2, read_user/2]).

create_filename(UserName) ->
    {ok, io_lib:format("./users/~s.txt", [UserName])}.
        


% Saving user data
save_user(to_file, UserData = #{ user := UserName }) -> 
    {ok, Filename} = create_filename(UserName),
    {ok, File} = utils:open_file(Filename, [write]),
    {ok} = save_user(File, UserData),
    file:close(File),
    {ok};

save_user(IODevice, #{user := UserName, password := Password, rooms := Rooms}) -> 
    FormattedRooms = [[X,"\n"] || X <- Rooms],
    io:format(IODevice, "~s~n~s~n~s", [UserName, Password, FormattedRooms]), 
    {ok};     
save_user(IODevice, UserData = #{user := UserName, password := Password}) ->
    save_user(IODevice, maps:merge(UserData, #{rooms => []})).

%


% Reading user data
read_user(from_file, UserName) -> 
    {ok, Filename} = create_filename(UserName),
    {Status, File} = utils:open_file(Filename, [read]),
    case Status of
        ok -> try parse(user_object, File) after file:close(File)
            end;
        error -> {user_not_exists, #{user => UserName}}
    end.

parse(user_object, IODevice) -> 
    UserName = string:strip(io:get_line(IODevice, ""), both, $\n),
    Password = string:strip(io:get_line(IODevice, ""), both, $\n),
    Rooms = parse(user_rooms, IODevice),
    {ok, #{user => binary:list_to_bin(UserName), password => binary:list_to_bin(Password), rooms =>  Rooms}};

parse(user_rooms, IODevice) ->
    case io:get_line(IODevice, "") of
        eof  -> [];
        Line -> 
            Stripped = string:strip(Line, both, $\n),
            [binary:list_to_bin(Stripped)] ++ parse(user_rooms, IODevice)
    end.
%


