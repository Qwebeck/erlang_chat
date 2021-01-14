-module(user_manager).
-export([create_user_action/2, get_user_data_action/3, add_user_chat_room_action/3, user_manager/0]).

create_user_action(UserManagerPID, UserData) ->
    UserManagerPID ! {self(), {create_new_user, UserData}},
    receive
        {UserManagerPID, Msg} -> Msg
    end.

get_user_data_action(UserManagerPID, UserName, UserPassword) -> 
    UserManagerPID ! {self(), {get_user, UserName, UserPassword}},
    receive
        {UserManagerPID, UserData} -> UserData
    end.

add_user_chat_room_action(UserManagerPID, UserName, ChatRoomName) -> 
    erlang:display("add_user_chat_room_action"),
    UserManagerPID ! {self(), {add_chat_room_to_user, UserName, ChatRoomName}},
    receive
        {UserManagerPID, UpdatedUserData} -> UpdatedUserData
    end.

user_manager() ->
    receive
        {From, {create_new_user, UserData}} ->
            NewUser = create_new_user(UserData),
            From ! {self(), NewUser},
            user_manager();
        {From, {get_user, UserName, UserPassword}} -> 
            ParsedUserData = get_user(UserName, UserPassword),
            From ! { self(), ParsedUserData },
            user_manager();
        {From, {add_chat_room_to_user, UserName, ChatRoomName}} ->
            UpdatedUser = add_chat_room_to_user(UserName, ChatRoomName),
            From ! { self(), UpdatedUser },
            user_manager();        
        terminate ->
            ok
        end.

% Obtaining user info
get_user(UserName, UserPassword) -> 
    {Status, RequestedUserObject} = user_file_manager:read_user(from_file, UserName),
    case Status of 
        ok -> check_if_authorized(UserPassword, RequestedUserObject);
        user_not_exists -> create_new_user(UserName, UserPassword)
    end.

% Creating new User
create_new_user(UserName, UserPassword) ->
    create_new_user(#{user => UserName, password => UserPassword}).
create_new_user(UserData) -> 
    io:format("here~n"),
    { ok } = user_file_manager:save_user(to_file, UserData),
    { ok, maps:remove(password, UserData)}.
% 

% Adding rooms to user
add_chat_room_to_user(UserName, NewRoom) ->
    {ok, UserObject} = user_file_manager:read_user(from_file, UserName),
    #{rooms := ExistingRooms } = UserObject,
    IsMember = lists:member(NewRoom, ExistingRooms),
    {ok, UpdatedUser} = update_user_rooms(IsMember, UserObject, NewRoom),
    erlang:display("udpated User"),
    erlang:display(UpdatedUser),
    user_file_manager:save_user(to_file, UpdatedUser),
    {ok, UpdatedUser}.

update_user_rooms(false, UserObject, NewRoom) ->
    #{rooms:=UserRooms} = UserObject,
    ExtendedRooms = [NewRoom] ++ UserRooms,
    {ok, maps:update(rooms, ExtendedRooms, UserObject)};
update_user_rooms(true, UserObject, _) -> {ok, UserObject}.
%

% Checking user credentials
check_if_authorized(UserPassword, RequestedUserObject) -> 
    #{password := RequiredPassword} = RequestedUserObject,        
    IsPasswordMatch = string:equal(RequiredPassword, UserPassword),
    if 
        IsPasswordMatch ->
            {ok, maps:remove(password, RequestedUserObject)};
        true -> 
            {wrong_credentials, []}
    end.
