-module(user_process).

-export([user_process/0, terminate/1, create_user/2, get_user_data/3, add_chat_room_to_user/3]).

% Api
create_user(ProcessPID, UserData) ->
    ProcessPID ! {self(), {create_user, UserData}}.

get_user_data(ProcessPID, UserName, UserPassword) ->
    ProcessPID ! {self(), {get_user_data, UserName, UserPassword}}.

add_chat_room_to_user(ProcessPID, UserName, RoomName) ->
    ProcessPID ! {self(), {add_chat_room_to_user, UserName, RoomName}}.

terminate(ProcessPID) ->
    ProcessPID ! terminate.

% Process 
user_process() ->
    receive
        {_From, {create_user, UserData}} ->
            NewUser = create_new_user(UserData),
            user_supervisor:on_user_created(UserData),
            user_process();
        {_From, {get_user_data, UserName, UserPassword}} ->
            UserData = get_user(UserName, UserPassword),
            user_supervisor:on_user_data_received(UserData),
            user_process();
        {_From, {add_chat_room_to_user, UserName, NewRoomName}} ->
            {Status, UpdatedUser} = add_chat_room_to_user(UserName, NewRoomName),
            user_supervisor:on_chat_room_added(Status, UpdatedUser),
            user_process();
        terminate ->
            ok
    end.

% Process logic

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
    { ok } = user_file_manager:save_user(to_file, UserData),
    { ok, maps:remove(password, UserData)}.
% 

% Adding rooms to user
add_chat_room_to_user(UserName, NewRoom) ->
    {ok, UserObject} = user_file_manager:read_user(from_file, UserName),
    #{rooms := ExistingRooms } = UserObject,
    erlang:display(ExistingRooms),
    erlang:display(NewRoom),
    IsMember = lists:member(NewRoom, ExistingRooms),
    {ok, UpdatedUser} = update_user_rooms(IsMember, UserObject, NewRoom),
    user_file_manager:save_user(to_file, UpdatedUser),
    {ok, UpdatedUser}.

update_user_rooms(false, UserObject, NewRoom) ->
    #{rooms:=UserRooms} = UserObject,
    ExtendedRooms = [NewRoom] ++ UserRooms,
    {ok, maps:update(rooms, ExtendedRooms, UserObject)};
update_user_rooms(true, UserObject, _) -> {ok, UserObject}.

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
