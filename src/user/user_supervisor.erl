-module(user_supervisor).
-export([create_user/1, 
         get_user_data/2, 
         add_user_chat_room/2,
         user_supervisor/0, 
         on_user_created/1,
         on_user_data_received/1,
         on_chat_room_added/2
         ]).

%% API that allows to sent messages to user process superviosr.
%% All function are synchronous: means every thread that will call a function await for respons from user manager.

% User creation
create_user(UserData) ->
    %% Makes request to user supervisor to crate a new user. 
    %% Blokcs calling process until UserIsCreated. 
    %% Returns tuple in form of {ok, CreatedUserData}
    UserManagerPID = whereis(user_supervisor_pid),
    UserManagerPID ! {self(), {create_user, UserData}},
    utils:await_response_to(UserManagerPID, user_created).

on_user_created(UserData) ->
    %% Called to notify process about new user creation by sending to him message with user data.
    %% Default implementation notifies user supervisor.
    UserManagerPID = whereis(user_supervisor_pid),
    on_user_created(UserManagerPID, UserData).
on_user_created(ProcessToNotify, UserData) ->
    ProcessToNotify ! { self(), {user_created, UserData}}.


% User data management
get_user_data(UserName, UserPassword) -> 
    %% Makes request to user supervisor to get information about user with given UserName. 
    %% Blokcs calling process until Data is received. 
    %% If password matches, returns tuple in form of {ok, RequestedUserData},
    %% otherwise returns tuple of form { wrong_credentials, []} 
    UserManagerPID = whereis(user_supervisor_pid),
    UserManagerPID ! {self(), {get_user_data, UserName, UserPassword}},
    utils:await_response_to(UserManagerPID, on_user_data_received).

on_user_data_received(UserData) -> 
    %% Called to notify process about new user creation by sending to him message with user data.
    %% Default implementation notifies user supervisor.
    UserManagerPID = whereis(user_supervisor_pid),
    on_user_data_received(UserManagerPID, UserData).
on_user_data_received(ProcessToNotify, UserData) ->    
    ProcessToNotify ! {self(), {on_user_data_received, UserData}}.
%

% Adding user to chat room
add_user_chat_room(UserName, ChatRoomName) -> 
    %% Makes request to user supervisor to add chat room with ChatRoomName to user with given UserName. 
    %% Blokcs calling process until new room is added. 
    UserManagerPID = whereis(user_supervisor_pid),
    UserManagerPID ! {self(), {add_user_chat_room, UserName, ChatRoomName}},
    utils:await_response_to(UserManagerPID, on_chat_room_added).

on_chat_room_added(Status, UserData) -> 
    %% Called to notify process about new user creation by sending to him message with user data.
    %% Default implementation notifies user supervisor.
    UserManagerPID = whereis(user_supervisor_pid),
    on_chat_room_added(UserManagerPID, Status, UserData).
on_chat_room_added(ProcessToNotify, Status, UserData) ->
    ProcessToNotify ! {self(), {on_chat_room_added, {Status, UserData}}}.
    
% User supervisor process
user_supervisor() ->
    InitialState = #{},
    user_supervisor(InitialState).
user_supervisor(ProcessStorage) ->
    receive
        {From, {create_user, UserData = #{user := ToUserName}}} ->
            {ok, {UserRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, ToUserName, ProcessStorage),
            user_process:create_user(UserRequestHandler),
            user_supervisor(UpdatedProcessStorage);

        {From, {on_user_created, {Status, UserData}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_user_created, Status, UserData, ProcessStorage),
            user_supervisor(UpdatedProcessStorage);            
            
        {From, {get_user_data, ToUserName, UserPassword}} -> 
            {ok, {UserRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, ToUserName, ProcessStorage),
            user_process:get_user_data(UserRequestHandler, ToUserName, UserPassword),
            user_supervisor(UpdatedProcessStorage);

        {From, {on_user_data_received, {Status, UserData}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_user_data_received, Status, UserData, ProcessStorage),
            user_supervisor(UpdatedProcessStorage);
        
        {From, {add_user_chat_room, ToUserName, ChatRoomName}} ->
            {ok, {UserRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, ToUserName, ProcessStorage),
            user_process:add_chat_room_to_user(UserRequestHandler, ToUserName, ChatRoomName),
            user_supervisor(UpdatedProcessStorage);

        {From, {on_chat_room_added, {Status, UserData}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_chat_room_added, Status, UserData, ProcessStorage),            
            user_supervisor(UpdatedProcessStorage);
        terminate ->
            ok
        end.


on_response_received(ChildProcessPID, MessageAtom, Status, UserData, ProcessStorage) -> 
    #{user := UserName} = UserData,
    supervisor_events:on_response_received(fun user_process:terminate/1, ChildProcessPID, MessageAtom, UserName, Status, UserData, ProcessStorage).    
