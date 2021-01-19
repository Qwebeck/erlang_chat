-module(room_process).

-define(ROOM_DIRECTORY, "rooms/").

-define(ROOM_FILE_ENDING, ".room").

-export([create_room_message/2,
         read_room_message/2,
         room_exist/1,
         room_manager/0,
         write_to_room_message/4,
         add_user_to_room_message/3,
         terminate/1]).

add_user_to_room_message(ProcessPID,Room,User) ->
    ProcessPID ! {self(), {add_user_to_room, Room,User}}.
read_room_message(ProcessPID, Room) ->
    ProcessPID ! {self(), {read_room, Room}}.

write_to_room_message(ProcessPID, Room,
                              Message, User) ->
    ProcessPID ! {self(), {write_message_to_room, Room,Message,User}}.
create_room_message(ProcessPID, Room) ->
    ProcessPID ! {self(), {create_room, Room}}.

terminate(ProcessPID) ->
        ProcessPID ! terminate.
        
room_manager() ->
    receive
        {From, {create_room, Room}} ->
            NewRoom = create_room(Room),
            From ! {self(), NewRoom},
            room_manager();
        {From, {write_message_to_room, Room, Message, User}} ->
            Content = write_message_to_room(Room, Message, User),
            From ! {self(), Content},
            room_manager();
        {From, {read_room, Room}} ->
            RoomContent = read_room(Room),
            From ! {self(), RoomContent},
            room_manager();
        {From, {add_user_to_room,Room,User}} ->
            RoomContent = add_user_to_room(Room,User),
            From ! {self(), RoomContent},
            room_manager();
        terminate -> ok
    end.

add_user_to_room(Room, User) ->
    {ok, _} = user_supervisor:add_user_chat_room(User,
                                                 Room),
    {ok, File} = file:open(room_file(Room), [append]),
    io:format(File, "~s joined the room!~n", [User]).

room_file(Room) ->
    RoomString = binary:bin_to_list(Room),
    (?ROOM_DIRECTORY) ++ RoomString ++ (?ROOM_FILE_ENDING).

write_message_to_room(_, <<"">>, _) -> ok;
write_message_to_room(Room, Message, User) ->
    {ok, File} = file:open(room_file(Room), [append]),
    io:format(File, "~s says:\"~s\"~n", [User, Message]).

read_room(Room) -> readlines(room_file(Room)).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

room_exist(Room) -> filelib:is_file(room_file(Room)).

create_room(Room) ->
    FileName = room_file(Room),
    filelib:ensure_dir(FileName),
    file:open(FileName, [write]),
    ok.