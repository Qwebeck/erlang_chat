-module(room_manager).

-define(ROOM_DIRECTORY, "rooms/").

-define(ROOM_FILE_ENDING, ".room").

-export([add_user_to_room/2,
         create_room_message/2,
         read_room_message/2,
         room_exist/1,
         room_manager/0,
         write_message_to_room_action/4]).

add_user_to_room(User, Room) ->
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
        terminate -> ok
    end.

read_room_message(RoomManagerPID, Room) ->
    RoomManagerPID ! {self(), {read_room, Room}},
    receive {RoomManagerPID, Msg} -> Msg end.

write_message_to_room_action(RoomManagerPID, Room,
                             Message, User) ->
    RoomManagerPID !
        {self(), {write_message_to_room, Room, Message, User}},
    receive {RoomManagerPID, Msg} -> Msg end.

create_room_message(RoomManagerPID, Room) ->
    RoomManagerPID ! {self(), {create_room, Room}},
    receive {RoomManagerPID, Msg} -> Msg end.
