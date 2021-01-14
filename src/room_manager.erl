-module(room_manager).

-define(ROOM_DIRECTORY, "rooms/").

-define(ROOM_FILE_ENDING, ".room").

-export([add_user_to_room/2,
         create_room_message/2,
         read_room_message/2,
         room_exist/1,
         room_manager/0,
         user_in_room/2,
         write_message_to_room_action/4]).

add_user_to_room(User, Room) ->
    erlang:display("adding user to room"),
    {ok, File} = file:open(room_file(Room), [append]),
    io:format(File, "~s~n", [User]),
    erlang:display("added user to room").

user_in_room(User, Room) ->
    %FileName = user_account_handler:create_filename(User),
    % Rooms = user_account_handler:parse_user_rooms(FileName),
    % erlang:display(Rooms),
    true.

room_file(Room) ->
    (?ROOM_DIRECTORY) ++ Room ++ (?ROOM_FILE_ENDING).

write_message_to_room(Room, Message, User) ->
    {ok, File} = file:open(room_file(Room), [append]),
    io:format(File, "~s says:\"~s\"~n", [User, Message]).

read_room(Room) ->
     readlines(room_file(Room)).
    

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

room_exist(Room) -> filelib:is_file(room_file(Room)).

create_room(Room) ->
    FileName = room_file(Room),
    filelib:ensure_dir(FileName),
    {ok, File} = file:open(FileName, [write]),
    io:format(File, "", []),
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
