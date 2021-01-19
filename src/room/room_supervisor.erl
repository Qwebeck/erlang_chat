-module(room_supervisor).
-export([create_room/1, on_room_created/1, write_message_to_room/3, on_message_added/3, read_room/1, on_room_data_readed/1,room_supervisor/0]).

create_room(Room) ->
    RoomSupervisorPID = whereis(room_supervisor),
    RoomSupervisorPID ! {self(), {create_room, Room}},
    utils:await_response_to(RoomSupervisorPID, on_room_created).

on_room_created(Room) ->
    RoomSupervisorPID = whereis(room_supervisor),
    RoomSupervisorPID ! {self(), {on_room_created, Room}}.

write_message_to_room(Room, Message, User) ->
    RoomSupervisorPID = whereis(room_supervisor),
    RoomSupervisorPID ! {self(), {write_message, Room, Message, User}},
    utils:await_response_to(RoomSupervisorPID, on_message_added).

on_message_added(Room, Message, User) ->
    RoomSupervisorPID = whereis(room_supervisor),
    RoomSupervisorPID ! {self(), {on_message_added, Room, Message, User}}.

read_room(Room) ->
    RoomSupervisorPID = whereis(room_supervisor),
    RoomSupervisorPID ! {self(), {read_room, Room}},
    utils:await_response_to(RoomSupervisorPID, on_read_room).

on_room_data_readed(Room) ->
    RoomSupervisorPID = whereis(room_supervisor),
    RoomSupervisorPID ! {self(), {on_read_room, Room}}.



room_supervisor() ->
    room_supervisor(#{}).
room_supervisor(ProcessStorage) ->
    receive
        {From, {create_room, Room}} ->
            {ok, {RoomRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, Room, ProcessStorage),
            room_process:create_room_message(RoomRequestHandler,Room),
            room_supervisor(UpdatedProcessStorage);
        {From, {on_room_created, {Status, Room}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_room_created, Status, Room, ProcessStorage),
            room_supervisor(UpdatedProcessStorage);

        {From, {write_message, Room, Message, User}} ->
            {ok, {RoomRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, Room, ProcessStorage),
            room_process:write_to_room_message(RoomRequestHandler,Room,Message,User),
            room_supervisor(UpdatedProcessStorage);
        {From, {on_message_added, {Status, Room}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_message_added, Status, Room, ProcessStorage),
            room_supervisor(UpdatedProcessStorage);
                    
        {From, {read_room, Room}} ->
            {ok, {RoomRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, Room, ProcessStorage),
            room_process:read_room_message(RoomRequestHandler,Room),
            room_supervisor(UpdatedProcessStorage);
        {From, {on_read_room, {Status, Room}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_read_room, Status, Room, ProcessStorage),
            room_supervisor(UpdatedProcessStorage);
        terminate ->
            ok
        end.

on_response_received(ChildProcessPID, MessageAtom, Status, Room, ProcessStorage) -> 
    supervisor_events:on_response_received(fun room_process:terminate/1, ChildProcessPID, MessageAtom, Room, Status, Room,ProcessStorage).    
