-module(room_supervisor).
-export([create_room/1, on_room_created/1, write_message_to_room/3, on_message_added/3, read_room/1, on_room_data_readed/1]).

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
    RoomSupervisorPID ! {self(), {read_room, Room, Message, User}},
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
            room_process:create_room(RoomRequestHandler),
            room_supervisor(UpdatedProcessStorage);
        {From, {on_room_created, {Status, Room}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_room_created, Status, UserData, ProcessStorage),
            room_supervisor(UpdatedProcessStorage);

        {From, {write_message, Room, Message, User}} ->
            {ok, {RoomRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, Room, ProcessStorage),
            room_process:write_message(RoomRequestHandler),
            room_supervisor(UpdatedProcessStorage);
        {From, {on_message_added, {Status, Room}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_message_added, Status, UserData, ProcessStorage),
            room_supervisor(UpdatedProcessStorage);
                    
        {From, {read_room, Room}} ->
            {ok, {RoomRequestHandler, UpdatedProcessStorage}} =  supervisor_events:on_request_received(From, Room, ProcessStorage),
            room_process:create_room(RoomRequestHandler),
            room_supervisor(UpdatedProcessStorage);
        {From, {on_read_room, {Status, Room}}} ->
            {ok, UpdatedProcessStorage} = on_response_received(From, on_read_room, Status, UserData, ProcessStorage),
            room_supervisor(UpdatedProcessStorage);


end.

on_response_received(ChildProcessPID, MessageAtom, Status, RoomData, ProcessStorage) -> 
    #{room := RoomName} = RoomData,
    supervisor_events:on_response_received(fun room_process:terminate/1, ChildProcessPID, MessageAtom, UserName, Status, UserData, ProcessStorage).    
