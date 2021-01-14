-module(chat_room_handler).

-export([init/2]).

init(Req0,
     State = #{room_manager_pid := RoomManagerPID,
               user_manager_pid := UserManagerPID}) ->
    #{room := RoomBinary, user := UserBinary,
      message := MessageBinary} =
        cowboy_req:match_qs([room, user, {message, [], <<"">>}],
                            Req0),
    Room = erlang:binary_to_list(RoomBinary),
    User = erlang:binary_to_list(UserBinary),
    Message = erlang:binary_to_list(MessageBinary),
    RequestObject = #{room_manager_pid => RoomManagerPID,
                      user_manager_pid => UserManagerPID, room => Room,
                      user => User, message => Message, request => Req0,
                      state => State},
    check_chain(RequestObject),
    reply(RequestObject).

check_chain(RequestObject) -> check_room(RequestObject).

check_room(RequestObject = #{room := Room}) ->
    RoomExists = room_manager:room_exist(Room),
    check_room(RoomExists, RequestObject).

check_room(false,
           RequestObject = #{room_manager_pid := RoomManagerPID,
                             room := Room}) ->
    room_manager:create_room_message(RoomManagerPID, Room),
    check_room(RequestObject);
check_room(true,
           RequestObject = #{user := User, room := Room, user_manager_pid:= UserManagerPID}) ->
    user_manager:add_user_chat_room_action(UserManagerPID, User, Room),
    check_message(RequestObject).

check_message(_RequestObject = #{message := ""}) -> ok;
check_message(_RequestObject = #{room_manager_pid :=
                                     RoomManagerPID,
                                 room := Room, message := Message,
                                 user := User}) ->
    room_manager:write_message_to_room_action(RoomManagerPID,
                                              Room,
                                              Message,
                                              User),
    ok.

reply(_RequestObject = #{room_manager_pid :=
                             RoomManagerPID,
                         request := Req, room := Room, state := State}) ->
    ChatLog = room_manager:read_room_message(RoomManagerPID,
                                             Room),
    Response = jsx:encode(#{<<"chatLog">> => ChatLog}),
    Request = cowboy_req:reply(200,
                               #{<<"content-type">> => <<"application/json">>},
                               Response,
                               Req),
    {ok, Request, State}.
