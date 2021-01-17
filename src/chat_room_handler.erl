-module(chat_room_handler).

-export([init/2]).

init(Req0,
     State = #{room_manager_pid := RoomManagerPID}) ->
    #{room := Room, user := User, message := Message,
      password := Password} =
        cowboy_req:match_qs([room,
                             user,
                             password,
                             {message, [], <<"">>}],
                            Req0),
    RequestObject = #{room_manager_pid => RoomManagerPID,
                      room => Room, user => User, message => Message,
                      request => Req0, state => State, password => Password},
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
check_room(true, RequestObject) ->
    check_user_rooms(RequestObject).

check_user_rooms(RequestObject = #{user := User,
                                   password := Password}) ->
    {Status, UserData} = user_supervisor:get_user_data(User,
                                                       Password),
    case Status of
        ok -> check_user_rooms(UserData, RequestObject);
        wrong_credentials -> {wrong_credentials, []}
    end.

check_user_rooms(_UserData = #{rooms := Rooms},
                 RequestObject = #{room := Room}) ->
    UserHasRoom = lists:member(Room, Rooms),
    check_user_rooms(UserHasRoom, RequestObject);
check_user_rooms(false,
                 RequestObject = #{user := User, room := Room}) ->
    room_manager:add_user_to_room(User, Room),
    check_user_rooms(true, RequestObject);
check_user_rooms(true, RequestObject) ->
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
