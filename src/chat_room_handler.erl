-module(chat_room_handler).

-export([init/2]).

init(Req0,
     State = #{room_manager_pid := RoomManagerPID}) ->
    #{room := RoomBinary, user := UserBinary,
      message := MessageBinary} =
        cowboy_req:match_qs([room, user, {message, [], <<"">>}],
                            Req0),
    Room = erlang:binary_to_list(RoomBinary),
    User = erlang:binary_to_list(UserBinary),
    Message = erlang:binary_to_list(MessageBinary),
    RequsetObject = #{room_manager_pid => RoomManagerPID,
                      room => Room, user => User, message => Message,
                      request => Req0, state => State},
    check_chain(RequsetObject),
    reply(RequsetObject).

check_chain(RequsetObject) ->
    checkRoom(RequsetObject).

checkRoom(RequsetObject= #{room:=Room}) ->
    RoomExists = room_manager:room_exist(Room),
    checkRoom(RoomExists,RequsetObject).

checkRoom(false, RequsetObject = #{room_manager_pid:=RoomManagerPID,room:=Room}) ->
    room_manager:create_room_message(RoomManagerPID, Room),
    checkRoom(RequsetObject);
checkRoom(true, RequsetObject)->
    checkUserInRoom(RequsetObject).

checkUserInRoom(RequsetObject= #{user:=User,room:=Room}) ->
    UserInRoom = room_manager:user_in_room(User, Room),
    checkUserInRoom(UserInRoom,RequsetObject).

checkUserInRoom(false, _RequsetObject=#{user:=User,room:=Room}) ->
    room_manager:add_user_to_room(User, Room);
checkUserInRoom(true, RequsetObject) ->
    checkMessage(RequsetObject).

checkMessage(_RequsetObject=#{message:=""}) -> ok;
checkMessage(_RequsetObject=#{room_manager_pid:=RoomManagerPID,room:=Room,message:=Message,user:=User}) ->
    room_manager:write_message_to_room_action(RoomManagerPID, Room, Message, User),ok.


reply(_RequsetObject=#{room_manager_pid:=RoomManagerPID,request:=Req,room:=Room,state:=State}) ->
    ChatLog = room_manager:read_room_message(RoomManagerPID,
                                             Room),
    Response = erlang:list_to_binary(ChatLog),
    Request = cowboy_req:reply(200,
                               #{<<"content-type">> => <<"text/plain">>},
                               Response,
                               Req),
    {ok, Request, State}.
