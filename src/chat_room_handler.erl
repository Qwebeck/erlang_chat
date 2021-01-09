-module(chat_room_handler).

-export([init/2]).

-define(ROOM_DIRECTORY, "rooms/").

-define(ROOM_FILE_ENDING, ".room").

init(Req0, State) ->
    #{room := RoomBinary, user := UserBinary,
      message := MessageBinary} =
        cowboy_req:match_qs([room, user, {message, [], none}],
                            Req0),
    Room = erlang:binary_to_list(RoomBinary),
    User = erlang:binary_to_list(UserBinary),

    %FileExisted= filelib:is_regular( User++".room"),
    %erlang:display(FileExisted),
    write_user_to_room(User, Room),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello from chat room!\r\n">>,
                           Req0),
    {ok, Req, State}.

write_user_to_room(User, Room) ->
    erlang:display((?ROOM_DIRECTORY) ++
                       Room ++ (?ROOM_FILE_ENDING)),
    case filelib:is_file(Room ++ ".room") of
        true -> erlang:display("file exists");
        false -> erlang:display("no such file")
    end.

    

% if no room file:
%    create room file
% open room file
% if not user in room file
%    add user to file
% if message
%    add user says: ""
% send  file to user

