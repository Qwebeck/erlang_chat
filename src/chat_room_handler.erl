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
    RoomExists = room_exist(Room),
    erlang:display(RoomExists),
    if not RoomExists -> create_room(Room);
       true -> ok
    end,

    UserInRoom = user_in_room(User, Room),
    erlang:display(UserInRoom),
    if not UserInRoom -> add_user_to_room(User, Room);
       true -> ok
    end,
    if MessageBinary /= none ->
           Message = erlang:binary_to_list(MessageBinary),
           erlang:display(Message),
           write_message_to_room(Room, Message, User);
       true -> ok
    end,
    ChatLog = read_room_file(Room),
    Response = erlang:list_to_binary(ChatLog),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           Response,
                           Req0),
    {ok, Req, State}.

add_user_to_room(User, Room) ->
    {ok, File} = file:open(room_file(Room), [append]),
    io:format(File, "~s~n", [User]).

user_in_room(User, Room) ->
    FileName = user_account_handler:create_filename(User),
    % Rooms = user_account_handler:parse_user_rooms(FileName),
    % erlang:display(Rooms),
    true.

room_file(Room) ->
    (?ROOM_DIRECTORY) ++ Room ++ (?ROOM_FILE_ENDING).

write_message_to_room(Room, Message, User) ->
    {ok, File} = file:open(room_file(Room), [append]),
    io:format(File, "~s says:\"~s\"~n", [User, Message]).

read_room_file(Room) ->
    {Status, File} = file:read_file(room_file(Room)),
    case Status of
        ok ->
            Content = unicode:characters_to_list(File),
            file:close(File),
            Content;
        error -> error
    end.

room_exist(Room) -> filelib:is_file(room_file(Room)).

create_room(Room) ->
    FileName = room_file(Room),
    filelib:ensure_dir(FileName),
    {ok, File} = file:open(FileName, [write]),
    io:format(File, "", []),
    ok.
