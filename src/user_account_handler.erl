-module(user_account_handler).

-export([init/2]).

init(Req,
     State = #{user_manager_pid := UserManagerPID}) ->
    #{name := User, pass := Password} =
        cowboy_req:match_qs([name, pass], Req),
    {Status, Data} = get_user_info(UserManagerPID,
                                   User,
                                   Password),
    reply(Status, Data, Req, State).

get_user_info(UserManagerPID, User, Password) ->
    {Status, UserData} =
        user_manager:get_user_data(UserManagerPID,
                                   User,
                                   Password),
    case Status of
        ok -> {ok, UserData};
        wrong_credentials -> {wrong_credentials, []}
    end.

reply(ok, _UserData = #{user := UserName}, Req,
      State) ->
    Content = jsx:encode(#{<<"user">> =>
                               erlang:list_to_binary(UserName),
                           <<"loggedIn">> => true}),
    Res = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           Content,
                           Req),
    {ok, Res, State};
reply(wrong_credentials, _, Req, State) ->
    Res = cowboy_req:reply(403,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Wrong password!\n">>,
                           Req),
    {ok, Res, State}.
