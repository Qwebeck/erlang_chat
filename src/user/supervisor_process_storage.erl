-module(supervisor_process_storage).
-export([get_user_process_pid/2, add_process_item/2, remove_process_item/2, add_invoker/3, notify_invokers/3]).
% Functions to operate on sotrage of processes created by supervisor


% Creates valid records for user process storage
create_user_process_item(UserProcessPID) ->
    create_user_process_item(UserProcessPID, []).
create_user_process_item(UserProcessPID, Invokers) ->
    #{user_process_pid => UserProcessPID, invokers => Invokers}.

% Adds process item to ProcessStorage 
add_process_item(UserName, ProcessStorage) ->
    case maps:is_key(UserName, ProcessStorage) of 
        true-> 
            UserProcessItem = maps:get(UserName),
            {ok, {UserProcessItem, ProcessStorage}};
        false -> create_new_process(UserName, ProcessStorage)
    end.
create_new_process(UserName, ProcessStorage) ->
    NewProcessPID = spawn(user_process, user_process, []),    
    NewItem = create_user_process_item(NewProcessPID, []),
    {ok, {NewItem, maps:put(UserName, NewItem, ProcessStorage)}}.

% Obtains user process's PID. In case if process not exists - creates new process and associates it with user
get_user_process_pid(UserName, ProcessStorage) -> 
    case maps:is_key(UserName, ProcessStorage) of 
        true ->
            #{user_process_pid := UserProcessPID} = maps:get(UserName, ProcessStorage),
            {ok, {UserProcessPID, ProcessStorage}};
        false ->
            {ok, {#{user_process_pid := UserProcessPID}, UpdateProcessStorage}} = add_process_item(UserName, ProcessStorage),
            {ok, {UserProcessPID, UpdateProcessStorage}}
    end.

% Removes process associated with user name if its exists and returns updated storage
remove_process_item(UserName, ProcessStorage) -> 
    case maps:is_key(UserName, ProcessStorage) of 
        true -> {ok, maps:remove(UserName, ProcessStorage)};
        false -> {ok, ProcessStorage}
end.

get_invokers(UserName, ProcessStorage) -> 
    erlang:display(ProcessStorage),
    #{invokers := Invokers} = maps:get(UserName, ProcessStorage),
    Invokers.

% Updates invokers in process storage
add_invoker(UserName, ProcessStorage, Invoker) ->
    #{user_process_pid := UserProcessPID, invokers := Invokers} = maps:get(UserName, ProcessStorage),
    UpdatedInvokers = [Invoker] ++ Invokers,
    UpdatedUserProcessItem = create_user_process_item(UserProcessPID, UpdatedInvokers),
    maps:update(UserName, UpdatedUserProcessItem, ProcessStorage).

% notify all invokers assigned to the user name, by sending to them message passed as param
notify_invokers(UserName, ProcessStorage, Message) ->
    Invokers = get_invokers(UserName, ProcessStorage),
    notify_invoker(Invokers, Message).
notify_invoker([], _) -> ok;
notify_invoker([InvokerPid|T], Message) ->
    InvokerPid ! {self(), Message},
    notify_invoker(T, Message).

