-module(supervisor_process_storage).
-export([get_process_pid/2, add_process_item/2, remove_process_item/2, add_invoker/3, notify_invokers/3]).
% Functions to operate on sotrage of processes created by supervisor


% Creates valid records for user process storage
create_process_item(ProcessPID) ->
    create_process_item(ProcessPID, []).
create_process_item(ProcessPID, Invokers) ->
    #{process_pid => ProcessPID, invokers => Invokers}.

% Adds process item to ProcessStorage 
add_process_item(ProcessName, ProcessStorage) ->
    case maps:is_key(ProcessName, ProcessStorage) of 
        true-> 
            UserProcessItem = maps:get(ProcessName),
            {ok, {UserProcessItem, ProcessStorage}};
        false -> create_new_process(ProcessName, ProcessStorage)
    end.
create_new_process(ProcessName, ProcessStorage) ->
    NewProcessPID = spawn(user_process, user_process, []),    
    NewItem = create_process_item(NewProcessPID, []),
    {ok, {NewItem, maps:put(ProcessName, NewItem, ProcessStorage)}}.

% Obtains user process's PID. In case if process not exists - creates new process and associates it with user
get_process_pid(ProcessName, ProcessStorage) -> 
    case maps:is_key(ProcessName, ProcessStorage) of 
        true ->
            #{process_pid := ProcessPID} = maps:get(ProcessName, ProcessStorage),
            {ok, {ProcessPID, ProcessStorage}};
        false ->
            {ok, {#{process_pid := ProcessPID}, UpdateProcessStorage}} = add_process_item(ProcessName, ProcessStorage),
            {ok, {ProcessPID, UpdateProcessStorage}}
    end.

% Removes process associated with user name if its exists and returns updated storage
remove_process_item(ProcessName, ProcessStorage) -> 
    case maps:is_key(ProcessName, ProcessStorage) of 
        true -> {ok, maps:remove(ProcessName, ProcessStorage)};
        false -> {ok, ProcessStorage}
end.

get_invokers(ProcessName, ProcessStorage) -> 
    #{invokers := Invokers} = maps:get(ProcessName, ProcessStorage),
    Invokers.

% Updates invokers in process storage
add_invoker(ProcessName, ProcessStorage, Invoker) ->
    #{process_pid := ProcessPID, invokers := Invokers} = maps:get(ProcessName, ProcessStorage),
    UpdatedInvokers = [Invoker] ++ Invokers,
    UpdatedUserProcessItem = create_process_item(ProcessPID, UpdatedInvokers),
    maps:update(ProcessName, UpdatedUserProcessItem, ProcessStorage).

% notify all invokers assigned to the user name, by sending to them message passed as param
notify_invokers(ProcessName, ProcessStorage, Message) ->
    Invokers = get_invokers(ProcessName, ProcessStorage),
    notify_invoker(Invokers, Message).
notify_invoker([], _) -> ok;
notify_invoker([InvokerPid|T], Message) ->
    InvokerPid ! {self(), Message},
    notify_invoker(T, Message).

