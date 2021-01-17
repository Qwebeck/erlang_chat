-module(supervisor_events).
-export([on_request_received/3,on_response_received/7]).

on_request_received(FromProcessPID, ProcessName, ProcessStorage) ->
    {ok, {UserRequestHandler, ProcessStorageWithUserProcess}} = supervisor_process_storage:get_process_pid(ProcessName, ProcessStorage),
    UpdatedProcessStorage = supervisor_process_storage:add_invoker(ProcessName, ProcessStorageWithUserProcess, FromProcessPID),
    {ok, {UserRequestHandler, UpdatedProcessStorage}}.


on_response_received(TerminateChildProcess, ChildProcessPID, MessageAtom, ProcessName, Status, UserData, ProcessStorage) -> 
    if 
        erlang:process_info(ChildProcessPID, messages_queue_len) == 0
            -> TerminateChildProcess(ChildProcessPID)
    end,
    Notification = {Status, {MessageAtom, UserData}},
    supervisor_process_storage:notify_invokers(ProcessName, ProcessStorage, Notification),
    supervisor_process_storage:remove_process_item(ProcessName, ProcessStorage).
