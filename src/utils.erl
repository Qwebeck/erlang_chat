-module(utils).
-export([open_file/2, await_response_to/1, await_response_to/2]).

open_file(Filename, Mode) -> 
    filelib:ensure_dir(Filename),
    file:open(Filename, Mode).

await_response_to(PID) ->
    receive
        {PID, Data} -> Data
    end.

await_response_to(PID, AwaitedMessage) -> 
    %% Awaits for specific message from PID
    receive 
        {PID, {Status, {AwaitedMessage, Payload}}} -> {Status, Payload}
    end.
