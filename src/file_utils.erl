-module(file_utils).
-export([open_file/2]).

open_file(Filename, Mode) -> 
    filelib:ensure_dir(Filename),
    file:open(Filename, Mode).