main(_) ->
    usage().

usage() ->
    io:format("usage: app <input_filename>r\n"),
    halt(1).
