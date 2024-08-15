%#!/usr/bin/env escript

-module(main).
-export([parse_part/1, parse_workflow/1, parse_workflow2/2]).

is_numeric(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    is_number(Float) orelse is_number(Int).

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

readlines(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

parse_workflow(Workflow) ->
    RegExp = "(?<WN>[a-zA-Z]*){(x(?<XS>[<>])(?<XN>[0-9]*):(?<XWN>[a-zA-Z]*),)?(m(?<MS>[<>])(?<MN>[0-9]*):(?<MWN>[a-zA-Z]*),)?(a(?<AS>[<>])(?<AN>[0-9]*):(?<AWN>[a-zA-Z]*),)?(s(?<SS>[<>])(?<SN>[0-9]*):(?<SWN>[a-zA-Z]*),)?                        (?<EWN>[a-zA-Z]*)}",
    case re:run(Workflow, RegExp) of
        {match, Matchings} -> 
            io:format("re:run result = ~w ~n", [Matchings]);
        nomatch -> -1
    end.

% { ValueInfo { name, cmp_s, value, term_rule }, otherwise_rule }

parse_workflow2([WH1 | WT1], []) 
  when WH1 == "x"; WH1 == "m"; WH1 == "a"; WH1 == "s" ->
    WH1;
parse_workflow2([WH1 | WT1], []) 
  when WH1 == "," ->
    WH1;
parse_workflow2([WH1 | WT1], []) 
  when WH1 == "<"; WH1 == ">" ->
    WH1;
parse_workflow2([WH1 | WT1], []) ->
    is_numeric(WH1),
    comma_pos = string:str([WH1 | WT1], ","),
    result = string:str([WH1 | WT1], comma_pos),
    string:to_integer(result).

parse_part(Part) ->
    RegExp = "{x=(?<X>[0-9]*),m=(?<M>[0-9]*),a=(?<A>[0-9]*),s=(?<S>[0-9]*)}",
    case re:run(Part, RegExp) of
        {match, [_, {X0S, X0L}, {M1S, M1L}, {A2S, A2L}, {S3S, S3L}]} -> 
            {X, []} = string:to_integer(string:sub_string(Part, X0S + 1, X0S + X0L)),
            {M, []} = string:to_integer(string:sub_string(Part, M1S + 1, M1S + M1L)),
            {A, []} = string:to_integer(string:sub_string(Part, A2S + 1, A2S + A2L)),
            {S, []} = string:to_integer(string:sub_string(Part, S3S + 1, S3S + S3L)),
            io:format("re:run result = [~p, ~p, ~p, ~p] ~n", [X, M, A, S]);
        nomatch -> -1
    end.

part1(_) ->
    0.

part2(_) ->
    0.

main([Filename]) ->
    try
        Lines = readlines(Filename),
        Part1_sln = part1(Lines),
        Part2_sln = part2(Lines),
        io:format("part1_sln = ~w\n", [Part1_sln]),
        io:format("part2_sln = ~w\n", [Part2_sln])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: app <input_filename>r\n"),
    halt(1).
