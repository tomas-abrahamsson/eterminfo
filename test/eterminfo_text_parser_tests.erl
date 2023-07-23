%%% Copyright (C) 2023  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%%% MA  02110-1301  USA
-module(eterminfo_text_parser_tests).

-include_lib("eunit/include/eunit.hrl").

term_name_test() ->
    #{'$terminfo_names' := ["dummyname", "alias", "description"]} =
        parse_str_full(["#\tComment",
                        "dummyname|alias|description,"
                        "\ts=x,"]).

bool_capability_test() ->
    #{x := true} = parse_simple("x"),
    ok.

numeric_capability_test() ->
    #{n := 10} = parse_simple("n#10"),
    ok.

string_capability_test() ->
    #{s := "abc"} = parse_simple("s=abc").

backslash_escape_test() ->
    #{escapes := "\e\e"}   = parse_simple("escapes=\\e\\E"), % two forms
    #{newline := "\r\n"}   = parse_simple("newline=\\n"),
    #{linefeed := "\n"}    = parse_simple("linefeed=\\l"),
    #{return := "\r"}      = parse_simple("return=\\r"),
    #{tab := "\t"}         = parse_simple("tab=\\t"),
    #{backspace := "\b"}   = parse_simple("backspace=\\b"),
    #{formfeed := "\f"}    = parse_simple("formfeed=\\f"),
    #{space := " "}        = parse_simple("space=\\s"),
    #{circumflex  := "^"}  = parse_simple("circumflex=\\^"),
    #{backslash := "\\"}   = parse_simple("backslash=\\\\"),
    #{comma := ","}        = parse_simple("comma=\\,"),
    #{colon := ":"}        = parse_simple("colon=\\:"),
    %% "\0 will produce \200, which does not terminate a string but behaves
    %% as a null character on most terminals, providing CS7 is  specified.
    %% See stty(1)." -- the terminfo(5) man page.
    #{nul := [128]} = parse_simple("nul=\\0"), % \200 = 128
    ok.

control_char_test() ->
    %% The special case ^? is interpreted as DEL (127)
    #{del := [127]} = parse_simple("del=^?"),
    %% In all other cases, the character  value  is AND'd  with 0x1f,
    %% mapping to ASCII control codes in the range 0 through 31.
    %% #{ctrlchar := [128]}   = parse_simple("ctrlchar=^@"), % FIXME
    #{ctrlchar := [1]}     = parse_simple("ctrlchar=^A"),
    #{ctrlchar := [7]}     = parse_simple("ctrlchar=^G"),
    #{ctrlchar := [26]}    = parse_simple("ctrlchar=^Z"),
    #{ctrlchar := [27]}    = parse_simple("ctrlchar=^["),
    %%#{ctrlchar := [28]}  = parse_simple("ctrlchar=^\\"), % FIXME
    #{ctrlchar := [29]}    = parse_simple("ctrlchar=^]"),
    #{ctrlchar := [30]}    = parse_simple("ctrlchar=^^"),
    #{ctrlchar := [31]}    = parse_simple("ctrlchar=^_"),
    %%#{ctrlchar := [128]}  = parse_simple("ctrlchar=^`"), % FIXME
    #{ctrlchar := [1]}      = parse_simple("ctrlchar=^a"),
    %% #{ctrlchar := [27]}     = parse_simple("ctrlchar=^{"), % FIXME
    %% #{ctrlchar := [30]}     = parse_simple("ctrlchar=^~"), % FIXME
    ok.

octal_test() ->
    #{oct := [128]}   = parse_simple("oct=\\0"),
    %% #{oct := [128]}   = parse_simple("oct=\\000"), % FIXME
    #{oct := [128]}   = parse_simple("oct=\\200"),
    #{oct := [7]}     = parse_simple("oct=\\007"),
    #{oct := [255]}   = parse_simple("oct=\\377"),
    ok.

bitwise_xor_does_not_start_control_char_test() ->
    %% Check that for the the bitwise xor, %^, the ^ is not treated as
    %% the start of a control char.
    %% The expression reads: push 6, push 2, xor (this results in 4 on
    %% the stack) then the letter X in the output, then pop that number 4.
    #{bitxor := Bitxor,
      '$str_literals' := #{bitxor := "%{6}%{2}%^X%d"}} =
        parse_simple("bitxor=%{6}%{2}%^X%d"),
    "X4" = Bitxor(#{}),
    ok.

parse_simple(CapabilityText) ->
    parse_str(["\t" ++ CapabilityText ++ ","]).

parse_str(CapabilityLines) ->
    parse_str_full(["#\tComment",
                    "tname|some altname with spaces,"
                   | CapabilityLines]).

parse_str_full(Lines) ->
    S = ensure_ends_with_newline(string:join(Lines, "\n")),
    {ok, Result} = eterminfo_text_parser:string(S),
    Result.

ensure_ends_with_newline(S) ->
    case lists:last(S) of
        $\n -> S;
        _   -> S ++ "\n"
    end.
