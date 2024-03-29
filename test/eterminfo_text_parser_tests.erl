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
    #{nul := [128]} = parse_simple("nul=\\000"), % \200 = 128
    #{nul := [128]} = parse_simple("nul=\\200"), % \200 = 128
    ok.

control_char_test() ->
    %% The special case ^? is interpreted as DEL (127)
    #{del := [127]} = parse_simple("del=^?"),
    %% "In all other cases, the character  value  is AND'd  with 0x1f,
    %% mapping to ASCII control codes in the range 0 through 31."
    %% ... but ^@ turns info \200 = 128 just like \0 and \000
    #{ctrlchar := [128]}   = parse_simple("ctrlchar=^@"),
    #{ctrlchar := [1]}     = parse_simple("ctrlchar=^A"),
    #{ctrlchar := [1]}     = parse_simple("ctrlchar=^a"),
    #{ctrlchar := [7]}     = parse_simple("ctrlchar=^G"),
    #{ctrlchar := [7]}     = parse_simple("ctrlchar=^g"), % lower case = upper
    #{ctrlchar := [26]}    = parse_simple("ctrlchar=^Z"),
    #{ctrlchar := [26]}    = parse_simple("ctrlchar=^z"),
    #{ctrlchar := [27]}    = parse_simple("ctrlchar=^["),
    #{ctrlchar := [28]}    = parse_simple("ctrlchar=^\\"),
    #{ctrlchar := [29]}    = parse_simple("ctrlchar=^]"),
    #{ctrlchar := [30]}    = parse_simple("ctrlchar=^^"),
    #{ctrlchar := [31]}    = parse_simple("ctrlchar=^_"),
    #{ctrlchar := [128]}   = parse_simple("ctrlchar=^`"),
    #{ctrlchar := [27]}    = parse_simple("ctrlchar=^{"),
    #{ctrlchar := [28]}    = parse_simple("ctrlchar=^|"),
    #{ctrlchar := [29]}    = parse_simple("ctrlchar=^}"),
    #{ctrlchar := [30]}    = parse_simple("ctrlchar=^~"),
    ok.

octal_test() ->
    #{oct := [128]}   = parse_simple("oct=\\0"),
    #{oct := [128]}   = parse_simple("oct=\\000"),
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

multiline_str_cap_test() ->
    %% From the terminfo(5) map page in ncurses 6.4:
    %%
    %%   "String capabilities can be split into multiple lines, just
    %%    as the fields comprising a terminal entry can be split into
    %%    multiple lines.  While blanks between fields are ignored,
    %%    blanks embedded within a string value are retained, except
    %%    for leading blanks on a line."
    %%
    %% For examples of such multi-line entries, run infocmp -1 -f on a
    %% a sufficiently complex term, such as xterm. It gets indented with
    %% several TAB characters.
    #{'$str_literals' := #{multiline := "mab"},
      multiline := "mab"} =
        parse_str_full(["#\tComment",
                        "dummyname|dummydescr,",
                        "\tmultiline=m", % <- does not end with comma
                        "\t\ta",
                        "\t\tb,"]), % <- here is the terminating comma
    %% Something more complex from a real entry:
    #{'$str_literals' := #{initc := ("\e]4;"
                                     "%p1%d;rgb:"
                                     "%p2%{255}%*%{1000}%/%2.2X/"
                                     "%p3%{255}%*%{1000}%/%2.2X/"
                                     "%p4%{255}%*%{1000}%/%2.2X\e\\"),
                           setaf := ("\e["
                                     "%?"
                                     ""    "%p1%{8}%<"
                                     ""    "%t3"
                                     ""    "%p1%d"
                                     "%e"
                                     ""    "%p1%{16}%<"
                                     ""    "%t9"
                                     ""    "%p1%{8}%-%d"
                                     "%e38;5;"
                                     ""    "%p1%d"
                                     "%;"
                                     "m")},
      initc := Initc,
      setaf := SetAF} =
        parse_str_full(["#\tComment",
                        "dummyname|alias|description,",
                        "\tinitc=\\E]4;",
                        "\t\t%p1%d;rgb:",
                        "\t\t%p2%{255}%*%{1000}%/%2.2X/",
                        "\t\t%p3%{255}%*%{1000}%/%2.2X/",
                        "\t\t%p4%{255}%*%{1000}%/%2.2X\\E\\\\,",
                        "\ts=x,",
                        "\tsetaf=\\E[",
                        "\t\t%?",
                        "\t\t\t%p1%{8}%<",
                        "\t\t\t%t3",
                        "\t\t\t%p1%d",
                        "\t\t%e",
                        "\t\t\t%p1%{16}%<",
                        "\t\t\t%t9",
                        "\t\t\t%p1%{8}%-%d",
                        "\t\t%e38;5;",
                        "\t\t\t%p1%d",
                        "\t\t%;",
                        "\t\tm,"]),
    "\e]4;1;rgb:FF/7F/3F\e\\" = Initc(1, 1000, 500, 250, #{}),
    "\e[31m"      = SetAF(1, #{}),
    "\e[91m"      = SetAF(9, #{}),
    "\e[38;5;17m" = SetAF(17, #{}),
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
