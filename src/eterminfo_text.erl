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

%% Print a terminfo in a way similar to infocmp

-module(eterminfo_text).

-export([parse/1]).
-export([format/1, format/2]).

-export_type([format_opts/0]).

-type format_opts() :: #{comment => string(), % without leading #
                         _ => _}.

-spec parse(string()) -> {ok, eterminfo:terminfo()} | {error, term()}.
parse(String) ->
    eterminfo_text_parser:string(String).


-spec format(eterminfo:terminfo()) -> iolist().
format(TermInfo) ->
    format(TermInfo, #{}).

-spec format(eterminfo:terminfo(), format_opts()) -> iolist().
format(#{'$terminfo_names' := TermNames}=TermInfo, Opts) ->
    BoolCaps = maps:filter(fun(_K, V) -> V == true end,
                           TermInfo),
    NumericCaps = maps:filter(fun(_K, V) -> is_integer(V) end,
                              TermInfo),
    StrCaps = case TermInfo of
                  #{'$str_literals' := Literals} -> Literals;
                  #{} -> #{}
              end,
    Strs = append_caps(
             lists:sort([atom_to_list(B)
                         || B <- maps:keys(BoolCaps)]),
             lists:sort([atom_to_list(C) ++ "#" ++ integer_to_list(V)
                         || {C,V} <- maps:to_list(NumericCaps)]),
             lists:sort([atom_to_list(C) ++ "=" ++ to_infocmp_str(V)
                         || {C,V} <- maps:to_list(StrCaps)])),
    Lead = case Opts of
               #{comment := Comment} -> ["# ", Comment, "\n"];
               #{} -> ""
           end,
    [Lead,
     lists:join("|", TermNames), "\n",
     format_caps(Strs)].


to_infocmp_str("\e" ++ Rest)    -> "\\E" ++ to_infocmp_str(Rest);
to_infocmp_str("," ++ Rest)     -> "\\," ++ to_infocmp_str(Rest);
to_infocmp_str(":" ++ Rest)     -> "\\:" ++ to_infocmp_str(Rest);
to_infocmp_str("^" ++ Rest)     -> "\\^" ++ to_infocmp_str(Rest);
to_infocmp_str("\\" ++ Rest)    -> "\\\\" ++ to_infocmp_str(Rest);
to_infocmp_str("\200" ++ Rest)  -> "\\0" ++ to_infocmp_str(Rest);
to_infocmp_str([13,10] ++ Rest) -> "\\n" ++ to_infocmp_str(Rest);
to_infocmp_str([10] ++ Rest)    -> "\\l" ++ to_infocmp_str(Rest);
to_infocmp_str([13] ++ Rest)    -> "\\r" ++ to_infocmp_str(Rest);
to_infocmp_str([9] ++ Rest)     -> "\\t" ++ to_infocmp_str(Rest);
to_infocmp_str([8] ++ Rest)     -> "\\b" ++ to_infocmp_str(Rest);
to_infocmp_str([12] ++ Rest)    -> "\\f" ++ to_infocmp_str(Rest);
to_infocmp_str([32] ++ Rest)    -> "\\s" ++ to_infocmp_str(Rest);
to_infocmp_str([$%, C | Rest])  -> "%" ++ [C] ++ to_infocmp_str(Rest);
to_infocmp_str([C | Rest])      ->
    if C == 0   -> "^@" ++ to_infocmp_str(Rest);
       C < 27   -> "^" ++ [$A + C] ++ to_infocmp_str(Rest);
       C < 32   -> "\\" ++ to_oct(C) ++ to_infocmp_str(Rest);
       C >= 127 -> "\\" ++ to_oct(C) ++ to_infocmp_str(Rest);
       true     -> [C | to_infocmp_str(Rest)]
    end;
to_infocmp_str("") ->
    "".

to_oct(C) ->
    io_lib:format("~.8b", [C]).

append_caps(BoolCaps, NumCaps, StrCaps) ->
    newline_sep_unless_empty([BoolCaps, NumCaps, StrCaps]).

newline_sep_unless_empty([C1 | Rest]) ->
    if C1 == [] ->
            newline_sep_unless_empty(Rest);
       true ->
            case newline_sep_unless_empty(Rest) of
                [] -> C1;
                CRest -> lists:append([C1, [newline], CRest])
            end
    end;
newline_sep_unless_empty([]) ->
    [].

format_caps([]) -> "";
format_caps(Strs) -> [format_c2(Strs, 0, none, [])].

format_c2([Str | Rest], CurrCol, CurrLine, Lines) when is_list(Str) ->
    Str1 = [Str, ","],
    Str1Len = iolist_size(Str1),
    Sep = " ",
    SepLen = length(Sep),
    if CurrCol == 0 , CurrLine == none ->
            %% First entry on a line, always include it, never check for wrap.
            format_c2(Rest, Str1Len, [Str1], Lines);
       (CurrCol + SepLen + Str1Len) > 70 ->
            %% Adding makes it too long. Wrap before.
            Lines1 = acc_line(CurrLine, Lines),
            format_c2(Rest, Str1Len, [Str1], Lines1);
       true ->
            CurrLine1 = acc_str(CurrLine, Sep, Str1),
            CurrCol1 = CurrCol + SepLen + Str1Len,
            format_c2(Rest, CurrCol1, CurrLine1, Lines)
    end;
format_c2([newline | Rest], _CurrCol, CurrLine, Lines) ->
    Lines1 = acc_line(CurrLine, Lines),
    format_c2(Rest, 0, none, Lines1);
format_c2([], _, CurrLine, Lines) ->
    [["\t", Line, "\n"]
     || Line <- lists:reverse(acc_line(CurrLine, Lines))].

acc_line(none, Lines) -> Lines;
acc_line(Line, Lines) -> [lists:reverse(Line) | Lines].

acc_str(Line, Sep,  Str) -> [Str, Sep | Line].
