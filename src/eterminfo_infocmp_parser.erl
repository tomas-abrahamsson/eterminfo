%%% Copyright (C) 2013  Tomas Abrahamsson
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

-module(eterminfo_infocmp_parser).
-export([string/1]).

-import(lists, [flatten/1, reverse/1]).

string(S) ->
    Dict = dict:new(),
    ParseFun = mk_parser_fun(),
    parse(S++[eof], ParseFun, Dict).

parse(S, ParseFun, Dict) ->
    case ParseFun(S) of
        {ok, {Key, Value}, ParseFun1} ->
            parse("", ParseFun1, dict:store(Key, Value, Dict));
        {error, _Reason} = Error->
            Error;
        done ->
            {ok, Dict};
        {more, ParseFun1} ->
            {more, ParseFun1}
    end.

mk_parser_fun() -> mk_cont("", 1).

mk_cont(S, EntryNum) ->
    fun(Data) when Data /= "" -> p(S++Data, EntryNum);
       ("")                   -> p(S, EntryNum)
    end.

mk_cont_lit(S, EntryNum, Key, LitVal) ->
    fun(Data) when Data /= "" ->
            {ok, {{literal,Key}, LitVal}, mk_cont(S++Data, EntryNum+1)};
       ("") ->
            {ok, {{literal,Key}, LitVal}, mk_cont(S, EntryNum+1)}
    end.

%% p parses one line at a time
p("#"++Rest, EntryNum) ->
    %% Comments start with a # in first column and last the line out
    case skip_rest_of_line(Rest) of
        {ok, Rest2} -> p(Rest2, EntryNum);
        need_more   -> {more, mk_cont("#"++Rest, EntryNum)};
        eof_seen    -> done
    end;
p(Data, EntryNum) ->
    case skip_whitespace_or_till_eoln(Data) of
        {ok, at_end_of_line, Rest} ->
            p(Rest, EntryNum);
        {ok, after_whitespace, Rest} ->
            case read_until_comma(Rest) of
                {ok, S, Rest2} when EntryNum == 1 ->
                    Ids = string:tokens(S, "|"),
                    {ok, {terminfo_id, Ids}, mk_cont(Rest2, EntryNum+1)};
                {ok, S, Rest2} when EntryNum > 1 ->
                    case parse_terminfo_string(S) of
                        {ok, Key, Value, LiteralValue} ->
                            {ok, {Key, Value},
                             mk_cont_lit(Rest2, EntryNum, Key, LiteralValue)};
                        cancelled_key ->
                            p(Rest2, EntryNum);
                        {error, _Reason} = Error ->
                            Error
                    end;
                need_more ->
                    {more, mk_cont(Rest, EntryNum)};
                eof_seen ->
                    done
            end;
        need_more ->
            {more, mk_cont(Data, EntryNum)};
        eof_seen ->
            done
    end.

skip_rest_of_line("\n"++Rest)  -> {ok, Rest};
skip_rest_of_line([eof])       -> eof_seen;
skip_rest_of_line([_C | Rest]) -> skip_rest_of_line(Rest);
skip_rest_of_line("")          -> need_more.

skip_whitespace_or_till_eoln("\n"++Rest) -> {ok, at_end_of_line, Rest};
skip_whitespace_or_till_eoln([eof])      -> eof_seen;
skip_whitespace_or_till_eoln(" "++Rest)  -> skip_whitespace_or_till_eoln(Rest);
skip_whitespace_or_till_eoln("\t"++Rest) -> skip_whitespace_or_till_eoln(Rest);
skip_whitespace_or_till_eoln("\r"++Rest) -> skip_whitespace_or_till_eoln(Rest);
skip_whitespace_or_till_eoln("\f"++Rest) -> skip_whitespace_or_till_eoln(Rest);
skip_whitespace_or_till_eoln("\v"++Rest) -> skip_whitespace_or_till_eoln(Rest);
skip_whitespace_or_till_eoln("")         -> need_more;
skip_whitespace_or_till_eoln(S)          -> {ok, after_whitespace, S}.

read_until_comma(S) -> read_until_comma(S, _Acc="").

read_until_comma("\\"++[C|T], Acc) -> read_until_comma(T, [C|[$\\|Acc]]);
read_until_comma(","++Rest, Acc)   -> {ok, flatten(reverse(Acc)), Rest};
read_until_comma([eof], _Acc)      -> eof_seen;
read_until_comma([C | Rest], Acc)  -> read_until_comma(Rest, [C | Acc]);
read_until_comma("", _Acc)         -> need_more.

parse_terminfo_string(S) ->
    case read_terminfo_key(S) of
        {ok, Key, ""} ->
            {ok, Key, true, ""};
        {ok, Key, "#"++Rest} ->
            case read_number(Rest) of
                {ok, N}         -> {ok, Key, N, Rest};
                {error, Reason} -> {error, {bad_number, Key, Rest, Reason}}
            end;
        {ok, Key, "="++Rest} ->
            case eterminfo_strscanner:string(Rest) of
                {ok, Tokens, EndLine} ->
                    EndToken = {'$end', EndLine},
                    case eterminfo_strparser:parse(Tokens++[EndToken]) of
                        {ok, Value}   -> {ok, Key, Value, Rest};
                        {error, Info} -> {error, {parse_error, Key,Rest, Info}}
                    end;
                {error, Info} ->
                    {error, {scan_error, Key,Rest, Info}}
            end;
        cancelled_key ->
            cancelled_key;
        {error, Reason} ->
            {error, {bad_key, S, Reason}}
    end.

read_terminfo_key(S) -> read_key(S, _Acc = "").

read_key([C | Rest], Acc) when C >= $a, C =< $z -> read_key(Rest, [C | Acc]);
read_key([C | Rest], Acc) when C >= $A, C =< $Z -> read_key(Rest, [C | Acc]);
read_key([C | Rest], Acc) when C >= $0, C =< $9 -> read_key(Rest, [C | Acc]);
read_key("."++Rest, Acc)                        -> read_key(Rest, "."++Acc);
read_key("_"++Rest, Acc)                        -> read_key(Rest, "_"++Acc);
read_key("#"++Rest, Acc)                        -> {ok,reverse(Acc),"#"++Rest};
read_key("="++Rest, Acc)                        -> {ok,reverse(Acc),"="++Rest};
read_key(" = "++Rest, Acc)                      -> {ok,reverse(Acc),"="++Rest};
read_key("", Acc)                               -> {ok,reverse(Acc),""};
read_key("@"++_Rest, _Acc)                      -> cancelled_key;
read_key([C | Rest], Acc) ->
    {error, {unexpected_char, {found, [C|Rest]}, {'after', reverse(Acc)}}}.


read_number(S) ->
    case catch read_number2(S) of
        N when is_integer(N) -> {ok, N};
        Crash                -> {error, Crash}
    end.

read_number2("0x" ++ Rest) -> read_hex(Rest);
read_number2("0" ++ Rest)  -> read_oct(Rest);
read_number2(Rest)         -> read_dec(Rest).

read_hex(S) -> rh(S, _Acc = 0).
rh([C | Rest], Acc) when C >= $0, C =< $9 -> rh(Rest, (Acc bsl 4) + (C-$0));
rh([C | Rest], Acc) when C >= $a, C =< $f -> rh(Rest, (Acc bsl 4) + (C-$a+10));
rh([C | Rest], Acc) when C >= $A, C =< $F -> rh(Rest, (Acc bsl 4) + (C-$A+10));
rh("", Acc)                               -> Acc.

read_oct(S) -> ro(S, _Acc = 0).
ro([C | Rest], Acc) when C >= $0, C =< $7 -> ro(Rest, (Acc bsl 3) + (C-$0));
ro("", Acc)                               -> Acc.

read_dec(S) -> list_to_integer(S).
