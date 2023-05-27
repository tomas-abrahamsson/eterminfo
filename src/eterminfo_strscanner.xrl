%% This line tells emacs to use -*- erlang -*- mode for this file

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

Definitions.

Rules.

\\E  :         {token, {char, TokenLine, 27}}.
\\e  :         {token, {char, TokenLine, 27}}.
\^.  :         {token, {char, TokenLine, extr_ctrlchar(TokenChars)}}.
\\,  :         {token, {char, TokenLine, $,}}.
\\:  :         {token, {char, TokenLine, $:}}.
\\\^ :         {token, {char, TokenLine, $^}}.
\\\\ :         {token, {char, TokenLine, $\\}}.
\\0 :          {token, {char, TokenLine, $\200}}.
\\[0-3][0-7][0-7] :
               {token, {char, TokenLine, extr_oct(TokenChars)}}.
\\n  :         {token, {chars,TokenLine, [13,10]}}. %% newline
\\l  :         {token, {char, TokenLine, 10}}.    %% line-feed
\\r  :         {token, {char, TokenLine, 13}}.    %% return
\\t  :         {token, {char, TokenLine, 9}}.     %% tab
\\b  :         {token, {char, TokenLine, 8}}.     %% backspace
\\f  :         {token, {char, TokenLine, 12}}.    %% form-feed
\\s  :         {token, {char, TokenLine, 32}}.    %% space

\%\%    :      {token, {char, TokenLine, $%}}.
\%((:?([+#\s-])*)?([1-9][0-9]*(\.([-]?[0-9]+)?)?)?)?[doxXs]        :
               {token, {pop,  TokenLine,{printf,split_fmt_str(TokenChars)}}}.
\%c :          {token, {pop,  TokenLine, as_char}}.
\%s :          {token, {pop,  TokenLine, as_string}}.
\%p[1-9] :     {token, {push, TokenLine, {param,extr_paramnum(TokenChars)}}}.
\%P[a-z] :     {token, {pop,  TokenLine, {dyn_var,extr_var(TokenChars)}}}.
\%g[a-z] :     {token, {push, TokenLine, {dyn_var,extr_var(TokenChars)}}}.
\%P[A-Z] :     {token, {pop,  TokenLine, {stat_var,extr_var(TokenChars)}}}.
\%g[A-Z] :     {token, {push, TokenLine, {stat_var,extr_var(TokenChars)}}}.
\%'.'    :     {token, {push, TokenLine, {int,extr_cconst(TokenChars)}}}.
\%\{[0-9]+\} : {token, {push, TokenLine, {int,extr_iconst(TokenChars)}}}.
\%l  :         {token, {op1,  TokenLine, strlen}}.
\%\+ :         {token, {op2,  TokenLine, add}}.
\%-  :         {token, {op2,  TokenLine, sub}}.
\%\* :         {token, {op2,  TokenLine, mul}}.
\%/  :         {token, {op2,  TokenLine, 'div'}}.
\%m  :         {token, {op2,  TokenLine, 'mod'}}.
\%&  :         {token, {op2,  TokenLine, bitand}}.
\%\| :         {token, {op2,  TokenLine, bitor}}.
\%\^ :         {token, {op2,  TokenLine, bitxor}}.
\%=  :         {token, {op2,  TokenLine, eq}}.
\%<  :         {token, {op2,  TokenLine, lt}}.
\%>  :         {token, {op2,  TokenLine, gt}}.
\%A  :         {token, {op2,  TokenLine, logand}}.
\%O  :         {token, {op2,  TokenLine, logor}}.
\%!  :         {token, {op1,  TokenLine, lognot}}.
\%~  :         {token, {op1,  TokenLine, bitnot}}.
\%i  :         {token, {op1,  TokenLine, incr}}.
\%\? :         {token, {'if', TokenLine}}.
\%t  :         {token, {then, TokenLine}}.
\%e  :         {token, {else, TokenLine}}.
\%;  :         {token, {endif,TokenLine}}.

\$<[0-9]+[*/]?[*/]?> :
               {token, {pad,  TokenLine, extr_padding(TokenChars)}}.

.    :         {token, {char, TokenLine, hd(TokenChars)}}.

Erlang code.

-import(lists, [reverse/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%% extraction routines...

extr_ctrlchar([$^, $@])  -> 0;
extr_ctrlchar([$^, C]) when C >= $A, C =< $Z -> C - $A + 1;
extr_ctrlchar([$^, C]) when C >= $a, C =< $z -> C - $a + 1;
extr_ctrlchar([$^, $\[]) -> 27;
extr_ctrlchar([$^, $\\]) -> 28;
extr_ctrlchar([$^, $\]]) -> 29;
extr_ctrlchar([$^, $^])  -> 30;
extr_ctrlchar([$^, $_])  -> 31.


extr_oct([$\\, O1, O2, O3]) ->
    ((O1 - $0) bsl (3+3)) + ((O2 - $0) bsl 3) + (O3 - $0).

extr_paramnum("%p"++[N]) -> N - $0.
extr_var("%g"++V) -> V;
extr_var("%P"++V) -> V.
extr_cconst([$\%, $', C, $']) -> C.

extr_iconst("%{"++Rest) -> extrint(Rest,0).
extrint("}", Acc)        -> Acc;
extrint([N | Rest], Acc) -> extrint(Rest, Acc*10 + (N - $0)).


extr_padding("$<"++Rest) ->
    {NStr, Rest2} = extr_padding_n(Rest),
    N = list_to_integer(NStr),
    Proportional = lists:member($*, Rest2),
    Mandatory = lists:member($/, Rest2),
    #{delay        => N,
      proportional => Proportional,
      mandatory    => Mandatory}.

extr_padding_n(Str) -> extr_p_n(Str, _Acc = "").
extr_p_n([C | Rest], Acc) when C >= $0, C =< $9 -> extr_p_n(Rest, [C | Acc]);
extr_p_n("*"++Rest, Acc)                        -> {reverse(Acc), "*"++Rest};
extr_p_n("/"++Rest, Acc)                        -> {reverse(Acc), "/"++Rest};
extr_p_n(">"++Rest, Acc)                        -> {reverse(Acc), Rest}.


split_fmt_str("%"++Rest) -> split_fmt_str1(Rest).

% Pull out colon, then continue
split_fmt_str1(":"++Rest) -> split_fmt_str2(Rest, true);
split_fmt_str1(Rest)      -> split_fmt_str2(Rest, false).

%% Pull out flags, then continue
split_fmt_str2(S, Colon) ->
    {Flags, Rest} = extr_flags(S),
    split_fmt_str3(Rest, Colon, Flags).

extr_flags(S) -> extr_flags2(S, _Acc = "").

extr_flags2("-"++Rest, Acc) -> extr_flags2(Rest, Acc++"-");
extr_flags2("+"++Rest, Acc) -> extr_flags2(Rest, Acc++"+");
extr_flags2("#"++Rest, Acc) -> extr_flags2(Rest, Acc++"#");
extr_flags2(" "++Rest, Acc) -> extr_flags2(Rest, Acc++" ");
extr_flags2(Rest, Acc)      -> {Acc, Rest}.

%% Pull out width, then continue
split_fmt_str3(S, Colon, Flags) ->
    case extr_width(S) of
        no_width      -> split_fmt_str5(S,Colon,Flags,no_width,no_precision);
        {Width, Rest} -> split_fmt_str4(Rest,Colon,Flags,Width)
    end.

extr_width(S) when hd(S) >= $0, hd(S) =< $9 -> extr_w2(S, _Acc = 0);
extr_width(_) -> no_width.

extr_w2([C|Rest], Acc) when C >= $0, C =< $9 -> extr_w2(Rest, Acc*10 + (C-$0));
extr_w2(Rest, Acc)                           -> {Acc, Rest}.

%% Pull out precision, then continue
split_fmt_str4("."++Rest, Colon, Flags, Width) ->
    {Precision, Rest2} = extr_precision(Rest),
    split_fmt_str5(Rest2, Colon, Flags, Width, Precision);
split_fmt_str4(Rest, Colon, Flags, Width) ->
    split_fmt_str5(Rest, Colon, Flags, Width, _Precision=no_precision).

extr_precision(S) -> extr_p2(S, 0).
extr_p2([C|Rest], Acc) when C >= $0, C =< $9 -> extr_p2(Rest, Acc*10 + (C-$0));
extr_p2(Rest, Acc)                           -> {Acc, Rest}.

%% Pull out conversion type, then return
split_fmt_str5("d",C,Fs,W,P) -> {C,Fs,W,P,dec};
split_fmt_str5("o",C,Fs,W,P) -> {C,Fs,W,P,oct};
split_fmt_str5("x",C,Fs,W,P) -> {C,Fs,W,P,hex_lc};
split_fmt_str5("X",C,Fs,W,P) -> {C,Fs,W,P,hex_uc};
split_fmt_str5("s",C,Fs,W,P) -> {C,Fs,W,P,str}.
