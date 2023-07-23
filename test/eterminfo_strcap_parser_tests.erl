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
-module(eterminfo_strcap_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%% This tests both the eterminfo_strcap_scanner and eterminfo_strcap_parser
%% modules

delay_capability_test() ->
    [27,75,{pad,#{delay        := 5,
                  proportional := false,
                  mandatory    := false}}] = parse_str("\eK$<5>").

%% Various parameterized string tests
%%
%% Parameterized strings are evaluated with a stack machine.
%% Values and parameters are pushed and added to the string when popped.
%% Variables (dynamic and static) can be set and retrieved.
%% The language which the stack machine evaluates, is described
%% in the terminfo(5) man page. (at least, that's the case on Linux)

paramstr_percent_test() ->
    "%" = parse_str("%%").

paramstr_simple_push_pop_test() ->
    %% push param(s), pop and printf with %d
    PP1 = parse_str("%p1%d"),
    "1"  = PP1(1, #{}),
    "2"  = PP1(2, #{}),
    PP2 = parse_str("%p1%p2%d%d"),
    "21" = PP2(1,2, #{}),
    "12" = PP2(2,1, #{}),

    %% %s for string formatting
    PS = parse_str("%p1%s"),
    "ab" = PS("ab", #{}),
    ""   = PS("", #{}),
    %% %c for char formatting
    PC = parse_str("%p1%c"),
    "a"  = PC($a, #{}),

    %% pushing a letter
    PX = parse_str("%'x'%c"),
    "x"  = PX(#{}),
    %% pushing a number
    PN = parse_str("%{65}%d"),
    "65" = PN(#{}),
    ok.

paramstr_str_formatting_test() ->
    %% %[[:]flags][width[.precision]][doxXs]
    %% where flags are [-+#] and space
    %% the : with flag=- avoids interpreting %- as an op
    D1  = parse_str(">%p1%2d<"),    % right-adjusted width 2
    D2  = parse_str(">%p1%:-2d<"),  % left-adjusted width 2
    X11 = parse_str(">%p1%:-4x<"),  % hex left-adj width 4
    X12 = parse_str(">%p1%#4x<"),   % hex left-adj width 4
    X13 = parse_str(">%p1%:-#6x<"), % hex left-adj width 6
    X21 = parse_str(">%p1%:-4X<"),  % upper-case hex left-adj width 4
    X22 = parse_str(">%p1%#4X<"),   % upper-case hex left-adj width 4
    X23 = parse_str(">%p1%:-#6X<"), % upper-case hex left-adj width 6
    O1  = parse_str(">%p1%:-4o<"),  % oct left-adj width 4
    S1  = parse_str(">%p1%:-4s<"),  % string left-adj width 4
    "> 2<"     = D1(2, #{}),
    ">2 <"     = D2(2, #{}),
    ">c0  <"   = X11(192, #{}),
    ">0xc0<"   = X12(192, #{}),
    ">0xc0  <" = X13(192, #{}),
    ">C0  <"   = X21(192, #{}),
    ">0XC0<"   = X22(192, #{}),
    ">0XC0  <" = X23(192, #{}),
    ">377 <"   = O1(255, #{}),
    ">abc <"   = S1("abc", #{}),
    ok.

paramstr_dyn_var_test() ->
    %% push param 1, (%p1)
    %% pop into a dynamic var a, (%Pa)
    %% get that dynamic var and push it (%ga)
    %% pop and printf it (%d)
    %% get+push and pop+printf it again
    PP2 = parse_str("%p1%Pa%ga%d%ga%d"),
    "22" = PP2(2, #{}),
    ok.

paramstr_static_var_test() ->
    %% same as dyn var, but with a static var
    %% (uppercase A means static var instead of dynamic)
    PP3 = parse_str("%p1%Pa%p2%PA%gA%d-%ga%d-%gC%d"),
    "3-4-5" = PP3(4, 3, #{"C" => 5}),
    ok.

paramstr_strlen_op_test() ->
    L = parse_str("%p1%l%d"),
    "6" = L("abcdef", #{}),
    "7" = L("abcdefg", #{}),
    ok.

paramstr_bin_arith_op_test() ->
    AAdd = parse_str("%p1%p2%+%d"),
    ASub = parse_str("%p1%p2%-%d"),
    AMul = parse_str("%p1%p2%*%d"),
    ADiv = parse_str("%p1%p2%/%d"),
    AMod = parse_str("%p1%p2%m%d"),
    AInc = parse_str("%i%p1%d%p2%d"),
    "3"  = AAdd(1, 2, #{}),
    "4"  = ASub(6, 2, #{}),
    "12" = AMul(6, 2, #{}),
    "4" = ADiv(8, 2, #{}),
    "1" = AMod(13, 3, #{}),
    "95" = AInc(8, 4, #{}),
    ok.

paramstr_bit_op_test() ->
    BAnd = parse_str("%p1%p2%&%d"),
    BOr  = parse_str("%p1%p2%|%d"),
    BXor = parse_str("%p1%p2%^%d"),
    BNot = parse_str("%p1%~%d"),
    "1"  = BAnd(1, 3, #{}),
    "7"  = BOr(6, 3, #{}),
    "5"  = BXor(6, 3, #{}),
    "-7" = BNot(6, #{}), %% FIXME: is this correct?
    ok.

paramstr_logical_op_test() ->
    LAnd = parse_str("%p1%p2%A%d"),
    LOr  = parse_str("%p1%p2%O%d"),
    LNot = parse_str("%p1%!%d"),

    "0"  = LAnd(1, 0, #{}),
    "1"  = LAnd(1, 3, #{}),

    "0"  = LOr(0, 0, #{}),
    "1"  = LOr(1, 0, #{}),
    "1"  = LOr(0, 7, #{}),

    "1"  = LNot(0, #{}),
    "0"  = LNot(1, #{}),
    ok.

paramstr_cmp_op_test() ->
    LT = parse_str("%p1%p2%<%d"),
    GT = parse_str("%p1%p2%>%d"),
    EQ = parse_str("%p1%p2%=%d"),

    "1"  = LT(2, 3, #{}),
    "0"  = LT(2, 2, #{}),
    "0"  = LT(2, 1, #{}),

    "0"  = GT(2, 3, #{}),
    "0"  = GT(2, 2, #{}),
    "1"  = GT(2, 1, #{}),

    "1"  = EQ(0, 0, #{}),
    "1"  = EQ(1, 1, #{}),
    "0"  = EQ(2, 3, #{}),
    ok.

paramstr_if_then_else_test() ->
    %% if-then:
    %% push p1 (%p1)
    %% push 3  (%{3})
    %% test equality (%=)
    %% if-then %? <test-for-3> %t <then-expr> %;

    %% the <then-expr> is the string t
    IfThen = parse_str("%?%p1%{3}%=%tt%;"),
    "t" = IfThen(3, #{}),
    ""  = IfThen (4, #{}),

    %% if-then-else: %e <else-part>
    IfThenElse = parse_str("%?%p1%{3}%=%tt%ee%;"),
    "t"  = IfThenElse(3, #{}),
    "e"  = IfThenElse(4, #{}),

    %% if-then-else-if-then-else
    IfThenElsif = parse_str("%?%p1%{3}%>%!%tt1%e%p1%{8}%<%tt2%ee%;"),
    "t1"  = IfThenElsif(2, #{}),
    "t2"  = IfThenElsif(7, #{}),
    "e"   = IfThenElsif(9, #{}),
    ok.

paramstr_if_no_then_test() ->
    %% wy350 has an add cap: is3=\E%?
    %% Like an if clause but neither then nor else?!
    %% The %? evaluates to the empty string when calling tparm with ncurses
    "\e" = parse_str("\e%?"),
    ok.

missing_endif_test() ->
    %% tw52 has this text for the capability 'setf' (set foreground):
    %% "\\Ec%?%p1%{0}%=%t?%e%p1%{7}%=%t0%e%p1%{15}%=%t7%e%p1%'0'%+%c"
    %% It is missing the trailing %;
    %% decomposing:
    %% "\\Ec"
    %%   %? %p1%{0}%=           % if (p1 == 0)
    %%   %t ?                   % then ?
    %%   %e %p1%{7}%=           % else if (p1 == 7)
    %%   %t 0                   % then 0
    %%   %e %p1%{15}%=          % else if (p1 == 15)
    %%   %t 7                   % then 7
    %%   %e %p1%'0'%+%c"        % else p1 + '0'
    Setf = parse_str("\ec"
                     "%?%p1%{0}%="
                     "%t?"
                     "%e%p1%{7}%="
                     "%t0"
                     "%e%p1%{15}%="
                     "%t7"
                     "%e%p1%'0'%+%c" % The end-if marker %; is missing
                    ),
    "\ec?" = Setf(0, #{}),
    "\ec0" = Setf(7, #{}),
    "\ec3" = Setf(3, #{}),
    ok.

parse_str(S) ->
    {ok, Tokens, EndPos} = eterminfo_strcap_scanner:string(S),
    {ok, Res} = eterminfo_strcap_parser:parse(Tokens ++ [{'$end', EndPos}]),
    Res.
