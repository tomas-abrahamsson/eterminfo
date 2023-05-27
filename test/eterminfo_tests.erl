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
-module(eterminfo_tests).

-include_lib("eunit/include/eunit.hrl").


vt100_long_test() ->
    {ok, M} = parse_terminfo_str("vt100", vt100_long_lines()),
    #{terminfo_id := ["vt100","vt100-am","dec vt100 "++_]} = M,
    %% Numeric capability
    #{"init_tabs" := 8} = M, %% tabs initially every # spaces
    %% String capabilities
    #{"carriage_return" := "\r",
      "bell" := [7]} = M,
    %% Variable string capabilities
    StaticVars = #{}, %% any vars referenced with %g[A-Z] or %P[A-Z]
    #{"parm_up_cursor"   := CUU,
      "parm_down_cursor" := CUD} = M,
    "\e[1A" = CUU(1, StaticVars),
    "\e[1B" = CUD(1, StaticVars),
    ok.

vt100_short_test() ->
    {ok, M} = parse_terminfo_str("vt100", vt100_short_lines()),
    #{terminfo_id := ["vt100","vt100-am","dec vt100 "++_]} = M,
    %% Numeric capability
    #{"it" := 8} = M, %% tabs initially every # spaces
    %% String capabilities
    #{"cr" := "\r",
      "bel" := [7]} = M,
    %% Variable string capabilities
    StaticVars = #{}, %% any vars referenced with %g[A-Z] or %P[A-Z]
    #{"cuu" := CUU, %% cursor up a parameterized number of lines
      "cud" := CUD  %% cursor down a parameterized number of lines
     } = M,
    "\e[1A" = CUU(1, StaticVars),
    "\e[1B" = CUD(1, StaticVars),
    ok.

bool_capability_test() ->
    {ok, M} = parse_terminfo_str(["\tx,"]),
    %% fetch boolean capability "x"
    #{"x" := true} = M.

string_capability_test() ->
    {ok, M} = parse_terminfo_str(["\ts=abc,",
                                  "\tescapes=\\e\\E,", %% escape, in two forms
                                  "\tnewline=\\n,"
                                  "\tlinefeed=\\l,",
                                  "\treturn=\\r,",
                                  "\ttab=\\t,",
                                  "\tbackspace=\\b,",
                                  "\tformfeed=\\f,",
                                  "\tspace=\\s,",
                                  "\tctrlchar=^G,",
                                  "\tcircumflex=\\^,",
                                  "\tbackslash=\\\\,",
                                  "\tcomma=\\,,",
                                  "\tcolon=\\:,",
                                  "\tnul=\\0,"
                                 ]),
    #{"s"          := "abc",
      "escapes"    := "\e\e",
      "ctrlchar"   := [7],
      "newline"    := "\r\n",
      "linefeed"   := "\n",
      "return"     := "\r",
      "backspace"  := "\b",
      "formfeed"   := "\f",
      "space"      := " ",
      "circumflex" := "^",
      "backslash"  := "\\",
      "comma"      := ",",
      "colon"      := ":",
      "nul"        := [128]
     } = M,
    ok.

delay_capability_test() ->
    %% $<5> means delay 5 milliseconds
    {ok, M} = parse_terminfo_str(["\ts=\eK$<5>,"]),
    %% fixme is this ok?
    #{"s" := [27,75,{pad,#{delay        := 5,
                           proportional := false,
                           mandatory    := false}}]} = M.

%% Various parameterized string tests
%%
%% Parameterized strings are evaluated with a stack machine.
%% Values and parameters are pushed and added to the string when popped.
%% Variables (dynamic and static) can be set and retrieved.
%% The language which the stack machine evaluates, is described
%% in the terminfo(5) man page. (at least, that's the case on Linux)

paramstr_percent_test() ->
    {ok, M} = parse_terminfo_str(["\tpercent=%%,"]),
    #{"percent" := "%"} = M.

paramstr_simple_push_pop_test() ->
    {ok, M} = parse_terminfo_str([%% push param(s), pop and printf with %d
                                  "\tpushpop1=%p1%d,",
                                  "\tpushpop2=%p1%p2%d%d,",
                                  %% %s for string formatting
                                  "\tpopstr=%p1%s,",
                                  %% %c for char formatting
                                  "\tpopchar=%p1%c,",
                                  %% pushing a letter
                                  "\tpushchar=%'x'%c,",
                                  %% pushing a number
                                  "\tpushnum=%{65}%d,"
                                 ]),
    #{"pushpop1" := PP1,
      "pushpop2" := PP2,
      "popstr"   := PS,
      "popchar"  := PC,
      "pushchar" := PX,
      "pushnum"  := PN} = M,
    "1"  = PP1(1, []),
    "2"  = PP1(2, []),
    "21" = PP2(1,2, []),
    "12" = PP2(2,1, []),
    "ab" = PS("ab", []),
    ""   = PS("", []),
    "a"  = PC($a, []),
    "x"  = PX([]),
    "65" = PN([]),
    ok.

paramstr_str_formatting_test() ->
    %% %[[:]flags][width[.precision]][doxXs]
    %% where flags are [-+#] and space
    %% the : with flag=- avoids interpreting %- as an op
    {ok, M} = parse_terminfo_str(["\trightw2=>%p1%2d<,",
                                  "\tleftw2=>%p1%:-2d<,",
                                  "\thexleft1w4=>%p1%:-4x<,",
                                  "\thexleft2w4=>%p1%#4x<,",
                                  "\thexleft3w6=>%p1%:-#6x<,",
                                  "\tuhexleft1w4=>%p1%:-4X<,",
                                  "\tuhexleft2w4=>%p1%#4X<,",
                                  "\tuhexleft3w6=>%p1%:-#6X<,",
                                  "\toctleftw4=>%p1%:-4o<,",
                                  "\tstrleftw4=>%p1%:-4s<,",
                                  ""
                                 ]),
    #{"rightw2"     := D1,
      "leftw2"      := D2,
      "hexleft1w4"  := X11,
      "hexleft2w4"  := X12,
      "hexleft3w6"  := X13,
      "uhexleft1w4" := X21,
      "uhexleft2w4" := X22,
      "uhexleft3w6" := X23,
      "octleftw4"   := O1,
      "strleftw4"   := S1} = M,
    "> 2<"     = D1(2, []),
    ">2 <"     = D2(2, []),
    ">c0  <"   = X11(192, []),
    ">0xc0<"   = X12(192, []),
    ">0xc0  <" = X13(192, []),
    ">C0  <"   = X21(192, []),
    ">0XC0<"   = X22(192, []),
    ">0XC0  <" = X23(192, []),
    ">377 <"   = O1(255, []),
    ">abc <"   = S1("abc", []),
    ok.

paramstr_dyn_var_test() ->
    {ok, M} = parse_terminfo_str([%% push param 1, (%p1)
                                  %% pop into a dynamic var a, (%Pa)
                                  %% get that dynamic var and push it (%ga)
                                  %% pop and printf it (%d)
                                  %% get+push and pop+printf it again
                                  "\tdynvar=%p1%Pa%ga%d%ga%d,"]),
    #{"dynvar" := PP2} = M,
    "22" = PP2(2, []).

paramstr_static_var_test() ->
    {ok, M} = parse_terminfo_str(
                [%% same as dyn var, but with a static var
                 %% (uppercase A means static var instead
                 %% of dynamic)
                 "\tdynstaticvar=%p1%Pa%p2%PA%gA%d-%ga%d-%gC%d,"]),
    #{"dynstaticvar" := PP3} = M,
    "3-4-5" = PP3(4, 3, #{"C" => 5}).

paramstr_strlen_op_test() ->
    {ok, M} = parse_terminfo_str(["\tstrlen=%p1%l%d,"]),
    #{"strlen" := L} = M,
    "6" = L("abcdef", []),
    "7" = L("abcdefg", []).

paramstr_bin_arith_op_test() ->
    {ok, M} = parse_terminfo_str(["\tadd=%p1%p2%+%d,",
                                  "\tsub=%p1%p2%-%d,",
                                  "\tmul=%p1%p2%*%d,",
                                  "\tdiv=%p1%p2%/%d,",
                                  "\tmod=%p1%p2%m%d,"]),
    %% arithmetic operations
    #{"add" := AAdd,
      "sub" := ASub,
      "mul" := AMul,
      "div" := ADiv,
      "mod" := AMod} = M,
    "3"  = AAdd(1, 2, []),
    "4"  = ASub(6, 2, []),
    "12" = AMul(6, 2, []),
    "4" = ADiv(8, 2, []),
    "1" = AMod(13, 3, []).

paramstr_arith_op_test() ->
    {ok, M} = parse_terminfo_str(["\tadd=%p1%p2%+%d,",
                                  "\tsub=%p1%p2%-%d,",
                                  "\tmul=%p1%p2%*%d,",
                                  "\tdiv=%p1%p2%/%d,",
                                  "\tmod=%p1%p2%m%d,",
                                  "\tinc=%i%p1%d%p2%d,"]),
    #{"add" := AAdd,
      "sub" := ASub,
      "mul" := AMul,
      "div" := ADiv,
      "mod" := AMod,
      "inc" := AInc} = M,
    "3"  = AAdd(1, 2, []),
    "4"  = ASub(6, 2, []),
    "12" = AMul(6, 2, []),
    "4"  = ADiv(8, 2, []),
    "1"  = AMod(13, 3, []),
    "95" = AInc(8, 4, []).

paramstr_bit_op_test() ->
    {ok, M} = parse_terminfo_str(["\tand=%p1%p2%&%d,",
                                  "\tor=%p1%p2%|%d,",
                                  "\txor=%p1%p2%^%d,",
                                  "\tnot=%p1%~%d,"]),
    #{"and" := BAnd,
      "or"  := BOr,
      "xor" := BXor,
      "not" := BNot} = M,
    "1"  = BAnd(1, 3, []),
    "7"  = BOr(6, 3, []),
    "5"  = BXor(6, 3, []),
    "-7" = BNot(6, []). %% FIXME: is this correct?

paramstr_logical_op_test() ->
    {ok, M} = parse_terminfo_str(["\tand=%p1%p2%A%d,",
                                  "\tor=%p1%p2%O%d,",
                                  "\tnot=%p1%!%d,"]),
    #{"and" := LAnd,
      "or"  := LOr,
      "not" := LNot} = M,

    "0"  = LAnd(1, 0, []),
    "1"  = LAnd(1, 3, []),

    "0"  = LOr(0, 0, []),
    "1"  = LOr(1, 0, []),
    "1"  = LOr(0, 7, []),

    "1"  = LNot(0, []),
    "0"  = LNot(1, []).

paramstr_cmp_op_test() ->
    {ok, M} = parse_terminfo_str(["\tlt=%p1%p2%<%d,",
                                  "\tgt=%p1%p2%>%d,",
                                  "\teq=%p1%p2%=%d,"]),
    #{"lt" := LT,
      "gt" := GT,
      "eq" := EQ} = M,

    "1"  = LT(2, 3, []),
    "0"  = LT(2, 2, []),
    "0"  = LT(2, 1, []),

    "0"  = GT(2, 3, []),
    "0"  = GT(2, 2, []),
    "1"  = GT(2, 1, []),

    "1"  = EQ(0, 0, []),
    "1"  = EQ(1, 1, []),
    "0"  = EQ(2, 3, []).

paramstr_if_then_else_test() ->
    {ok, M} = parse_terminfo_str(
                [%% if-then:
                 %% push p1 (%p1)
                 %% push 3  (%{3})
                 %% test equality (%=)
                 %% if-then %? <test-for-3> %t <then-expr> %;
                 %% the <then-expr> is the string t
                 "\tift=%?%p1%{3}%=%tt%;,"
                 %% if-then-else: %e <else-part>
                 "\tifte=%?%p1%{3}%=%tt%ee%;,"
                 %% if-then-else-if-then-else
                 "\telsif=%?%p1%{3}%>%!%tt1%e%p1%{8}%<%tt2%ee%;,"]),

    #{"ift" := IFT,     % if-then
      "ifte" := IFTE,   % if-then-else
      "elsif" := ElsIf  % if-then-elsif-else
     } = M,

    "t" = IFT(3, []),
    ""  = IFT(4, []),

    "t"  = IFTE(3, []),
    "e"  = IFTE(4, []),

    "t1"  = ElsIf(2, []),
    "t2"  = ElsIf(7, []),
    "e"   = ElsIf(9, []),
    ok.

parse_terminfo_str(CapabilityLines) ->
    parse_terminfo_str("dummy", ["#\tComment",
                                 "tname|some altname with spaces,"
                                 | CapabilityLines]).

parse_terminfo_str(TermName, Lines) ->
    S = ensure_ends_with_newline(string:join(Lines, "\n")),
    Opts = [{terminfo_string, S}],
    eterminfo:setup_by_infocmp(TermName, Opts).

ensure_ends_with_newline(S) ->
    case lists:last(S) of
        $\n -> S;
        _   -> S ++ "\n"
    end.

vt100_long_lines() ->
    ["#\tReconstructed via infocmp from file: /lib/terminfo/v/vt100",
     "vt100|vt100-am|dec vt100 (w/advanced video),",
     "\tauto_right_margin, backspaces_with_bs,",
     "\teat_newline_glitch, move_standout_mode, prtr_silent,",
     "\txon_xoff,",
     "\tcolumns#80, init_tabs#8, lines#24, virtual_terminal#3,",
     "\tacs_chars=``aaffggjjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~,",
     "\tbell=^G, carriage_return=^M,",
     "\tchange_scroll_region=\\E[%i%p1%d;%p2%dr,",
     "\tclear_all_tabs=\\E[3g, clear_screen=\\E[H\\E[J$<50>,",
     "\tclr_bol=\\E[1K$<3>, clr_eol=\\E[K$<3>, clr_eos=\\E[J$<50>,",
     "\tcursor_address=\\E[%i%p1%d;%p2%dH$<5>, cursor_down=^J,",
     "\tcursor_home=\\E[H, cursor_left=^H,",
     "\tcursor_right=\\E[C$<2>, cursor_up=\\E[A$<2>,",
     "\tena_acs=\\E(B\\E)0, enter_alt_charset_mode=^N,",
     "\tenter_am_mode=\\E[?7h, enter_blink_mode=\\E[5m$<2>,",
     "\tenter_bold_mode=\\E[1m$<2>,",
     "\tenter_reverse_mode=\\E[7m$<2>,",
     "\tenter_standout_mode=\\E[7m$<2>,",
     "\tenter_underline_mode=\\E[4m$<2>,",
     "\texit_alt_charset_mode=^O, exit_am_mode=\\E[?7l,",
     "\texit_attribute_mode=\\E[m\\017$<2>,",
     "\texit_standout_mode=\\E[m$<2>,",
     "\texit_underline_mode=\\E[m$<2>, key_a1=\\EOq, key_a3=\\EOs,",
     "\tkey_b2=\\EOr, key_backspace=^H, key_c1=\\EOp, key_c3=\\EOn,",
     "\tkey_down=\\EOB, key_enter=\\EOM, key_f0=\\EOy, key_f1=\\EOP,",
     "\tkey_f10=\\EOx, key_f2=\\EOQ, key_f3=\\EOR, key_f4=\\EOS,",
     "\tkey_f5=\\EOt, key_f6=\\EOu, key_f7=\\EOv, key_f8=\\EOl,",
     "\tkey_f9=\\EOw, key_left=\\EOD, key_right=\\EOC, key_up=\\EOA,",
     "\tkeypad_local=\\E[?1l\\E>, keypad_xmit=\\E[?1h\\E=,",
     "\tlab_f1=pf1, lab_f2=pf2, lab_f3=pf3, lab_f4=pf4,",
     "\tparm_down_cursor=\\E[%p1%dB,",
     "\tparm_left_cursor=\\E[%p1%dD,",
     "\tparm_right_cursor=\\E[%p1%dC,",
     "\tparm_up_cursor=\\E[%p1%dA, print_screen=\\E[0i,",
     "\tprtr_off=\\E[4i, prtr_on=\\E[5i,",
     "\treset_2string=\\E>\\E[?3l\\E[?4l\\E[?5l\\E[?7h\\E[?8h,",
     "\trestore_cursor=\\E8, save_cursor=\\E7, scroll_forward=^J,",
     "\tscroll_reverse=\\EM$<5>,",
     "\tset_attributes=\\E[0%?%p1%p6%|%t;1%;%?%p2%t;4%;%?%p1%p3%|%t;7%;%?%p4%t;5%;m%?%p9%t\\016%e\\017%;$<2>,",
     "\tset_tab=\\EH, tab=^I,"].

vt100_short_lines() ->
    ["#\tReconstructed via infocmp from file: /lib/terminfo/v/vt100",
     "vt100|vt100-am|dec vt100 (w/advanced video),",
     "\tam, mc5i, msgr, xenl, xon,",
     "\tcols#80, it#8, lines#24, vt#3,",
     "\tacsc=``aaffggjjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~,",
     "\tbel=^G, blink=\\E[5m$<2>, bold=\\E[1m$<2>,",
     "\tclear=\\E[H\\E[J$<50>, cr=^M, csr=\\E[%i%p1%d;%p2%dr,",
     "\tcub=\\E[%p1%dD, cub1=^H, cud=\\E[%p1%dB, cud1=^J,",
     "\tcuf=\\E[%p1%dC, cuf1=\\E[C$<2>,",
     "\tcup=\\E[%i%p1%d;%p2%dH$<5>, cuu=\\E[%p1%dA,",
     "\tcuu1=\\E[A$<2>, ed=\\E[J$<50>, el=\\E[K$<3>, el1=\\E[1K$<3>,",
     "\tenacs=\\E(B\\E)0, home=\\E[H, ht=^I, hts=\\EH, ind=^J, ka1=\\EOq,",
     "\tka3=\\EOs, kb2=\\EOr, kbs=^H, kc1=\\EOp, kc3=\\EOn, kcub1=\\EOD,",
     "\tkcud1=\\EOB, kcuf1=\\EOC, kcuu1=\\EOA, kent=\\EOM, kf0=\\EOy,",
     "\tkf1=\\EOP, kf10=\\EOx, kf2=\\EOQ, kf3=\\EOR, kf4=\\EOS, kf5=\\EOt,",
     "\tkf6=\\EOu, kf7=\\EOv, kf8=\\EOl, kf9=\\EOw, lf1=pf1, lf2=pf2,",
     "\tlf3=pf3, lf4=pf4, mc0=\\E[0i, mc4=\\E[4i, mc5=\\E[5i, rc=\\E8,",
     "\trev=\\E[7m$<2>, ri=\\EM$<5>, rmacs=^O, rmam=\\E[?7l,",
     "\trmkx=\\E[?1l\\E>, rmso=\\E[m$<2>, rmul=\\E[m$<2>,",
     "\trs2=\\E>\\E[?3l\\E[?4l\\E[?5l\\E[?7h\\E[?8h, sc=\\E7,",
     "\tsgr=\\E[0%?%p1%p6%|%t;1%;%?%p2%t;4%;%?%p1%p3%|%t;7%;%?%p4%t;5%;m%?%p9%t\\016%e\\017%;$<2>,",
     "\tsgr0=\\E[m\\017$<2>, smacs=^N, smam=\\E[?7h, smkx=\\E[?1h\\E=,",
     "\tsmso=\\E[7m$<2>, smul=\\E[4m$<2>, tbc=\\E[3g,"].
