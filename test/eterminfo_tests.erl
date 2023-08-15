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
    #{'$terminfo_names' := ["vt100","vt100-am","dec vt100 "++_]} = M,
    %% Numeric capability
    #{init_tabs := 8} = M, %% tabs initially every # spaces
    %% String capabilities
    #{carriage_return := "\r",
      bell := [7]} = M,
    %% Variable string capabilities
    StaticVars = #{}, %% any vars referenced with %g[A-Z] or %P[A-Z]
    #{parm_up_cursor   := CUU,
      parm_down_cursor := CUD} = M,
    "\e[1A" = CUU(1, StaticVars),
    "\e[1B" = CUD(1, StaticVars),
    ok.

vt100_short_test() ->
    {ok, M} = parse_terminfo_str("vt100", vt100_short_lines()),
    #{'$terminfo_names' := ["vt100","vt100-am","dec vt100 "++_]} = M,
    %% Numeric capability
    #{it := 8} = M, %% tabs initially every # spaces
    %% String capabilities
    #{cr := "\r",
      bel := [7]} = M,
    %% Variable string capabilities
    StaticVars = #{}, %% any vars referenced with %g[A-Z] or %P[A-Z]
    #{cuu := CUU, %% cursor up a parameterized number of lines
      cud := CUD  %% cursor down a parameterized number of lines
     } = M,
    "\e[1A" = CUU(1, StaticVars),
    "\e[1B" = CUD(1, StaticVars),
    ok.

file_non_extended_test() ->
    {ok, M} = parse_terminfo_file_from_hexdump("adm3a", adm3a_hexdump()),
    #{am := true,
      cols := 80, lines := 24,
      bel := [7], clear := [8#32, {pad, #{delay := 1,
                                          mandatory := false,
                                          proportional := false}}],
      cr := [13], cub1 := [8], cud1 := [10], cuf1 := [12],
      cup := CupF, cuu1 := [11], home := [30], ind := [10]} = M,
    "\e" ++ _ = CupF(1, 1, #{}),
    ok.


tigetstr_test() ->
    with(terminfo_installed_from_file_hexdump(adm3a_hexdump()),
         fun(Spec) ->
                 [7] = eterminfo:tigetstr(bel, Spec)
         end).

tigetnum_test() ->
    with(terminfo_installed_from_file_hexdump(adm3a_hexdump()),
         fun(Spec) ->
                 80 = eterminfo:tigetnum(cols, Spec),
                 24 = eterminfo:tigetnum(lines, Spec)
         end).

tparm_test() ->
    with(terminfo_installed_from_file_hexdump(adm3a_hexdump()),
         fun(Spec) ->
                 "\e=!!" = eterminfo:tparm(cup, 1, 1, Spec)
         end).

tputs_test() ->
    with(terminfo_installed_from_file_hexdump(adm3a_hexdump()),
         fun(Spec) ->
                 eterminfo:tputs(eterminfo:tparm(cup, 1, 1, Spec)),
                 "\e=!!" = unicode:characters_to_list(?capturedOutput)
         end).

tputs_with_delay_test() ->
    with(terminfo_installed_from_infocmp_str(["dummy,", "\tclear=x$<50/>y,"]),
         fun(Spec) ->
                 [$x, {pad, #{delay := Delay, mandatory := true}}, $y] =
                     Clear =
                     eterminfo:tigetstr(clear, Spec),
                 T0 = erlang:monotonic_time(millisecond),
                 eterminfo:tputs(Clear),
                 T1 = erlang:monotonic_time(millisecond),
                 "xy" = unicode:characters_to_list(?capturedOutput),
                 Margin = Delay * 0.1,
                 ?assert((T1 - T0) >= floor(Delay - Margin),
                         #{actual_delay => T1 - T0,
                           expect_delay => Delay})
         end).

parse_terminfo_str(TermName, Lines) ->
    S = ensure_ends_with_newline(string:join(Lines, "\n")),
    Opts = #{terminfo_string => S},
    eterminfo:read_by_infocmp(TermName, Opts).

ensure_ends_with_newline(S) ->
    case lists:last(S) of
        $\n -> S;
        _   -> S ++ "\n"
    end.

parse_terminfo_file_from_hexdump(TermName, HexDump) ->
    Bin = bin_from_hexdump(HexDump),
    eterminfo:read_by_file(TermName, #{terminfo_bin => Bin}).

bin_from_hexdump(HexDump) ->
    <<<<(list_to_integer([C], 16)):4>>
      || <<C>> <= iolist_to_binary(HexDump),
         is_hex_digit(C)>>.

is_hex_digit(D) when $0 =< D, D =< $9 -> true;
is_hex_digit(D) when $A =< D, D =< $F -> true;
is_hex_digit(D) when $a =< D, D =< $f -> true;
is_hex_digit(_) -> false.

with(ReadF, TestF) ->
    TermName = "test-dummy",
    Spec = #{term => TermName},
    {ok, TermInfo} = ReadF(Spec),
    ok = eterminfo:install_terminfo(Spec, TermInfo),
    try
        TestF(Spec)
    after
        ok = eterminfo:uninstall(Spec)
    end.

terminfo_installed_from_infocmp_str(Lines) ->
    fun(#{term := TermName}) ->
            parse_terminfo_str(TermName, Lines)
    end.

terminfo_installed_from_file_hexdump(HexDump) ->
    fun(#{term := TermName}) ->
            parse_terminfo_file_from_hexdump(TermName, HexDump)
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

adm3a_hexdump() ->
    %% From the example in the term(5) man page
    <<"1a 01 10 00 02 00 03 00  82 00 31 00 61 64 6d 33
       61 7c 6c 73 69 20 61 64  6d 33 61 00 00 01 50 00
       ff ff 18 00 ff ff 00 00  02 00 ff ff ff ff 04 00
       ff ff ff ff ff ff ff ff  0a 00 25 00 27 00 ff ff
       29 00 ff ff ff ff 2b 00  ff ff 2d 00 ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff ff ff  ff ff ff ff ff ff ff ff
       ff ff ff ff ff ff 2f 00  07 00 0d 00 1a 24 3c 31
       3e 00 1b 3d 25 70 31 25  7b 33 32 7d 25 2b 25 63
       25 70 32 25 7b 33 32 7d  25 2b 25 63 00 0a 00 1e
       00 08 00 0c 00 0b 00 0a  00">>.
