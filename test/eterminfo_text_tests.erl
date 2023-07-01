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
-module(eterminfo_text_tests).

-include_lib("eunit/include/eunit.hrl").

-define(acs_chars, "``aaffggjjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~").
-define(forty_a, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").
-define(long_str_cap, ?forty_a ?forty_a).

bool_numeric_and_str_caps_test() ->
    "vt100|vt100-am|dec vt100 (w/advanced video)\n"
        %% Each line of caps shoul dCaps should all begin with a tab char:
        "\tauto_right_margin, backspaces_with_bs,\n" % newline after bool caps
        "\tcolumns#80, init_tabs#8,\n"               % and after numeric caps
        "\tacs_chars=" ?acs_chars ", bell=^H,\n"     % Should be a comma last.
        =
        lists:flatten(
          eterminfo_text:format(
            merge_maps(
              [vt100_terminfo_names(),
               some_vt100_bool_caps(2),
               some_vt100_numeric_caps(2),
               some_vt100_string_caps(2)]),
            #{})).

only_bool_caps_test() ->
    "vt100|vt100-am|dec vt100 (w/advanced video)\n"
        "\tauto_right_margin, backspaces_with_bs,\n" =
        lists:flatten(
          eterminfo_text:format(
            merge_maps(
              [vt100_terminfo_names(),
               some_vt100_bool_caps(2)]),
            #{})).

only_bool_and_str_single_cap_test() ->
    "vt100|vt100-am|dec vt100 (w/advanced video)\n"
        "\tauto_right_margin,\n"
        "\tacs_chars=" ?acs_chars ",\n" =
        lists:flatten(
          eterminfo_text:format(
            merge_maps(
              [vt100_terminfo_names(),
               some_vt100_bool_caps(1),
               some_vt100_string_caps(1)]),
            #{})).

only_numeric_and_str_single_cap_test() ->
    "vt100|vt100-am|dec vt100 (w/advanced video)\n"
        "\tcolumns#80,\n"
        "\tacs_chars=" ?acs_chars ",\n" =
        lists:flatten(
          eterminfo_text:format(
            merge_maps(
              [vt100_terminfo_names(),
               some_vt100_numeric_caps(1),
               some_vt100_string_caps(1)]),
            #{})).

long_string_cap_first_on_line_test() ->
    C = #{a_long_string => ?long_str_cap},
    StrCaps = C#{'$str_literals' => C},
    "vt100|vt100-am|dec vt100 (w/advanced video)\n"
        "\ta_long_string=" ? long_str_cap ",\n" =
        lists:flatten(
          eterminfo_text:format(
            merge_maps(
              [vt100_terminfo_names(),
               StrCaps]),
            #{})).

comment_test() ->
    "# abc\n"
        "vt100|vt100-am|dec vt100 (w/advanced video)\n"
        "\tauto_right_margin,\n"
        "\tacs_chars=" ?acs_chars ",\n" =
        lists:flatten(
          eterminfo_text:format(
            merge_maps(
              [vt100_terminfo_names(),
               some_vt100_bool_caps(1),
               some_vt100_string_caps(1)]),
            #{comment => "abc"})).

merge_maps(Maps) ->
    lists:foldl(fun maps:merge/2, #{}, Maps).

vt100_terminfo_names() ->
    #{'$terminfo_names' =>
          ["vt100","vt100-am","dec vt100 (w/advanced video)"]}.

some_vt100_bool_caps(HowMany) ->
    maps:from_list(
      lists:sublist(lists:sort(maps:to_list(vt100_bool_caps())),
                    HowMany)).

some_vt100_numeric_caps(HowMany) ->
    maps:from_list(
      lists:sublist(lists:sort(maps:to_list(vt100_numeric_caps())),
                    HowMany)).

some_vt100_string_caps(HowMany) ->
    #{'$str_literals' := Caps0} = vt100_string_caps(),
    Caps1 = maps:from_list(lists:sublist(lists:sort(maps:to_list(Caps0)),
                                         HowMany)),
    Caps1#{'$str_literals' => Caps1}.

vt100_bool_caps() ->
    #{auto_right_margin => true,backspaces_with_bs => true,
      eat_newline_glitch => true,move_standout_mode => true,
      prtr_silent => true,xon_xoff => true}.

vt100_numeric_caps() ->
    #{columns => 80, lines => 24,
      init_tabs => 8, virtual_terminal => 3}.

vt100_string_caps() ->
    C = #{cursor_down => "\n",
          exit_attribute_mode => [27,91,109,15,36,60,50,62],
          key_right => "\eOC",enter_bold_mode => "\e[1m$<2>",
          cursor_left => "\b",
          set_attributes =>
              [27,91,48,37,63,37,112,49,37,112,54,37,124,37,116,59,49,37,
               59,37,63,37,112,50,37,116,59,52,37,59,37,63,37,112,49,37,
               112,51,37,124,37,116,59,55,37,59,37,63,37,112,52,37,116,59,
               53,37,59,109,37,63,37,112,57,37,116,14,37,101,15,37,59,36,
               60,50,62],
          reset_2string => "\e>\e[?3l\e[?4l\e[?5l\e[?7h\e[?8h",
          enter_underline_mode => "\e[4m$<2>",
          clear_all_tabs => "\e[3g",key_f6 => "\eOu",
          key_backspace => "\b",key_f3 => "\eOR",columns => "80",
          exit_standout_mode => "\e[m$<2>",
          key_f10 => "\eOx",
          exit_alt_charset_mode => [15],
          lines => "24",key_f4 => "\eOS",
          acs_chars =>
              "``aaffggjjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~",
          key_f5 => "\eOt",enter_standout_mode => "\e[7m$<2>",
          tab => "\t",lab_f2 => "pf2",save_cursor => "\e7",
          lab_f4 => "pf4",clr_eos => "\e[J$<50>",key_down => "\eOB",
          scroll_reverse => "\eM$<5>",key_f9 => "\eOw",
          parm_right_cursor => "\e[%p1%dC",key_f7 => "\eOv",
          parm_left_cursor => "\e[%p1%dD",ena_acs => "\e(B\e)0",
          set_tab => "\eH",clear_screen => "\e[H\e[J$<50>",
          cursor_home => "\e[H",
          bell => [7],
          key_c3 => "\eOn",cursor_address => "\e[%i%p1%d;%p2%dH$<5>",
          enter_blink_mode => "\e[5m$<2>",
          parm_down_cursor => "\e[%p1%dB",
          exit_underline_mode => "\e[m$<2>",clr_eol => "\e[K$<3>",
          key_f8 => "\eOl",key_up => "\eOA",key_a3 => "\eOs",
          carriage_return => "\r",
          enter_am_mode => "\e[?7h",cursor_right => "\e[C$<2>",
          enter_alt_charset_mode => [14],
          lab_f3 => "pf3",key_left => "\eOD",key_f0 => "\eOy",
          key_b2 => "\eOr",init_tabs => "8",lab_f1 => "pf1",
          key_f1 => "\eOP",keypad_local => "\e[?1l\e>",
          keypad_xmit => "\e[?1h\e=",key_f2 => "\eOQ",
          parm_up_cursor => "\e[%p1%dA",cursor_up => "\e[A$<2>",
          key_c1 => "\eOp",print_screen => "\e[0i",key_a1 => "\eOq",
          scroll_forward => "\n",key_enter => "\eOM",
          change_scroll_region => "\e[%i%p1%d;%p2%dr",
          prtr_off => "\e[4i",restore_cursor => "\e8",
          virtual_terminal => "3",exit_am_mode => "\e[?7l",
          enter_reverse_mode => "\e[7m$<2>",
          prtr_on => "\e[5i",
          clr_bol => "\e[1K$<3>"},
    C#{'$str_literals' => C}.
