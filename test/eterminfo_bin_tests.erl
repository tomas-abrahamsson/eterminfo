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
-module(eterminfo_bin_tests).

-include_lib("eunit/include/eunit.hrl").

%% This tests the eterminfo_bin
%% (and implicitly the eterminfo_strcap_parser/scanner modules)

term_type_test() ->
    B1 = mk_terminfo(#{'$terminfo_names' => ["test", "alias", "descr text"]},
                     #{}),
    ?assert(is_binary(B1)),
    {ok, #{'$terminfo_names' := ["test", "alias", "descr text"]}} =
        eterminfo_bin:decode_terminfo(B1, #{}).

legacy_padding_test() ->
    %% In the legacy section: if heder + term type + boolean
    %% end on an odd byte, then a padding byte is needed so that
    %% the numeric values start on even byte boudary.
    %%
    %% If we increase term type name length by 1, we'll get (or not need)
    %% one byte of padding. If we increase twice, we're sure we'll get
    %% an extra padding in one of the steps.
    B1 = mk_terminfo(#{'$terminfo_names' => ["test"]}, #{}),
    {ok, #{}} = eterminfo_bin:decode_terminfo(B1, #{}),
    B2 = mk_terminfo(#{'$terminfo_names' => ["test1"]}, #{}),
    {ok, #{}} = eterminfo_bin:decode_terminfo(B2, #{}),
    B3 = mk_terminfo(#{'$terminfo_names' => ["test11"]}, #{}),
    {ok, #{}} = eterminfo_bin:decode_terminfo(B3, #{}),
    if byte_size(B1) == byte_size(B2) ->
            ?assertEqual(byte_size(B2) + 2, byte_size(B3));
       byte_size(B1) < byte_size(B2) ->
            ?assertEqual(byte_size(B1) + 2, byte_size(B2)),
            ?assertEqual(byte_size(B2), byte_size(B3))
    end.

ext_padding_test() ->
    %% Padding between legacy and ext sections: must be present
    %% if legacy-section's string table ends on an odd byte.
    S1 = #{bel => "x", extstr => "extstr"},
    S2 = #{bel => "x2", extstr => "extstr"},
    S3 = #{bel => "x33", extstr => "extstr"},
    B1 = mk_terminfo(S1#{'$str_literals' => S1}, #{}),
    B2 = mk_terminfo(S2#{'$str_literals' => S2}, #{}),
    B3 = mk_terminfo(S3#{'$str_literals' => S3}, #{}),
    {ok, #{bel := "x", extstr := "extstr"}} =
        eterminfo_bin:decode_terminfo(B1, #{}),
    {ok, #{bel := "x2", extstr := "extstr"}} =
        eterminfo_bin:decode_terminfo(B2, #{}),
    {ok, #{bel := "x33", extstr := "extstr"}} =
        eterminfo_bin:decode_terminfo(B3, #{}),

    %% Padding in the ext sections: must be present
    %% if extended booleans end on an odd byte.
    Bools1 = #{extbool_a => true},
    Bools2 = #{extbool_a => true, extbool_b => true},
    Bools3 = #{extbool_a => true, extbool_b => true, extbool_c => true},
    B10 = mk_terminfo(Bools1, #{}),
    B11 = mk_terminfo(Bools2, #{}),
    B12 = mk_terminfo(Bools3, #{}),
    {ok, #{extbool_a := true}} =
        eterminfo_bin:decode_terminfo(B10, #{}),
    {ok, #{extbool_a := true, extbool_b := true}} =
        eterminfo_bin:decode_terminfo(B11, #{}),
    {ok, #{extbool_a := true, extbool_b := true, extbool_c := true}} =
        eterminfo_bin:decode_terminfo(B12, #{}),
    ok.

decode_legacy_bool_cap_test() ->
    %% am is the 2nd cap in cap_names, so it will be preceded by an unset
    B1 = mk_terminfo(#{am => true}, #{}),
    ?assert(is_binary(B1)),
    {ok, #{am := true}} = eterminfo_bin:decode_terminfo(B1, #{}).

decode_legacy_numeric_cap_test() ->
    %% cols is the 2nd cap in cap_names, so it will be preceded by an unset
    B1 = mk_terminfo(#{cols => 80}, #{}),
    ?assert(is_binary(B1)),
    {ok, #{cols := 80}} = eterminfo_bin:decode_terminfo(B1, #{}).

decode_legacy_str_cap_test() ->
    %% bel is the 2nd cap in cap_names, so it will be preceded by an unset
    B1 = mk_terminfo(#{bel => "x",
                       '$str_literals' => #{bel => "x"}},
                     #{}),
    ?assert(is_binary(B1)),
    {ok, #{bel := "x",
           '$str_literals' := #{bel := "x"}}} =
        eterminfo_bin:decode_terminfo(B1, #{}).

extended_numeric_values_test() ->
    %% 32769 is slighly too big to fit as an signed 16 bit value.
    B1 = mk_terminfo(#{cols => 32769,
                       extnum => 32769},
                     #{num_len => 32}),
    {ok, #{cols := 32769,
           extnum := 32769}} =
        eterminfo_bin:decode_terminfo(B1, #{}),
    %% 65537 is slighly too big to fit as an _un_signed 16 bit value.
    B2 = mk_terminfo(#{cols => 65537,
                       extnum => 65537},
                     #{num_len => 32}),
    {ok, #{cols := 65537,
           extnum := 65537}} =
        eterminfo_bin:decode_terminfo(B2, #{}).

decode_ext_caps_test() ->
    B1 = mk_terminfo(#{extbool => true,
                       extnum => 80,
                       extstr => "x",
                       '$str_literals' => #{extstr => "x"}},
                     #{}),
    {ok, #{extbool := true,
           extnum := 80,
           extstr := "x", '$str_literals' := #{extstr := "x"}}} =
        eterminfo_bin:decode_terminfo(B1, #{}).

ext_absent_and_cancelled_strs_test() ->
    S = #{ext_a => absent,
          ext_b => "x",
          ext_c => cancelled},
    B1 = mk_terminfo(S#{'$str_literals' => S}, #{}),
    {ok, #{ext_b := "x",
          '$str_literals' := #{ext_b := "x"}}} =
        eterminfo_bin:decode_terminfo(B1, #{}).

%% -- helpers ---

mk_terminfo(Terminfo, Opts) ->
    eterminfo_bin:encode_terminfo(Terminfo, Opts).
