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

%% Parse a compiled terminfo filee such as /lib/terminfo/v/vt100
%%
%% Use eterminfo_strscanner and eterminfo_strparser
%% to parse string capabilities.

-module(eterminfo_bin).

-export([find_terminfo/2]).
-export([decode_terminfo/1, decode_terminfo/2]).
-export([encode_terminfo/1, encode_terminfo/2]).

-export_type([decode_opts/0, encode_opts/0]).

-type decode_opts() :: #{cap_names => terminfo | long,
                         terminfo_bin => binary(),
                         no_env => boolean(),
                         std_paths => [Dir::file:name_all()],
                         _ => _}.

-type encode_opts() :: #{cap_names => terminfo | long,
                         _ => _}.

find_terminfo(_TermType, #{terminfo_bin := Bin}) ->
    {ok, Bin};
find_terminfo(TermType, Opts) ->
    case read_env_vars(Opts) of
        {data, Bin} ->
            {ok, Bin};
        {dirs, EnvDirs, UseStdDirs} ->
            StdDirs = if UseStdDirs     -> std_dirs(Opts);
                         not UseStdDirs -> []
                      end,
            Dirs = EnvDirs ++ StdDirs,
            CandPaths = calc_candidate_paths(TermType, Dirs),
            case try_paths(CandPaths, []) of
                {ok, Bin} ->
                    {ok, Bin};
                {error, Reasons} ->
                    {error, {failed_to_find_term,
                             #{name => TermType,
                               paths_tried => CandPaths,
                               errors => Reasons}}}
            end
    end.

read_env_vars(#{no_env := true}) ->
    {dirs, []};
read_env_vars(_Opts) ->
    %% Environment variables to read/use:
    %% - $TERMINFO if set: a dir or a terminfo value
    %% - $TERMINFO_DIRS if set: a colon-separated list of dirs
    %% - $HOME/.terminfo
    case os:getenv("TERMINFO") of
        false        -> {dirs, read_more_env_var_dirs(), true};
        "hex:"++Rest -> {data, binary:decode_hex(list_to_binary(Rest))};
        "b64:"++Rest -> {data, base64:decode(Rest)};
        P1           -> {dirs, [P1], false}
    end.

read_more_env_var_dirs() ->
    terminfo_dirs() ++ home_dot_terminfo().

terminfo_dirs() ->
    case os:getenv("TERMINFO_DIRS") of
        false -> [];
        Ps -> process_terminfo_paths(string:split(Ps, ":", all))
    end.

process_terminfo_paths([Path | Rest]) when Path /= [] ->
    [Path] ++  process_terminfo_paths(Rest);
process_terminfo_paths([[] | Rest]) ->
    std_dirs(#{}) ++ process_terminfo_paths(Rest);
process_terminfo_paths([]) ->
    [].

home_dot_terminfo() ->
    case os:getenv("HOME") of
        false -> [];
        Home  -> [filename:join(Home, ".terminfo")]
    end.

std_dirs(#{std_paths := Dirs}) ->
    Dirs;
std_dirs(_Opts) ->
    ["/etc/terminfo",
     "/lib/terminfo",
     "/usr/share/terminfo"].

calc_candidate_paths([FirstChar | _]=TermType, Dirs) ->
    [filename:join([Dir, FirstCharAsStr, TermType])
     || Dir <- Dirs,
        FirstCharAsStr <- first_char_strs(FirstChar)].

first_char_strs(Char) ->
    %% term(5) says: "If the underlying filesystem ignores the
    %% difference between uppercase and lowercase, ncurses represents
    %% the 'first character' of the terminal name used as the
    %% intermediate level of a directory tree in (two-character)
    %% hexadecimal form."
    Hex = unicode:characters_to_list(
            binary:encode_hex(
              unicode:characters_to_binary([Char]))),
    [[Char], string:to_lower(Hex), string:to_upper(Hex)].


try_paths([Path | Rest], Reasons) ->
    case file:read_file(Path) of
        {ok, Bin} -> {ok, Bin};
        {error, Reason} -> try_paths(Rest, [Reason | Reasons])
    end;
try_paths([], Reasons) ->
    {error, lists:reverse(Reasons)}.

%% @equiv decode_terminfo(TermInfo, #{})
-spec decode_terminfo(binary()) -> Res when
      Res :: {ok, eterminfo:terminfo()} | {error, term()}.
decode_terminfo(TermInfo) ->
    decode_terminfo(TermInfo, #{}).

%% See term(5) and term.h for info on this format.
-spec decode_terminfo(binary(), decode_opts()) -> Res when
      Res :: {ok, eterminfo:terminfo()} | {error, term()}.
decode_terminfo(Bin, Opts) ->
    case Bin of
        <<8#432:16/little, _/binary>>  -> decode_aux1(Bin, 2, Opts);
        <<8#1036:16/little, _/binary>> -> decode_aux1(Bin, 4, Opts)
    end.

decode_aux1(Bin, Nn, Opts) ->
    case decode_legacy(Bin, Nn, Opts) of
        {ok, {TermInfo, _, <<>>}} ->
            {ok, TermInfo};
        {ok, {TermInfo, NumRead, Rest}} ->
            PadLen = case is_even(NumRead) of
                         true -> 0;
                         false -> 1
                     end,
            <<_:PadLen/binary, ExtStorage/binary>> = Rest,
            decode_ext(ExtStorage, Nn, TermInfo, Opts)
    end.

decode_legacy(Bin, Nn, Opts) ->
    %% The Bin defines capability values in the same order as in term.h.
    %% Short ints are aligned => pad after bools and after strtable if
    %% extensions are present.

    %% Header:
    <<_Magic:16/little, % octal 432 | 1036, but see also PORTABILITY in man 5 term
      SizeTerminalNames:16/signed-little,
      NumBools:16/signed-little,
      NumNumerics:16/signed-little,
      NumOffsets:16/signed-little,
      StringTableSize:16/signed-little,
      Rest1/binary>> = Bin,

    PadLen = case is_even(SizeTerminalNames + NumBools) of
                 true -> 0;
                 false -> 1
             end,

    %% Data sections (as raw binaries):
    <<TerminalNames:SizeTerminalNames/binary,
      Bools:NumBools/binary,
      _:PadLen/binary,
      Numbers:(NumNumerics * Nn)/binary,
      StrOffsets:NumOffsets/binary-unit:16,
      StrTable:StringTableSize/binary,
      Rest/binary>> = Rest1,

    %% Unpack data sections:
    TermNames = split_terminal_names(str_drop_trailing_nul(TerminalNames)),

    %% Capabilities:
    %% (Legacy) Capabilities are values for a predefined set of (arrays of)
    %% booleans, numbers and strings. Some systems have a deviating set of
    %% capabilties. HP-UX, AIX, and OSF/1 according to the term(5) man page.
    %% We don't support those.
    %% Capabilities can be absent or cancelled.
    %% Don't include such capabilities in the resulting map.
    BoolCaps = [bcap(B) || <<B/signed>> <= Bools],
    NumericCaps = if Nn == 2 -> [ncap(N) || <<N:16/signed-little>> <= Numbers];
                     Nn == 4 -> [ncap(N) || <<N:32/signed-little>> <= Numbers]
                  end,
    MaxOffset = StringTableSize - 1,
    Offsets = [scap(N, MaxOffset) || <<N:16/signed-little>> <= StrOffsets],
    Strings = strings_from_table(Offsets, StrTable),

    NumRead = (6 * 2  % header
               + SizeTerminalNames
               + NumBools + PadLen
               + NumNumerics * Nn
               + NumOffsets * 2
               + StringTableSize),

    NameSelector = mk_name_selector(Opts),

    try
        {ok, {merge_maps(
                [#{'$terminfo_names' => TermNames},
                 mk_flag_capabilities(NameSelector, BoolCaps),
                 mk_numeric_capabilities(NameSelector, NumericCaps),
                 mk_string_capabilities(NameSelector, Strings)]),
              NumRead,
              Rest}}
    catch throw:{error, Reason} ->
            {error, Reason}
    end.

decode_ext(ExtStorage, Nn, TermInfo, _Opts) ->
    %% Extension header
    <<NumBoolCaps:16/little,
      NumNumericCaps:16/little,
      NumStrCaps:16/little,
      _NumUsedStrs:16/little,
      StrTableSize:16/little,
      Rest/binary>> = ExtStorage,

    PadLen = case is_even(NumBoolCaps) of
                 true -> 0;
                 false -> 1
             end,

    %% The string table contains strings for non-absent/non-cancelled
    %% strings, and for names of all capabilities.
    %% The non-absent/non-cancelled strings consume an offset entry each.
    NumCapNameOffsets = (NumBoolCaps + NumNumericCaps + NumStrCaps),
    NumOffsets = NumStrCaps + NumCapNameOffsets,

    %% Data sections (as raw binaries):
    <<ExtBools:NumBoolCaps/binary,
      _:PadLen/binary,
      Numbers:(NumNumericCaps * Nn)/binary,
      BinOffsets:NumOffsets/binary-unit:16,
      StrTable:StrTableSize/binary,
      _Rest1/binary>> = Rest,

    %% Unpack data boolean and numeric caps
    ExtBoolCaps = [bcap(B) || <<B/signed>> <= ExtBools],
    ExtNumCaps = if Nn == 2 -> [ncap(N) || <<N:16/signed-little>> <= Numbers];
                    Nn == 4 -> [ncap(N) || <<N:32/signed-little>> <= Numbers]
                 end,

    %% A string table in a terminfo file contains first offsets, then strings.
    %%
    %% An offset is an byte-index into the strings. It is a 16-bit
    %% signed value. An offset value can also indicate that the string
    %% is absent (-1) or cancelled (-2).
    %%
    %% The strings for the extended section contains strings first for
    %% (non-absent, non-cancelled) string capabilities, and then for
    %% capability names. (In the legacy section the string section
    %% does not contain capability names, these are known implicitly.)
    %% Each string includes a terminating NUL character, $\0.
    %%
    %% Offsets for capability names start over on zero.
    %%
    %% Example:
    %% An extended section contains:
    %% - 2 bool capabilities
    %% - 0 numeric capabilties
    %% - 3 strings capabilities, of which one is cancelled (-2).
    %% The string table will contain:
    %% - 3 offsets for string capabilities, lets say [0, -2, 3]
    %% - 5 offsets for capability names, let's say [0, 9, 13, 17, 21]
    %% - 7 strings (first the 2 strings, remembe one was cancelled,
    %%   then the 5 names). Let's say these strings with offsets
    %%   indicated below each string.
    %%     ["s1", "s3", "capname1", "nm2", "nm3", "nm4", "nm5"]
    %%      ^0    ^3     ^0          ^9     ^13    ^17    ^21
    %%   Each string include a trailing NUL not shown here.
    %%
    %% First unpack the offsets, then the string capabilities.
    MaxOffset = StrTableSize - 1,
    Offsets = [scap(N, MaxOffset) || <<N:16/signed-little>> <= BinOffsets],
    {[StrCapOffsets, CapNameOffsets], []} =
        l_take([NumStrCaps, NumCapNameOffsets], Offsets),
    ExtStrCaps = strings_from_table(StrCapOffsets, StrTable),

    %% Now we can calculate the base offset for capability names. It is the
    %% sum of the lengths of capability strings, including the trailing NULs.
    %% Assume that there are no absent or cancelled capability names.
    %% There can be absent or cancelled capability values though.
    Base = lists:sum([length(Cap) + 1 || Cap <- ExtStrCaps,
                                         is_list(Cap)]),
    CapNameOffsets1 = [Offs + Base || Offs <- CapNameOffsets], 
    CapNames = to_atoms(strings_from_table(CapNameOffsets1, StrTable)),

    {[BoolCapNames, NumCapNames, StrCapNames], []} =
        l_take([length(ExtBoolCaps), length(ExtNumCaps), length(ExtStrCaps)],
               CapNames),

    BoolCapMap0 = maps:from_list(lists:zip(BoolCapNames, ExtBoolCaps)),
    NumCapMap0 = maps:from_list(lists:zip(NumCapNames, ExtNumCaps)),
    StrCapMap0 = maps:from_list(lists:zip(StrCapNames, ExtStrCaps)),
    %% Find out what's to be cancelled, drop the absent, and keep the rest.
    #{cancelled := CancelledBools,
      extending := ExtBoolCapMap1} = group_ext_cap_map(BoolCapMap0),
    #{cancelled := CancelledNums,
      extending := ExtNumCapMap1} = group_ext_cap_map(NumCapMap0),
    #{cancelled := CancelledStrs,
      extending := ExtStrCapMap1} = group_ext_cap_map(StrCapMap0),

    %% Drop cancelled entries.
    StrCapsToDrop = mk_key_map(CancelledStrs),
    ToDrop = merge_maps(
               [mk_key_map(CancelledBools),
                mk_key_map(CancelledNums),
                StrCapsToDrop,
                #{'$str_literals' => StrCapsToDrop}]),
    TermInfo1 = subtract_maps(TermInfo, ToDrop),

    %% Parse string capabilities
    try maps:map(fun do_parse_map_str_cap/2, ExtStrCapMap1) of
        ExtStrCapMap2 ->
            {ok, merge_maps(
                   [TermInfo1,
                    ExtBoolCapMap1,
                    ExtNumCapMap1,
                    ExtStrCapMap2,
                    #{'$str_literals' => ExtStrCapMap1}])}
    catch throw:{error, Reason} ->
            {error, Reason}
    end.

%% @equiv encode_terminfo(TermInfo, #{})
-spec encode_terminfo(eterminfo:terminfo()) -> binary().
encode_terminfo(TermInfo) ->
    encode_terminfo(TermInfo, #{}).

-spec encode_terminfo(eterminfo:terminfo(), encode_opts()) -> binary().
encode_terminfo(TermInfo, Opts) ->
    {BooleanCapNames, NumericCapNames, StringCapNames} =
        get_legacy_cap_names(Opts),
    {TermType, Rest1} = case maps:take('$terminfo_names', TermInfo) of
                            {_, _}=V1 -> V1;
                            error     -> {["dummy"], TermInfo}
                        end,
    {Strs, Rest2} = case maps:take('$str_literals', Rest1) of
                        {_, _}=V2 -> V2;
                        error     -> {#{}, Rest1}
                    end,
    Rest3 = maps:without(maps:keys(Strs), Rest2),
    Bools = maps:filter(fun(_, V) -> V == true end, Rest3),
    Nums = maps:without(maps:keys(Bools), Rest3),
    %% The reader must be prepared for both too few and too many
    %% boolean, numeric and string capabilities visavis its table
    %% of known capabilities.
    %% So we can pack just as many/few as we have.
    {Bools1, ExtBools} = split_legacy_ext(Bools, BooleanCapNames),
    {Nums1, ExtNums} = split_legacy_ext(Nums, NumericCapNames),
    {Strs1, ExtStrs} = split_legacy_ext(Strs, StringCapNames),
    NumLen = maps:get(num_len, Opts, 16), % 16 or 32
    if map_size(ExtBools) == 0,
       map_size(ExtNums) == 0,
       map_size(ExtStrs) == 0 ->
            enc_legacy(TermType, Bools1, Nums1, Strs1, NumLen,
                       BooleanCapNames, NumericCapNames, StringCapNames);
       true ->
            Legacy = enc_legacy(
                       TermType, Bools1, Nums1, Strs1, NumLen,
                       BooleanCapNames, NumericCapNames, StringCapNames),
            Pad = mk_pad(byte_size(Legacy)),
            Ext = enc_ext(ExtBools, ExtNums, ExtStrs, NumLen),
            iolist_to_binary([Legacy, Pad, Ext])
    end.

enc_legacy(TermType, Bools, Nums, Strs, NumLen,
           BooleanCapNames, NumericCapNames, StringCapNames) ->
    EncTermType = encode_termtype(TermType),
    EncBools = encode_bools(Bools, BooleanCapNames),
    EncNums = encode_nums(Nums, NumericCapNames, NumLen),
    {Offsets, StrTable} = encode_string_table(Strs, StringCapNames),
    Header = <<(case NumLen of
                    16 -> 8#432;
                    32 -> 8#1036
                end):16/little-signed,
               (iolist_size(EncTermType)):16/little-signed,
               (length(EncBools)):16/little-signed,
               (length(EncNums)):16/little-signed,
               (length(Offsets)):16/little-signed,
               (iolist_size(StrTable)):16/little-signed>>,
    Pad = mk_pad(iolist_size([Header, EncTermType, EncBools])),
    iolist_to_binary([Header, EncTermType, EncBools, Pad,
                      EncNums, Offsets, StrTable]).

enc_ext(ExtBools, ExtNums, ExtStrs, NumLen) ->
    BoolNames = maps:keys(ExtBools),
    NumNames = maps:keys(ExtNums),
    StrNames = maps:keys(ExtStrs),
    AllExt = lists:foldl(fun maps:merge/2, #{}, [ExtBools, ExtNums, ExtStrs]),
    NumCapNames = map_size(AllExt),
    NumPresentStrCaps = map_size(maps:filter(fun is_present/2, ExtStrs)),
    EncBools = encode_bools(ExtBools, BoolNames),
    EncNums = encode_nums(ExtNums, NumNames, NumLen),
    {StrOffsets, StrTable} = encode_string_table(ExtStrs, StrNames),
    {NameOffsets, NameTable} = encode_string_table(
                                 names_to_map(maps:keys(AllExt)),
                                 BoolNames ++ NumNames ++ StrNames),
    Header = <<(map_size(ExtBools)):16/little-signed,
               (map_size(ExtNums)):16/little-signed,
               (map_size(ExtStrs)):16/little-signed,
               (NumPresentStrCaps + NumCapNames):16/little-signed,
               (iolist_size(StrTable)
                + iolist_size(NameTable)):16/little-signed>>,
    Pad = mk_pad(iolist_size([Header, EncBools])),
    iolist_to_binary([Header, EncBools, Pad, EncNums,
                      StrOffsets, NameOffsets, StrTable, NameTable]).

%% -------------------------------------------------------------------
%% Helpers for decode
%% -------------------------------------------------------------------

split_terminal_names(Str) ->
    string:split(Str, "|", all).

str_drop_trailing_nul(Bin) when byte_size(Bin) >= 1 ->
    SizeMinus1 = byte_size(Bin) - 1,
    case Bin of
        <<Str:SizeMinus1/binary, 0>> ->
            binary_to_list(Str);
        _Other ->
            binary_to_list(Bin)
    end.

is_even(N) ->
    (N rem 2) == 0.

bcap(0) -> absent;
bcap(-2) -> cancelled;
bcap(N) when N >= 1 -> true.

ncap(-1) -> absent;
ncap(-2) -> cancelled;
ncap(N) when N >= 0 -> N.

scap(-1, _MaxOffset) -> absent;
scap(-2, _MaxOffset) -> cancelled;
scap(Offset, MaxOffset) when Offset >= 0 ->
    if Offset > MaxOffset -> absent;
       true               -> Offset
    end.

strings_from_table([Offset | Rest], Bin) ->
    if is_atom(Offset) -> % absent | cancelled
            [Offset | strings_from_table(Rest, Bin)];
       Offset >= 0 ->
            <<_:Offset/binary, StrRest/binary>> = Bin,
            [bin_to_str_until_nul(StrRest)
             | strings_from_table(Rest, Bin)]
    end;
strings_from_table([], _Bin) ->
    [].

bin_to_str_until_nul(<<C, Rest/binary>>) when C /= 0 ->
    [C | bin_to_str_until_nul(Rest)];
bin_to_str_until_nul(<<0, _/binary>>) ->
    "".

mk_flag_capabilities(NameSelector, Flags) ->
    map_zip_map(NameSelector,
                eterminfo_cap_names:boolean_names(),
                Flags).

mk_numeric_capabilities(NameSelector, Numbers) ->
    map_zip_map(NameSelector,
                eterminfo_cap_names:numeric_names(),
                Numbers).

mk_string_capabilities(NameSelector, Strings) ->
    Literals = map_zip_map(NameSelector,
                           eterminfo_cap_names:string_names(),
                           Strings),
    merge_maps(
      [map_zip_map(
         fun(Ks, V) ->
                 Key = NameSelector(Ks),
                 case parse_str_capability(V) of
                     {ok, Cap} -> {Key, Cap};
                     {error, Reason} ->
                         throw({error, {parse_str_cap, Key, V, Reason}})
                 end
         end,
         eterminfo_cap_names:string_names(),
         Strings),
       #{'$str_literals' => Literals}]).

parse_str_capability(S) ->
    case eterminfo_strcap_scanner:string(S) of
        {ok, Tokens, EndLine} ->
            EndToken = {'$end', EndLine},
            case eterminfo_strcap_parser:parse(Tokens++[EndToken]) of
                {ok, Value}   -> {ok, Value};
                {error, Reason} -> {error, {parse_error, Reason}}
            end;
        {error, Info, _EndLine} ->
            {error, {scan_error, Info}}
    end.

merge_maps([Map | Rest]) ->
    lists:foldl(fun merge_deep/2, Map, Rest).

merge_deep(Map1, Map2) ->
    maps:merge_with(
      fun(_K, Submap1, Submap2) when is_map(Submap1),
                                     is_map(Submap2) ->
              merge_deep(Submap1, Submap2)
      end,
      Map1, Map2).

%% Keys and Values may be of different lengths, stop when the shortesst stops.
%% Don't include unset values (false, absent or cancelled)
-spec map_zip_map(F, Keys, Values) -> #{Key => V | Value} when
      Keys   :: [K],
      Values :: [V],
      F      :: fun((K)    -> Key) |
                fun((K, V) -> {Key, Value}).
map_zip_map(F, Keys, Values) ->
    mzm_2(F, Keys, Values, #{}).

mzm_2(F, [K | RestK], [V | RestV], Acc) ->
    if V == absent    -> mzm_2(F, RestK, RestV, Acc);
       V == cancelled -> mzm_2(F, RestK, RestV, Acc);
       true ->
            {K1, V1} = if is_function(F, 1) -> {F(K), V};
                          is_function(F, 2) -> F(K, V)
                       end,
            mzm_2(F, RestK, RestV, Acc#{K1 => V1})
    end;
mzm_2(_, [], _, Acc) ->
    Acc;
mzm_2(_, _, [], Acc) ->
    Acc.

mk_name_selector(Opts) ->
    ElNum = case Opts of
                #{cap_names := terminfo} -> 2;
                #{cap_names := long}     -> 1;
                #{cap_names := _}        -> error(badarg);
                #{}                      -> 2
            end,
    fun(Names) -> element(ElNum, Names) end.

group_ext_cap_map(CapMap) ->
    maps:fold(
      fun(Name, X, #{cancelled := Cancelled, extending := Ext}=Acc) ->
              if X == cancelled -> Acc#{cancelled := [Name | Cancelled]};
                 X == absent    -> Acc;
                 true           -> Acc#{extending := Ext#{Name => X}}
              end
      end,
      #{extending => #{}, cancelled => []},
      CapMap).

mk_key_map(Keys) ->
    lists:foldl(fun(K, Acc) -> Acc#{K => []} end, #{}, Keys).

%% From Maps, remove entries in ToDrop, recursively if the entry in ToDrop
%% is another map.
subtract_maps(Maps, ToDrop) ->
    maps:fold(
      fun(K, [], Acc) ->
              maps:remove(K, Acc);
         (K, SubentriesToDrop, Acc) when is_map(SubentriesToDrop) ->
              case Acc of
                  #{K := SubMap} ->
                      Acc#{K := subtract_maps(SubMap, SubentriesToDrop)};
                  #{} ->
                      Acc
              end
      end,
      Maps,
      ToDrop).

do_parse_map_str_cap(Name, Str) ->
    case parse_str_capability(Str) of
        {ok, Cap} ->
            Cap;
        {error, Reason} ->
            throw({error, {parse_str_cap, Name, Str, Reason}})
    end.

to_atoms(Strs) ->
    [list_to_atom(S) || S <- Strs].

%% @doc Take elements from the beginning of a list.
%% The elements to take is expressed as a list of number of elements to take.
%% Return a tuple, a list of lists of taken elements, and the remaining
%% elements.
%%
%% Examples:
%% ```
%% l_take([1],     [a,b,c,d,e]) -> {[[a]],             [b,c,d,e]}
%% l_take([2],     [a,b,c,d,e]) -> {[[a,b]],           [c,d,e]}
%% l_take([1,1],   [a,b,c,d,e]) -> {[[a], [b]],        [c,d,e]}
%% l_take([1,2],   [a,b,c,d,e]) -> {[[a], [b,c]],      [d,e]}
%% l_take([1,2,1], [a,b,c,d,e]) -> {[[a], [b,c], [d]], [e]}
%% '''
-spec l_take([non_neg_integer()], [Elem]) -> {[[Elem]], [Elem]}.
l_take(NumsToTake, List) ->
    reverse_element1(
      lists:foldl(
        fun(NumToTake, {Taken, Rest}) ->
                {T, Rest2} = take_elems(NumToTake, Rest, []),
                {[T | Taken], Rest2}
        end,
        {[], List},
        NumsToTake)).

reverse_element1({L1, X}) -> {lists:reverse(L1), X}.

take_elems(0, Rest, Acc)     -> {lists:reverse(Acc), Rest};
take_elems(N, [H | Tl], Acc) -> take_elems(N - 1, Tl, [H | Acc]).

%% -------------------------------------------------------------------
%% Helpers for encode
%% -------------------------------------------------------------------

get_legacy_cap_names(Opts) ->
    ElNum = case Opts of
                #{cap_names := terminfo} -> 2;
                #{cap_names := long} -> 1;
                #{} -> 2
            end,
    {[element(ElNum, Nm) || Nm <- eterminfo_cap_names:boolean_names()],
     [element(ElNum, Nm) || Nm <- eterminfo_cap_names:numeric_names()],
     [element(ElNum, Nm) || Nm <- eterminfo_cap_names:string_names()]}.

split_legacy_ext(Map, LegacyNames) ->
    maps:fold(fun(K, V, {Legacy, Ext}) ->
                      case lists:member(K, LegacyNames) of
                          true  -> {Legacy#{K => V}, Ext};
                          false -> {Legacy, Ext#{K => V}}
                      end
              end,
              {#{}, #{}},
              Map).

encode_termtype(Strs) ->
    [lists:join("|", Strs), 0].

encode_bools(BoolCapMap, CapNames) ->
    Values = [maps:get(Name, BoolCapMap, absent) || Name <- CapNames],
    [<<(enc_bool(V)):8/signed>>
      || V <- drop_trailing_absent(Values)].

enc_bool(true) -> 1;
enc_bool(absent) -> 0;
enc_bool(cancelled) -> -2.

encode_nums(NumCapMap, CapNames, NumLen) ->
    Values = [maps:get(Name, NumCapMap, absent) || Name <- CapNames],
    [<<(enc_num(V)):NumLen/little-signed>>
      || V <- drop_trailing_absent(Values)].

enc_num(V) when is_integer(V) -> V;
enc_num(absent) -> -1;
enc_num(cancelled) -> -2.

encode_string_table(StrCapMap, CapNames) ->
    Values = [maps:get(Name, StrCapMap, absent) || Name <- CapNames],
    two_tuple_reverse_both(
      lists:foldl(
        fun(S, {Indices, StrTab}) when S /= absent,
                                       S /= cancelled ->
                Index = <<(iolist_size(StrTab)):16/little-signed>>,
                {[Index | Indices], [[S, 0] | StrTab]};
           (absent, {Indices, StrTab}) ->
                {[<<-1:16/little-signed>> | Indices], StrTab};
           (cancelled, {Indices, StrTab}) ->
                {[<<-2:16/little-signed>> | Indices], StrTab}
        end,
        {[], []},
        drop_trailing_absent(Values))).

mk_pad(N) when N rem 2 == 0 -> <<>>;
mk_pad(N) when N rem 2 == 1 -> <<0>>.

two_tuple_reverse_both({L1, L2}) ->
    {lists:reverse(L1), lists:reverse(L2)}.

drop_trailing_absent(L) ->
    lists:reverse(lists:dropwhile(fun is_absent/1, lists:reverse(L))).

is_absent(absent) -> true;
is_absent(_)      -> false.

is_present(_, absent) -> false;
is_present(_, cancelled) -> false;
is_present(_, _) -> true.

names_to_map(Names) ->
    lists:foldl(fun(Name, Acc) -> Acc#{Name => atom_to_list(Name)} end,
                #{},
                Names).
