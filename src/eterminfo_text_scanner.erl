%% This line tells emacs to use -*- erlang -*- mode for this file

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

-module(eterminfo_text_scanner).

-export([string/1]).

-define(is_oct3(O1, O2, O3),
        $0 =< O1, O1 =< $3,
        $0 =< O2, O2 =< $7,
        $0 =< O3, O3 =< $7).

-spec string(string()) -> string().

string(Str) ->
    s(Str).

s("\\E" ++ Rest)   -> "\e" ++ s(Rest);
s("\\e" ++ Rest)   -> "\e" ++ s(Rest);
s([$^, C |  Rest]) -> [extr_ctrlchar([C]) | s(Rest)];
s("\\," ++ Rest)   -> "," ++ s(Rest);
s("\\:" ++ Rest)   -> ":" ++ s(Rest);
s("\\^" ++ Rest)   -> "^" ++ s(Rest);
s("\\\\" ++ Rest)  -> "\\" ++ s(Rest);
s([$\\, O1, O2, O3 | Rest]) when ?is_oct3(O1, O2, O3) ->
    [extr_oct([O1, O2, O3])] ++ s(Rest);
s("\\0" ++ Rest) -> "\200" ++ s(Rest);
s("\\n" ++ Rest) -> [13,10] ++ s(Rest); %% newline
s("\\l" ++ Rest) -> [10] ++ s(Rest);    %% line-feed
s("\\r" ++ Rest) -> [13] ++ s(Rest);    %% return
s("\\t" ++ Rest) -> [9] ++ s(Rest);     %% tab
s("\\b" ++ Rest) -> [8] ++ s(Rest);     %% backspace
s("\\f" ++ Rest) -> [12] ++ s(Rest);    %% form-feed
s("\\s" ++ Rest) -> [32] ++ s(Rest);    %% space
s("%" ++ [C | Rest]) -> "%" ++ [C] ++ s(Rest); % eat ^ in %^
s([C | Rest])    -> [C | s(Rest)];
s("")            -> "".

extr_ctrlchar([$@])  -> 0;
extr_ctrlchar([C]) when C >= $A, C =< $Z -> C - $A + 1;
extr_ctrlchar([C]) when C >= $a, C =< $z -> C - $a + 1;
extr_ctrlchar([$\[]) -> 27;
extr_ctrlchar([$\\]) -> 28;
extr_ctrlchar([$\]]) -> 29;
extr_ctrlchar([$^])  -> 30;
extr_ctrlchar([$_])  -> 31;
extr_ctrlchar([$?])  -> 127.

extr_oct([O1, O2, O3]) ->
    ((O1 - $0) bsl (3+3)) + ((O2 - $0) bsl 3) + (O3 - $0).
