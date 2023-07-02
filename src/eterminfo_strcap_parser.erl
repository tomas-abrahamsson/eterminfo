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

%% Parse a compiled terminfo filee such as /lib/terminfo/v/vt100
%%
%% Use eterminfo_strscanner and eterminfo_strparser
%% to parse string capabilities.

-module(eterminfo_strcap_parser).

-import(lists, [foldl/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-export([parse/1]).

-export_type([input/0, input_elem/0, parsed/0, pstr/0, pfun/0]).

%% For testing...
-export([deep_char_list_or_padding/1]).
-export([printf_format/2]).

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
-type input() :: [input_elem()].
-type input_elem() :: token() | {'$end', pos()}.
-type token() :: eterminfo_strcap_scanner:token().
-type pos() :: eterminfo_strcap_scanner:pos().

-type parsed() :: pstr() | pfun().
-type pstr() :: [parsed_elem()].
-type parsed_elem() :: char() | {pad, eterminfo_strcap_scanner:pad_info()}.
-type pfun() :: fun((static_vars()) -> pstr()) % 0..9 parameters
              | fun((param(), static_vars()) -> pstr())
              | fun((param(), param(), static_vars()) -> pstr())
              | fun((param(), param(), param(), static_vars()) -> pstr())
              | fun((param(), param(), param(), param(), static_vars()) ->
                        pstr())
              | fun((param(), param(), param(), param(), param(),
                     static_vars()) ->
                        pstr())
              | fun((param(), param(), param(), param(), param(), param(),
                     static_vars()) ->
                        pstr())
              | fun((param(), param(), param(), param(), param(), param(),
                     param(), static_vars()) ->
                        pstr())
              | fun((param(), param(), param(), param(), param(), param(),
                     param(), param(), static_vars()) ->
                        pstr())
              | fun((param(), param(), param(), param(), param(), param(),
                     param(), param(), param(), static_vars()) ->
                        pstr()).
-type static_vars() :: #{var() => value()}.
-type var() :: string(). % single-letter variable name, upper-case
-type value() :: integer(). % | string() ?
-type param() :: integer(). % | string() ?

%% ir = intermediary representation
-type parsed_ir() :: [parsed_ir_elem()].
-type parsed_ir_elem() :: char()
                        | {push, eterminfo_strcap_scanner:push()}
                        | {pop, eterminfo_strcap_scanner:pop()}
                        | {pad, eterminfo_strcap_scanner:pad_info()}
                        | eterminfo_strcap_scanner:unary_op()
                        | eterminfo_strcap_scanner:binary_op()
                        | if_expr().
-type if_expr() :: {'if', Cond::parsed_ir(),
                    {then, parsed_ir()},
                    {else, parsed_ir() | if_expr()}}.

-define(is_end(Rest),
        ((Rest == []) orelse (element(1, hd(Rest)) == '$end'))).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Code
%%--------------------------------------------------------------------

-spec parse(input()) -> {ok, parsed()} | {error, term()}.
parse(Tokens) ->
    case p2(Tokens, []) of
        {Parsed, Rest} when ?is_end(Rest) ->
            {ok, finalize(Parsed)};
        X ->
            {error, X}
    end.

-spec p2(input(), parsed()) -> {parsed_ir(), input()}.
p2([Token | Rest], Acc) ->
    case Token of
        {char, _Pos, _C}  -> p2(Rest, [mk_char(Token) | Acc]);
        {pad, _Pos, _Pad} -> p2(Rest, [mk_pad(Token) | Acc]);
        {push, _Pos, _X}  -> p2(Rest, [mk_push(Token) | Acc]);
        {pop, _Pos, _X}   -> p2(Rest, [mk_pop(Token) | Acc]);
        {op1, _Pos, _X}   -> p2(Rest, [mk_unary_op(Token) | Acc]);
        {op2, _Pos, _X}   -> p2(Rest, [mk_binary_op(Token) | Acc]);
        {'if', _Pos} when ?is_end(Rest)->
            %% In wy350, the is3 str capability ends with just "%?".
            %% The ncurses tparm() renders this as if there was no "%?" at all.
            {lists:reverse(Acc), Rest};
        {'if', _Pos} ->
            {Expr, Rest2} = p2_if_then_else(Rest),
            p2(Rest2, [Expr | Acc]);
        _Other -> {lists:reverse(Acc), [Token | Rest]}
    end;
p2([], Acc) ->
    {lists:reverse(Acc), []}.

p2_if_then_else(Tokens) ->
    case p2(Tokens, []) of
        {Condition, [{then, _Pos1} | Rest1]} ->
            p2_then_else(Condition, Rest1)
    end.

p2_then_else(Condition, Rest) ->
    case p2(Rest, []) of
        {Then, [{endif, _Pos} | Rest1]} ->
            Expr = mk_if(Condition, Then),
            {Expr, Rest1};
        {Then, [{else, _Pos} | Rest1]} ->
            case p2(Rest1, []) of
                {Else, [{endif, _Pos2} | Rest2]} ->
                    Expr = mk_if(Condition, Then, Else),
                    {Expr, Rest2};
                {Cond2, [{then, _Pos2} | Rest2]} ->
                    {Elseif, Rest3} = p2_then_else(Cond2, Rest2),
                    Expr = mk_if(Condition, Then, Elseif),
                    {Expr, Rest3};
                {Else, Rest2} when ?is_end(Rest2) ->
                    %% tw52 is missing the endif token ("%;") after an else
                    Expr = mk_if(Condition, Then, Else),
                    {Expr, Rest2}
            end
    end.

mk_char({char, _YYline, C})      -> C.
mk_pad({pad, _YYline, PadInfo})  -> {pad, PadInfo}.
mk_push({push, _YYline, What})   -> {push, What}.
mk_pop({pop, _YYline, What})     -> {pop, What}.
mk_unary_op({op1, _YYline, Op})  -> Op.
mk_binary_op({op2, _YYline, Op}) -> Op.
mk_if(Cond, Then)                -> mk_if(Cond, Then, []).
mk_if(Cond, Then, Else)          -> {'if', Cond, {then,Then}, {else, Else}}.

-spec finalize(parsed_ir()) -> parsed().
finalize(Parsed) ->
    case deep_char_list_or_padding(Parsed) of
        true ->
            ensure_flatlist(Parsed);
        false ->
            Parsed1 = ensure_flatlist(Parsed),
            case get_param_max(Parsed1) of
                0 -> fun(SVs) -> eval_parsed({},SVs,Parsed1) end;
                1 -> fun(A,SVs) -> eval_parsed({A},SVs,Parsed1) end;
                2 -> fun(A,B,SVs) -> eval_parsed({A,B},SVs,Parsed1) end;
                3 -> fun(A,B,C,SVs) -> eval_parsed({A,B,C},SVs,Parsed1) end;
                4 -> fun(A,B,C,D,SVs) -> eval_parsed({A,B,C,D},SVs,Parsed1) end;
                5 -> fun(A,B,C,D,E,SVs) ->
                             eval_parsed({A,B,C,D,E},SVs,Parsed1) end;
                6 -> fun(A,B,C,D,E,F,SVs) ->
                             eval_parsed({A,B,C,D,E,F},SVs,Parsed1) end;
                7 -> fun(A,B,C,D,E,F,G,SVs) ->
                             eval_parsed({A,B,C,D,E,F,G},SVs,Parsed1) end;
                8 -> fun(A,B,C,D,E,F,G,H,SVs) ->
                             eval_parsed({A,B,C,D,E,F,G,H},SVs,Parsed1) end;
                9 -> fun(A,B,C,D,E,F,G,H,I,SVs) ->
                             eval_parsed({A,B,C,D,E,F,G,H,I},SVs,Parsed1) end
            end
    end.

get_param_max(Parsed) ->
    case lists:flatten(get_pnums(Parsed)) of
        [] -> 0;
        L  -> lists:max(L)
    end.

get_pnums([{push, {param,N}} | R])  -> [N | get_pnums(R)];
get_pnums([L | R]) when is_list(L) -> [get_pnums(L) | get_pnums(R)];
get_pnums([{'if', C, {then,T}, {else,E}} | R]) ->
    [get_pnums(ensure_flatlist(C)),
     get_pnums(ensure_flatlist(T)),
     get_pnums(ensure_flatlist(E)) | get_pnums(R)];
get_pnums([_ | R]) ->
    get_pnums(R);
get_pnums([]) ->
    [].

eval_parsed(Params0, StatVars, Parsed) ->
    {_, Params} = foldl(fun(V, {K, Ps}) -> {K+1, Ps#{K => V}} end,
                        {1, #{}},
                        tuple_to_list(Params0)),
    DynVars = #{},
    {_St, _Ps, _DVs, _SVs, Acc} =
        ep(Parsed, _Stack = [], Params, DynVars, StatVars, _Acc = []),
    _Result = lists:flatten(lists:reverse(Acc)).

ep([{push, {param, N}} | Rest], St, Ps, DVs, SVs, Acc) ->
    case Ps of
        #{N := V} -> ep(Rest, [V | St], Ps, DVs, SVs, Acc);
        #{}       -> ep(Rest, St,       Ps, DVs, SVs, Acc)
    end;
ep([{push, {dyn_var, K}} | Rest], St, Ps, DVs, SVs, Acc) ->
    case DVs of
        #{K := V} -> ep(Rest, [V | St], Ps, DVs, SVs, Acc);
        #{}       -> ep(Rest, St,       Ps, DVs, SVs, Acc)
    end;
ep([{push, {stat_var, K}} | Rest], St, Ps, DVs, SVs, Acc) ->
    case SVs of
        #{K := V} -> ep(Rest, [V | St], Ps, DVs, SVs, Acc);
        #{}       -> ep(Rest, St,       Ps, DVs, SVs, Acc)
    end;
ep([{push, {int, N}} | Rest], St, Ps, DVs, SVs, Acc) ->
    ep(Rest, [N | St], Ps, DVs, SVs, Acc);
ep([{pop, as_char} | Rest], [V | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, St, Ps, DVs, SVs, [V | Acc]);
ep([{pop, as_string} | Rest], [V | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, St, Ps, DVs, SVs, [V | Acc]);
ep([{pop, {printf, FmtInfo}} | Rest], [V | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, St, Ps, DVs, SVs, [printf_format(FmtInfo,V) | Acc]);
ep([{pop, {dyn_var, K}} | Rest], [V | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, St, Ps, DVs#{K => V}, SVs, Acc);
ep([{pop, {stat_var, K}} | Rest], [V | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, St, Ps, DVs, SVs#{K => V}, Acc);
ep([strlen | Rest], [V | St], Ps, DVs, SVs, Acc) ->
    if is_integer(V) -> ep(Rest, [1 | St], Ps, DVs, SVs, Acc);
       is_list(V)    -> ep(Rest, [length(V) | St], Ps, DVs, SVs, Acc)
    end;
ep([add | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [V2 + V1 | St], Ps, DVs, SVs, Acc);
ep([sub | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [V2 - V1 | St], Ps, DVs, SVs, Acc);
ep([mul | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [V2 * V1 | St], Ps, DVs, SVs, Acc);
ep(['div' | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [(V2 div V1) | St], Ps, DVs, SVs, Acc);
ep([mod | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [(V2 rem V1) | St], Ps, DVs, SVs, Acc);
ep([bitand | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [(V2 band V1) | St], Ps, DVs, SVs, Acc);
ep([bitor | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [(V2 bor V1) | St], Ps, DVs, SVs, Acc);
ep([bitxor | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [(V2 bxor V1) | St], Ps, DVs, SVs, Acc);
ep([eq | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [logeq(V2, V1) | St], Ps, DVs, SVs, Acc);
ep([lt | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [loglt(V2, V1) | St], Ps, DVs, SVs, Acc);
ep([gt | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [loggt(V2, V1) | St], Ps, DVs, SVs, Acc);
ep([logand | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [logand(V2, V1) | St], Ps, DVs, SVs, Acc);
ep([logor | Rest], [V1, V2 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [logor(V2, V1) | St], Ps, DVs, SVs, Acc);
ep([lognot | Rest], [V1 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [lognot(V1) | St], Ps, DVs, SVs, Acc);
ep([bitnot | Rest], [V1 | St], Ps, DVs, SVs, Acc) ->
    ep(Rest, [(bnot V1) | St], Ps, DVs, SVs, Acc);
ep([incr | Rest], St, Ps, DVs, SVs, Acc) ->
    ep(Rest, St, incr_first_two_params(Ps), DVs, SVs, Acc);
ep([{'if', Cond, {then, Then}, {else, Else}} | Rest], St, Ps, DVs, SVs, Acc) ->
    {St1, Ps1, DVs1, SVs1, Acc1} =
        eval_if_then_else(Cond, Then, Else, St, Ps, DVs, SVs, Acc),
    ep(Rest, St1, Ps1, DVs1, SVs1, Acc1);
ep([S | Rest], St, Ps, DVs, SVs, Acc) ->
    ep(Rest, St, Ps, DVs, SVs, [S | Acc]);
ep([], St, Ps, DVs, SVs, Acc) ->
    {St, Ps, DVs, SVs, Acc}.

incr_first_two_params(#{1 := V1, 2 := V2}=Ps) ->
    Ps#{1 := V1+1,
        2 := V2+1}.

eval_if_then_else(Cond, Then, Else, St0, Ps0, DVs0, SVs0, Acc0) ->
    case ep(Cond, St0, Ps0, DVs0, SVs0, Acc0) of
        {[H | St1], Ps1, DVs1, SVs1, Acc1} when H /= 0 ->
            Then1 = ensure_flatlist(Then),
            ep(Then1, St1, Ps1, DVs1, SVs1,Acc1);
        {[0 | St1], Ps1, DVs1, SVs1, Acc1} ->
            Else1 = ensure_flatlist(Else),
            ep(Else1, St1, Ps1, DVs1, SVs1,Acc1)
    end.

logeq(V2,   V1) when V2 == V1 -> 1;
logeq(_V2, _V1)               -> 0.

loglt(V2,   V1) when V2 < V1 -> 1;
loglt(_V2, _V1)              -> 0.

loggt(V2,   V1) when V2 > V1 -> 1;
loggt(_V2, _V1)              -> 0.


logand(V2,  V1) when V2 /= 0, V1 /= 0 -> 1;
logand(_V2,_V1)                       -> 0.

logor(V2, _V1) when V2 /= 0 -> 1;
logor(_V2, V1) when V1 /= 0 -> 1;
logor(_V2,_V1)              -> 0.

lognot(V) when V /= 0 -> 0;
lognot(V) when V == 0 -> 1.

ensure_flatlist(L) when is_list(L) -> lists:flatten(L);
ensure_flatlist(X)                 -> [X].

deep_char_list_or_padding(L) when is_list(L) -> dclop(L);
deep_char_list_or_padding(_L)                -> false.
dclop([H | T]) when is_list(H)    -> dclop(H) andalso dclop(T);
dclop([H | T]) when is_integer(H) -> dclop(T);
dclop([{pad, _P} | T])            -> dclop(T);
dclop([_ | _T])                   -> false;
dclop([])                         -> true.


printf_format({_Colon, Flags, Width, Precisison, ConvType}, Value) ->
    S1 = convert_value(ConvType, Precisison, Value),

    S2 = case {lists:member($#, Flags), ConvType} of
             {true, hex_lc} when is_integer(Value), Value /= 0 -> "0x"++S1;
             {true, hex_uc} when is_integer(Value), Value /= 0 -> "0X"++S1;
             {true, oct}    when hd(S1) /= $0                  -> "0"++S1;
             {_,    _}                                         -> S1
         end,

    S3 = case {lists:member($\s, Flags), ConvType, S2} of
             {true, _,   ""}       -> " ";
             {true, str, _}        -> S2;
             {true, _, "+"++_Rest} -> S2;
             {true, _, "-"++_Rest} -> S2;
             {true, _, _}          -> " "++S2;
             {false, _, _}         -> S2
         end,

    %% Note: if Value is a negative number, S1 will already have a "-" first.
    S4 = case lists:member($+, Flags) of
             true when is_integer(Value), Value >= 0 -> "+"++S3;
             true                                    -> S3;
             false                                   -> S3
         end,

    S5 = if Width == no_width   -> S4;
            Width =< length(S4) -> S4;
            Width >= length(S4) ->
                 Padding = lists:duplicate(Width - length(S4), $\s),
                 case lists:member($-, Flags) of
                     true  -> S4 ++ Padding;
                     false -> Padding ++ S4
                 end
         end,
    S5.

convert_value(str,   no_precision, V) -> convert_value2(str, unlimited, V);
convert_value(CType, no_precision, V) -> convert_value2(CType, 1, V);
convert_value(CType, Precision, V)    -> convert_value2(CType, Precision, V).

convert_value2(ConvType, Precision, V) ->
    S1 = convert_value3(ConvType, V),
    if Precision == 0, V == 0 ->
            "";
       is_integer(V), length(S1) < Precision ->
            Zeros = lists:duplicate(Precision - length(S1), $0),
            Zeros ++ S1;
       is_integer(V), length(S1) >= Precision ->
            S1;
       is_list(V), Precision /= unlimited, length(S1) > Precision ->
            %% Only the Precision first bytes are to be included
            lists:sublist(S1, Precision);
       is_list(V) ->
            S1
    end.

convert_value3(dec, V) when is_integer(V)    -> f("~w", [V]);
convert_value3(oct, V) when is_integer(V)    -> f("~.8b", [V]);
convert_value3(hex_lc, V) when is_integer(V) -> f("~.16b", [V]);
convert_value3(hex_uc, V) when is_integer(V) -> f("~.16B", [V]);
convert_value3(str, V) when is_list(V)       -> f("~s", [V]).

f(F, A) -> lists:flatten(io_lib:format(F, A)).
