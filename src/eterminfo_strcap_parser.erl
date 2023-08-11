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
-type presult() :: pstr() % default
                   %% If the option return_static_vars is set to true:
                 | {pstr(), #{static_vars := static_vars()}}.
-type pstr() :: [parsed_elem()].
-type parsed_elem() :: char() | {pad, eterminfo_strcap_scanner:pad_info()}.
-type pfun() :: fun((eval_opts()) -> presult()) % 0..9 parameters
              | fun((param(), eval_opts()) -> presult())
              | fun((param(), param(), eval_opts()) -> presult())
              | fun((param(), param(), param(), eval_opts()) -> presult())
              | fun((param(), param(), param(), param(), eval_opts()) ->
                        presult())
              | fun((param(), param(), param(), param(), param(),
                     eval_opts()) ->
                        presult())
              | fun((param(), param(), param(), param(), param(), param(),
                     eval_opts()) ->
                        presult())
              | fun((param(), param(), param(), param(), param(), param(),
                     param(), eval_opts()) ->
                        presult())
              | fun((param(), param(), param(), param(), param(), param(),
                     param(), param(), eval_opts()) ->
                        presult())
              | fun((param(), param(), param(), param(), param(), param(),
                     param(), param(), param(), eval_opts()) ->
                        presult()).
-type eval_opts() :: #{static_vars => static_vars(),
                       return_static_vars => boolean()}.
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
                    {else, parsed_ir()}}.

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
                    Expr = mk_if(Condition, Then, [Elseif]),
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
            Parsed;
        false ->
            case get_param_max(Parsed) of
                0 -> fun(EvalOpts) ->
                             eval_parsed({}, Parsed, EvalOpts)
                     end;
                1 -> fun(A, EvalOpts) ->
                             eval_parsed({A}, Parsed, EvalOpts) end;
                2 -> fun(A,B,EvalOpts) ->
                             eval_parsed({A, B}, Parsed, EvalOpts) end;
                3 -> fun(A,B,C,EvalOpts) ->
                             eval_parsed({A, B, C}, Parsed, EvalOpts) end;
                4 -> fun(A,B,C,D,EvalOpts) ->
                             eval_parsed({A, B, C, D}, Parsed, EvalOpts) end;
                5 -> fun(A,B,C,D,E,EvalOpts) ->
                             eval_parsed({A, B, C, D, E}, Parsed, EvalOpts)
                     end;
                6 -> fun(A, B, C, D, E, F, EvalOpts) ->
                             eval_parsed({A, B, C, D, E, F},
                                         Parsed, EvalOpts) end;
                7 -> fun(A, B, C, D, E, F, G, EvalOpts) ->
                             eval_parsed({A, B, C, D, E, F, G},
                                         Parsed, EvalOpts) end;
                8 -> fun(A, B, C, D, E, F, G, H, EvalOpts) ->
                             eval_parsed({A, B, C, D, E, F, G, H},
                                         Parsed, EvalOpts)
                     end;
                9 -> fun(A, B, C, D, E, F, G, H, I, EvalOpts) ->
                             eval_parsed({A, B, C, D, E, F, G, H, I},
                                         Parsed, EvalOpts)
                     end
            end
    end.

get_param_max(Parsed) ->
    get_pmax(Parsed, 0).

get_pmax([{push, {param,N}} | Rest], Max)  -> get_pmax(Rest, max(N, Max));
get_pmax([{'if', Cond, {then,Then}, {else, Else}} | Rest], Max) ->
    Max1 = lists:foldl(fun get_pmax/2, Max, [Cond, Then, Else]),
    get_pmax(Rest, Max1);
get_pmax([_ | Rest], Max) ->
    get_pmax(Rest, Max);
get_pmax([], Max) ->
    Max.

eval_parsed(Params0, Parsed, EvalOpts) ->
    State0 = #{stack => [],
               params => maps:from_list(enumerate(tuple_to_list(Params0))),
               dyn_vars => #{},
               static_vars => maps:get(static_vars, EvalOpts, #{}),
               incr_done => false,
               %% The result:
               acc => []},
    #{acc := Acc, static_vars := StaticOutVars} = ep(Parsed, State0),
    Res = lists:reverse(Acc),
    case EvalOpts of
        #{return_static_vars := true} ->
            {Res, #{static_vars => StaticOutVars}};
        #{} ->
            Res
    end.

ep([Elem | Rest], #{stack := Stack, params := Ps,
                    dyn_vars := DVs, static_vars := SVs,
                    incr_done := IncrDone,
                    acc := Acc}=State) ->
    case Elem of
        {push, {param, N}} ->
            case Ps of
                #{N := V} -> ep(Rest, State#{stack := [V | Stack]});
                #{}       -> ep(Rest, State)
            end;
        {push, {dyn_var, K}} ->
            {V, DVs1} = get_var_or_initialize_to_zero(K, DVs),
            ep(Rest, State#{stack := [V | Stack], dyn_vars := DVs1});
        {push, {static_var, K}} ->
            {V, SVs1} = get_var_or_initialize_to_zero(K, SVs),
            ep(Rest, State#{stack := [V | Stack], static_vars := SVs1});
        {push, {int, N}} ->
            ep(Rest, State#{stack := [N | Stack]});
        {pop, as_char} ->
            {V, Stack1} = pop_number(Stack),
            ep(Rest, State#{stack := Stack1, acc := [V | Acc]});
        {pop, as_string} ->
            {V, Stack1} = pop_string(Stack),
            ep(Rest, State#{stack := Stack1, acc := lists:reverse(V, Acc)});
        {pop, {printf, FmtInfo}} ->
            {V, Stack1} = pop_elem(Stack),
            Acc1 = lists:reverse(printf_format(FmtInfo,V), Acc),
            ep(Rest, State#{stack := Stack1, acc := Acc1});
        {pop, {dyn_var, K}} ->
            {V, Stack1} = pop_elem(Stack),
            ep(Rest, State#{stack := Stack1, dyn_vars := DVs#{K => V}});
        {pop, {static_var, K}} ->
            {V, Stack1} = pop_elem(Stack),
            ep(Rest, State#{stack := Stack1, static_vars := SVs#{K => V}});
        strlen ->
            {V, Stack1} = pop_string(Stack),
            ep(Rest, State#{stack := [length(V) | Stack1]});
        add ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [V1 + V2 | Stack1]});
        sub ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [V1 - V2 | Stack1]});
        mul ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [V1 * V2 | Stack1]});
        'div' ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            if V2 /= 0 -> ep(Rest, State#{stack := [(V1 div V2) | Stack1]});
               V2 == 0 -> ep(Rest, State#{stack := [0 | Stack1]})
            end;
        mod ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            if V2 /= 0 -> ep(Rest, State#{stack := [(V1 rem V2) | Stack1]});
               V2 == 0 -> ep(Rest, State#{stack := [0 | Stack1]})
            end;
        bitand ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [(V1 band V2) | Stack1]});
        bitor ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [(V1 bor V2) | Stack1]});
        bitxor ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [(V1 bxor V2) | Stack1]});
        eq ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [logeq(V1, V2) | Stack1]});
        lt -> % less than
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [loglt(V1, V2) | Stack1]});
        gt -> % greater than
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [loggt(V1, V2) | Stack1]});
        logand ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [logand(V1, V2) | Stack1]});
        logor ->
            {V1, V2, Stack1} = pop2_numbers(Stack),
            ep(Rest, State#{stack := [logor(V1, V2) | Stack1]});
        lognot ->
            {V1, Stack1} = pop_number(Stack),
            ep(Rest, State#{stack := [lognot(V1) | Stack1]});
        bitnot ->
            {V1, Stack1} = pop_number(Stack),
            ep(Rest, State#{stack := [(bnot V1) | Stack1]});
        incr ->
            if not IncrDone ->
                    Ps1 = incr_first_two_params_if_ints(Ps),
                    ep(Rest, State#{params := Ps1, incr_done := true});
               IncrDone ->
                    ep(Rest, State)
            end;
        {'if', Cond, {then, Then}, {else, Else}} ->
            #{stack := Stack1}=State1 = ep(Cond, State),
            {H, Stack2} = pop_number(Stack1),
            State2 = State1#{stack := Stack2},
            State3 = if H /= 0 -> ep(Then, State2);
                        H == 0 -> ep(Else, State2)
                     end,
            ep(Rest, State3);
        _ ->
            ep(Rest, State#{acc := [Elem | Acc]})
    end;
ep([], State) ->
    State.

get_var_or_initialize_to_zero(K, Vars) ->
    case maps:find(K, Vars) of
        {ok, V} -> {V, Vars};
        error   -> {0, Vars#{K => 0}}
    end.

incr_first_two_params_if_ints(Ps0) ->
    Ps1 = case Ps0 of
              #{1 := V1} when is_integer(V1) -> Ps0#{1 := V1 + 1};
              #{} -> Ps0
          end,
    case Ps1 of
        #{2 := V2} when is_integer(V2) -> Ps1#{2 := V2 + 1};
        #{} -> Ps1
    end.

pop_number([X | RestStack]) -> {coerce_number(X), RestStack};
pop_number([])              -> {0, []}.

%% Order of elems: calculate 15 mod 4:
%% assume we've done: push n1, push n2 (n1 = 15, n2=4)
%% and are about to calculate n1 mod n2
%% pop2_numbers(Stack) -> {n1, n2, RestStack}
pop2_numbers(Stack) ->
    {N2, Stack1} = pop_number(Stack),
    {N1, Stack2} = pop_number(Stack1),
    {N1, N2, Stack2}.

pop_string([X | RestStack]) -> {coerce_string(X), RestStack};
pop_string([])              -> {"", []}.

coerce_number(N) when is_integer(N) -> N;
coerce_number(_)                    -> 0.

coerce_string(S) when is_list(S) -> S;
coerce_string(_)                 -> "".

pop_elem([Elem | RestStack]) -> {Elem, RestStack};
pop_elem([])                 -> {0, []}.

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

deep_char_list_or_padding(L) when is_list(L) -> dclop(L);
deep_char_list_or_padding(_L)                -> false.
dclop([H | T]) when is_list(H)    -> dclop(H) andalso dclop(T);
dclop([H | T]) when is_integer(H) -> dclop(T);
dclop([{pad, _P} | T])            -> dclop(T);
dclop([_ | _T])                   -> false;
dclop([])                         -> true.


printf_format({_Colon, Flags, Width, Precisison, ConvType}, Value) ->
    Value1 = coerce_printf_value(ConvType, Value),
    S1 = convert_value(ConvType, Precisison, Value1),

    S2 = case {lists:member($#, Flags), ConvType} of
             {true, hex_lc} when is_integer(Value1), Value1 /= 0 -> "0x"++S1;
             {true, hex_uc} when is_integer(Value1), Value1 /= 0 -> "0X"++S1;
             {true, oct}    when hd(S1) /= $0                    -> "0"++S1;
             {_,    _}                                           -> S1
         end,

    S3 = case {lists:member($\s, Flags), ConvType, S2} of
             {true, _,   ""}       -> " ";
             {true, str, _}        -> S2;
             {true, _, "+"++_Rest} -> S2;
             {true, _, "-"++_Rest} -> S2;
             {true, _, _}          -> " "++S2;
             {false, _, _}         -> S2
         end,

    %% Note: if Value1 is a negative number, S1 will already have a "-" first.
    S4 = case lists:member($+, Flags) of
             true when is_integer(Value1), Value1 >= 0 -> "+"++S3;
             true                                      -> S3;
             false                                     -> S3
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

coerce_printf_value(ConvType, X) ->
    GConvType = case ConvType of
                    str -> str;
                    _   -> int % there are many types of int
                end,
    if GConvType == str, is_list(X) -> X;
       GConvType == int, is_integer(X) -> X;
       GConvType == str -> "";
       GConvType == int -> 0
    end.

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

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

f(F, A) -> lists:flatten(io_lib:format(F, A)).
