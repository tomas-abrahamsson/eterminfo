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

Nonterminals
        ti ti_elements ti_element
        expr expr_element
        if_then_else then_else elses
        .
Terminals
        char chars pad
        pop push
        op2 op1
        'if' then else endif
        .
Rootsymbol
        ti.
Endsymbol
        '$end'.

ti ->   ti_elements:                    finalize('$1').

ti_elements -> ti_element ti_elements:  ['$1' | '$2'].
ti_elements -> '$empty':                [].

ti_element -> char:                     mk_char('$1').
ti_element -> chars:                    mk_chars('$1').
ti_element -> pad:                      mk_pad('$1').
ti_element -> expr:                     '$1'.


expr -> expr_element expr:              ['$1' | '$2'].
expr -> expr_element:                   ['$1'].

expr_element -> push:                   mk_push('$1').
expr_element -> pop:                    mk_pop('$1').
expr_element -> op1:                    mk_unary_op('$1').
expr_element -> op2:                    mk_binary_op('$1').
expr_element -> if_then_else:           '$1'.

if_then_else -> 'if' then_else:         '$2'.

then_else -> expr then ti_elements endif:       mk_if('$1', '$3').
then_else -> expr then ti_elements elses:       mk_if('$1', '$3','$4').

elses -> else ti_elements endif:        mk_else('$2').
elses -> else then_else:                mk_else('$2').
elses -> else endif:                    mk_else({}).

Erlang code.

-import(lists, [foldl/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-export([]).

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

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Code
%%--------------------------------------------------------------------

mk_char({char, _YYline, C})      -> C.
mk_chars({chars, _YYline, Cs})   -> Cs.
mk_pad({pad, _YYline, PadInfo})  -> {pad, PadInfo}.
mk_push({push, _YYline, What})   -> {push, What}.
mk_pop({pop, _YYline, What})     -> {pop, What}.
mk_unary_op({op1, _YYline, Op})  -> Op.
mk_binary_op({op2, _YYline, Op}) -> Op.
mk_if(Cond, Then)                -> {'if', Cond, {then,Then}, {else,{}}}.
mk_if(Cond, Then, Else)          -> {'if', Cond, {then,Then}, Else}.
mk_else(Else)                    -> {else, Else}.


finalize(PTree) ->
    case deep_char_list_or_padding(PTree) of
        true ->
            ensure_flatlist(PTree);
        false ->
            PTree1 = ensure_flatlist(PTree),
            case get_param_max(PTree1) of
                0 -> fun(SVs) -> eval_ptree({},SVs,PTree1) end;
                1 -> fun(A,SVs) -> eval_ptree({A},SVs,PTree1) end;
                2 -> fun(A,B,SVs) -> eval_ptree({A,B},SVs,PTree1) end;
                3 -> fun(A,B,C,SVs) -> eval_ptree({A,B,C},SVs,PTree1) end;
                4 -> fun(A,B,C,D,SVs) -> eval_ptree({A,B,C,D},SVs,PTree1) end;
                5 -> fun(A,B,C,D,E,SVs) ->
                             eval_ptree({A,B,C,D,E},SVs,PTree1) end;
                6 -> fun(A,B,C,D,E,F,SVs) ->
                             eval_ptree({A,B,C,D,E,F},SVs,PTree1) end;
                7 -> fun(A,B,C,D,E,F,G,SVs) ->
                             eval_ptree({A,B,C,D,E,F,G},SVs,PTree1) end;
                8 -> fun(A,B,C,D,E,F,G,H,SVs) ->
                             eval_ptree({A,B,C,D,E,F,G,H},SVs,PTree1) end;
                9 -> fun(A,B,C,D,E,F,G,H,I,SVs) ->
                             eval_ptree({A,B,C,D,E,F,G,H,I},SVs,PTree1) end
            end
    end.

get_param_max(PTree) ->
    case lists:flatten(get_pnums(PTree)) of
        [] -> 0;
        L  -> lists:max(L)
    end.

get_pnums([{push,{param,N}} | R])  -> [N | get_pnums(R)];
get_pnums([L | R]) when is_list(L) -> [get_pnums(L) | get_pnums(R)];
get_pnums([{'if',C,{then,T},{else,E}} | R]) ->
    [get_pnums(ensure_flatlist(C)),
     get_pnums(ensure_flatlist(T)),
     get_pnums(ensure_flatlist(E)) | get_pnums(R)];
get_pnums([_ | R]) ->
    get_pnums(R);
get_pnums([]) ->
    [].

-record(env,
        {st,    % list()        % stack
         ps,    % dict()        % params
         dvs,   % dict()        % dynamic vars
         svs    % dict()        % static vars
        }).

eval_ptree(Params0, StatVars, PTree) ->
    {_, Params} = foldl(fun(V, {K, Ps}) -> {K+1, Ps#{K => V}} end,
                        {1, #{}},
                        tuple_to_list(Params0)),
    DynVars = #{},
    {_St, _Ps, _DVs, _SVs, Acc} =
        ep(PTree, _Stack = [], Params, DynVars, StatVars, _Acc = []),
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
        {[0 | St1], Ps1, DVs1, SVs1, Acc1} when Else /= {} ->
            Else1 = ensure_flatlist(Else),
            ep(Else1, St1, Ps1, DVs1, SVs1,Acc1);
        {[0 | St1], Ps1, DVs1, SVs1, Acc1} when Else == {} ->
            {St1, Ps1, DVs1, SVs1, Acc1}
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
