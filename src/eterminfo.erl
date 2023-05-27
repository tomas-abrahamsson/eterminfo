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

-module(eterminfo).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([setup_by_infocmp/1, setup_by_infocmp/2]).
-export([tparm/2, tparm/3, tparm/4, tparm/5, tparm/6]).
-export([tparm/7, tparm/8, tparm/9, tparm/10, tparm/11]).
-export([tigetflag/2]).
-export([tigetnum/2]).
-export([tigetstr/2]).

-export_type([term_name/0, terminfo/0]).
-export_type([infocmp_opts/0, infocmp_opt/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

-type term_name() :: string().
-type terminfo() :: #{terminfo_id := terminfo_id(),
                      cap_name() => cap(),
                      literal_key(cap_name()) => string()}.

-type terminfo_id() :: [string()].
-type cap_name() :: string().
-type cap() :: out_seq() % string capabilities
             | integer() % numeric capabilities
             | boolean() % boolean capabilities
             | fun((...) -> out_seq()) % parameterized capabilities
               .
-type literal_key(ForX) :: {literal, ForX}.
-type out_seq() :: [char() | pad()].
-type pad() :: {pad, #{delay := non_neg_integer(), % milliseconds
                       proportional := boolean(),
                       mandatory := boolean()}}.

-type bool_opt(Opt) :: Opt | {Opt, boolean()}.

-type infocmp_opts() :: [infocmp_opt() | _IgnoredOpt::term()].
-type infocmp_opt() :: bool_opt(terminfo_names)
                     | bool_opt(long_names).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec setup_by_infocmp(term_name()) -> {ok, terminfo()} | {error, term()}.
setup_by_infocmp(TermType) ->
    setup_by_infocmp(TermType, _Opts=[]).

-spec setup_by_infocmp(term_name(), infocmp_opts()) ->
          {ok, terminfo()} | {error, term()}.
setup_by_infocmp(TermType, Opts) ->
    ProgOpts = case {proplists:get_bool(terminfo_names, Opts),
                     proplists:get_bool(long_names, Opts)} of
                   {false, false} -> []; %% Default (termcap names)
                   {true,  false} -> []; %% Default (termcap names)
                   {false,  true} -> ["-L"];
                   _              -> erlang:error(badarg)
               end,
    case find_infocmp(Opts) of
        false ->
            {error, {cannot_locate_program, "infocmp"}};
        Program ->
            case run_infocmp(Program, ProgOpts ++ [TermType]) of
                {ok, TermInfoStr} ->
                    case eterminfo_infocmp_parser:string(TermInfoStr) of
                        {ok, _TermInfoM} = Result ->
                            Result;
                        {error, Reason} ->
                            {error, {infocmp_parse_error,Reason,TermInfoStr}}
                    end;
                {error, Reason} ->
                    {error, {infocmp_error, Reason}}
            end
    end.

find_infocmp(Opts) ->
    case proplists:get_value(terminfo_string, Opts) of
        undefined ->
            case os:find_executable("infocmp") of
                false ->
                    os:find_executable("infocmp", "/bin:/usr/bin");
                Program ->
                    Program
            end;
        String ->
            %% use this value instead of the output from infocmp
            {terminfo_str, String}
    end.

run_infocmp(Program, Args) when is_list(Program) ->
    run_cmd(Program, Args);
run_infocmp({terminfo_str, TerminfoOutput}, _) ->
    {ok, TerminfoOutput}.

run_cmd(Cmd, Args) ->
    Port = open_port({spawn_executable, Cmd},
                     [stream, in, stderr_to_stdout, exit_status,
                      {args, Args},
                      {env, [{"LC_ALL", "C"}]}]),
    collect_stdout(Port, []).

collect_stdout(Port, Acc) ->
    receive
        {Port, {data, D}} ->
            collect_stdout(Port, [D | Acc]);
        {Port, {exit_status, St}} ->
            Output = lists:flatten(lists:reverse(Acc)),
            if St =:= 0 -> {ok, Output};
               St =/= 0 -> {error, Output}
            end
    end.


%% API for parsed map
%%
%% Keys are capability names (Capnames in the termcap(5) man page).
%%
-spec tparm(terminfo(), cap_name()) -> out_seq().

tparm(M, Str) -> (maps:get(Str, M))(#{}).
tparm(M, Str, A) -> (maps:get(Str, M))(A,#{}).
tparm(M, Str, A,B) -> (maps:get(Str, M))(A,B,#{}).
tparm(M, Str, A,B,C) -> (maps:get(Str, M))(A,B,C,#{}).
tparm(M, Str, A,B,C,D) -> (maps:get(Str, M))(A,B,C,D,#{}).
tparm(M, Str, A,B,C,D,E) -> (maps:get(Str, M))(A,B,C,D,E,#{}).
tparm(M, Str, A,B,C,D,E,F) -> (maps:get(Str, M))(A,B,C,D,E,F,#{}).
tparm(M, Str, A,B,C,D,E,F,G) -> (maps:get(Str, M))(A,B,C,D,E,F,G,#{}).
tparm(M, Str, A,B,C,D,E,F,G,H) -> (maps:get(Str, M))(A,B,C,D,E,F,G,H,#{}).
tparm(M, Str, A,B,C,D,E,F,G,H,I) -> (maps:get(Str, M))(A,B,C,D,E,F,G,H,I,#{}).

tigetflag(M, Str) ->
    case maps:find(Str, M) of
        {ok, true}  -> true;
        {ok, false} -> false;
        {ok, _}     -> {error, {not_a_boolean_capability, Str}};
        error       -> false
    end.

tigetnum(M, Str) ->
    case maps:find(Str, M) of
        {ok, N} when is_integer(N) -> N;
        {ok, X}                    -> {error,{not_a_numeric_capability,Str,X}};
        error                      -> 0
    end.

tigetstr(M, Str) ->
    case maps:find(Str, M) of
        {ok, S} when is_list(S)     -> S;
        {ok, F} when is_function(F) -> maps:get({literal,Str}, M);
        {ok, X}                     -> {error,{not_a_string_capability,Str,X}};
        error                       -> {error,{string_capability_not_found,Str}}
    end.
