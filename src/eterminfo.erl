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

%%--------------------------------------------------------------------
%% @doc
%% This program can acquire terminfo capabilities. The capabilities
%% are represented as a map of entries for various types of capabilities:
%%
%% <dl>
%%   <dt>string capabilities</dt>
%%   <dd>A sequence of characters as integers (a string),
%%       or `{pad,Info}' elements.</dd>
%%   <dt>numerical capabilities</dt>
%%   <dd>An integer</dd>
%%   <dt>boolean capabilities</dt>
%%   <dd>`true' or `false'</dd>
%%   <dt>parameterized capabilities</dt>
%%   <dd>A function, that takes as last parameter a map of static variables.
%%   </dd>
%% </dl>
%%
%% Example:
%% ```
%%   1> {ok, M} = eterminfo:setup_by_infocmp("vt100", [long_names]).
%%   {ok,#{"key_right" => "\eOC",
%%         "enter_am_mode" => "\e[?7h",
%%         "carriage_return" => "\r",
%%         "exit_standout_mode" =>
%%             [27,91,109,
%%              {pad,#{delay => 2,mandatory => false,proportional => false}}],
%%         "parm_left_cursor" => #Fun<...>,
%%         ...}}
%%   2> #{"parm_left_cursor" := Left} = M.
%%   3> Left(10, #{}).
%%   "\e[10D"
%% '''
%% @end
%%--------------------------------------------------------------------

-module(eterminfo).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([install_by_infocmp/0, install_by_infocmp/1]).

-export([tparm/1, tparm/2, tparm/3, tparm/4, tparm/5]).
-export([tparm/6, tparm/7, tparm/8, tparm/9, tparm/10]).
-export([tigetflag/1]).
-export([tigetnum/1]).
-export([tigetstr/1]).

%% Lower-level contituents api:

-export([setup_by_infocmp/1, setup_by_infocmp/2]).
-export([install_terminfo/2]).
-export([is_terminfo_installed/0, is_terminfo_installed/1]).
-export([get_installed_terminfo/0, get_installed_terminfo/1]).
-export([get_term_type/0]).
-export([get_term_type_or_default/0, get_term_type_or_default/1]).

-export([tparm_m/2, tparm_m/3, tparm_m/4, tparm_m/5, tparm_m/6]).
-export([tparm_m/7, tparm_m/8, tparm_m/9, tparm_m/10, tparm_m/11]).
-export([tigetflag_m/2]).
-export([tigetnum_m/2]).
-export([tigetstr_m/2]).

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


%% @equiv install_by_infocmp(#{})
install_by_infocmp() ->
    install_by_infocmp(#{}).

%%--------------------------------------------------------------------
%% @doc Read a terminfo definition using the `infocmp' program and
%%      install it. By default, it will try to find terminfo for `$TERM'
%% with long names.
%%
%% The {@link tparm/1}..{@link tparm/10}, {@link tigetflag/1}, {@link
%% tigetnum/1} and {@link tigetstr/1} functions use the installed
%% term for the current terminal type.
%%
%% Invoking this function will more than once for a particular
%% terminal type will cause it to overwrite the persistent term,
%% which will trigger a system-wide garbage collection in the Erlang vm.
%% It is intended to be invoked only once initially.
%% @end
%% --------------------------------------------------------------------
install_by_infocmp(Spec) ->
    TermType = maps:get(term, Spec, get_term_type_or_default()),
    LongNamesOpts = case maps:get(long_names, Spec, true) of
                        true -> [long_names];
                        false -> []
                    end,
    case setup_by_infocmp(TermType, LongNamesOpts) of
        {ok, TermInfo} ->
            install_terminfo(TermType, TermInfo),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Retrieve the current terminal type.
%% Example:
%% ```
%%   1> eterminfo:get_term_type().
%%   "vt100"
%% '''
%% @end
%%--------------------------------------------------------------------
get_term_type() ->
    case os:getenv("TERM") of
        false -> error(no_TERM);
        Term  -> Term
    end.

%% @equiv get_term_type_or_default("ansi")
get_term_type_or_default() ->
    get_term_type_or_default("ansi").

%%--------------------------------------------------------------------
%% @doc Retrieve the current terminal type, or a default.
%% Example:
%% ```
%%   1> eterminfo:get_term_type("ansi").
%%   "vt100"
%%   2> os:unsetenv("TERM").
%%   3> eterminfo:get_term_type("ansi").
%%   "ansi"
%% '''
%% @end
%%--------------------------------------------------------------------
get_term_type_or_default(Default) ->
    case os:getenv("TERM") of
        false -> Default;
        Term  -> Term
    end.

%%--------------------------------------------------------------------
%% @doc For a particular terminal type, install a terminfo as a
%% persistent term.
%% @end
%%--------------------------------------------------------------------
install_terminfo(TermType, TermInfo) ->
    persistent_term:put({?MODULE, TermType}, TermInfo).


%% @equiv get_term_type_or_default(get_term_type_or_default())
is_terminfo_installed() ->
    is_terminfo_installed(get_term_type_or_default()).

%%--------------------------------------------------------------------
%% @doc Check if a terminal type is installed.
%% Example:
%% ```
%%   1> eterminfo:is_terminfo_installed().
%%   false
%%   2> eterminfo:install_by_infocmp().
%%   ok
%%   3> eterminfo:is_terminfo_installed().
%%   true
%% '''
%% @end
%%--------------------------------------------------------------------
is_terminfo_installed(TermType) ->
    try persistent_term:get({?MODULE, TermType}) of
        _ -> true
    catch error:badarg ->
            false
    end.

get_installed_terminfo() ->
    get_installed_terminfo(get_term_type_or_default()).

get_installed_terminfo(TermType) ->
    persistent_term:get({?MODULE, TermType}).


%% @equiv setup_by_infocmp(TermType, [])
-spec setup_by_infocmp(term_name()) -> {ok, terminfo()} | {error, term()}.
setup_by_infocmp(TermType) ->
    setup_by_infocmp(TermType, _Opts=[]).

%%--------------------------------------------------------------------
%% @doc Use the `infocmp' program to read terminal info by terminal name.
%% Return a map with capability entries.
%% @end
%%--------------------------------------------------------------------
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

tparm(Str) ->
    tparm_m(get_installed_terminfo(), Str).
tparm(Str, A) ->
    tparm_m(get_installed_terminfo(), Str, A).
tparm(Str, A,B) ->
    tparm_m(get_installed_terminfo(), Str, A,B).
tparm(Str, A,B,C) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C).
tparm(Str, A,B,C,D) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C,D).
tparm(Str, A,B,C,D,E) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C,D,E).
tparm(Str, A,B,C,D,E,F) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C,D,E,F).
tparm(Str, A,B,C,D,E,F,G) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C,D,E,F,G).
tparm(Str, A,B,C,D,E,F,G,H) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C,D,E,F,G,H).
tparm(Str, A,B,C,D,E,F,G,H,I) ->
    tparm_m(get_installed_terminfo(), Str, A,B,C,D,E,F,G,H,I).

tigetflag(Str) ->
    tigetflag_m(get_installed_terminfo(), Str).

tigetnum(Str) ->
    tigetnum_m(get_installed_terminfo(), Str).

tigetstr(Str) ->
    tigetstr_m(get_installed_terminfo(), Str).


-spec tparm_m(terminfo(), cap_name()) -> out_seq().

tparm_m(M, Str) -> (maps:get(Str, M))(#{}).
tparm_m(M, Str, A) -> (maps:get(Str, M))(A,#{}).
tparm_m(M, Str, A,B) -> (maps:get(Str, M))(A,B,#{}).
tparm_m(M, Str, A,B,C) -> (maps:get(Str, M))(A,B,C,#{}).
tparm_m(M, Str, A,B,C,D) -> (maps:get(Str, M))(A,B,C,D,#{}).
tparm_m(M, Str, A,B,C,D,E) -> (maps:get(Str, M))(A,B,C,D,E,#{}).
tparm_m(M, Str, A,B,C,D,E,F) -> (maps:get(Str, M))(A,B,C,D,E,F,#{}).
tparm_m(M, Str, A,B,C,D,E,F,G) -> (maps:get(Str, M))(A,B,C,D,E,F,G,#{}).
tparm_m(M, Str, A,B,C,D,E,F,G,H) -> (maps:get(Str, M))(A,B,C,D,E,F,G,H,#{}).
tparm_m(M, Str, A,B,C,D,E,F,G,H,I) -> (maps:get(Str, M))(A,B,C,D,E,F,G,H,I,#{}).

tigetflag_m(M, Str) ->
    case maps:find(Str, M) of
        {ok, true}  -> true;
        {ok, false} -> false;
        {ok, _}     -> {error, {not_a_boolean_capability, Str}};
        error       -> false
    end.

tigetnum_m(M, Str) ->
    case maps:find(Str, M) of
        {ok, N} when is_integer(N) -> N;
        {ok, X}                    -> {error,{not_a_numeric_capability,Str,X}};
        error                      -> 0
    end.

tigetstr_m(M, Str) ->
    case maps:find(Str, M) of
        {ok, S} when is_list(S)     -> S;
        {ok, F} when is_function(F) -> maps:get({literal,Str}, M);
        {ok, X}                     -> {error,{not_a_string_capability,Str,X}};
        error                       -> {error,{string_capability_not_found,Str}}
    end.
