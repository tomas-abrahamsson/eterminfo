#
# Makefile for eterminfo -- for projects that use (GNU) make to build
#
# Copyright (C) 2023  Tomas Abrahamsson
#
# Author: Tomas Abrahamsson <tab@lysator.liu.se>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA  02110-1301  USA


# Parameters:
# ----------
# ETERMINFO_PREFIX -- if this makefile is to be included in some larger project
#                    NB: must end with a slash!
# VERBOSE         -- set (to any value) to make the following steps verbose:
#                    * eunit testing
#                    * edoc generation command
#                    * xref checking command
# ERL             -- path to erl
# ERLC            -- path to erlc
# ERLC_FLAGS      -- any additional flags to erlc
# EDOC_DEST_DIR   -- destination directory for documentation
# EDOC_OPTS       -- any options to edoc (NB: _not_ including list brackets!)
# ERL_BATCH_FLAGS -- Flags to start the Erlang VM in batch mode


.SUFFIXES += .erl .hrl .yrl .xrl .beam .plt
.PHONY: all clean test doc xref dialyze build_plt clean_plt

SHELL = /bin/sh

ERL  ?= erl
ERLC ?= erlc

ETERMINFO_PREFIX =

src            = $(ETERMINFO_PREFIX)src
ebin           = $(ETERMINFO_PREFIX)ebin
incdir         = $(ETERMINFO_PREFIX)include
test           = $(ETERMINFO_PREFIX)test
priv           = $(ETERMINFO_PREFIX)priv
doc            = $(ETERMINFO_PREFIX)doc
build          = $(ETERMINFO_PREFIX)build

empty_str :=
space := $(empty_str) $(empty_str)
comma := ,

##
ifndef EDOC_DEST_DIR
EDOC_DEST_DIR=.
endif

ifndef EDOC_OPTS
EDOC_OPTS={preprocess,true},{pretty_printer,erl_pp}
endif

## Check verbosity
ifdef VERBOSE
verbose_opt := verbose
silencer    :=
else
verbose  :=
silencer := @
endif

# When making 'clean', don't run in parallel.
# Otherwise "make -j clean all" in an already-built dir will fail:
# it will clean while rebuilding, or fail to detect what needs to
# be rebuilt since it runs in parallel.
#
# If anyone tries that, revert to non-parallel execution, which
# is safe, but slower. Issue a warning about that.
ifneq ($(filter clean,$(MAKECMDGOALS)),)
.NOTPARALLEL:
ifneq ($(filter-out clean,$(MAKECMDGOALS)),)
# User is not _only_ making clean...
$(warning "WARNING: cannot make in parallel when _also_ making clean")
endif
endif


ERLC_FLAGS += -Wall +debug_info -I$(incdir)

ERL_BATCH_FLAGS = +B -noshell -noinput

OTP_MAJOR_MINOR = $(shell $(ERL) $(ERL_BATCH_FLAGS) -eval ' \
    GetVsnFromFile = \
        fun(V) -> case lists:reverse(string:strip(V,right,$$\n)) of \
                      "**"++Rest -> lists:reverse(Rest); \
                      Rest -> lists:reverse(Rest) \
                  end \
        end, \
    RootDir = code:root_dir(), \
    io:format( \
      "~s~n", \
      [case erlang:system_info(otp_release) of \
           "R"++_=Rel -> Rel; \
           RStr -> \
               lists:foldl( \
                 fun(_,R) when R /= RStr -> R; \
                    (F,R) -> case file:read_file(F) of \
                                 {ok,B} -> GetVsnFromFile(binary_to_list(B));\
                                 _ -> R \
                             end \
                 end, \
                 RStr, \
                 [filename:join([RootDir,"releases",RStr,"OTP_VERSION"]), \
                  filename:join([RootDir,"releases","OTP_VERSION"])]) \
       end]), \
    halt(0).')

# Use the same plt file as rebar would use, for compatibility
plt = $(ETERMINFO_PREFIX).eterminfo-$(OTP_MAJOR_MINOR).plt


# Sorting it also eliminates duplicates
MODULES := \
	$(sort \
	  $(patsubst $(src)/%.erl,%,$(wildcard $(src)/*.erl)) \
	  $(patsubst $(src)/%.yrl,%,$(wildcard $(src)/*.yrl)) \
	  $(patsubst $(src)/%.xrl,%,$(wildcard $(src)/*.xrl)))

TEST_MODULES := \
	$(patsubst $(test)/%.erl,%,$(wildcard $(test)/*.erl))

# Run eunit on these modules:
# - If module M and M_tests exist, only include M (M_tests is then implicit)
# - If M_tests exists, but no M, include M_tests
# sorting it also removes any duplicates
EUNIT_MODULES := \
	$(strip \
	  $(MODULES) \
	  $(filter-out $(patsubst %,%_tests,$(MODULES)),$(TEST_MODULES)))


BEAMS       := $(patsubst %,$(ebin)/%.beam,$(MODULES))
TEST_BEAMS  := $(patsubst %,$(test)/%.beam,$(TEST_MODULES))

TARGETS = \
	$(BEAMS) \
	$(ebin)/eterminfo.app


all:	$(TARGETS)

clean:
	$(RM) $(TARGETS)
	$(RM) $(TEST_BEAMS)
	$(RM) -r doc/*.html doc/*.png doc/*.css doc/edoc-info doc/html

test:	all $(TEST_BEAMS) FORCE
	@echo Testing...
	@: echo MODULES = '>>$(MODULES)<<'
	@: echo TEST_MODULES = '>>$(TEST_MODULES)<<'
	@: echo EUNIT_MODULES = '>>$(EUNIT_MODULES)<<'
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(test) -pa $(ebin) -eval " \
	    case eunit:test([$(subst $(space),$(comma),$(EUNIT_MODULES))], \
			    [$(verbose_opt)]) of \
		ok -> halt(0); \
		_  -> halt(1) \
	    end."

doc:
	@echo Generating documentation...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(ebin) -eval " \
	    case edoc:application(eterminfo,\"$(EDOC_DEST_DIR)\",[$(EDOC_OPTS)]) of \
		ok -> halt(0); \
		_  -> halt(1) \
	    end."

doc-check: doc
	$(silencer)tidy -config .tidyrc -q -e doc/eterminfo_*.html 2>&1 | \
	    egrep -v '(trimming empty|inserting implicit) <p>' | \
	    egrep -v '<table> lacks "summary" attribute' | \
	    egrep -v 'Warning.*proprietary attribute.*docgen-(rel|href)' || :

xref: all
	@echo Checking for calls to undefined functions...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -eval " \
	    Res = xref:d(\"$(ebin)\"), \
	    case lists:keyfind(undefined,1,Res) of \
		{undefined,[]}     -> halt(0); \
		{undefined,Undefs} -> io:format(\"~p~n\",[Undefs]), halt(1) \
	    end."

dialyze: all $(plt)
	dialyzer -q --plt $(plt) -r $(ebin)

build_plt: $(plt)

clean_plt:
	$(RM) -f $(plt)

$(plt):
	dialyzer -q --build_plt --output_plt $@ \
		--apps erts kernel stdlib compiler crypto


FORCE:

##
## General default rules for how to compile some files
##
$(ebin)/%.beam: $(src)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) -pa $(ebin) -o $(ebin) $<

$(test)/%.beam: $(test)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) $(EUNIT_ERLC_FLAGS) -pa $(ebin) -o $(test) $<

$(src)/%.erl: $(src)/%.xrl
	$(ERLC) -o $(src) $<

$(ebin):
	mkdir -pv $(ebin)

##
## Some extra dependencies, not covered by default rules above
##


# To compile eterminfo_codegen_tests, we first need the parse transform in eterminfo_codegen
$(test)/eterminfo_codegen_tests.beam: $(ebin)/eterminfo_codegen.beam

# eterminfo_compile_tests.erl includes eterminfo_tests.erl (see the files for details
# on this unorthodox setup), this dependency needs to be recorded
$(test)/eterminfo_compile_tests.beam: $(test)/eterminfo_compile_tests.erl \
				$(test)/eterminfo_tests.erl

# To generate the ebin/eterminfo.app file, process the src/eterminfo.app.src file
$(ebin)/eterminfo.app: $(src)/eterminfo.app.src | $(ebin)
	@echo Generating $@...
	$(silencer)$(ERL) +B -noshell -noinput -eval " \
	    try \
		{ok, [{application,App,KVs1}]} = file:consult(\"$<\"), \
		Vsn2 = case lists:keyfind(vsn,1,KVs1) of \
			   {vsn,{cmd,Cmd}} -> \
				string:strip(os:cmd(Cmd),right,$$\n); \
			   {vsn,Vsn1} -> \
				Vsn1 \
		       end, \
		KVs2 = lists:keystore(vsn,1,KVs1,{vsn,Vsn2}), \
		AppTerm  = {application,App,KVs2}, \
		ok = file:write_file( \
		       \"$@\", \
		       iolist_to_binary( \
		         io_lib:format(\"~p.~n\", [AppTerm]))), \
		halt(0) \
	    catch Class:Reason -> \
		io:format(\"ERROR: {~p,~p}~n\", \
			  [Class,Reason]), \
		halt(1) \
	    end."
