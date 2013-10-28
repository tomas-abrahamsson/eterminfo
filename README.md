The eterminfo is a terminfo handler for Erlang.

Features of eterminfo
---------------------

* Reads terminfo descriptions using the infocmp program,
  with an option to read long names

* Produces a dict, that holds the parsed terminfo capabilities.

Example usage
-------------

  > {ok,D} = eterminfo:setup_by_infocmp("vt100", [long_names]).

  > dict:fetch(terminfo_id, D).
  ["vt100","vt100-am","dec vt100 (w/advanced video)"]p

  %% Reading a bool capability
  > dict:fetch("xon_xoff", D).
  true

  %% Reading a numeric capability: tabs are initially 8 spaces wide
  > dict:fetch("init_tabs", D),
  8

  %% Reading a string capability: what string to send to produce a beep
  > dict:fetch("bell", D),
  [7]

  %% Reading parameterized strings:

  %% Move the cursor down a number of lines
  > Dn = dict:fetch("parm_down_cursor", D).
  #Fun<eterminfo_strparser.2.61294760>
  > Dn(2, []).
  "\e[2B"   %% \e means: escape, ascii 27

  %% Addressing the cursor
  > F = dict:fetch("cursor_address", D).
  #Fun<eterminfo_strparser.5.61294760>
  %% The {pad,{5,false,false}} below means:
  %%  - 5 milliseconds delay is needed
  %%  - First boolean: whether or not padding is not proportional
  %%    to the number of lines affected by the operation (according
  %%    to the terminfo man page)
  %%  - Second boolean: whether or not padding is mandatory,
  %%    also with xon/xoff flow control
  %%    (according to the terminfo man page)
  > F(3, 12, []).
  [27,91,52,59,49,51,72,{pad,{5,false,false}}]
 


Version numbering
-----------------

The gpb version number is fetched from the git latest git tag
matching N.M where N and M are integers.  This version is
inserted into the gpb.app file as well as into the
include/gpb_version.hrl.  The version is the result of the command

  git describe --always --tags --match '[0-9]*.[0-9]*'

Thus, so create a new version of gpb, the single source from where
this version is fetched, is the git tag.   (If you are importing
gpb into another version control system than git, or using another
build tool than rebar, you might have to adapt rebar.config and
src/gpb.app.src accordingly.)

Places to update when making a new version:
* Write about the changes in the ChangeLog file
* tag it in git


Contributing
------------

Contributions are welcome, preferrably as pull requests or git patches
or git fetch requests.  Here are some guide lines:

* Use only spaces for indentation, no tabs
* The code must fit 80 columns
* Verify that the code and documentation compiles and that tests are ok:
  rebar clean compile eunit doc xref
* If you add a feature, test cases are most welcome,
  so that the feature won't get lost in any future refactorization
