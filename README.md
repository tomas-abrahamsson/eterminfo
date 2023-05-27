The eterminfo is a terminfo handler for Erlang.

Features of eterminfo
---------------------

* Reads terminfo descriptions using the infocmp program,
  with an option to read long names

* Produces a dict, that holds the parsed terminfo capabilities.

Example usage
-------------
```
  > {ok,M} = eterminfo:setup_by_infocmp("vt100", [long_names]).
  #{...}

  > maps:get(terminfo_id, M).
  ["vt100","vt100-am","dec vt100 (w/advanced video)"]p

  %% Reading a bool capability
  > maps:get("xon_xoff", M).
  true

  %% Reading a numeric capability: tabs are initially 8 spaces wide
  > maps:get("init_tabs", M),
  8

  %% Reading a string capability: what string to send to produce a beep
  > maps:get("bell", M),
  [7]

  %% Reading parameterized strings:
  %% We use the "parm_down_cursor" as an example. This moves the cursor down
  %% a number of lines.
  %% The second parameter is a map of static vars on the form  #{"A" => 10}
  %% The static parameter defaults to #{} in the eterminfo:tparm functions.
  %% The number of parameters varies dependending on capability. See
  %% terminfo(5) for further information.
  > F = maps:get("parm_down_cursor", M).
  > F(2, #{}).
  "\e[2B"      % \e means: escape, ascii 27
  > eterminfo:tparm(M, "parm_down_cursor", 2).
  "\e[2B"

  %% String parameters can also contain padding---delays in milliseconds,
  %% for example when addressing the cursor.
  > Fc = maps:get("cursor_address", M).
  #Fun<eterminfo_strparser.5.61294760>
  %% The {pad,{5,false,false}} below means:
  %%  - 5 milliseconds delay is needed
  %%  - First boolean: whether or not padding is not proportional
  %%    to the number of lines affected by the operation (according
  %%    to the terminfo man page)
  %%  - Second boolean: whether or not padding is mandatory,
  %%    also with xon/xoff flow control
  %%    (according to the terminfo man page)
  > Fc(3, 12, #{}).
  [27,91,52,59,49,51,72,
   {pad,#{delay => 5,mandatory => false,proportional => false}}]
```


Version numbering
-----------------

The version number is fetched from the git latest git tag
matching N.M where N and M are integers.  This version is
inserted into the eterminfo.app file.
The version is the result of the command

  git describe --always --tags --match '[0-9]*.[0-9]*'

Places to update when making a new version:
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
