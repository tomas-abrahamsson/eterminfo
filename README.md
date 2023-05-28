The eterminfo is a terminfo handler for Erlang.

Features of eterminfo
---------------------

* Reads terminfo descriptions using the infocmp program,
  with an option to read long names

* Produces a dict, that holds the parsed terminfo capabilities.

Example usage
-------------
```
  > ok = eterminfo:get_term_type().
  "vt100"
  > eterminfo:install_by_infocmp().
  ok

  %% Reading a string capability: what string to send to produce a beep
  > eterminfo:tigetstr("bell").
  [7]

  %% Reading a numeric capability: tabs are initially 8 spaces wide
  > eterminfo:tigetnum("init_tabs").
  8

  %% Reading a bool capability
  > eterminfo:tigetflag("xon_xoff").
  true

  %% Getting a parameterized capability.
  %% The parm_down_cursor capability takes one parameter, number of lines.
  > eterminfo:tparm("parm_down_cursor", 10).
  "\e[10B"

  %% Reading the name of the terminfo.
  > eterminfo:tigetstr(terminfo_id).
  ["vt100","vt100-am","DEC VT100 (w/advanced video)"]

  %% Capability string can have padding---delays in milliseconds---at some places.
  %% If the padding may be proportional to the number of lines.
  %% See terminfo(5) for more info.
  > eterminfo:tparm("cursor_address", 10, 10).
  [27,91,49,49,59,49,49,72,
   {pad,#{delay => 5,mandatory => false,proportional => false}}]

  %% Reading all installed capabilities as a map.
  %% For each parametersized capability, there is also a
  %% `{literal,Capability}` entry.
  > eterminfo:get_installed_terminfo().
  #{"key_right" => "\eOC",
    "enter_am_mode" => "\e[?7h","carriage_return" => "\r",
    "exit_standout_mode" =>
        [27,91,109,
         {pad,#{delay => 2,mandatory => false,proportional => false}}],
    "key_left" => "\eOD",
    "parm_left_cursor" => #Fun<eterminfo_strparser.8.19824310>,
    ...
    {literal,"parm_left_cursor"} => "\\E[%p1%dD"",
    ...}
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
