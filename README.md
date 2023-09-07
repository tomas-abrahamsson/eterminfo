The eterminfo is a terminfo handler for Erlang.

Features of eterminfo
---------------------

* Reads terminfo descriptions, either using the infocmp program,
  or from compiled terminfo files. There is an an option to use
  long names.

* Produces a map, that holds the parsed terminfo capabilities.

Example usage
-------------
```
  > ok = eterminfo:get_term_type().
  "vt100"
  > eterminfo:install_by_file().
  ok

  %% Reading a string capability: what string to send to produce a beep
  > eterminfo:tigetstr(bell).
  [7]

  %% Reading a numeric capability: tabs are initially 8 spaces wide
  > eterminfo:tigetnum(init_tabs).
  8

  %% Reading a bool capability
  > eterminfo:tigetflag(xon_xoff).
  true

  %% Getting a parameterized capability.
  %% The parm_down_cursor capability takes one parameter, number of lines.
  > eterminfo:tparm(parm_down_cursor, 10).
  "\e[10B"

  %% Reading the name of the terminfo.
  > eterminfo:tigetstr('$terminfo_names').
  ["vt100","vt100-am","DEC VT100 (w/advanced video)"]

  %% Capability string can have padding---ie delays (in milliseconds)---at some places.
  %% The padding may also be proportional to the number of lines.
  %% See terminfo(5) for more info.
  %% See eterminof:tputs/1,2 for outputting a string with paddings.
  > eterminfo:tparm(cursor_address, 10, 10).
  [27,91,49,49,59,49,49,72,
   {pad,#{delay => 5,mandatory => false,proportional => false}}]

  %% Reading all installed capabilities as a map.
  %% For each parametersized capability, the string representation is also
  %% present in the '$str_literals' map
  > eterminfo:get_installed_terminfo().
  #{key_right => "\eOC",
    enter_am_mode => "\e[?7h","carriage_return" => "\r",
    exit_standout_mode =>
        [27,91,109,
         {pad,#{delay => 2,mandatory => false,proportional => false}}],
    key_left => "\eOD",
    parm_left_cursor => #Fun<...>,
    ...
    '$str_literals' => #{..., parm_left_cursor => "\\E[%p1%dD"", ...},
    ...}
```

Some properties
---------------
* For boolean capabilities, only those that are set are included
  in the map. That is, the map never contains any associations on
  the form `capability_name() => false`.

* The terminfo capability names are known to not be compatible with
  HP-UX, AIX, and OSF/1, as mentioned on the
  [term(5)](https://man7.org/linux/man-pages/man5/term.5.html) and
  [terminfo(5)](https://man7.org/linux/man-pages/man5/terminfo.5.html)
  man pages.

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
