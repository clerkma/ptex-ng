% dvitype.ch for C compilation with web2c.
%
% 04/04/83 (PC)  Merged with Pavel's change file and made to work with the
%                version 1.0 of DVItype released with version 0.95 of TeX in
%                February, 1983.
% 04/18/83 (PC)  Added changes to module 47 so that it would work the same
%                when input was a file (or pipe) as with a terminal.
% 06/29/83 (HWT) Brought up to version 1.1 as released with version 0.99 of
%                TeX, with new change file format
% 07/28/83 (HWT) Brought up to version 2 as released with version 0.999.
%	         Only the banner changes.
% 11/21/83 (HWT) Brought up to version 2.2 as released with version 1.0.
% 02/19/84 (HWT) Made it use the TEXFONTS environment variable.
% 03/23/84 (HWT) Brought up to version 2.3.
% 07/11/84 (HWT) Brought up to version 2.6 as released with version 1.1.
% 11/07/84 (ETM) Brought up to version 2.7 as released with version 1.2.
% 03/09/88 (ETM) Brought up to version 2.9
% 03/16/88 (ETM) Converted for use with WEB to C.
% 11/30/89 (KB)  To version 3.
% 01/16/90 (SR)  To version 3.2.
% (more recent changes in the ChangeLog)

@x [0] l.29
\def\(#1){} % this is used to make section names sort themselves better
@y
\def\({} % this is used to make section names sort themselves better
@z

@x [0] l.32
\def\title{DVI$\,$\lowercase{type}}
@y
\def\title{DVI$\,$\lowercase{type} changes for C}
@z

@x [0] l.50 WEAVE: print changes only.
\pageno=\contentspagenumber \advance\pageno by 1
@y
\pageno=\contentspagenumber \advance\pageno by 1
\let\maybe=\iffalse
@z

@x [1.1] l.90 Define my_name
@d banner=='This is DVItype, Version 3.6' {printed when the program starts}
@y
@d my_name=='dvitype'
@d banner=='This is DVItype, Version 3.6' {printed when the program starts}
@z

% [1.3] Specify the output file to simplify web2c, and don't print the
% banner until later.
@x [1.3] l.126
@d print(#)==write(#)
@d print_ln(#)==write_ln(#)
@y
@d print(#)==write(stdout, #)
@d print_ln(#)==write_ln(stdout, #)
@z

@x [1.3] l.134 kpse initialize
procedure initialize; {this procedure gets things started properly}
  var i:integer; {loop index for initializations}
  begin print_ln(banner);@/
@y
@<Define \(|parse_arguments|@>
procedure initialize; {this procedure gets things started properly}
  var i:integer; {loop index for initializations}
  begin
  kpse_set_program_name (argv[0], my_name);
  parse_arguments;
  print (banner);
  print_ln (version_string);
@z

@x [1.4] l.140 Purge non-local 'goto'.
@ If the program has to stop prematurely, it goes to the
`|final_end|'. Another label, |done|, is used when stopping normally.

@d final_end=9999 {label for the end of it all}
@d done=30 {go here when finished with a subtask}

@<Labels...@>=final_end,done;
@y
@ Label |done| is used when stopping normally.

@d done=30 {go here when finished with a subtask}

@<Labels...@>=done;
@z

% There have been no complaints that these values are too small, and adjusting
% them dynamically requires resizing a large number of arrays, so
% doesn't seem worth the effort.
@x [1.5] l.152 More fonts, more widths, no max filename length, no dialog.
@!max_fonts=100; {maximum number of distinct fonts per \.{DVI} file}
@!max_widths=10000; {maximum number of different characters among all fonts}
@y
@!max_fonts=500; {maximum number of distinct fonts per \.{DVI} file}
@!max_widths=25000; {maximum number of different characters among all fonts}
@z

@x [1.5] l.155
@!terminal_line_length=150; {maximum number of characters input in a single
  line of input from the terminal}
@y
@z

@x [1.5] l.158
@!name_size=1000; {total length of all font file names}
@!name_length=50; {a file name shouldn't be longer than this}
@y
@!name_size=10000; {total length of all font file names}
@z

@x [1.7] l.172 Remove non-local goto.
so a procedure called |jump_out| has been introduced. This procedure, which
simply transfers control to the label |final_end| at the end of the program,
contains the only non-local |goto| statement in \.{DVItype}.
@y
so a procedure called |jump_out| has been introduced. We avoid the
non-local |goto| in the original.
@z

@x [1.7] l.177
@d abort(#)==begin print(' ',#); jump_out;
    end
@d bad_dvi(#)==abort('Bad DVI file: ',#,'!')
@.Bad DVI file@>

@p procedure jump_out;
begin goto final_end;
end;
@y
@d jump_out==uexit(1)
@d abort(#)==begin write_ln(stderr,#); jump_out; end
@d bad_dvi(#)==abort('Bad DVI file: ',#,'!')
@.Bad DVI file@>
@z

@x [2.8] l.201 Permissive input.
@!ASCII_code=" ".."~"; {a subrange of the integers}
@y
@!ASCII_code=0..255; {a subrange of the integers}
@z

% [2.9] The text_char type is used as an array index into `xord'.  The
% default type `char' produces signed integers, which are bad array
% indices in C.
@x [2.9] l.221
@d text_char == char {the data type of characters in text files}
@d first_text_char=0 {ordinal number of the smallest element of |text_char|}
@d last_text_char=127 {ordinal number of the largest element of |text_char|}
@y
@d text_char == ASCII_code {the data type of characters in text files}
@d first_text_char=0 {ordinal number of the smallest element of |text_char|}
@d last_text_char=255 {ordinal number of the largest element of |text_char|}
@z

@x [4.23] l.877 Fix up opening the files.
@p procedure open_dvi_file; {prepares to read packed bytes in |dvi_file|}
begin reset(dvi_file);
cur_loc:=0;
end;
@#
procedure open_tfm_file; {prepares to read packed bytes in |tfm_file|}
begin reset(tfm_file,cur_name);
end;
@y
@p procedure open_dvi_file; {prepares to read packed bytes in |dvi_file|}
begin
  resetbin (dvi_file, dvi_name);
  cur_loc := 0;
end;
@#
procedure open_tfm_file; {prepares to read packed bytes in |tfm_file|}
var full_name: ^char;
begin
  full_name := kpse_find_tfm (cur_name);
  if full_name then begin
    tfm_file := fopen (full_name, FOPEN_RBIN_MODE);
  end else begin
    tfm_file := nil;
  end;
end;
@z

@x [4.24] l.894 No arbitrary limit on filename length.
@!cur_name:packed array[1..name_length] of char; {external name,
  with no lower case letters}
@y
@!cur_name:^char; {external name}
@z

@x [4.28] l.987 dvi_length and move_to_byte.
@p function dvi_length:integer;
begin set_pos(dvi_file,-1); dvi_length:=cur_pos(dvi_file);
end;
@#
procedure move_to_byte(n:integer);
begin set_pos(dvi_file,n); cur_loc:=n;
end;
@y
@p function dvi_length:integer;
begin
  xfseek (dvi_file, 0, 2, dvi_name);
  cur_loc := xftell(dvi_file, dvi_name);
  dvi_length := cur_loc;
end;
@#
procedure move_to_byte(n:integer);
begin
  xfseek (dvi_file, n, 0, dvi_name);
  cur_loc:=n;
end;
@z

@x [6.42] l.1300 [42-43] Initialize optional variables sooner.
@!count:array[0..9] of integer; {the count values on the current page}

@ @<Set init...@>=
out_mode:=the_works; max_pages:=1000000; start_vals:=0; start_there[0]:=false;
@y
@!count:array[0..9] of integer; {the count values on the current page}

@ Initializations are done sooner now.
@z

@x [6.45] l.1317 No dialog.
@ The |input_ln| routine waits for the user to type a line at his or her
terminal; then it puts ASCII-code equivalents for the characters on that line
into the |buffer| array. The |term_in| file is used for terminal input,
and |term_out| for terminal output.
@^system dependencies@>

@<Glob...@>=
@!buffer:array[0..terminal_line_length] of ASCII_code;
@!term_in:text_file; {the terminal, considered as an input file}
@!term_out:text_file; {the terminal, considered as an output file}
@y
@ No dialog.
@z

@x [6.47] l.1343 No input_ln.
@p procedure input_ln; {inputs a line from the terminal}
var k:0..terminal_line_length;
begin update_terminal; reset(term_in);
if eoln(term_in) then read_ln(term_in);
k:=0;
while (k<terminal_line_length)and not eoln(term_in) do
  begin buffer[k]:=xord[term_in^]; incr(k); get(term_in);
  end;
buffer[k]:=" ";
end;
@y
@z

@x [6.48] l.1354 No dialog.
@ The global variable |buf_ptr| is used while scanning each line of input;
it points to the first unread character in |buffer|.

@<Glob...@>=
@!buf_ptr:0..terminal_line_length; {the number of characters read}
@y
@ No dialog.
@z

@x [6.49] l.1360 No dialog.
@ Here is a routine that scans a (possibly signed) integer and computes
the decimal value. If no decimal integer starts at |buf_ptr|, the
value 0 is returned. The integer should be less than $2^{31}$ in
absolute value.

@p function get_integer:integer;
var x:integer; {accumulates the value}
@!negative:boolean; {should the value be negated?}
begin if buffer[buf_ptr]="-" then
  begin negative:=true; incr(buf_ptr);
  end
else negative:=false;
x:=0;
while (buffer[buf_ptr]>="0")and(buffer[buf_ptr]<="9") do
  begin x:=10*x+buffer[buf_ptr]-"0"; incr(buf_ptr);
  end;
if negative then get_integer:=-x @+ else get_integer:=x;
end;

@y
@ No dialog.
@z

@x [6.50] l.1379 [50-55] No dialog.
@ The selected options are put into global variables by the |dialog|
procedure, which is called just as \.{DVItype} begins.
@^system dependencies@>

@p procedure dialog;
label 1,2,3,4,5;
var k:integer; {loop variable}
begin rewrite(term_out); {prepare the terminal for output}
write_ln(term_out,banner);
@<Determine the desired |out_mode|@>;
@<Determine the desired |start_count| values@>;
@<Determine the desired |max_pages|@>;
@<Determine the desired |resolution|@>;
@<Determine the desired |new_mag|@>;
@<Print all the selected options@>;
end;

@ @<Determine the desired |out_mode|@>=
1: write(term_out,'Output level (default=4, ? for help): ');
out_mode:=the_works; input_ln;
if buffer[0]<>" " then
  if (buffer[0]>="0")and(buffer[0]<="4") then out_mode:=buffer[0]-"0"
  else  begin write(term_out,'Type 4 for complete listing,');
    write(term_out,' 0 for errors and fonts only,');
    write_ln(term_out,' 1 or 2 or 3 for something in between.');
    goto 1;
    end

@ @<Determine the desired |start...@>=
2: write(term_out,'Starting page (default=*): ');
start_vals:=0; start_there[0]:=false;
input_ln; buf_ptr:=0; k:=0;
if buffer[0]<>" " then
  repeat if buffer[buf_ptr]="*" then
    begin start_there[k]:=false; incr(buf_ptr);
    end
  else  begin start_there[k]:=true; start_count[k]:=get_integer;
    end;
  if (k<9)and(buffer[buf_ptr]=".") then
    begin incr(k); incr(buf_ptr);
    end
  else if buffer[buf_ptr]=" " then start_vals:=k
  else  begin write(term_out,'Type, e.g., 1.*.-5 to specify the ');
    write_ln(term_out,'first page with \count0=1, \count2=-5.');
    goto 2;
    end;
  until start_vals=k

@ @<Determine the desired |max_pages|@>=
3: write(term_out,'Maximum number of pages (default=1000000): ');
max_pages:=1000000; input_ln; buf_ptr:=0;
if buffer[0]<>" " then
  begin max_pages:=get_integer;
  if max_pages<=0 then
    begin write_ln(term_out,'Please type a positive number.');
    goto 3;
    end;
  end

@ @<Determine the desired |resolution|@>=
4: write(term_out,'Assumed device resolution');
write(term_out,' in pixels per inch (default=300/1): ');
resolution:=300.0; input_ln; buf_ptr:=0;
if buffer[0]<>" " then
  begin k:=get_integer;
  if (k>0)and(buffer[buf_ptr]="/")and
    (buffer[buf_ptr+1]>"0")and(buffer[buf_ptr+1]<="9") then
    begin incr(buf_ptr); resolution:=k/get_integer;
    end
  else  begin write(term_out,'Type a ratio of positive integers;');
    write_ln(term_out,' (1 pixel per mm would be 254/10).');
    goto 4;
    end;
  end

@ @<Determine the desired |new_mag|@>=
5: write(term_out,'New magnification (default=0 to keep the old one): ');
new_mag:=0; input_ln; buf_ptr:=0;
if buffer[0]<>" " then
  if (buffer[0]>="0")and(buffer[0]<="9") then new_mag:=get_integer
  else  begin write(term_out,'Type a positive integer to override ');
    write_ln(term_out,'the magnification in the DVI file.');
    goto 5;
    end
@y
@ No dialog (50).
@ No dialog (51).
@ No dialog (52).
@ No dialog (53).
@ No dialog (54).
@ No dialog (55).
@z

@x [6.56] l.1489 Fix printing of floating point number.
print_ln('  Resolution = ',resolution:12:8,' pixels per inch');
if new_mag>0 then print_ln('  New magnification factor = ',new_mag/1000:8:3)
@y
print ('  Resolution = ');
print_real (resolution, 12, 8);
print_ln (' pixels per inch');
if new_mag > 0
then begin
  print ('  New magnification factor = ');
  print_real (new_mag / 1000.0, 8, 3);
  print_ln('')
end
@z

@x [7.59] l.1518 We use r for something else.
@!r:0..name_length; {index into |cur_name|}
@y
@!r:0..name_size; {current filename length}
@z

@x [7.62] l.1597 <Load the new font...> close the file when we're done
if out_mode=errors_only then print_ln(' ');
@y
if out_mode=errors_only then print_ln(' ');
if tfm_file then
  xfclose (tfm_file, cur_name); {should be the |kpse_find_tfm| result}
free (cur_name); {We |xmalloc|'d this before we got called.}
@z

@x [7.64] l.1631 Don't set default_directory_name.
@d default_directory_name=='TeXfonts:' {change this to the correct name}
@d default_directory_name_length=9 {change this to the correct length}

@<Glob...@>=
@!default_directory:packed array[1..default_directory_name_length] of char;
@y
In Web2C, users have a path searched for fonts, there's no single
default directory.
@z

@x [7.65] l.1637 Remove initialization of default_directory.
@ @<Set init...@>=
default_directory:=default_directory_name;
@y
@ (No initialization needs to be done.  Keep this module to preserve
numbering.)
@z

@x [7.66] l.1640 Don't append `.tfm' here, and keep lowercase.
@ The string |cur_name| is supposed to be set to the external name of the
\.{TFM} file for the current font. This usually means that we need to
prepend the name of the default directory, and
to append the suffix `\.{.TFM}'. Furthermore, we change lower case letters
to upper case, since |cur_name| is a \PASCAL\ string.
@^system dependencies@>

@<Move font name into the |cur_name| string@>=
for k:=1 to name_length do cur_name[k]:=' ';
if p=0 then
  begin for k:=1 to default_directory_name_length do
    cur_name[k]:=default_directory[k];
  r:=default_directory_name_length;
  end
else r:=0;
for k:=font_name[nf] to font_name[nf+1]-1 do
  begin incr(r);
  if r+4>name_length then
    abort('DVItype capacity exceeded (max font name length=',
      name_length:1,')!');
@.DVItype capacity exceeded...@>
  if (names[k]>="a")and(names[k]<="z") then
      cur_name[r]:=xchr[names[k]-@'40]
  else cur_name[r]:=xchr[names[k]];
  end;
cur_name[r+1]:='.'; cur_name[r+2]:='T'; cur_name[r+3]:='F'; cur_name[r+4]:='M'
@y
@ The string |cur_name| is supposed to be set to the external name of the
\.{TFM} file for the current font.  We do not impose a maximum limit
here.  It's too bad there is a limit on the total length of all
filenames, but it doesn't seem worth reprogramming all that.
@^system dependencies@>

@d name_start == font_name[nf]
@d name_end == font_name[nf+1]

@<Move font name into the |cur_name| string@>=
r := name_end - name_start;
cur_name := xmalloc_array (char, r);
{|strncpy| might be faster, but it's probably a good idea to keep the
 |xchr| translation.}
for k := name_start to name_end do begin
  cur_name[k - name_start] := xchr[names[k]];
end;
cur_name[r] := 0; {Append null byte for C.}
@z

@x [9.75] l.1775 Diagnose impossible cases.
sixty_four_cases(fnt_num_0): first_par:=o-fnt_num_0;
end;
@y
sixty_four_cases(fnt_num_0): first_par:=o-fnt_num_0;
othercases abort('internal error');
endcases;
@z

@x [9.80] l.1847 (major,minor) optionally show opcode
@d show(#)==begin flush_text; showing:=true; print(a:1,': ',#);
  end
@d major(#)==if out_mode>errors_only then show(#)
@d minor(#)==if out_mode>terse then
  begin showing:=true; print(a:1,': ',#);
@y
@d show(#)==begin flush_text; showing:=true; print(a:1,': ',#);
  if show_opcodes and (o >= 128) then print (' {', o:1, '}');
  end
@d major(#)==if out_mode>errors_only then show(#)
@d minor(#)==if out_mode>terse then
  begin showing:=true; print(a:1,': ',#);
  if show_opcodes and (o >= 128) then print (' {', o:1, '}');
@z

% skip_pages calls first_par, which can (uselessly) access w,x,y,z.
% https://tug.org/texmfbug/newbug.html#dvitypeinitglobal
@x [10.95] l.2171
@!down_the_drain:integer; {garbage}
begin showing:=false;
@y
@!down_the_drain:integer; {garbage}
begin
w:=0; x:=0; y:=0; z:=0; {|first_par| fetches these}
showing:=false;
@z

% show full fnt_def command after preamble but before bop, or between eop/bop.
% https://tug.org/texmfbug/newbug.html#dvitypefontoutput
@x [10.99] l.2222
procedure scan_bop;
var k:0..255; {command code}
begin repeat if eof(dvi_file) then bad_dvi('the file ended prematurely');
@.the file ended prematurely@>
  k:=get_byte;
  if (k>=fnt_def1)and(k<fnt_def1+4) then
    begin define_font(first_par(k)); k:=nop;
    end;
@y
procedure scan_bop;
var k:0..255; {command code}
@!a:integer; {byte number of the current command}
@!p:integer; {parameter of the current command}
@!o:0..255;  {copy of command code}
begin repeat if eof(dvi_file) then bad_dvi('the file ended prematurely');
@.the file ended prematurely@>
  a:=cur_loc;
  k:=get_byte;
  if (k>=fnt_def1)and(k<fnt_def1+4) then
    begin
    p:=first_par(k);
    {Web2C's |dvitype| provides a |-show-opcodes| option, the output
     from which requires that the opcode be in the variable |o|,
     as it is elsewhere. So duplicate the value. Sorry.}
    o:=k;
    if max_pages>0 then
      major('fntdef',k-fnt_def1+1:1,' ',p:1);
    define_font(p);
    if out_mode<>errors_only then print_ln(' ');
    k:=nop;
    end;
@z

@x [13.107] l.2404 (main) No dialog; remove unused label.
dialog; {set up all the options}
@y
@<Print all the selected options@>;
@z

@x [13.107] l.2421
final_end:end.
@y
end.
@z

@x [13.110] l.2469 Fix another floating point print.
print_ln('magnification=',mag:1,'; ',conv:16:8,' pixels per DVI unit')
@y
print ('magnification=', mag:1, '; ');
print_real (conv, 16, 8);
print_ln (' pixels per DVI unit')
@z

@x [14.112] l.2495 System-dependent changes.
This section should be replaced, if necessary, by changes to the program
that are necessary to make \.{DVItype} work at a particular installation.
It is usually best to design your change file so that all changes to
previous sections preserve the section numbering; then everybody's version
will be consistent with the printed program. More extensive changes,
which introduce new sections, can be inserted here; then only the index
itself will get a new section number.
@^system dependencies@>
@y
Parse a Unix-style command line.

@d argument_is (#) == (strcmp (long_options[option_index].name, #) = 0)

@<Define \(|parse_arguments|@> =
procedure parse_arguments;
const n_options = 8; {Pascal won't count array lengths for us.}
var @!long_options: array[0..n_options] of getopt_struct;
    @!getopt_return_val: integer;
    @!option_index: c_int_type;
    @!current_option: 0..n_options;
    @!end_num:^char; {for \.{page-start}}
begin
  @<Define the option table@>;
  repeat
    getopt_return_val := getopt_long_only (argc, argv, '', long_options,
                                           address_of (option_index));
    if getopt_return_val = -1 then begin
      do_nothing; {End of arguments; we exit the loop below.}

    end else if getopt_return_val = "?" then begin
      usage (my_name);

    end else if argument_is ('help') then begin
      usage_help (DVITYPE_HELP, nil);

    end else if argument_is ('version') then begin
      print_version_and_exit (banner, nil, 'D.E. Knuth', nil);

    end else if argument_is ('output-level') then begin
      if (optarg[0] < '0') or (optarg[0] > '4') or (optarg[1] <> 0) then begin
        write_ln (stderr, 'Value for --output-level must be >= 0 and <= 4.');
        uexit (1);
      end;
      out_mode := optarg[0] - '0';

    end else if argument_is ('page-start') then begin
      @<Determine the desired |start_count| values from |optarg|@>;

    end else if argument_is ('max-pages') then begin
      max_pages := atou (optarg);

    end else if argument_is ('dpi') then begin
      resolution := atof (optarg);

    end else if argument_is ('magnification') then begin
      new_mag := atou (optarg);

    end; {Else it was a flag; |getopt| has already done the assignment.}
  until getopt_return_val = -1;

  {Now |optind| is the index of first non-option on the command line.}
  if (optind + 1 <> argc) then begin
    write_ln (stderr, my_name, ': Need exactly one file argument.');
    usage (my_name);
  end;
  dvi_name := extend_filename (cmdline (optind), 'dvi');
end;

@ Here are the options we allow.  The first is one of the standard GNU options.
@.-help@>

@<Define the option...@> =
current_option := 0;
long_options[current_option].name := 'help';
long_options[current_option].has_arg := 0;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ Another of the standard options.
@.-version@>

@<Define the option...@> =
long_options[current_option].name := 'version';
long_options[current_option].has_arg := 0;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ How verbose to be.
@.-output-level@>

@<Define the option...@> =
long_options[current_option].name := 'output-level';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);
out_mode := the_works; {default}

@ What page to start at.
@.-page-start@>

@<Define the option...@> =
long_options[current_option].name := 'page-start';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ Parsing the starting page specification is a bit complicated.

@<Determine the desired |start_count|...@> =
k := 0; {which \.{\\count} register we're on}
m := 0; {position in |optarg|}
while optarg[m] do begin
  if optarg[m] = "*" then begin
    start_there[k] := false;
    incr (m);

  end else if optarg[m] = "." then begin
    incr (k);
    if k >= 10 then begin
      write_ln (stderr, my_name, ': More than ten count registers specified.');
      uexit (1);
    end;
    incr (m);

  end else begin
    start_count[k] := strtol (optarg + m, address_of (end_num), 10);
    if end_num = optarg + m then begin
      write_ln (stderr, my_name, ': -page-start values must be numeric or *.');
      uexit (1);
    end;
    start_there[k] := true;
    m := m + end_num - (optarg + m);
  end;
end;
start_vals := k;

@ How many pages to do.
@.-max-pages@>

@<Define the option...@> =
long_options[current_option].name := 'max-pages';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);
max_pages := 1000000; {default}

@ Resolution, in pixels per inch.
@.-dpi@>

@<Define the option...@> =
long_options[current_option].name := 'dpi';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);
resolution := 300.0; {default}

@ Magnification to apply.
@.-magnification@>

@<Define the option...@> =
long_options[current_option].name := 'magnification';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);
new_mag := 0; {default is to keep the old one}

@ Whether to show numeric opcodes.
@.-show-opcodes@>

@<Define the option...@> =
long_options[current_option].name := 'show-opcodes';
long_options[current_option].has_arg := 0;
long_options[current_option].flag := address_of (show_opcodes);
long_options[current_option].val := 1;
incr (current_option);

@ @<Glob...@> =
@!show_opcodes: c_int_type;

@ An element with all zeros always ends the list.

@<Define the option...@> =
long_options[current_option].name := 0;
long_options[current_option].has_arg := 0;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;

@ Global filenames.

@<Global...@> =
@!dvi_name:const_c_string;
@z
