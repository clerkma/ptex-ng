% dvicopy.ch for C compilation with web2c.
% The original version of this file was created by Monika Jayme and
% Klaus Guntermann at TH Darmstadt (THD), FR Germany.
% Some parts are borrowed from the changes to dvitype, vftovp and vptovf.
%
%  July 90   THD  First versions for dvicopy 0.91 and 0.92
%  Aug 09 90 THD  Updated to dvicopy 1.0 and released
%  Mar 20 91 THD  Updated to dvicopy 1.2
% (more recent changes in the ChangeLog)

@x [0] l.60 WEAVE: print changes only
\pageno=\contentspagenumber \advance\pageno by 1
@y
\pageno=\contentspagenumber \advance\pageno by 1
\let\maybe=\iffalse
@z

@x [1] l.98 Define my_name
@d banner=='This is DVIcopy, Version 1.6' {printed when the program starts}
@y
@d my_name=='dvicopy'
@d banner=='This is DVIcopy, Version 1.6' {printed when the program starts}
@z

@x [2] l.124 No random reading on stdin, may be not seekable.
@d random_reading==true {should we skip around in the file?}
@y
@<Globals in the outer block@>=
@!random_reading:boolean; {should we skip around in the file?}
@z

@x [3] l.135 Purge non-local 'goto'.
calls the `|jump_out|' procedure, which goes to the label |final_end|.

@d final_end = 9999 {go here to wrap it up}
@y
calls the `|jump_out|' procedure.
@z
@x [3] l.141
label final_end;
@y
@z

@x [3] l.146 Set up kpathsea.
procedure initialize; {this procedure gets things started properly}
  var @<Local variables for initialization@>@/
  begin print_ln(banner);@/
@y
@<Define \(|parse_arguments|@>
procedure initialize; {this procedure gets things started properly}
  var @<Local variables for initialization@>@/
  begin
    kpse_set_program_name (argv[0], my_name);
    parse_arguments;
    print (banner); print_ln (version_string);
@z

@x [5] l.178 Big enough arrays to convert the dvilj sample font document.
@<Constants...@>=
@!max_fonts=100; {maximum number of distinct fonts}
@!max_chars=10000; {maximum number of different characters among all fonts}
@!max_widths=3000; {maximum number of different characters widths}
@!max_packets=5000; {maximum number of different characters packets;
  must be less than 65536}
@!max_bytes=30000; {maximum number of bytes for characters packets}
@!max_recursion=10; {\.{VF} files shouldn't recurse beyond this level}
@!stack_size=100; {\.{DVI} files shouldn't |push| beyond this depth}
@!terminal_line_length=150; {maximum number of characters input in a single
  line of input from the terminal}
@!name_length=50; {a file name shouldn't be longer than this}
@y
@<Constants...@>=
@!max_fonts=400; {maximum number of distinct fonts}
@!max_chars=750000; {maximum number of different characters among all fonts}
@!max_widths=16000; {maximum number of different characters widths}
@!max_packets=65530; {maximum number of different characters packets;
  must be less than 65536}
@!max_bytes=250000; {maximum number of bytes for characters packets}
@!max_recursion=10; {\.{VF} files shouldn't recurse beyond this level}
@!stack_size=100; {\.{DVI} files shouldn't |push| beyond this depth}
@!terminal_line_length=256; {maximum number of characters input in a single
  line of input from the terminal}
@z

% [7] Rename the integer types, as they collide with names used by C99.
% Rather than change the code all over the place, we use macros to do
% the renaming.  This could also be done at C preprocessor level.
@x [7] l.245
@d int_32 == integer {signed 32~bit integers}
@y
@d int_32 == integer {signed 32~bit integers}
@d int_31 == int_31_t
@d int_24u == int_24u_t
@d int_24 == int_24_t
@d int_23 == int_23_t
@d int_16u == int_16u_t
@d int_16 == int_16_t
@d int_15 == int_15_t
@d int_8u == int_8u_t
@d int_8 == int_8_t
@d int_7 == int_7_t
@z

% [11] Redirect output, so it can go to either stdout or stderr,
% depending on where the output dvi file is going.
@x [11] l.311
@d print(#)==write(output,#)
@d print_ln(#)==write_ln(output,#)
@d new_line==write_ln(output) {start new line}
@y
@d print(#)==write(term_out,#)
@d print_ln(#)==write_ln(term_out,#)
@d new_line==write_ln(term_out) {start new line}
@z

@x [14] l.380 Permissive input.
@!ASCII_code=" ".."~"; {a subrange of the integers}
@y
@!ASCII_code=0..255; {a subrange of the integers}
@z

% [15] The text_char type is used as an array index into xord.  The
% default type `char' produces signed integers, which are bad array
% indices in C.
@x [15] l.400
@d text_char == char {the data type of characters in text files}
@d first_text_char=0 {ordinal number of the smallest element of |text_char|}
@d last_text_char=127 {ordinal number of the largest element of |text_char|}
@y
@d text_char == ASCII_code {the data type of characters in text files}
@d first_text_char=0 {ordinal number of the smallest element of |text_char|}
@d last_text_char=255 {ordinal number of the largest element of |text_char|}
@z

@x [23] l.573 Remove non-local goto, declare jump_out as noreturn
so a procedure called |jump_out| has been introduced. This procedure, which
transfers control to the label |final_end| at the end of the program,
contains the only non-local |@!goto| statement in \.{\title}.
@^system dependencies@>
Some \PASCAL\ compilers do not implement non-local |goto| statements. In
such cases the |goto final_end| in |jump_out| should simply be replaced
by a call on some system procedure that quietly terminates the program.
@y
so a procedure called |jump_out| has been introduced.
@z
@x [23] l.582
@d abort(#)==begin print_ln(' ',#,'.'); jump_out;
    end

@<Error handling...@>=
@<Basic printing procedures@>@;
procedure close_files_and_terminate; forward;
@#
procedure jump_out;
begin mark_fatal; close_files_and_terminate;
goto final_end;
end;
@y
@f noreturn==procedure
@d abort(#)==begin write_ln(stderr, ' ',#,'.'); jump_out;
    end

@<Error handling...@>=
@<Basic printing procedures@>@;
procedure close_files_and_terminate; forward;
@#
noreturn procedure jump_out;
begin mark_fatal; close_files_and_terminate;
uexit(1);
end;
@z

@x [24] l.597 Declare confusion as noreturn
says |confusion(|indication of where we are|)|.
@y
says |confusion|(indication of where we are).
@z
@x [24] l.600
procedure confusion(@!p:pckt_pointer);
@y
noreturn procedure confusion(@!p:pckt_pointer);
@z

@x [25] l.609 Declare overflow as noreturn
procedure overflow(@!p:pckt_pointer;@!n:int_16u);
@y
noreturn procedure overflow(@!p:pckt_pointer;@!n:int_16u);
@z

@x [62] l.1236 cur_name is no longer a fixed-size array.
@!cur_name:packed array[1..name_length] of char; {external name,
  with no lower case letters}
@y
@!cur_name:^char;
@z

@x [63] l.1241 Ignore arguments to make_font_name.
@ For \.{TFM} and \.{VF} files we just append the appropriate extension
to the file name packet; in addition a system dependent area part
(usually different for \.{TFM} and \.{VF} files) is prepended if
the file name packet contains no area part.
@^system dependencies@>

@d append_to_name(#)==
  if l_cur_name<name_length then
    begin incr(l_cur_name); cur_name[l_cur_name]:=#;
    end
  else overflow(str_name_length,name_length)
@d make_font_name_end(#)==
  append_to_name(#[l]); make_name
@d make_font_name(#)==
  l_cur_name:=0; for l:=1 to # do make_font_name_end
@y
@ Since files are actually searched through path definitions,
the area definitions are ignored here.
To reduce the required changes we simply ignore the parameters given
to |make_font_name|.
@^system dependencies@>

@d append_to_name(#)== begin
    cur_name[l_cur_name]:=#;
    incr(l_cur_name);
    end
@d make_font_name_end(#)==
  make_name
@d make_font_name(#)==
  l_cur_name:=0; make_font_name_end
@z

% [67] No conversion of filenames in lower case, and initialize and
% terminate for C strings.  Eliminate now unused variable.
@x [67] l.1311
@!c:char; {a character to be appended to |cur_name|}
@y
@z

@x [67] l.1313
cur_loc:=pckt_start[n]; cur_limit:=pckt_start[n+1];
@y
cur_name := xmalloc_array (char, pckt_length (n) + pckt_length (e));
cur_loc:=pckt_start[n]; cur_limit:=pckt_start[n+1];
@z

@x [67] l.1318
  if (b>="a")and(b<="z") then Decr(b)(("a"-"A")); {convert to upper case}
@y
@z

@x [67] l.1321
cur_loc:=pckt_start[e]; cur_limit:=pckt_start[e+1];
while cur_loc<cur_limit do
  begin pckt_extract(b); append_res_to_name(xchr[b]);
  end;
while l_cur_name<name_length do
  begin incr(l_cur_name); cur_name[l_cur_name]:=' ';
  end;
@y Skip copying in the extension, kpathsea knows what to do.
  cur_name[l_cur_name] := 0;
@z

@x [91] l.1717 Lower case file name extensions.
id4(".")("T")("F")("M")(tfm_ext); {file name extension for \.{TFM} files}
@y
id4(".")("t")("f")("m")(tfm_ext); {file name extension for \.{TFM} files}
@z

@x [92] l.1719 Set default directory name
@ If no font directory has been specified, \.{\title} is supposed to use
the default \.{TFM} directory, which is a system-dependent place where
the \.{TFM} files for standard fonts are kept.
The string variable |TFM_default_area| contains the name of this area.
@^system dependencies@>

@d TFM_default_area_name=='TeXfonts:' {change this to the correct name}
@d TFM_default_area_name_length=9 {change this to the correct length}

@<Glob...@>=
@!TFM_default_area:packed array[1..TFM_default_area_name_length] of char;
@y
@ If no font directory has been specified, we search paths.
@z

@x [93] l.1731 Remove initialization of now-defunct array
@ @<Set init...@>=
TFM_default_area:=TFM_default_area_name;
@y
@ (No initialization to be done.  Keep this module to preserve numbering.)
@z

@x [94] l.1739 Declare bad_tfm as noreturn
procedure bad_tfm;
@y
noreturn procedure bad_tfm;
@z

@x [94] l.1746 Declare bad_font as noreturn
procedure bad_font;
@y
noreturn procedure bad_font;
@z

@x [95] l.1758 Open TFM file
@<TFM: Open |tfm_file|@>=
make_font_name(TFM_default_area_name_length)(TFM_default_area)(tfm_ext);
reset(tfm_file,cur_name);
if eof(tfm_file) then
@y
|TFM_default_area_name_length| and |TFM_default_area| will not
be used by |make_font_name|.

@<TFM: Open |tfm_file|@>=
make_font_name(TFM_default_area_name_length)(TFM_default_area)(tfm_ext);
full_name := kpse_find_tfm (cur_name);
if full_name then begin
  resetbin (tfm_file, full_name);
  free (cur_name);
  free (full_name);
end else
@z

@x [104] l.1991 Compute alpha and beta.
alpha:=16;
while z>=@'40000000 do
@y
alpha:=16;
if z>=@'1000000000 then abort('Character size is too large!');
while z>=@'40000000 do
@z

@x [108] l.2061 Declare full_name.
@!dvi_loc:int_32; {where we are about to look, in |dvi_file|}
@y
@!dvi_loc:int_32; {where we are about to look, in |dvi_file|}
@!full_name: ^char;
@z

@x [109] l.2068 Declare bad_dvi as noreturn
procedure bad_dvi;
@y
noreturn procedure bad_dvi;
@z

@x [110] l.2077 Fix up opening the binary files
@ To prepare |dvi_file| for input, we |reset| it.

@<Open input file(s)@>=
reset(dvi_file); {prepares to read packed bytes from |dvi_file|}
dvi_loc:=0;
@y
@ To prepare |dvi_file| for input, we |reset| it.

@<Open input file(s)@>=
dvi_loc:=0;
@z

@x [112] l.2115 Make dvi_length() and dvi_move() work.
@p function dvi_length:int_32;
begin set_pos(dvi_file,-1); dvi_length:=cur_pos(dvi_file);
end;
@#
procedure dvi_move(@!n:int_32);
begin set_pos(dvi_file,n); dvi_loc:=n;
end;
@y
@p function dvi_length:int_32;
begin xfseek(dvi_file, 0, 2, dvi_name);
dvi_loc:=xftell(dvi_file, dvi_name);
dvi_length:=dvi_loc;
end;
@#
procedure dvi_move(n:int_32);
begin xfseek(dvi_file, n, 0, dvi_name);
dvi_loc:=n;
end;
@z

@x [135] l.2468 Lower case file name extensions.
id3(".")("V")("F")(vf_ext); {file name extension for \.{VF} files}
@y
id3(".")("v")("f")(vf_ext); {file name extension for \.{VF} files}
@z

@x [137] l.2482 Set default directory name
@ If no font directory has been specified, \.{\title} is supposed to use
the default \.{VF} directory, which is a system-dependent place where
the \.{VF} files for standard fonts are kept.
The string variable |VF_default_area| contains the name of this area.
@^system dependencies@>

@d VF_default_area_name=='TeXvfonts:' {change this to the correct name}
@d VF_default_area_name_length=10 {change this to the correct length}

@<Glob...@>=
@!VF_default_area:packed array[1..VF_default_area_name_length] of char;

@ @<Set init...@>=
VF_default_area:=VF_default_area_name;
@y
@ If no font directory has been specified, \.{\title} is supposed to use
the default \.{VF} directory, which is a system-dependent place where
the \.{VF} files for standard fonts are kept.

Actually, under UNIX the standard area is defined in an external
file \.{site.h}.  And the users have a path searched for fonts,
by setting the \.{VFFONTS} environment variable.

@ (No initialization to be done.  Keep this module to preserve numbering.)
@z

@x [139] l.2499 Open VF file
@<VF: Open |vf_file| or |goto not_found|@>=
make_font_name(VF_default_area_name_length)(VF_default_area)(vf_ext);
reset(vf_file,cur_name);
if eof(vf_file) then
@^system dependencies@>
@y
Do path searching. But the \.{VF} file may not exist.

@<VF: Open |vf_file| or |goto not_found|@>=
make_font_name(VF_default_area_name_length)(VF_default_area)(vf_ext);
full_name := kpse_find_vf (cur_name);
if full_name then begin
  resetbin (vf_file, full_name);
  free (cur_name);
  free (full_name);
end else
@z

@x [163] l.2938 copy elements of array piece by piece
@ @<VF: Start a new level@>=
append_one(push);
vf_move[vf_ptr]:=vf_move[vf_ptr-1];
@y
@ \.{web2c} does not like array assignments. So we need to do them
through a macro replacement.

@d do_vf_move(#) == vf_move[vf_ptr]# := vf_move[vf_ptr-1]#
@d vf_move_assign == begin do_vf_move([0][0]); do_vf_move([0][1]);
			   do_vf_move([1][0]); do_vf_move([1][1])
		     end

@<VF: Start a new level@>=
append_one(push);
vf_move_assign;
@z

@x [170] l.3034 and again...
  vf_move[vf_ptr]:=vf_move[vf_ptr-1];
@y
  vf_move_assign;
@z

@x [176] l.3139 break is fflush.
@d update_terminal == break(output) {empty the terminal output buffer}
@y
@d update_terminal == fflush(stdout) {empty the terminal output buffer}
@z

@x [176] l.3149
procedure input_ln; {inputs a line from the terminal}
var k:0..terminal_line_length;
begin if n_opt=0 then
  begin print('Enter option: '); update_terminal; reset(input);
  if eoln(input) then read_ln(input);
  k:=0; pckt_room(terminal_line_length);
  while (k<terminal_line_length)and not eoln(input) do
    begin append_byte(xord[input^]); incr(k); get(input);
    end;
  end
else if k_opt<n_opt then
  begin incr(k_opt);
  {Copy command line option number |k_opt| into |byte_mem| array!}
  end;
end;
@y
procedure input_ln; {inputs a line from the terminal}
var k:0..terminal_line_length;
begin print('Enter option: '); update_terminal;
{|if eoln(input) then read_ln(input);|}
k:=0; pckt_room(terminal_line_length);
while (k<terminal_line_length)and not eoln(input) do
  begin append_byte(xord[getc(input)]); incr(k);
  end;
end;
@z

@x [241] l.4071 No dialog, remove unused final label.
dialog; {get options}
@y
@z
@x [241] l.4076
final_end:end.
@y
end.
@z

@x [246] l.4125 Do this later, to avoid creating empty files.
@<Open output file(s)@>=
rewrite(out_file); {prepares to write packed bytes to |out_file|}
@y
@<Open output file(s)@>=
@z

@x [248] l.4142 Use external routine to output bytes.
@d out_byte(#) == write(out_file,#) {write next \.{DVI} byte}
@y
@d out_byte(#) == put_byte(#,out_file) {write next \.{DVI} byte}
@z

@x [260] l.4260 String declaration.
@!comment:packed array[1..comm_length] of char; {preamble comment prefix}
@y
@!comment:const_c_string; {preamble comment prefix}
@z

@x [261] l.4271 Output the string from 0 to len-1, not 1 to len.
for k:=1 to comm_length do append_byte(xord[comment[k]]);
@y
for k:=0 to comm_length - 1 do append_byte(xord[ucharcast(comment[k])]);
@z

@x [293] l.4481 System-dependent changes.
This section should be replaced, if necessary, by changes to the program
that are necessary to make \.{DVIcopy} work at a particular installation.
It is usually best to design your change file so that all changes to
previous sections preserve the section numbering; then everybody's version
will be consistent with the printed program. More extensive changes,
which introduce new sections, can be inserted here; then only the index
itself will get a new section number.
@^system dependencies@>
@y
Parse a Unix-style command line.

This macro tests if its argument is the current option, as represented
by the index variable |option_index|.

@d argument_is (#) == (strcmp (long_options[option_index].name, #) = 0)

@<Define \(|parse_arguments|@> =
procedure parse_arguments;
const n_options = 5; {Pascal won't count array lengths for us.}
var @!long_options: array[0..n_options] of getopt_struct;
    @!getopt_return_val: integer;
    @!option_index: c_int_type;
    @!current_option: 0..n_options;
    @!k, @!m: c_int_type;
    @!end_num: ^char;
begin
  @<Define the option table@>;
  @<Initialize options@>;
  repeat
    getopt_return_val := getopt_long_only (argc, argv, '', long_options,
                                           address_of (option_index));
    if getopt_return_val = -1 then begin
      do_nothing; {End of arguments; we exit the loop below.}

    end else if getopt_return_val = "?" then begin
      usage (my_name);

    end else if argument_is ('help') then begin
      usage_help (DVICOPY_HELP, nil);

    end else if argument_is ('version') then begin
      print_version_and_exit (banner, 'Peter Breitenlohner', nil, nil);

    end else if argument_is ('magnification') then begin
      out_mag := atou (optarg);

    end else if argument_is ('max-pages') then begin
      max_pages := atou (optarg);
      incr (cur_select);

    end else if argument_is ('page-start') then begin
      @<Determine the desired |start_count| values from |optarg|@>;

    end; {Else it was a flag; |getopt| has already done the assignment.}
  until getopt_return_val = -1;

  {Now |optind| is the index of first non-option on the command line.
   We can have zero, one, or two remaining arguments.}
  if (optind > argc) or (optind + 2 < argc) then begin
    write_ln (stderr, my_name, ': Need at most two file arguments.');
    usage (my_name);
  end;

  if optind = argc then begin
    dvi_name := '<stdin>';
    dvi_file := make_binary_file (stdin);
    random_reading := false;
  end else begin
    dvi_name := extend_filename (cmdline (optind), 'dvi');
    resetbin (dvi_file, dvi_name);
    random_reading := true;
  end;

  if optind + 2 = argc then begin
    rewritebin (out_file, extend_filename (cmdline (optind + 1), 'dvi'));
    term_out := stdout;
  end else begin
    out_file := make_binary_file (stdout);
    term_out := stderr;
  end;
end;

@ Here is the first of the options we allow.
@.-help@>

@<Define the option...@> =
current_option := 0;
long_options[0].name := 'help';
long_options[0].has_arg := 0;
long_options[0].flag := 0;
long_options[0].val := 0;
incr (current_option);

@ Another of the standard options.
@.-version@>

@<Define the option...@> =
long_options[current_option].name := 'version';
long_options[current_option].has_arg := 0;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ Magnification to apply.
@.-magnification@>

@<Define the option...@> =
long_options[current_option].name := 'magnification';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ How many pages to do.
@.-max-pages@>

@<Define the option...@> =
long_options[current_option].name := 'max-pages';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ What page to start at.
@.-page-start@>

@<Define the option...@> =
long_options[current_option].name := 'page-start';
long_options[current_option].has_arg := 1;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;
incr (current_option);

@ Parsing the starting page specification is a bit complicated.
(This is the same as in \.{DVItype}.)

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
selected := false;

@ An element with all zeros always ends the list.

@<Define the option...@> =
long_options[current_option].name := 0;
long_options[current_option].has_arg := 0;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;

@ @<Glob...@> =
@!term_out:text;
@!dvi_name:const_c_string;
@z
