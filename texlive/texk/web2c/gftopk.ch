% gftopk.ch for C compilation with web2c.
%
% 09/19/88 Pierre A. MacKay	Version 1.4.
% 12/02/89 Karl Berry		2.1.
% 01/20/90 Karl			2.2.
% (more recent changes in the ChangeLog)
%
% One major change in output format is made by this change file.  The
% local gftopk preamble comment is ignored and the dated METAFONT
% comment is passed through unaltered.  This provides a continuous check
% on the origin of fonts in both gf and pk formats.  The program runs
% silently unless it is given the -v switch in the command line.

@x [0] l.41
\def\(#1){} % this is used to make section names sort themselves better
@y
\def\({} % this is used to make section names sort themselves better
@z

@x [0] l.44
\def\title{GFtoPK}
@y
\def\title{GF$\,$\lowercase{to}$\,$PK changes C}
@z

@x [0] WEAVE: print changes only.
\pageno=\contentspagenumber \advance\pageno by 1
@y
\pageno=\contentspagenumber \advance\pageno by 1
\let\maybe=\iffalse
@z

@x [1] Define my_name
@d banner=='This is GFtoPK, Version 2.4' {printed when the program starts}
@y
@d my_name=='gftopk'
@d banner=='This is GFtoPK, Version 2.4' {printed when the program starts}
@z

@x [4] No global labels.
@ The binary input comes from |gf_file|, and the output font is written
on |pk_file|.  All text output is written on \PASCAL's standard |output|
file.  The term |print| is used instead of |write| when this program writes
on |output|, so that all such output could easily be redirected if desired.

@d print(#)==write(#)
@d print_ln(#)==write_ln(#)

@p program GFtoPK(@!gf_file,@!pk_file,@!output);
label @<Labels in the outer block@>@/
const @<Constants in the outer block@>@/
type @<Types in the outer block@>@/
var @<Globals in the outer block@>@/
procedure initialize; {this procedure gets things started properly}
  var i:integer; {loop index for initializations}
  begin print_ln(banner);@/
@y
@ The binary input comes from |gf_file|, and the output font is written
on |pk_file|.  All text output is written on \PASCAL's standard |output|
file.  The term |print| is used instead of |write| when this program writes
on |output|, so that all such output could easily be redirected if desired.
Since the terminal output is really not very interesting, it is
produced only when the \.{-v} command line flag is presented.

@d print(#)==if verbose then write(stdout, #)
@d print_ln(#)==if verbose then write_ln(stdout, #)

@p program GFtoPK(@!gf_file,@!pk_file,@!output);
const @<Constants in the outer block@>@/
type @<Types in the outer block@>@/
var @<Globals in the outer block@>@/
@<Define \(|parse_arguments|@>
procedure initialize; {this procedure gets things started properly}
  var i:integer; {loop index for initializations}
begin
  kpse_set_program_name (argv[0], my_name);
  kpse_init_prog ('GFTOPK', 0, nil, nil);
  parse_arguments;
  print(banner); print_ln(version_string);
@z

@x [5] Eliminate the |final_end| label.
@ If the program has to stop prematurely, it goes to the
`|final_end|'.

@d final_end=9999 {label for the end of it all}

@<Labels...@>=final_end;
@y
@ This module is deleted, because it is only useful for
a non-local |goto|\unskip, which we can't use in C.
@z

@x [6] Dynamic allocation of |row| array.
@!max_row=16000; {largest index in the main |row| array}
@y
@!MAX_ROW=16000; {largest index in the initial main |row| array}
@z

@x [8] Make `abort' end with a newline, and remove the nonlocal goto.
so a procedure called |jump_out| has been introduced. This procedure, which
simply transfers control to the label |final_end| at the end of the program,
contains the only non-local |goto| statement in \.{GFtoPK}.
@y
so we might want to |abort| the program with an error message.
@z
@x [8]
@d abort(#)==begin print(' ',#); jump_out;
    end
@d bad_gf(#)==abort('Bad GF file: ',#,'!')
@.Bad GF file@>

@p procedure jump_out;
begin goto final_end;
end;
@y
@d abort(#)==begin write_ln(stderr, #); uexit (1);
    end
@d bad_gf(#)==abort('Bad GF file: ',#,'!')
@.Bad GF file@>
@z

@x [10] Pascal Web's char
@d text_char == char {the data type of characters in text files}
@y
@d char == 0..255
@d text_char == char {the data type of characters in text files}
@z

@x [39] Use paths in open_gf_file.
@ To prepare the |gf_file| for input, we |reset| it.

@p procedure open_gf_file; {prepares to read packed bytes in |gf_file|}
begin reset(gf_file);
gf_loc := 0 ;
end;
@y
@ In C, we do path searching based on the user's environment or the
default paths.

@p procedure open_gf_file; {prepares to read packed bytes in |gf_file|}
begin
  gf_file := kpse_open_file (gf_name, kpse_gf_format);
  gf_loc := 0;
end;
@z

% [40] If the PK filename isn't given on the command line, we construct
% it from the GF filename.
@x [40]
@p procedure open_pk_file; {prepares to write packed bytes in |pk_file|}
begin rewrite(pk_file);
pk_loc := 0 ; pk_open := true ;
end;
@y
@p procedure open_pk_file; {prepares to write packed bytes in |pk_file|}
begin
  rewritebin (pk_file, pk_name);
  pk_loc := 0;
  pk_open := true;
end;
@z

@x [44] Redefine pk_byte, pk_halfword, pk_three_bytes, and pk_word.
@ We also need a few routines to write data to the \.{PK} file.  We write
data in 4-, 8-, 16-, 24-, and 32-bit chunks, so we define the appropriate
routines. We must be careful not to let the sign bit mess us up, as some
\PASCAL s implement division of a negative integer differently.

@p procedure pk_byte(a:integer) ;
begin
   if pk_open then begin
      if a < 0 then a := a + 256 ;
      write(pk_file, a) ;
      incr(pk_loc) ;
   end ;
end ;
@#
procedure pk_halfword(a:integer) ;
begin
   if a < 0 then a := a + 65536 ;
   write(pk_file, a div 256) ;
   write(pk_file, a mod 256) ;
   pk_loc := pk_loc + 2 ;
end ;
@#
procedure pk_three_bytes(a:integer);
begin
   write(pk_file, a div 65536 mod 256) ;
   write(pk_file, a div 256 mod 256) ;
   write(pk_file, a mod 256) ;
   pk_loc := pk_loc + 3 ;
end ;
@#
procedure pk_word(a:integer) ;
var b : integer ;
begin
   if pk_open then begin
      if a < 0 then begin
         a := a + @'10000000000 ;
         a := a + @'10000000000 ;
         b := 128 + a div 16777216 ;
      end else b := a div 16777216 ;
      write(pk_file, b) ;
      write(pk_file, a div 65536 mod 256) ;
      write(pk_file, a div 256 mod 256) ;
      write(pk_file, a mod 256) ;
      pk_loc := pk_loc + 4 ;
   end ;
end ;
@y
@ We also need a few routines to write data to the \.{PK} file.  We write
data in 4-, 8-, 16-, 24-, and 32-bit chunks, so we define the appropriate
routines. We must be careful not to let the sign bit mess us up, as some
\PASCAL s implement division of a negative integer differently.

Output is handled through |putbyte| which is supplied by web2c.

@d pk_byte(#)==begin putbyte(#, pk_file); incr(pk_loc) end

@p procedure pk_halfword(a:integer) ;
begin
   if a < 0 then a := a + 65536 ;
   putbyte(a div 256, pk_file) ;
   putbyte(a mod 256, pk_file) ;
   pk_loc := pk_loc + 2 ;
end ;
@#
procedure pk_three_bytes(a:integer);
begin
   putbyte(a div 65536 mod 256, pk_file) ;
   putbyte(a div 256 mod 256, pk_file) ;
   putbyte(a mod 256, pk_file) ;
   pk_loc := pk_loc + 3 ;
end ;
@#
procedure pk_word(a:integer) ;
var b : integer ;
begin
   if a < 0 then begin
      a := a + @'10000000000 ;
      a := a + @'10000000000 ;
      b := 128 + a div 16777216 ;
   end else b := a div 16777216 ;
   putbyte(b, pk_file) ;
   putbyte(a div 65536 mod 256, pk_file) ;
   putbyte(a div 256 mod 256, pk_file) ;
   putbyte(a mod 256, pk_file) ;
   pk_loc := pk_loc + 4 ;
end ;
@z

@x [46] Redefine find_gf_length and move_to_byte.
@p procedure find_gf_length ;
begin
   set_pos(gf_file, -1) ; gf_len := cur_pos(gf_file) ;
end ;
@#
procedure move_to_byte(@!n : integer) ;
begin
   set_pos(gf_file, n); gf_loc := n ;
end ;
@y
@d find_gf_length==gf_len:=gf_length

@p function gf_length:integer;
begin
  xfseek (gf_file, 0, 2, gf_name);
  gf_length := xftell (gf_file, gf_name);
end;
@#
procedure move_to_byte (n:integer);
begin xfseek (gf_file, n, 0, gf_name);
end;
@z

@x [48] Dynamic allocation of |row| array.
@!row: array[0..max_row] of integer; {the row counts for working}
@y
@!row: ^integer; {the row counts for working}
@!max_row: integer; {largest index in the main |row| array}
@z

@x [49] Dynamic allocation of |row| array.
@<Set init...@>=
@y
@<Set init...@>=
row := xmalloc_array (integer, MAX_ROW);
max_row := MAX_ROW;
@z

@x [51] Dynamic allocation of |row| array.
procedure convert_gf_file;
@y
procedure row_overflow;
var @!new_row: integer;
begin
  new_row := max_row + MAX_ROW;
  print_ln('Reallocated row array to ', new_row:1, ' items from ', max_row:1, '.');
  row := xrealloc_array (row, integer, new_row);
  max_row := new_row;
end;
@#
procedure convert_gf_file;
@z

@x [still 51] Add do_the_rows to break up huge run of cases.
   repeat
     gf_com := gf_byte ;
     case gf_com of
@y
   repeat
     gf_com := gf_byte ;
     do_the_rows:=false;
     case gf_com of
@z

@x [52] Declare |thirty_seven_cases| to help avoid breaking yacc.
@d one_sixty_five_cases(#)==sixty_four_cases(#),sixty_four_cases(#+64),
         sixteen_cases(#+128),sixteen_cases(#+144),four_cases(#+160),#+164
@y
@d thirty_seven_cases(#)==sixteen_cases(#),sixteen_cases(#+16),
	 four_cases(#+32),#+36
@d new_row_64=new_row_0 + 64
@d new_row_128=new_row_64 + 64
@z

@x [56] Dynamic allocation of |row| array.
@d put_in_rows(#)==begin if row_ptr > max_row then bad := true else begin
row[row_ptr]:=#; incr(row_ptr); end ; end
@y
@d put_in_rows(#)==begin if row_ptr > max_row then row_overflow;
row[row_ptr]:=#; incr(row_ptr); end
@z

@x [57] Dynamic allocation of |row| array.
  bad := false ;
@y
@z

@x [still 57] Break up an oversized sequence of cases for yacc.
one_sixty_five_cases(new_row_0) : begin
  if on = state then put_in_rows(extra) ;
  put_in_rows(end_of_row) ;
  on := true ; extra := gf_com - new_row_0 ; state := false ;
end ;
@y
sixty_four_cases(new_row_0) : do_the_rows:=true;
sixty_four_cases(new_row_64) : do_the_rows:=true;
thirty_seven_cases(new_row_128) : do_the_rows:=true;
@z

@x [still 57] Dynamic allocation of |row| array.
  if bad then abort('Ran out of internal memory for row counts!') ;
@.Ran out of memory@>
@y
@z

@x [still 57] Break up an oversized sequence of cases for yacc.
    endcases ;
@y
    endcases ;
if do_the_rows then begin
  do_the_rows:=false;
  if on = state then put_in_rows(extra) ;
  put_in_rows(end_of_row) ;
  on := true ; extra := gf_com - new_row_0 ; state := false ;
end ;
@z

@x [58] Add do_the_rows to break up huge run of cases.
@ A few more locals used above and below:

@<Locals to |convert_gf_file|@>=
@y
@ A few more locals used above and below:

@<Locals to |convert_gf_file|@>=
@!do_the_rows:boolean;
@z

@x [still 58] Dynamic allocation of |row| array.
@!bad : boolean ; {did we run out of space?}
@y
@z

@x [81] Don't add `GFtoPK 2.3 output from ' to the font comment.
@d comm_length = 23 {length of |preamble_comment|}
@d from_length = 6 {length of its |' from '| part}
@y
@d comm_length = 0 {length of |preamble_comment|}
@d from_length = 0 {length of its |' from '| part}
@z

@x [83] Don't do any assignments to |preamble_comment|.
@ @<Set init...@>=
comment := preamble_comment ;
@y
@ This module is empty in the C version.
@z

@x [86] Remove the final_end label
final_end : end .
@y
end.
@z

@x [88] System-dependent changes.
This section should be replaced, if necessary, by changes to the program
that are necessary to make \.{GFtoPK} work at a particular installation.
It is usually best to design your change file so that all changes to
previous sections preserve the section numbering; then everybody's version
will be consistent with the printed program. More extensive changes,
which introduce new sections, can be inserted here; then only the index
itself will get a new section number.
@^system dependencies@>
@y
Parse a Unix-style command line.

@d argument_is (#) == (strcmp (long_options[option_index].name, #) = 0)
@d do_nothing ==        {empty statement}

@<Define \(|parse_arguments|@> =
procedure parse_arguments;
const n_options = 3; {Pascal won't count array lengths for us.}
var @!long_options: array[0..n_options] of getopt_struct;
    @!getopt_return_val: integer;
    @!option_index: c_int_type;
    @!current_option: 0..n_options;
begin
  @<Initialize the option variables@>;
  @<Define the option table@>;
  repeat
    getopt_return_val := getopt_long_only (argc, argv, '', long_options,
                                           address_of (option_index));
    if getopt_return_val = -1 then begin
      do_nothing; {End of arguments; we exit the loop below.}

    end else if getopt_return_val = "?" then begin
      usage (my_name); {|getopt| has already given an error message.}

    end else if argument_is ('help') then begin
      usage_help (GFTOPK_HELP, nil);

    end else if argument_is ('version') then begin
      print_version_and_exit (banner, nil, 'Tomas Rokicki', nil);

    end; {Else it was a flag; |getopt| has already done the assignment.}
  until getopt_return_val = -1;

  {Now |optind| is the index of first non-option on the command line.
   We must have one or two remaining arguments.}
  if (optind + 1 <> argc) and (optind + 2 <> argc) then begin
    write_ln (stderr, my_name, ': Need one or two file arguments.');
    usage (my_name);
  end;

  gf_name := cmdline (optind);

  {If an explicit output filename isn't given, construct it from |gf_name|.}
  if optind + 2 = argc then begin
    pk_name := cmdline (optind + 1);
  end else begin
    pk_name := basename_change_suffix (gf_name, 'gf', 'pk');
  end;
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

@ Print progress information?

@<Define the option...@> =
long_options[current_option].name := 'verbose';
long_options[current_option].has_arg := 0;
long_options[current_option].flag := address_of (verbose);
long_options[current_option].val := 1;
incr (current_option);

@ @<Glob...@> =
@!verbose: c_int_type;

@ @<Initialize the option...@> =
verbose := false;

@ An element with all zeros always ends the list.

@<Define the option...@> =
long_options[current_option].name := 0;
long_options[current_option].has_arg := 0;
long_options[current_option].flag := 0;
long_options[current_option].val := 0;

@ Global filenames.

@<Global...@> =
@!gf_name, @!pk_name:const_c_string;
@z
