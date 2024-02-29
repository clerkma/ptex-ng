% vim: ft=change
% TODO: sparse arrays
% This is the first of the set of
% WEB change file containing code for various features extending Omega;
% these files define the Aleph program, and are designed to be applied
% to omega.web compiled without XML support.
% This change-file is heavily based on etex.ch,
% defining version 2.1 of e-TeX.

% ATTENTION: The software is currently in pre-alpha stage.

% The TeX program is copyright (C) 1982 by D. E. Knuth.
% TeX is a trademark of the American Mathematical Society.
% e-TeX and NTS are trademarks of the NTS group.

% All line numbers refer to TEX.WEB 3.14159 as of March 21, 1995.
% TODO: double line number reference, having them for both TEX.WEB
%       and (X)OMEGA.WEB

@x limbo l.1 - this is Aleph
% (C) 1994--2000 by John Plaice and Yannis Haralambous.
@y
% (C) 2002--2004 by Giuseppe Bilotta and the Aleph task force;
% all rights are reserved.
% 
% Aleph is directly derived from e-TeX, an extension of
% Donald E. Knuth's TeX developed by Peter Breitenlohner and
% the NTS team, and Omega, another extension of TeX, developed by
% John Plaice and Yannis Haralambous.

% Aleph is designed in the form of a set of WEB changefiles
% to be applied to Omega
% compiled without XML support. The changefiles are heavily based
% on the e-TeX 2.1 changefiles. 

% Omega is copyright (C) 1994--2000 by John Plaice and Yannis Haralambous.
@z
%---------------------------------------
@x limbo l.1 - this is e-TeX
%
% This program is copyright (C) 1982 by D. E. Knuth; all rights are reserved.
% Unlimited copying and redistribution of this file are permitted as long
% as this file is not modified. Modifications are permitted, but only if
% the resulting file is not named tex.web. (The WEB system provides
% for alterations via an auxiliary file; the master file should stay intact.)
% See Appendix H of the WEB manual for hints on how to install this program.
% And see Appendix A of the TRIP manual for details about how to validate it.

% TeX is a trademark of the American Mathematical Society.
% METAFONT is a trademark of Addison-Wesley Publishing Company.
@y
% e-TeX is copyright (C) 1999-2012 by P. Breitenlohner (1994,98 by the NTS
% team); all rights are reserved.

% e-TeX and NTS are trademarks of the NTS group.
% TeX is a trademark of the American Mathematical Society.
% METAFONT is a trademark of Addison-Wesley Publishing Company.

% e-TeX is directly derived from Donald E. Knuth's TeX;
% the change history which follows and the reward offered for finders of
% bugs refer specifically to TeX; they should not be taken as referring
% to e-TeX, although the change history is relevant in that it
% demonstrates the evolutionary path followed.  This program is not TeX;
% that name is reserved strictly for the program which is the creation
% and sole responsibility of Professor Knuth.
@z
%---------------------------------------
@x limbo l.50 - e-TeX history
% Although considerable effort has been expended to make the TeX program
% correct and reliable, no warranty is implied; the author disclaims any
% obligation or liability for damages, including but not limited to
% special, indirect, or consequential damages arising out of or in
% connection with the use or performance of this software. This work has
% been a ``labor of love'' and the author hopes that users enjoy it.
@y
% A preliminary version of TeX--XeT was released in April 1992.
% TeX--XeT version 1.0 was released in June 1992,
%   version 1.1 prevented arith overflow in glue computation (Oct 1992).
% A preliminary e-TeX version 0.95 was operational in March 1994.
% Version 1.0beta was released in May 1995.
% Version 1.01beta fixed bugs in just_copy and every_eof (December 1995).
% Version 1.02beta allowed 256 mark classes (March 1996).
% Version 1.1 changed \group{type,level} -> \currentgroup{type,level},
%             first public release (October 1996).
% Version 2.0 development was started in March 1997;
%             fixed a ligature-\beginR bug in January 1998;
%             was released in March 1998.
% Version 2.1 fixed a marks bug (when min_halfword<>0) (January 1999).

% Aleph includes all the e-TeX extensions, except for TeX--XeT and
% some optimizations.

% Release Candidate 0 was released in Janurary 2003.
% Release Candidate 1 was released in June 2003.
%                     fixed Omega overfull box bug
% Release Candidate 2 has to be released
%                     fixed Omega overfull rule bug
%                     fixed Omega leader tripping bug
%                     reorganized source code

% Although considerable effort has been expended to make the Aleph program
% correct and reliable, no warranty is implied; the authors disclaim any
% obligation or liability for damages, including but not limited to
% special, indirect, or consequential damages arising out of or in
% connection with the use or performance of this software. This work has
% been a ``labor of love'' and the authors hope that users enjoy it.
@z
%---------------------------------------
@x limbo l.61 - e-TeX logo, TeXXeT
\let\mc=\ninerm % medium caps for names like SAIL
@y
\let\mc=\ninerm % medium caps for names like SAIL
\def\eTeX{$\varepsilon$-\TeX}
\def\Aleph{$\aleph$}
% \font\revrm=xbmc10 % for right-to-left text
% % to generate xbmc10 (i.e., reflected cmbx10) use a file
% % xbmc10.mf containing:
% %+++++++++++++++++++++++++++++++++++++++++++++++++
% %     if unknown cmbase: input cmbase fi
% %     extra_endchar := extra_endchar &
% %       "currentpicture:=currentpicture " &
% %       "reflectedabout((.5[l,r],0),(.5[l,r],1));";
% %     input cmbx10
% %+++++++++++++++++++++++++++++++++++++++++++++++++
% \ifx\beginL\undefined % this is TeX
  \def\XeT{X\kern-.125em\lower.5ex\hbox{E}\kern-.1667emT}
  \def\TeXeT{\TeX-\hbox{\revrm \XeT}}   % for TeX-XeT
  \def\TeXXeT{\TeX-\hbox{\revrm -\XeT}} % for TeX--XeT
% \else
%   \ifx\eTeXversion\undefined % this is \TeXeT
%     \def\TeXeT{\TeX-{\revrm\beginR\TeX\endR}}   % for TeX-XeT
%     \def\TeXXeT{\TeX-{\revrm\beginR\TeX-\endR}} % for TeX--XeT
%   \else % this is \eTeX
%     \def\TeXeT{\TeX-{\TeXXeTstate=1\revrm\beginR\TeX\endR}}   % for TeX-XeT
%     \def\TeXXeT{\TeX-{\TeXXeTstate=1\revrm\beginR\TeX-\endR}} % for TeX--XeT
%   \fi
% \fi
@z
%---------------------------------------
@x limbo l.64 - bug fix (print only changed modules)
\def\pct!{{\char`\%}} % percent sign in ordinary text
@y
\def\pct!{{\char`\%}} % percent sign in ordinary text
\def\grp{\.{\char'173...\char'175}}
@z
%---------------------------------------
@x limbo l.80 - e-TeX basic
\def\title{$\Omega$ (OMEGA)}
@y
\def\title{\Aleph\ (ALEPH)}
% system dependent redefinitions of \title should come later
% and should use:
%    \toks0=\expandafter{\title}
%    \edef\title{...\the\toks0...}
\let\maybe=\iffalse % print only changed modules
\let\maybe=\iftrue % print everything
@z
%---------------------------------------
@x [1] m.1 l.91 - this is e-TeX
This is $\Omega$, a document compiler intended to simplify high-quality
@y
This is \Aleph, an extension of $\Omega$. $\Omega$ is a document compiler
intended to simplify high-quality
@z
%---------------------------------------
@x [1] m.2 l.179 - e-TeX basic
If this program is changed, the resulting system should not be called
@y
This program contains code from both the $\Omega$ (developed by
John Plaice and Yannis Haralambous) and \eTeX\ 
(developed by Peter Breitenlohner and the NTS team) extensions
to \TeX, therefore this program is called `\Aleph' and not
@z
%---------------------------------------
@x [1] m.2 l.185 - e-TeX basic
November 1984].
@y
November 1984].

A similar test suite called the ``\.{e-BUMP} test'' is (or will be made)
available for helping to determine whether a particular implementation deserves
to be known as `\Aleph'.
@z
%---------------------------------------
@x [1] m.2 l.187 - e-TeX basic
@d banner=='This is Omega, Version 3.141592653--1.15' {printed when \TeX\ starts}
@y
@d banner=='This is Aleph, Version 3.141592653--1.15--2.1' {printed when \TeX\ starts}
@#
@d eTeX_version_string=='3.141592653--1.15--2.1' {current \eTeX\ version}
@d eTeX_version=2 { \.{\\eTeXversion} }
@d eTeX_revision==".1" { \.{\\eTeXrevision} }
@#
@d eTeX_banner=='This is Aleph, Version ',eTeX_version_string
  {printed when \eTeX\ starts}
@#
@d OMEGA==ALEPH {change program name into |ALEPH|}
@#
@d eTeX_states=1 {number of \eTeX\ state variables in |eqtb|}
@z
%---------------------------------------
@x [1] m.15 l.493 - e-TeX basic
@d not_found=45 {go here when you've found nothing}
@y
@d not_found=45 {go here when you've found nothing}
@d not_found1=46 {like |not_found|, when there's more than one}
@d not_found2=47 {like |not_found|, when there's more than two}
@d not_found3=48 {like |not_found|, when there's more than three}
@d not_found4=49 {like |not_found|, when there's more than four}
@z
%---------------------------------------
@x [5] m.61 l.1556 - e-TeX basic
wterm(banner);
@y
wterm(eTeX_banner);
@z
%---------------------------------------
@x [10] m.141 l.2965 - e-TeX marks
This field occupies a full word instead of a halfword, because
there's nothing to put in the other halfword; it is easier in \PASCAL\ to
use the full word than to risk leaving garbage in the unused half.
@y
In addition there is a |mark_class| field that contains the mark class.
@z
%---------------------------------------
@x [10] m.141 l.2971 - e-TeX marks
@d mark_ptr(#)==mem[#+1].int {head of the token list for a mark}
@y
@d mark_ptr(#)==link(#+1) {head of the token list for a mark}
@d mark_class(#)==info(#+1) {the mark class}
@z
%---------------------------------------
@x [10] m.142 l.2980 - e-TeX marks
@d adjust_ptr==mark_ptr {vertical list to be moved out of horizontal list}
@y
@d adjust_ptr(#)==mem[#+1].int
  {vertical list to be moved out of horizontal list}
@z
%---------------------------------------
@x [12] m.196 l.3844 - e-TeX marks
begin print_esc("mark"); print_mark(mark_ptr(p));
@y
begin print_esc("mark");
if mark_class(p)<>0 then
  begin print_char("s"); print_int(mark_class(p));
  end;
print_mark(mark_ptr(p));
@z
%---------------------------------------
@x [15] m.208 l.4081 - e-TeX saved_items
@d un_vbox=24 {unglue a box ( \.{\\unvbox}, \.{\\unvcopy} )}
@y
@d un_vbox=24 {unglue a box ( \.{\\unvbox}, \.{\\unvcopy} )}
  {( or \.{\\pagediscards}, \.{\\splitdiscards} )}
@z
%---------------------------------------
@x [15] m.208 l.4107 - e-TeX middle
@d left_right=49 {variable delimiter ( \.{\\left}, \.{\\right} )}
@y
@d left_right=49 {variable delimiter ( \.{\\left}, \.{\\right} )}
  {( or \.{\\middle} )}
@z
%---------------------------------------
@x [15] m.209 l.4151 - e-TeX basic
  \.{\\insertpenalties} )}
@y
  \.{\\insertpenalties} )}
  {( or \.{\\interactionmode} )}
@z
%---------------------------------------
@x [15] m.209 l.4153 - e-TeX penalties
@d set_shape=set_box_dimen+1
   {specify fancy paragraph shape ( \.{\\parshape} )}
@y
@d set_shape=set_box_dimen+1
   {specify fancy paragraph shape ( \.{\\parshape} )}
  {(or \.{\\interlinepenalties}, etc.~)}
@z
%---------------------------------------
@x [15] m.209 l.4163 - e-TeX protected
@d prefix=divide+1
   {qualify a definition ( \.{\\global}, \.{\\long}, \.{\\outer} )}
@y
@d prefix=divide+1
   {qualify a definition ( \.{\\global}, \.{\\long}, \.{\\outer} )}
  {( or \.{\\protected} )}
@z
%---------------------------------------
@x [15] m.209 l.4166 - e-TeX read_line
@d read_to_cs=shorthand_def+1
   {read into a control sequence ( \.{\\read} )}
@y
@d read_to_cs=shorthand_def+1
   {read into a control sequence ( \.{\\read} )}
  {( or \.{\\readline} )}
@z
%---------------------------------------
@x [15] m.210 l.4181 - e-TeX scan_tokens
@d input=max_command+4 {input a source file ( \.{\\input}, \.{\\endinput} )}
@y
@d input=max_command+4 {input a source file ( \.{\\input}, \.{\\endinput} )}
  {( or \.{\\scantokens} )}
@z
%---------------------------------------
@x [15] m.210 l.4186 - e-TeX unexpanded
@d the=max_command+9 {expand an internal quantity ( \.{\\the} )}
@y
@d the=max_command+9 {expand an internal quantity ( \.{\\the} )}
  {( or \.{\\unexpanded}, \.{\\detokenize} )}
@z
%---------------------------------------
@x [16] m.212 l.4289 - e-TeX basic
user's output routine.
@y
user's output routine.

A seventh quantity, |eTeX_aux|, is used by the extended features \eTeX.
In vertical modes it is known as |LR_save| and holds the LR stack when a
paragraph is interrupted by a displayed formula.  In display math mode
it is known as |LR_box| and holds a pointer to a prototype box for the
display.  In math mode it is known as |delim_ptr| and points to the most
recent |left_noad| or |middle_noad| of a |math_left_group|.

In \Aleph it is only used in this last sense.
@z
%---------------------------------------
@x [16] m.212 l.4304 - e-TeX basic
  @!head_field,@!tail_field: pointer;
@y
  @!head_field,@!tail_field: pointer;
  @!eTeX_aux_field: pointer;
@z
%---------------------------------------
@x [16] m.213 l.4311 - e-TeX basic
@d tail==cur_list.tail_field {final node on current list}
@y
@d tail==cur_list.tail_field {final node on current list}
@d eTeX_aux==cur_list.eTeX_aux_field {auxiliary data for \eTeX}
@d delim_ptr==eTeX_aux {most recent left or right noad of a math left group}
@z
%---------------------------------------
@x [16] m.215 l.4342 - e-TeX basic
mode:=vmode; head:=contrib_head; tail:=contrib_head;
@y
mode:=vmode; head:=contrib_head; tail:=contrib_head;
eTeX_aux:=null;
@z
%---------------------------------------
@x [16] m.216 l.4358 push_nest - e-TeX basic
incr(nest_ptr); head:=get_avail; tail:=head; prev_graf:=0; mode_line:=line;
@y
incr(nest_ptr); head:=get_avail; tail:=head; prev_graf:=0; mode_line:=line;
eTeX_aux:=null;
@z
%---------------------------------------
@x [17] m.230 l.4712 - e-TeX basic, penalties
@d output_routine_loc=local_base+1 {points to token list for \.{\\output}}
@d every_par_loc=local_base+2 {points to token list for \.{\\everypar}}
@d every_math_loc=local_base+3 {points to token list for \.{\\everymath}}
@d every_display_loc=local_base+4 {points to token list for \.{\\everydisplay}}
@d every_hbox_loc=local_base+5 {points to token list for \.{\\everyhbox}}
@d every_vbox_loc=local_base+6 {points to token list for \.{\\everyvbox}}
@d every_job_loc=local_base+7 {points to token list for \.{\\everyjob}}
@d every_cr_loc=local_base+8 {points to token list for \.{\\everycr}}
@d err_help_loc=local_base+9 {points to token list for \.{\\errhelp}}
@d ocp_trace_level_base=local_base+10
@y
@d inter_line_penalties_loc=local_base+1 {additional penalties between lines}
@d club_penalties_loc=local_base+2 {penalties for creating club lines}
@d widow_penalties_loc=local_base+3 {penalties for creating widow lines}
@d display_widow_penalties_loc=local_base+4 {ditto, just before a display}
@d token_base=local_base+5 {table of token list parameters}
@d output_routine_loc=token_base {points to token list for \.{\\output}}
@d every_par_loc=token_base+1 {points to token list for \.{\\everypar}}
@d every_math_loc=token_base+2 {points to token list for \.{\\everymath}}
@d every_display_loc=token_base+3 {points to token list for \.{\\everydisplay}}
@d every_hbox_loc=token_base+4 {points to token list for \.{\\everyhbox}}
@d every_vbox_loc=token_base+5 {points to token list for \.{\\everyvbox}}
@d every_job_loc=token_base+6 {points to token list for \.{\\everyjob}}
@d every_cr_loc=token_base+7 {points to token list for \.{\\everycr}}
@d every_eof_loc=token_base+8 {points to token list for \.{\\everyeof}}
@d err_help_loc=token_base+9 {points to token list for \.{\\errhelp}}
@d ocp_trace_level_base=token_base+10
@z
%---------------------------------------
@x [17] m.231 l.4787 - e-TeX basic
  othercases print_esc("errhelp")
@y
  @/@<Cases of |assign_toks| for |print_cmd_chr|@>@/
  othercases print_esc("errhelp")
@z
%---------------------------------------
@x [17] m.233 l.4833 - e-TeX penalties
if n=par_shape_loc then
  begin print_esc("parshape"); print_char("=");
  if par_shape_ptr=null then print_char("0")
  else print_int(info(par_shape_ptr));
@y
if n<token_base then
  begin print_cmd_chr(set_shape,n); print_char("=");
  if equiv(n)=null then print_char("0")
  else if n>par_shape_loc then
    begin print_int(penalty(equiv(n))); print_char(" ");
    print_int(penalty(equiv(n)+1));
    if penalty(equiv(n))>1 then print_esc("ETC.");
    end
  else print_int(info(par_shape_ptr));
@z
%---------------------------------------
% FIXME: in this section and in the next
% we should remove pre_display_direction[_code]
% stuff, both here and down below
@x [17] m.236 l.4955 - e-TeX basic
@d dir_base=int_base+int_pars
@y
@d tracing_assigns_code=int_pars {show assignments}
@d tracing_groups_code=int_pars+1 {show save/restore groups}
@d tracing_ifs_code=int_pars+2 {show conditionals}
@d tracing_scan_tokens_code=int_pars+3 {show pseudo file open and close}
@d tracing_nesting_code=int_pars+4
  {show incomplete groups and ifs within files}
@d pre_display_direction_code=int_pars+5 {text direction preceding a display}
@d last_line_fit_code=int_pars+6 {adjustment for last line of paragraph}
@d saving_vdiscards_code=int_pars+7 {save items discarded from vlists}
@d saving_hyph_codes_code=int_pars+8 {save hyphenation codes for languages}
@d eTeX_state_code=int_pars+9 {\eTeX\ state variables}
@d dir_base=int_base+eTeX_state_code+eTeX_states
@z
%---------------------------------------
@x [17] m.236 l.5016 - e-TeX basic
@d error_context_lines==int_par(error_context_lines_code)
@y
@d error_context_lines==int_par(error_context_lines_code)
@d tracing_assigns==int_par(tracing_assigns_code)
@d tracing_groups==int_par(tracing_groups_code)
@d tracing_ifs==int_par(tracing_ifs_code)
@d tracing_scan_tokens==int_par(tracing_scan_tokens_code)
@d tracing_nesting==int_par(tracing_nesting_code)
@d pre_display_direction==int_par(pre_display_direction_code)
@d last_line_fit==int_par(last_line_fit_code)
@d saving_vdiscards==int_par(saving_vdiscards_code)
@d saving_hyph_codes==int_par(saving_hyph_codes_code)
@z
%---------------------------------------
@x [17] m.237 l.5081 print_param - e-TeX basic
othercases print("[unknown integer parameter!]")
@y
@/@<Cases for |print_param|@>@/
othercases print("[unknown integer parameter!]")
@z
%---------------------------------------
% FIXED(?): moved to init_eqtb_entry in
% omega.web at l.5813
@x [17] m.232 l.4805 - e-TeX penalties
   { Region 4 }
   else if (p>=par_shape_loc) and
           (p<=toks_base+biggest_reg) then begin
     equiv_field(mw):=null;
     eq_type_field(mw):=undefined_cs;
     eq_level_field(mw):=level_zero;
     end
@y
   { Region 4 }
   else if (p>=par_shape_loc) and
           (p< token_base) then begin
     equiv_field(mw):=null;
     eq_type_field(mw):=shape_ref;
     eq_level_field(mw):=level_one;
     end
   else if (p>=token_base) and
           (p<=toks_base+biggest_reg) then begin
     equiv_field(mw):=null;
     eq_type_field(mw):=undefined_cs;
     eq_level_field(mw):=level_zero;
     end
@z
%---------------------------------------
@x [18] m.264 l.5612 primitive - e-TeX basic
@!j:small_number; {index into |buffer|}
@y
@!j:0..buf_size; {index into |buffer|}
@z
%---------------------------------------
@x [18] m.264 l.5616 primitive - e-TeX basic
    {we will move |s| into the (empty) |buffer|}
  for j:=0 to l-1 do buffer[j]:=so(str_pool[k+j]);
  cur_val:=id_lookup(0,l); {|no_new_control_sequence| is |false|}
@y
    {we will move |s| into the (possibly non-empty) |buffer|}
  if first+l>buf_size+1 then
      overflow("buffer size",buf_size);
@:TeX capacity exceeded buffer size}{\quad buffer size@>
  for j:=0 to l-1 do buffer[first+j]:=so(str_pool[k+j]);
  cur_val:=id_lookup(first,l); {|no_new_control_sequence| is |false|}
@z
%---------------------------------------
@x [18] m.265 l.5691 - e-TeX penalties
primitive("parshape",set_shape,0);@/
@y
primitive("parshape",set_shape,par_shape_loc);@/
@z
%---------------------------------------
@x [18] m.265 l.5708 - e-TeX sparse arrays
primitive("toks",toks_register,0);@/
@y
primitive("toks",toks_register,mem_bot);@/
@z
%---------------------------------------
@x [18] m.266 l.5741 - e-TeX cond
expand_after: print_esc("expandafter");
@y
expand_after: if chr_code=0 then print_esc("expandafter")
  @<Cases of |expandafter| for |print_cmd_chr|@>;
@z
%---------------------------------------
@x [18] m.266 l.5747 - e-TeX marks
mark: print_esc("mark");
@y
mark: begin print_esc("mark");
  if chr_code>0 then print_char("s");
  end;
@z
%---------------------------------------
@x [18] m.266 l.5758 - e-TeX read_line
read_to_cs: print_esc("read");
@y
read_to_cs: if chr_code=0 then print_esc("read")
  @<Cases of |read| for |print_cmd_chr|@>;
@z
%---------------------------------------
@x [18] m.266 l.5762 - e-TeX penalties
set_shape: print_esc("parshape");
@y
set_shape: case chr_code of
  par_shape_loc: print_esc("parshape");
  @<Cases of |set_shape| for |print_cmd_chr|@>@;@/
  end; {there are no other cases}
@z
%---------------------------------------
@x [18] m.266 l.5763 - e-TeX unexpanded
the: print_esc("the");
@y
the: if chr_code=0 then print_esc("the")
  @<Cases of |the| for |print_cmd_chr|@>;
@z
%---------------------------------------
@x [18] m.266 l.5764 - e-TeX sparse arrays
toks_register: print_esc("toks");
@y
toks_register: @<Cases of |toks_register| for |print_cmd_chr|@>;
@z
%---------------------------------------
@x [19] m.268 l.5800 - e-TeX sparse arrays
interpreted in one of four ways:
@y
interpreted in one of five ways:
@z
%---------------------------------------
@x [19] m.268 l.5820 - e-TeX tracing
the entries for that group.
@y
the entries for that group.
Furthermore, in extended \eTeX\ mode, |save_stack[p-1]| contains the
source line number at which the current level of grouping was entered.

\yskip\hang 5) If |save_type(p)=restore_sa|, then |sa_chain| points to a
chain of sparse array entries to be restored at the end of the current
group. Furthermore |save_index(p)| and |save_level(p)| should replace
the values of |sa_chain| and |sa_level| respectively.
@z
%---------------------------------------
% FIXME restore_sa safe? (sparse arrays)
@x [19] m.268 l.5830 - e-TeX basic
@d level_boundary=3 {|save_type| corresponding to beginning of group}
@y
@d level_boundary=3 {|save_type| corresponding to beginning of group}
@d restore_sa=4 {|save_type| when sparse array entries should be restored}

@p@t\4@>@<Declare \eTeX\ procedures for tracing and input@>
@z
%---------------------------------------
@x [19] m.273 l.5888 - e-TeX tracing
@ The following macro is used to test if there is room for up to six more
@y
@ The following macro is used to test if there is room for up to seven more
@z
%---------------------------------------
@x [19] m.273 l.5894 check_full_save_stack - e-TeX tracing
  if max_save_stack>save_size-6 then overflow("save size",save_size);
@y
  if max_save_stack>save_size-7 then overflow("save size",save_size);
@z
%---------------------------------------
@x [19] m.274 l.5916 new_save_level - e-TeX tracing
begin check_full_save_stack;
@y
begin check_full_save_stack;
if eTeX_ex then
  begin saved(0):=line; incr(save_ptr);
  end;
@z
%---------------------------------------
@x [19] m.274 l.5923 new_save_level - e-TeX tracing
cur_boundary:=save_ptr; incr(cur_level); incr(save_ptr); cur_group:=c;
@y
cur_boundary:=save_ptr; cur_group:=c;
@!stat if tracing_groups>0 then group_trace(false);@+tats@;@/
incr(cur_level); incr(save_ptr);
@z
%---------------------------------------
@x [19] m.275 l.5941 eq_destroy - e-TeX sparse arrays
othercases do_nothing
@y
@/@<Cases for |eq_destroy|@>@/
othercases do_nothing
@z
%---------------------------------------
@x [19] m.277 l.5963 - e-TeX tracing
the call, since |eq_save| makes the necessary test.
@y
the call, since |eq_save| makes the necessary test.

@d assign_trace(#)==@!stat if tracing_assigns>0 then restore_trace(#);
  tats
@z
%---------------------------------------
@x [19] m.277 l.5967 eq_define - e-TeX tracing
begin if eq_level(p)=cur_level then eq_destroy(new_eqtb(p))
@y
label exit;
begin if eTeX_ex and(eq_type(p)=t)and(equiv(p)=e) then
  begin assign_trace(p,"reassigning")@;@/
  eq_destroy(new_eqtb(p)); return;
  end;
assign_trace(p,"changing")@;@/
if eq_level(p)=cur_level then eq_destroy(new_eqtb(p))
@z
%---------------------------------------
@x [19] m.277 l.5970 eq_define - e-TeX tracing
end;
@y
assign_trace(p,"into")@;@/
exit:end;
@z
%---------------------------------------
@x [19] m.278 l.5977 eq_word_define - e-TeX tracing
begin if xeq_level(p)<>cur_level then
@y
label exit;
begin if eTeX_ex and(eqtb[p].int=w) then
  begin assign_trace(p,"reassigning")@;@/
  return;
  end;
assign_trace(p,"changing")@;@/
if xeq_level(p)<>cur_level then
@z
%---------------------------------------
@x [19] m.278 l.5981 eq_word_define - e-TeX tracing
end;
@y
assign_trace(p,"into")@;@/
exit:end;
@z
%---------------------------------------
@x [19] m.279 l.5990 geq_define - e-TeX tracing
begin eq_destroy(new_eqtb(p));
set_eq_level(p,level_one); set_eq_type(p,t); set_equiv(p,e);
@y
begin assign_trace(p,"globally changing")@;@/
begin eq_destroy(new_eqtb(p));
set_eq_level(p,level_one); set_eq_type(p,t); set_equiv(p,e);
end;
assign_trace(p,"into")@;@/
@z
%---------------------------------------
@x [19] m.279 l.5995 geq_word_define - e-TeX tracing
begin set_new_eqtb_int(p,w); set_xeq_level(p,level_one);
@y
begin assign_trace(p,"globally changing")@;@/
begin set_new_eqtb_int(p,w); set_xeq_level(p,level_one);
end;
assign_trace(p,"into")@;@/
@z
%---------------------------------------
@x [19] m.281 l.6012 - e-TeX tracing
@p@t\4@>@<Declare the procedure called |restore_trace|@>@;@/
@y
@p
@z
%---------------------------------------
@x [19] m.281 l.6019 unsave - e-TeX optimized \aftergroup
begin if cur_level>level_one then
@y
@!a:boolean; {have we already processed an \.{\\aftergroup} ?}
begin a:=false;
if cur_level>level_one then
@z
%---------------------------------------
% FIXME restore_sa usage
@x [19] m.282 l.6033 - e-TeX sparse arrays
  else  begin if save_type(save_ptr)=restore_old_value then
@y
  else if save_type(save_ptr)=restore_sa then
    begin sa_restore; sa_chain:=p; sa_level:=save_level(save_ptr);
    end
  else  begin if save_type(save_ptr)=restore_old_value then
@z
%---------------------------------------
@x [19] m.282 l.6041 - e-TeX tracing
done: cur_group:=save_level(save_ptr); cur_boundary:=save_index(save_ptr)
@y
done: @!stat if tracing_groups>0 then group_trace(true);@+tats@;@/
if grp_stack[in_open]=cur_boundary then group_warning;
  {groups possibly not properly nested with files}
cur_group:=save_level(save_ptr); cur_boundary:=save_index(save_ptr);
if eTeX_ex then decr(save_ptr)
@z
%---------------------------------------
@x [19] m.284 l.6067 - e-TeX tracing
@ @<Declare the procedure called |restore_trace|@>=
@y
@ @<Declare \eTeX\ procedures for tr...@>=
@z
%---------------------------------------
@x [20] m.289 l.6155 - e-TeX protected
@d end_match_token=@"E0000 {$2^16\cdot|end_match|$}
@y
@d end_match_token=@"E0000 {$2^16\cdot|end_match|$}
@d protected_token=@"E0001 {$2^16\cdot|end_match|+1$}
@z
%---------------------------------------
@x [20] m.294 l.6280 - e-TeX protected
end_match: print("->");
@y
end_match: if c=0 then print("->");
@z
%---------------------------------------
@x [20] m.296 l.6301 print_meaning - e-TeX marks
else if cur_cmd=top_bot_mark then
@y
else if (cur_cmd=top_bot_mark)and(cur_chr<marks_code) then
@z
%---------------------------------------
@x [21] m.298 l.6375 print_cmd_chr - e-TeX protected
procedure print_cmd_chr(@!cmd:quarterword;@!chr_code:halfword);
@y
procedure print_cmd_chr(@!cmd:quarterword;@!chr_code:halfword);
var n:integer; {temp variable}
@z
%---------------------------------------
@x [21] m.299 l.6394 show_cur_cmd_chr - e-TeX tracing
@p procedure show_cur_cmd_chr;
@y
@p procedure show_cur_cmd_chr;
var n:integer; {level of \.{\\if...\\fi} nesting}
@!l:integer; {line where \.{\\if} started}
@!p:pointer;
@z
%---------------------------------------
@x [21] m.299 l.6399 show_cur_cmd_chr - e-TeX tracing
print_cmd_chr(cur_cmd,cur_chr); print_char("}");
@y
print_cmd_chr(cur_cmd,cur_chr);
if tracing_ifs>0 then
  if cur_cmd>=if_test then if cur_cmd<=fi_or_else then
    begin print(": ");
    if cur_cmd=fi_or_else then
      begin print_cmd_chr(if_test,cur_if); print_char(" ");
      n:=0; l:=if_line;
      end
    else  begin n:=1; l:=line;
      end;
    p:=cond_ptr;
    while p<>null do
      begin incr(n); p:=link(p);
      end;
    print("(level "); print_int(n); print_char(")"); print_if_line(l);
    end;
print_char("}");
@z
%---------------------------------------
% FIXME: What about \write18 compatibility?
@x [22] m.303 l.6475 show_context - e-TeX scan_tokens
the terminal, under control of the procedure |read_toks|.)
@y
the terminal, under control of the procedure |read_toks|.)
Finally |18<=name<=19| indicates that we are reading a pseudo file
created by the \.{\\scantokens} command.
@z
%---------------------------------------
@x [22] m.307 l.6692 - e-TeX basic
@d mark_text=14 {|token_type| code for \.{\\topmark}, etc.}
@d write_text=15 {|token_type| code for \.{\\write}}
@y
@d every_eof_text=14 {|token_type| code for \.{\\everyeof}}
@d mark_text=15 {|token_type| code for \.{\\topmark}, etc.}
@d write_text=16 {|token_type| code for \.{\\write}}
@z
%---------------------------------------
@x [22] m.311 l.6749 show_context - e-TeX scan_tokens
    if (name>17) or (base_ptr=0) then bottom_line:=true;
@y
    if (name>19) or (base_ptr=0) then bottom_line:=true;
@z
%---------------------------------------
@x [22] m.313 l.6794 - e-TeX scan_tokens
else  begin print_nl("l."); print_int(line);
@y
else  begin print_nl("l.");
  if index=in_open then print_int(line)
  else print_int(line_stack[index+1]); {input from a pseudo file}
@z
%---------------------------------------
@x [22] m.314 l.6814 - e-TeX basic
every_cr_text: print_nl("<everycr> ");
@y
every_cr_text: print_nl("<everycr> ");
every_eof_text: print_nl("<everyeof> ");
@z
%---------------------------------------
@x [23] m.326 l.7009 - e-TeX optimized \aftergroup
begin t:=cur_tok; cur_tok:=p; back_input; cur_tok:=t;
@y
begin t:=cur_tok; cur_tok:=p;
if a then
  begin p:=get_avail; info(p):=cur_tok; link(p):=loc; loc:=p; start:=p;
  if cur_tok<right_brace_limit then
    if cur_tok<left_brace_limit then decr(align_state)
    else incr(align_state);
  end
else  begin back_input; a:=eTeX_ex;
  end;
cur_tok:=t;
@z
%---------------------------------------
@x [23] m.328 l.7037 begin_file_reading - e-TeX every_eof, tracing_nesting
incr(in_open); push_input; index:=in_open;
@y
incr(in_open); push_input; index:=in_open;
eof_seen[index]:=false;
grp_stack[index]:=cur_boundary; if_stack[index]:=cond_ptr;
@z
%---------------------------------------
% FIXME: What about \write18 compatibility?
@x [23] m.329 l.7047 end_file_reading - e-TeX scan_tokens
if name>17 then a_close(cur_file); {forget it}
@y
if (name=18)or(name=19) then pseudo_close else
if name>17 then a_close(cur_file); {forget it}
@z
%---------------------------------------
@x [23] m.331 l.7066 - e-TeX tracing_nesting
in_open:=0; open_parens:=0; max_buf_stack:=0;
@y
in_open:=0; open_parens:=0; max_buf_stack:=0;
grp_stack[0]:=0; if_stack[0]:=null;
@z
%---------------------------------------
@x [24] m.362 l.7538 - e-TeX scan_tokens, every_eof
if not force_eof then
@y
if not force_eof then
  if name<=19 then
    begin if pseudo_input then {not end of file}
      firm_up_the_line {this sets |limit|}
    else if (every_eof<>null)and not eof_seen[index] then
      begin limit:=first-1; eof_seen[index]:=true; {fake one empty line}
      begin_token_list(every_eof,every_eof_text); goto restart;
      end
    else force_eof:=true;
    end
  else
@z
%---------------------------------------
@x [24] m.362 l.7541 - e-TeX every_eof
  else force_eof:=true;
@y
  else if (every_eof<>null)and not eof_seen[index] then
    begin limit:=first-1; eof_seen[index]:=true; {fake one empty line}
    begin_token_list(every_eof,every_eof_text); goto restart;
    end
  else force_eof:=true;
@z
%---------------------------------------
@x [24] m.362 l.7544 - e-TeX scan_tokens
  begin print_char(")"); decr(open_parens);
  update_terminal; {show user that file has been read}
@y
  begin if tracing_nesting>0 then
    if (grp_stack[in_open]<>cur_boundary)or@|
        (if_stack[in_open]<>cond_ptr) then file_warning;
    {give warning for some unfinished groups and/or conditionals}
  if name>=19 then
  begin print_char(")"); decr(open_parens);
  update_terminal; {show user that file has been read}
  end;
@z
%---------------------------------------
@x [25] m.366 l.7630 - e-TeX basic
@t\4@>@<Declare the procedure called |insert_relax|@>@;@/
@y
@t\4@>@<Declare the procedure called |insert_relax|@>@;@/
@t\4@>@<Declare \eTeX\ procedures for expanding@>@;@/
@z
%---------------------------------------
@x [25] m.366 l.7637 expand - e-TeX cond
procedure expand;
@y
procedure expand;
label reswitch;
@z
%---------------------------------------
@x [25] m.366 l.7648 expand - e-TeX cond
if cur_cmd<call then @<Expand a nonmacro@>
@y
reswitch:
if cur_cmd<call then @<Expand a nonmacro@>
@z
%---------------------------------------
@x [25] m.367 l.7659 - e-TeX cond
expand_after:@<Expand the token after the next token@>;
@y
expand_after:if cur_chr=0 then @<Expand the token after the next token@>
  else @<Negate a boolean conditional and |goto reswitch|@>;
@z
%---------------------------------------
@x [25] m.377 l.7779 - e-TeX scan_tokens
input: if chr_code=0 then print_esc("input")@+else print_esc("endinput");
@y
input: if chr_code=0 then print_esc("input")
  @/@<Cases of |input| for |print_cmd_chr|@>@/
  else print_esc("endinput");
@z
%---------------------------------------
@x [25] m.378 l.7782 - e-TeX scan_tokens
if cur_chr>0 then force_eof:=true
@y
if cur_chr=1 then force_eof:=true
@/@<Cases for |input|@>@/
@z
%---------------------------------------
@x [25] m.382 l.7838 - e-TeX marks
@d top_mark_code=0 {the mark in effect at the previous page break}
@y
@d marks_code==5 {add this for \.{\\topmarks} etc.}
@#
@d top_mark_code=0 {the mark in effect at the previous page break}
@z
%---------------------------------------
@x [25] m.385 l.7870 - e-TeX marks
top_bot_mark: case chr_code of
@y
top_bot_mark: begin case (chr_code mod marks_code) of
@z
%---------------------------------------
@x [25] m.385 l.7876 - e-TeX marks
  endcases;
@y
  endcases;
  if chr_code>=marks_code then print_char("s");
  end;
@z
%---------------------------------------
@x [25] m.386 l.7882 - e-TeX marks
begin if cur_mark[cur_chr]<>null then
  begin_token_list(cur_mark[cur_chr],mark_text);
@y
begin t:=cur_chr mod marks_code;
if cur_chr>=marks_code then scan_register_num@+else cur_val:=0;
if cur_val=0 then cur_ptr:=cur_mark[t]
else @<Compute the mark pointer for mark type |t| and class |cur_val|@>;
if cur_ptr<>null then begin_token_list(cur_ptr,mark_text);
@z
%---------------------------------------
@x [25] m.389 l.7945 macro_call - e-TeX protected
if info(r)<>end_match_token then
@y
if info(r)=protected_token then r:=link(r);
if info(r)<>end_match_token then
@z
%---------------------------------------
@x [26] m.409 l.8255 - e-TeX basic
@t\4\4@>@<Declare procedures that scan font-related stuff@>
@y
@t\4\4@>@<Declare \eTeX\ procedures for scanning@>@;
@t\4\4@>@<Declare procedures that scan font-related stuff@>
@z
%---------------------------------------
@x [26] m.411 l.8299 - e-TeX sparse arrays
|glue_val|, or |mu_val|.
@y
|glue_val|, or |mu_val| more than |mem_bot| (dynamic variable-size nodes
cannot have these values)
@z
%---------------------------------------
@x [26] m.411 l.8302 - e-TeX sparse arrays
primitive("count",register,int_val);
@!@:count_}{\.{\\count} primitive@>
primitive("dimen",register,dimen_val);
@!@:dimen_}{\.{\\dimen} primitive@>
primitive("skip",register,glue_val);
@!@:skip_}{\.{\\skip} primitive@>
primitive("muskip",register,mu_val);
@y
primitive("count",register,mem_bot+int_val);
@!@:count_}{\.{\\count} primitive@>
primitive("dimen",register,mem_bot+dimen_val);
@!@:dimen_}{\.{\\dimen} primitive@>
primitive("skip",register,mem_bot+glue_val);
@!@:skip_}{\.{\\skip} primitive@>
primitive("muskip",register,mem_bot+mu_val);
@z
%---------------------------------------
@x [26] m.412 l.8312 - e-TeX sparse arrays
register: if chr_code=int_val then print_esc("count")
  else if chr_code=dimen_val then print_esc("dimen")
  else if chr_code=glue_val then print_esc("skip")
  else print_esc("muskip");
@y
register: @<Cases of |register| for |print_cmd_chr|@>;
@z
%---------------------------------------
@x [26] m.413 l.8328 scan_something_internal - e-TeX basic
var m:halfword; {|chr_code| part of the operand token}
@y
label exit;
var m:halfword; {|chr_code| part of the operand token}
@!q:halfword; {general purpose index}
@!i:four_quarters; {character info}
@z
%---------------------------------------
@x [26] m.413 l.8354 scan_something_internal - e-TeX basic
end;
@y
exit:end;
@z
%---------------------------------------
@x [26] m.415 l.8375 - e-TeX sparse arrays
    begin scan_eight_bit_int; m:=toks_base+cur_val;
    end;
  scanned_result(equiv(m))(tok_val);
@y
    if m=mem_bot then
      begin scan_register_num;
      if cur_val<256 then cur_val:=equiv(toks_base+cur_val)
      else  begin find_sa_element(tok_val,cur_val,false);
        if cur_ptr=null then cur_val:=null
        else cur_val:=sa_ptr(cur_ptr);
        end;
      end
    else cur_val:=sa_ptr(m)
  else cur_val:=equiv(m);
  cur_val_level:=tok_val;
@z
%---------------------------------------
@x [26] m.416 l.8390 - e-TeX basic
|glue_val|, |input_line_no_code|, or |badness_code|.

@d input_line_no_code=glue_val+1 {code for \.{\\inputlineno}}
@d badness_code=glue_val+2 {code for \.{\\badness}}
@y
|glue_val|, |last_node_type_code|, |input_line_no_code|, |badness_code|,
|eTeX_version_code|, or one of the other codes for \eTeX\ extensions.

@d last_node_type_code=glue_val+1 {code for \.{\\lastnodetype}}
@d input_line_no_code=glue_val+2 {code for \.{\\inputlineno}}
@d badness_code=glue_val+3 {code for \.{\\badness}}
@d eTeX_int=glue_val+4 {first of \eTeX\ codes for integers}
@d eTeX_dim=eTeX_int+8 {first of \eTeX\ codes for dimensions}
@d eTeX_glue=eTeX_dim+9 {first of \eTeX\ codes for glue}
@d eTeX_mu=eTeX_glue+1 {first of \eTeX\ codes for muglue}
@d eTeX_expr=eTeX_mu+1 {first of \eTeX\ codes for expressions}
@z
%---------------------------------------
@x [26] m.417 l.8425 - e-TeX interaction_mode
@+else print_esc("insertpenalties");
@y
@/@<Cases of |set_page_int| for |print_cmd_chr|@>@/
@+else print_esc("insertpenalties");
@z
%---------------------------------------
@x [26] m.417 l.8434 - e-TeX basic
  othercases print_esc("badness")
@y
  @/@<Cases of |last_item| for |print_cmd_chr|@>@/
  othercases print_esc("badness")
@z
%---------------------------------------
@x [26] m.419 l.8457 - e-TeX interaction_mode
begin if m=0 then cur_val:=dead_cycles@+else cur_val:=insert_penalties;
@y
begin if m=0 then cur_val:=dead_cycles
@/@<Cases for `Fetch the |dead_cycles| or the |insert_penalties|'@>@/
else cur_val:=insert_penalties;
@z
%---------------------------------------
@x [26] m.420 l.8462 - e-TeX sparse arrays
begin
   scan_eight_bit_int;
   if box(cur_val)=null then cur_val:=0 @+else
   cur_val:=mem[box(cur_val)+m].sc;
@y
begin
   scan_register_num;
   fetch_box(q);
   if q=null then cur_val:=0 @+else
   cur_val:=mem[q+m].sc;
@z
%---------------------------------------
@x [26] m.423 l.8487 - e-TeX penalties
begin if par_shape_ptr=null then cur_val:=0
@y
begin if m>par_shape_loc then @<Fetch a penalties array element@>
else if par_shape_ptr=null then cur_val:=0
@z
%---------------------------------------
@x [26] m.424 l.8499 - e-TeX basic
if cur_chr>glue_val then
  begin if cur_chr=input_line_no_code then cur_val:=line
  else cur_val:=last_badness; {|cur_chr=badness_code|}
@y
if m>last_node_type_code then
 if m>=eTeX_glue then @<Process an expression and |return|@>@;
 else if m>=eTeX_dim then
  begin case m of
  @/@<Cases for fetching a dimension value@>@/
  end; {there are no other cases}
  cur_val_level:=dimen_val;
  end
 else begin case m of
  input_line_no_code: cur_val:=line;
  badness_code: cur_val:=last_badness;
  @/@<Cases for fetching an integer value@>@/
  end; {there are no other cases}
@z
%---------------------------------------
@x [26] m.424 l.8505 - e-TeX last_node_type
  cur_val_level:=cur_chr;
@y
  if cur_chr=last_node_type_code then
    begin cur_val:=int_val;
    if (tail=head)or(mode=0) then cur_val:=-1;
    end
  else cur_val_level:=cur_chr;
@z
%---------------------------------------
@x [26] m.424 l.8513 - e-TeX last_node_type
      end;
@y
      end;
    last_node_type_code:
      if (type(tail)<>math_node)or(subtype(tail)<>end_M_code) then
        if type(tail)<=unset_node then cur_val:=type(tail)+1
        else cur_val:=unset_node+2;
@z
%---------------------------------------
@x [26] m.424 l.8519 - e-TeX last_node_type
    glue_val: if last_glue<>max_halfword then cur_val:=last_glue;
@y
    glue_val: if last_glue<>max_halfword then cur_val:=last_glue;
    last_node_type_code: cur_val:=last_node_type;
@z
%---------------------------------------
% FIXME might need work
@x [26] m.427 l.8535 - e-TeX sparse arrays
begin scan_eight_bit_int;
case m of
@y
begin if (m<mem_bot)or(m>lo_mem_stat_max) then
  begin cur_val_level:=sa_type(m);
  if cur_val_level<glue_val then cur_val:=sa_int(m)
  else cur_val:=sa_ptr(m);
  end
else  begin scan_register_num; cur_val_level:=m-mem_bot;
  if cur_val>255 then
    begin find_sa_element(cur_val_level,cur_val,false);
    if cur_ptr=null then
      if cur_val_level<glue_val then cur_val:=0
      else cur_val:=zero_glue
    else if cur_val_level<glue_val then cur_val:=sa_int(cur_ptr)
    else cur_val:=sa_ptr(cur_ptr);
    end
  else
  case cur_val_level of
@z
%---------------------------------------
@x [26] m.427 l.8542 - e-TeX sparse arrays
cur_val_level:=m;
@y
  end;
@z
%---------------------------------------
@x [26] m.461 l.9070 - e-TeX expr
exit:end;
@y
exit:end;
@#
@<Declare procedures needed for expressions@>@;
@z
%---------------------------------------
@x [27] m.464 l.9129 - e-TeX basic
@p function str_toks(@!b:pool_pointer):pointer;
@y
@p @t\4@>@<Declare \eTeX\ procedures for token lists@>@;@/
function str_toks(@!b:pool_pointer):pointer;
@z
%---------------------------------------
@x [27] m.465 l.9154 the_toks - e-TeX unexpanded
@p function the_toks:pointer;
@y
@p function the_toks:pointer;
label exit;
@z
%---------------------------------------
@x [27] m.465 l.9158 the_toks - e-TeX unexpanded
begin get_x_token; scan_something_internal(tok_val,false);
@y
@!c:small_number; {value of |cur_chr|}
begin @<Handle \.{\\unexpanded} or \.{\\detokenize} and |return|@>;@/
get_x_token; scan_something_internal(tok_val,false);
@z
%---------------------------------------
@x [27] m.465 l.9172 the_toks - e-TeX unexpanded
end;
@y
exit:end;
@z
%---------------------------------------
@x [27] m.469 l.9223 - e-TeX basic
  othercases print_esc("jobname")
@y
  @/@<Cases of |convert| for |print_cmd_chr|@>@/
  othercases print_esc("jobname")
@z
%---------------------------------------
@x [27] m.470 l.9232 conv_toks - e-TeX basic
@!c:number_code..job_name_code; {desired type of conversion}
@y
@!c:small_number; {desired type of conversion}
@z
%---------------------------------------
@x [27] m.471 l.9249 - e-TeX basic
end {there are no other cases}
@y
@/@<Cases of `Scan the argument for command |c|'@>@/
end {there are no other cases}
@z
%---------------------------------------
@x [27] m.472 l.9265 - e-TeX basic
end {there are no other cases}
@y
@/@<Cases of `Print the result of command |c|'@>@/
end {there are no other cases}
@z
%---------------------------------------
@x [27] m.478 l.9374 - e-TeX protected
  if cur_cmd<=max_command then goto done2;
@y
  if cur_cmd>=call then
    if info(link(cur_chr))=protected_token then
      begin cur_cmd:=relax; cur_chr:=no_expand_flag;
      end;
  if cur_cmd<=max_command then goto done2;
@z
%---------------------------------------
@x [27] m.482 l.9422 read_toks - e-TeX read_line
@p procedure read_toks(@!n:integer;@!r:pointer);
@y
@p procedure read_toks(@!n:integer;@!r:pointer;@!j:halfword);
@z
%---------------------------------------
@x [27] m.483 l.9448 - e-TeX read_line
loop@+  begin get_token;
@y
@<Handle \.{\\readline} and |goto done|@>;@/
loop@+  begin get_token;
@z
%---------------------------------------
@x [28] m.487 l.9500 - e-TeX cond
@d if_char_code=0 { `\.{\\if}' }
@y
@d unless_code=32 {amount added for `\.{\\unless}' prefix}
@#
@d if_char_code=0 { `\.{\\if}' }
@z
%---------------------------------------
@x [28] m.488 l.9555 - e-TeX cond
if_test: case chr_code of
@y
if_test: begin if chr_code>=unless_code then print_esc("unless");
case chr_code mod unless_code of
@z
%---------------------------------------
@x [28] m.488 l.9572 - e-TeX cond
  othercases print_esc("if")
  endcases;
@y
  @/@<Cases of |if_test| for |print_cmd_chr|@>@/
  othercases print_esc("if")
  endcases;
end;
@z
%---------------------------------------
@x [28] m.494 l.9646 pass_text - e-TeX tracing
done: scanner_status:=save_scanner_status;
@y
done: scanner_status:=save_scanner_status;
if tracing_ifs>0 then show_cur_cmd_chr;
@z
%---------------------------------------
@x [28] m.496 l.9662 - e-TeX tracing_nesting
begin p:=cond_ptr; if_line:=if_line_field(p);
@y
begin if if_stack[in_open]=cond_ptr then if_warning;
  {conditionals possibly not properly nested with files}
p:=cond_ptr; if_line:=if_line_field(p);
@z
%---------------------------------------
@x [28] m.498 l.9699 conditional - e-TeX cond
begin @<Push the condition stack@>;@+save_cond_ptr:=cond_ptr;this_if:=cur_chr;@/
@<Either process \.{\\ifcase} or set |b| to the value of a boolean condition@>;
@y
@!is_unless:boolean; {was this if preceded by `\.{\\unless}' ?}
begin if tracing_ifs>0 then if tracing_commands<=1 then show_cur_cmd_chr;
@<Push the condition stack@>;@+save_cond_ptr:=cond_ptr;
is_unless:=(cur_chr>=unless_code); this_if:=cur_chr mod unless_code;@/
@<Either process \.{\\ifcase} or set |b| to the value of a boolean condition@>;
if is_unless then b:=not b;
@z
%---------------------------------------
@x [28] m.501 l.9742 - e-TeX cond
if_false_code: b:=false;
@y
if_false_code: b:=false;
@/@<Cases for |conditional|@>@/
@z
%---------------------------------------
@x [28] m.505 l.9781 - e-TeX sparse arrays
begin scan_eight_bit_int; p:=box(cur_val);
@y
begin scan_register_num; fetch_box(p);
@z
%---------------------------------------
@x [28] m.510 l.9871 - e-TeX cond
if cur_chr>if_limit then
@y
begin if tracing_ifs>0 then if tracing_commands<=1 then show_cur_cmd_chr;
if cur_chr>if_limit then
@z
%---------------------------------------
@x [28] m.510 l.9882 - e-TeX cond
  end
@y
  end;
end
@z
%---------------------------------------
@x [29] m.536 l.10324 - e-TeX basic
begin wlog(banner);
@y
begin wlog(eTeX_banner);
@z
%---------------------------------------
@x [29] m.536 l.10331 - e-TeX basic
end
@y
if eTeX_ex then
  begin; wlog_cr; wlog('entering extended mode');
  end;
end
@z
%---------------------------------------
@x [30] m.581 l.11263 char_warning - e-TeX tracing
begin if tracing_lost_chars>0 then
@y
var old_setting: integer; {saved value of |tracing_online|}
begin if tracing_lost_chars>0 then
 begin old_setting:=tracing_online;
 if eTeX_ex and(tracing_lost_chars>1) then tracing_online:=1;
@z
%---------------------------------------
@x [30] m.581 l.11270 char_warning - e-TeX tracing
end;
@y
 tracing_online:=old_setting;
 end;
end;
@z
%---------------------------------------
@x [34] m.687 l.13480 - e-TeX middle
\TeX's \.{\\left} and \.{\\right}. The |nucleus| of such noads is
@y
\TeX's \.{\\left} and \.{\\right} as well as \eTeX's \.{\\middle}.
The |nucleus| of such noads is
@z
%---------------------------------------
@x [34] m.687 l.13497 - e-TeX middle
@d delimiter==nucleus {|delimiter| field in left and right noads}
@y
@d delimiter==nucleus {|delimiter| field in left and right noads}
@d middle_noad==1 {|subtype| of right noad representing \.{\\middle}}
@z
%---------------------------------------
@x [34] m.696 l.13670 - e-TeX middle
right_noad: begin print_esc("right"); print_delimiter(delimiter(p));
  end;
end;
if subtype(p)<>normal then
  if subtype(p)=limits then print_esc("limits")
  else print_esc("nolimits");
if type(p)<left_noad then print_subsidiary_data(nucleus(p),".");
@y
right_noad: begin if subtype(p)=normal then print_esc("right")
  else print_esc("middle");
  print_delimiter(nucleus(p));
  end;
end;
if type(p)<left_noad then
  begin if subtype(p)<>normal then
    if subtype(p)=limits then print_esc("limits")
    else print_esc("nolimits");
  print_subsidiary_data(nucleus(p),".");
  end;
@z
%---------------------------------------
@x [36] m.727 l.14268 - e-TeX middle
done_with_noad: r:=q; r_type:=type(r);
@y
done_with_noad: r:=q; r_type:=type(r);
if r_type=right_noad then
  begin r_type:=left_noad; cur_style:=style; @<Set up the values...@>;
  end;
@z
%---------------------------------------
@x [36] m.760 l.14929 - e-TeX middle
  r_type:=t;
@y
  if type(q)=right_noad then t:=open_noad;
  r_type:=t;
@z
%---------------------------------------
@x [36] m.762 l.14970 make_left_right - e-TeX middle
begin if style<script_style then cur_size:=text_size
else cur_size:=script_size*((style-text_style) div 2);
@y
begin cur_style:=style; @<Set up the values...@>;
@z
%---------------------------------------
@x [37] m.785 l.15461 align_peek - e-TeX protected
begin restart: align_state:=1000000; @<Get the next non-blank non-call token@>;
@y
begin restart: align_state:=1000000;
repeat get_x_or_protected;
until cur_cmd<>spacer;
@z
%---------------------------------------
@x [37] m.791 l.15572 fin_col - e-TeX protected
align_state:=1000000; @<Get the next non-blank non-call token@>;
@y
align_state:=1000000;
repeat get_x_or_protected;
until cur_cmd<>spacer;
@z
%---------------------------------------
@x [38] m.814 l.15975 - e-TeX penalties
There is one explicit parameter:  |final_widow_penalty| is the amount of
additional penalty to be inserted before the final line of the paragraph.
@y
There is one explicit parameter:  |d| is true for partial paragraphs
preceding display math mode; in this case the amount of additional
penalty inserted before the final line is |display_widow_penalty|
instead of |widow_penalty|.
@z
%---------------------------------------
@x [38] m.815 l.16002 line_break - e-TeX penalties
procedure line_break(@!final_widow_penalty:integer);
@y
procedure line_break(@!d:boolean);
@z
%---------------------------------------
@x [38] m.815 l.16012 - e-TeX basic
end;
@y
end;
@#
@t\4@>@<Declare \eTeX\ procedures for use by |main_control|@>
@z
%---------------------------------------
@x [38] m.816 l.16032 - e-TeX last_line_fit
final_par_glue:=new_param_glue(par_fill_skip_code);
link(tail):=final_par_glue;
@y
final_par_glue:=new_param_glue(par_fill_skip_code);
link(tail):=final_par_glue;
last_line_fill:=link(tail);
@z
%---------------------------------------
@x [38] m.819 l.16097 - e-TeX last_line_fit
@d active_node_size=3 {number of words in active nodes}
@y
@d active_node_size_normal=3 {number of words in normal active nodes}
@z
%---------------------------------------
% FIXME might need fixes; TeX has 6 backgrounds
% Omega has 7. Why the difference?
@x [38] m.827 l.16260 - e-TeX last_line_fit
background[7]:=shrink(q)+shrink(r);
@y
background[7]:=shrink(q)+shrink(r);
@<Check for special treatment of last line of paragraph@>;
@z
%---------------------------------------
@x [38] m.829 l.16311 try_break - e-TeX last_line_fit
label exit,done,done1,continue,deactivate;
@y
label exit,done,done1,continue,deactivate,found,not_found;
@z
%---------------------------------------
@x [38] m.845 l.16603 - e-TeX last_line_fit
total_demerits(q):=minimal_demerits[fit_class];
@y
total_demerits(q):=minimal_demerits[fit_class];
if do_last_line_fit then
  @<Store \(a)additional data in the new active node@>;
@z
%---------------------------------------
@x [38] m.846 l.16616 - e-TeX last_line_fit
print(" t="); print_int(total_demerits(q));
@y
print(" t="); print_int(total_demerits(q));
if do_last_line_fit then @<Print additional data in the new active node@>;
@z
%---------------------------------------
@x [38] m.851 l.16715 - e-TeX last_line_fit
if (b>inf_bad)or(pi=eject_penalty) then
@y
if do_last_line_fit then @<Adjust \(t)the additional data for last line@>;
found:
if (b>inf_bad)or(pi=eject_penalty) then
@z
%---------------------------------------
@x [38] m.852 l.16739 - e-TeX last_line_fit
  begin b:=0; fit_class:=decent_fit; {infinite stretch}
@y
  begin if do_last_line_fit then
    begin if cur_p=null then {the last line of a paragraph}
      @<Perform computations for last line and |goto found|@>;
    shortfall:=0;
    end;
  b:=0; fit_class:=decent_fit; {infinite stretch}
@z
%---------------------------------------
@x [38] m.855 l.16796 - e-TeX last_line_fit
  best_place[fit_class]:=break_node(r); best_pl_line[fit_class]:=l;
@y
  best_place[fit_class]:=break_node(r); best_pl_line[fit_class]:=l;
  if do_last_line_fit then
    @<Store \(a)additional data for this feasible break@>;
@z
%---------------------------------------
@x [39] m.863 l.16969 - e-TeX last_line_fit
  end;@+tats@/
@y
  end;@+tats@/
if do_last_line_fit then @<Adjust \(t)the final line of the paragraph@>;
@z
%---------------------------------------
@x [39] m.864 l.16980 - e-TeX last_line_fit
line_number(q):=prev_graf+1; total_demerits(q):=0; link(active):=q;
@y
line_number(q):=prev_graf+1; total_demerits(q):=0; link(active):=q;
if do_last_line_fit then
  @<Initialize additional fields of the first active node@>;
@z
%---------------------------------------
@x [39] m.876 l.17192 - e-TeX penalties
post_line_break(final_widow_penalty)
@y
post_line_break(d)
@z
%---------------------------------------
@x [39] m.877 l.17206 post_line_break - e-TeX penalties
procedure post_line_break(@!final_widow_penalty:integer);
@y
procedure post_line_break(@!d:boolean);
@z
%---------------------------------------
% FIXME possible conflict
@x [39] m.890 l.17395 - e-TeX penalties
  if passive_pen_inter(cur_p)<>0 then
      pen:=passive_pen_inter(cur_p)
  else pen:=inter_line_penalty;
  if cur_line=prev_graf+1 then pen:=pen+club_penalty;
  if cur_line+2=best_line then pen:=pen+final_widow_penalty;
@y
  q:=inter_line_penalties_ptr;
  if q<>null then
    begin r:=cur_line;
    if r>penalty(q) then r:=penalty(q);
    pen:=penalty(q+r);
    end
  else begin
  if passive_pen_inter(cur_p)<>0 then
      pen:=passive_pen_inter(cur_p)
  else pen:=inter_line_penalty;
  end;
  q:=club_penalties_ptr;
  if q<>null then
    begin r:=cur_line-prev_graf;
    if r>penalty(q) then r:=penalty(q);
    pen:=pen+penalty(q+r);
    end
  else if cur_line=prev_graf+1 then pen:=pen+club_penalty;
  if d then q:=display_widow_penalties_ptr
  else q:=widow_penalties_ptr;
  if q<>null then
    begin r:=best_line-cur_line-1;
    if r>penalty(q) then r:=penalty(q);
    pen:=pen+penalty(q+r);
    end
  else if cur_line+2=best_line then
    if d then pen:=pen+display_widow_penalty
    else pen:=pen+widow_penalty;
@z
%---------------------------------------
@x [40] m.891 l.17460 - e-TeX hyph_codes
cur_lang:=init_cur_lang; l_hyf:=init_l_hyf; r_hyf:=init_r_hyf;
@y
cur_lang:=init_cur_lang; l_hyf:=init_l_hyf; r_hyf:=init_r_hyf;
set_hyph_index;
@z
%---------------------------------------
@x [40] m.896 l.17529 - e-TeX hyph_codes
  if lc_code(c)<>0 then
    if (lc_code(c)=c)or(uc_hyph>0) then goto done2
@y
  set_lc_code(c);
  if hc[0]<>0 then
    if (hc[0]=c)or(uc_hyph>0) then goto done2
@z
%---------------------------------------
@x [40] m.897 l.17546 - e-TeX hyph_codes
    if lc_code(c)=0 then goto done3;
    if lc_code(c)>max_hyph_char then goto done3;
    if hn=63 then goto done3;
    hb:=s; incr(hn); hu[hn]:=c; hc[hn]:=lc_code(c); hyf_bchar:=non_char;
@y
    set_lc_code(c);
    if hc[0]=0 then goto done3;
    if lc_code(c)>max_hyph_char then goto done3;
    if hn=63 then goto done3;
    hb:=s; incr(hn); hu[hn]:=c; hc[hn]:=hc[0]; hyf_bchar:=non_char;
@z
%---------------------------------------
@x [40] m.898 l.17572 - e-TeX hyph_codes
  if lc_code(c)=0 then goto done3;
  if lc_code(c)>max_hyph_char then goto done3;
  if j=63 then goto done3;
  incr(j); hu[j]:=c; hc[j]:=lc_code(c);@/
@y
  set_lc_code(c);
  if hc[0]=0 then goto done3;
  if lc_code(c)>max_hyph_char then goto done3;
  if j=63 then goto done3;
  incr(j); hu[j]:=c; hc[j]:=hc[0];@/
@z
%---------------------------------------
@x [42] m.934 l.18211 new_hyph_exceptions - e-TeX hyph_codes
label reswitch, exit, found, not_found;
@y
label reswitch, exit, found, not_found, not_found1;
@z
%---------------------------------------
@x [42] m.934 l.18221 new_hyph_exceptions - e-TeX hyph_codes
set_cur_lang;
@y
set_cur_lang;
@!init if trie_not_ready then
  begin hyph_index:=0; goto not_found1;
  end;
tini@/
set_hyph_index;
not_found1:
@z
%---------------------------------------
@x [42] m.937 l.18253 - e-TeX hyph_codes
else  begin if lc_code(cur_chr)=0 then
@y
else  begin set_lc_code(cur_chr);
  if hc[0]=0 then
@z
%---------------------------------------
@x [42] m.937 l.18261 - e-TeX hyph_codes
    begin incr(n); hc[n]:=lc_code(cur_chr);
@y
    begin incr(n); hc[n]:=hc[0];
@z
%---------------------------------------
@x [43] m.952 l.18549 - e-TeX hyph_codes
trie_root:=compress_trie(trie_root); {identify equivalent subtries}
@y
hyph_root:=compress_trie(hyph_root);
trie_root:=compress_trie(trie_root); {identify equivalent subtries}
@z
%---------------------------------------
@x [43] m.958 l.18637 - e-TeX hyph_codes
if trie_root=0 then {no patterns were given}
@y
if trie_max=0 then {no patterns were given}
@z
%---------------------------------------
@x [43] m.958 l.18641 - e-TeX hyph_codes
else begin trie_fix(trie_root); {this fixes the non-holes in |trie|}
@y
else begin if hyph_root>0 then trie_fix(hyph_root);
  if trie_root>0 then trie_fix(trie_root); {this fixes the non-holes in |trie|}
@z
%---------------------------------------
@x [43] m.960 l.18684 new_patterns - e-TeX hyph_codes
  brace@>;
@y
  brace@>;
  if saving_hyph_codes>0 then
    @<Store hyphenation codes for current language@>;
@z
%---------------------------------------
@x [43] m.966 l.18791 init_trie - e-TeX hyph_codes
@<Move the data into |trie|@>;
@y
if hyph_root<>0 then @<Pack all stored |hyph_codes|@>;
@<Move the data into |trie|@>;
@z
%---------------------------------------
@x [44] m.968 l.18807 - e-TeX saved_items
whenever this is possible without backspacing.
@y
whenever this is possible without backspacing.

When the second argument |s| is |false| the deleted nodes are destroyed,
otherwise they are collected in a list starting at |split_disc|.
@z
%---------------------------------------
@x [44] m.968 l.18814 prune_page_top - e-TeX saved_items
@p function prune_page_top(@!p:pointer):pointer; {adjust top after page break}
var prev_p:pointer; {lags one step behind |p|}
@!q:pointer; {temporary variable for list manipulation}
@y
@p function prune_page_top(@!p:pointer;@!s:boolean):pointer;
  {adjust top after page break}
var prev_p:pointer; {lags one step behind |p|}
@!q,@!r:pointer; {temporary variables for list manipulation}
@z
%---------------------------------------
@x [44] m.968 l.18825 prune_page_top - e-TeX saved_items
    link(prev_p):=p; flush_node_list(q);
@y
    link(prev_p):=p;
    if s then
      begin if split_disc=null then split_disc:=q@+else link(r):=q;
      r:=q;
      end
    else flush_node_list(q);
@z
%---------------------------------------
@x [44] m.977 l.18992 vsplit - e-TeX marks, sparse arrays
@p function vsplit(@!n:eight_bits; @!h:scaled):pointer;
@y
@p @t\4@>@<Declare the function called |do_marks|@>@;
function vsplit(@!n:halfword; @!h:scaled):pointer;
@z
%---------------------------------------
% FIXME incompatible?
@x [44] m.977 l.18998 vsplit - e-TeX sparse arrays
begin v:=box(n); vdir:=box_dir(v);
@y
begin cur_val:=n; fetch_box(v); vdir:=box_dir(v);
@z
%---------------------------------------
% FIXME: how to detach this from sparse arrays?
@x [44] m.977 l.18999 vsplit - e-TeX marks, saved_items
if split_first_mark<>null then
@y
flush_node_list(split_disc); split_disc:=null;
if sa_mark<>null then
  if do_marks(vsplit_init,0,sa_mark) then sa_mark:=null;
if split_first_mark<>null then
@z
%---------------------------------------
@x [44] m.977 l.19007 vsplit - e-TeX saved_items
q:=prune_page_top(q); p:=list_ptr(v); free_node(v,box_node_size);
@y
q:=prune_page_top(q,saving_vdiscards>0);
p:=list_ptr(v); free_node(v,box_node_size);
@z
%---------------------------------------
% FIXME might be incompatible?
@x [44] m.977 l.19008 vsplit - e-TeX sparse arrays
if q=null then set_equiv(box_base+n,null)
    {the |eq_level| of the box stays the same}
else set_equiv(box_base+n,vpack(q,natural));
@y
if q<>null then q:=vpack(q,natural);
change_box(q); {the |eq_level| of the box stays the same}
@z
%---------------------------------------
@x [44] m.979 l.19033 - e-TeX marks
    if split_first_mark=null then
@y
    if mark_class(p)<>0 then @<Update the current marks for |vsplit|@>
    else if split_first_mark=null then
@z
%---------------------------------------
@x [45] m.982 l.19167 - e-TeX last_node_type
The variables |last_penalty| and |last_kern| are similar.  And
@y
The variables |last_penalty|, |last_kern|, and |last_node_type|
are similar.  And
@z
%---------------------------------------
@x [45] m.982 l.19180 - e-TeX last_node_type
@!last_kern:scaled; {used to implement \.{\\lastkern}}
@y
@!last_kern:scaled; {used to implement \.{\\lastkern}}
@!last_node_type:integer; {used to implement \.{\\lastnodetype}}
@z
%---------------------------------------
@x [45] m.991 l.19317 - e-TeX last_node_type
last_glue:=max_halfword; last_penalty:=0; last_kern:=0;
@y
last_glue:=max_halfword; last_penalty:=0; last_kern:=0;
last_node_type:=-1;
@z
%---------------------------------------
@x [45] m.996 l.19384 - e-TeX last_node_type
last_penalty:=0; last_kern:=0;
@y
last_penalty:=0; last_kern:=0;
last_node_type:=type(p)+1;
@z
%---------------------------------------
@x [45] m.999 l.19421 - e-TeX saved_items
link(contrib_head):=link(p); link(p):=null; flush_node_list(p)
@y
link(contrib_head):=link(p); link(p):=null;
if saving_vdiscards>0 then
  begin if page_disc=null then page_disc:=p@+else link(tail_page_disc):=p;
  tail_page_disc:=p;
  end
else flush_node_list(p)
@z
%---------------------------------------
% FIXME: sparse arrays?
@x [45] m.1012 l.19661 fire_up - e-TeX marks
if bot_mark<>null then
@y
if sa_mark<>null then
  if do_marks(fire_up_init,0,sa_mark) then sa_mark:=null;
if bot_mark<>null then
@z
%---------------------------------------
@x [45] m.1012 l.19669 fire_up - e-TeX marks
if (top_mark<>null)and(first_mark=null) then
@y
if sa_mark<>null then
  if do_marks(fire_up_done,0,sa_mark) then sa_mark:=null;
if (top_mark<>null)and(first_mark=null) then
@z
%---------------------------------------
@x [45] m.1014 l.19706 - e-TeX marks
  else if type(p)=mark_node then @<Update the values of
@y
  else if type(p)=mark_node then
    if mark_class(p)<>0 then @<Update the current marks for |fire_up|@>
    else @<Update the values of
@z
%---------------------------------------
@x [45] m.1021 l.19818 - e-TeX saved_items
    ins_ptr(p):=prune_page_top(broken_ptr(r));
@y
    ins_ptr(p):=prune_page_top(broken_ptr(r),false);
@z
%---------------------------------------
@x [45] m.1023 l.19854 - e-TeX saved_items
ship_out(box(255)); set_equiv(box_base+255,null);
@y
flush_node_list(page_disc); page_disc:=null;
ship_out(box(255)); set_equiv(box_base+255,null);
@z
%---------------------------------------
@x [45] m.1026 l.19896 - e-TeX saved_items
pop_nest; build_page;
@y
flush_node_list(page_disc); page_disc:=null;
pop_nest; build_page;
@z
%---------------------------------------
@x [47] m.1070 l.20697 normal_paragraph - e-TeX penalties
if par_shape_ptr<>null then eq_define(par_shape_loc,shape_ref,null);
@y
if par_shape_ptr<>null then eq_define(par_shape_loc,shape_ref,null);
if inter_line_penalties_ptr<>null then
  eq_define(inter_line_penalties_loc,shape_ref,null);
@z
%---------------------------------------
@x [47] m.1071 l.20732 - e-TeX sparse arrays
@d ship_out_flag==box_flag+(number_regs+number_regs)
   {context code for `\.{\\shipout}'}
@y
@d global_box_flag==box_flag+(number_regs+number_regs) {context code for `\.{\\global\\setbox0}'}
@d ship_out_flag==global_box_flag+1 {context code for `\.{\\shipout}'}
@z
%---------------------------------------
@x [47] m.1075 l.20815 box_end - e-TeX sparse arrays
var p:pointer; {|ord_noad| for new box in math mode}
@y
var p:pointer; {|ord_noad| for new box in math mode}
@!a:small_number; {global prefix}
@z
%---------------------------------------
% FIXME: might not work this way
@x [47] m.1077 l.20853 - e-TeX sparse arrays
if box_context<box_flag+number_regs then
  eq_define(box_base-box_flag+box_context,box_ref,cur_box)
else geq_define(box_base-box_flag-number_regs+box_context,box_ref,cur_box)
@y
begin if box_context<global_box_flag then
  begin cur_val:=box_context-box_flag; a:=0;
  end
else  begin cur_val:=box_context-global_box_flag; a:=4;
  end;
if cur_val<256 then define(box_base+cur_val,box_ref,cur_box)
else sa_def_box;
end
@z
%---------------------------------------
@x [47] m.1079 l.20885 begin_box - e-TeX sparse arrays
@!n:eight_bits; {a box number}
begin case cur_chr of
box_code: begin scan_eight_bit_int; cur_box:=box(cur_val);
  set_equiv(box_base+cur_val,null);
      {the box becomes void, at the same level}
  end;
copy_code: begin scan_eight_bit_int; cur_box:=copy_node_list(box(cur_val));
@y
@!n:halfword; {a box number}
begin case cur_chr of
box_code: begin scan_register_num; fetch_box(cur_box);
  set_equiv(box_base+cur_val,null);
      {the box becomes void, at the same level}
  end;
copy_code: begin scan_register_num; fetch_box(q); cur_box:=copy_node_list(q);
@z
%---------------------------------------
@x [47] m.1082 l.20935 - e-TeX sparse arrays
begin scan_eight_bit_int; n:=cur_val;
@y
begin scan_register_num; n:=cur_val;
@z
%---------------------------------------
@x [47] m.1101 l.21175 make_mark - e-TeX marks
begin p:=scan_toks(false,true); p:=get_node(small_node_size);
@y
@!c:halfword; {the mark class}
begin if cur_chr=0 then c:=0
else  begin scan_register_num; c:=cur_val;
  end;
p:=scan_toks(false,true); p:=get_node(small_node_size);
mark_class(p):=c;
@z
%---------------------------------------
@x [47] m.1108 l.21262 - e-TeX saved_items
un_vbox: if chr_code=copy_code then print_esc("unvcopy")
@y
un_vbox: if chr_code=copy_code then print_esc("unvcopy")
  @<Cases of |un_vbox| for |print_cmd_chr|@>@/
@z
%---------------------------------------
@x [47] m.1110 l.21272 unpackage - e-TeX saved_items
label exit;
@y
label done, exit;
@z
%---------------------------------------
@x [47] m.1110 l.21275 unpackage - e-TeX saved_items, sparse arrays
begin c:=cur_chr; scan_eight_bit_int; p:=box(cur_val);
@y
begin if cur_chr>copy_code then @<Handle saved items and |goto done|@>;
c:=cur_chr; scan_register_num; fetch_box(p);
@z
%---------------------------------------
% FIXME: might be incompatible
@x [47] m.1110 l.21287 unpackage - e-TeX sparse arrays
else  begin link(tail):=list_ptr(p);
set_equiv(box_base+cur_val,null);
@y
else  begin link(tail):=list_ptr(p);
change_box(null);
@z
%---------------------------------------
@x [47] m.1110 l.21290 unpackage - e-TeX saved_items
while link(tail)<>null do tail:=link(tail);
@y
done:
while link(tail)<>null do tail:=link(tail);
@z
%---------------------------------------
@x [48] m.1185 l.22243 - e-TeX middle
  if type(q)<>left_noad then confusion("right");
@:this can't happen right}{\quad right@>
  info(numerator(incompleat_noad)):=link(q);
  link(q):=incompleat_noad; link(incompleat_noad):=p;
@y
  if (type(q)<>left_noad)or(delim_ptr=null) then confusion("right");
@:this can't happen right}{\quad right@>
  info(numerator(incompleat_noad)):=link(delim_ptr);
  link(delim_ptr):=incompleat_noad; link(incompleat_noad):=p;
@z
%---------------------------------------
@x [48] m.1189 l.22288 - e-TeX middle
else print_esc("right");
@y
@/@<Cases of |left_right| for |print_cmd_chr|@>@/
else print_esc("right");
@z
%---------------------------------------
@x [48] m.1191 l.22297 math_left_right - e-TeX middle
begin t:=cur_chr;
if (t=right_noad)and(cur_group<>math_left_group) then
@y
@!q:pointer; {resulting mlist}
begin t:=cur_chr;
if (t<>left_noad)and(cur_group<>math_left_group) then
@z
%---------------------------------------
% FIXME: might be incompatible?
@x [48] m.1191 l.22302 math_left_right - e-TeX middle
  if t=left_noad then
    begin push_math(math_left_group); link(head):=p; tail:=p;
    end
  else  begin p:=fin_mlist(p);
    @<DIR: |unsave| math@>; {end of |math_left_group|}
@y
  if t=middle_noad then
    begin type(p):=right_noad; subtype(p):=middle_noad;
    end;
  if t=left_noad then q:=p
  else  begin q:=fin_mlist(p);
    @<DIR: |unsave| math@>; {end of |math_left_group|}
    end;
  if t<>right_noad then
    begin push_math(math_left_group); link(head):=q; tail:=p;
    delim_ptr:=p;
    end
  else  begin
@z
%---------------------------------------
@x [48] m.1191 l.22308 math_left_right - e-TeX middle
    info(nucleus(tail)):=p;
@y
    info(nucleus(tail)):=q;
@z
%---------------------------------------
@x [48] m.1192 l.22316 - e-TeX middle
  print_err("Extra "); print_esc("right");
@.Extra \\right.@>
  help1("I'm ignoring a \right that had no matching \left.");
@y
  print_err("Extra ");
  if t=middle_noad then
    begin print_esc("middle");
@.Extra \\middle.@>
    help1("I'm ignoring a \middle that had no matching \left.");
    end
  else  begin print_esc("right");
@.Extra \\right.@>
    help1("I'm ignoring a \right that had no matching \left.");
    end;
@z
%---------------------------------------
@x [49] m.1208 l.22577 - e-TeX protected
control sequence can be defined to be `\.{\\long}' or `\.{\\outer}', and
it might or might not be expanded. The prefixes `\.{\\global}', `\.{\\long}',
@y
control sequence can be defined to be `\.{\\long}', `\.{\\protected}',
or `\.{\\outer}', and it might or might not be expanded. The prefixes
`\.{\\global}', `\.{\\long}', `\.{\\protected}',
@z
%---------------------------------------
@x [49] m.1209 l.22603 - e-TeX protected
  else print_esc("global");
@y
  @/@<Cases of |prefix| for |print_cmd_chr|@>@/
  else print_esc("global");
@z
%---------------------------------------
@x [49] m.1211 l.22665 prefixed_command - e-TeX tracing
    @<Discard erroneous prefixes and |return|@>;
@y
    @<Discard erroneous prefixes and |return|@>;
  if tracing_commands>2 then if eTeX_ex then show_cur_cmd_chr;
@z
%---------------------------------------
@x [49] m.1212 l.22730 - e-TeX protected
help1("I'll pretend you didn't say \long or \outer or \global.");
@y
help1("I'll pretend you didn't say \long or \outer or \global.");
if eTeX_ex then help_line[0]:=@|
  "I'll pretend you didn't say \long or \outer or \global or \protected.";
@z
%---------------------------------------
@x [49] m.1213 l.22686 - e-TeX protected
if (cur_cmd<>def)and(a mod 4<>0) then
  begin print_err("You can't use `"); print_esc("long"); print("' or `");
  print_esc("outer"); print("' with `");
@y
if a>=8 then
  begin j:=protected_token; a:=a-8;
  end
else j:=0;
if (cur_cmd<>def)and((a mod 4<>0)or(j<>0)) then
  begin print_err("You can't use `"); print_esc("long"); print("' or `");
  print_esc("outer");
  help1("I'll pretend you didn't say \long or \outer here.");
  if eTeX_ex then
    begin  help_line[0]:=@|
      "I'll pretend you didn't say \long or \outer or \protected here.";
    print("' or `"); print_esc("protected");
    end;
  print("' with `");
@z
%---------------------------------------
@x [49] m.1213 l.22691 - e-TeX protected
  help1("I'll pretend you didn't say \long or \outer here.");
@y
@z
%---------------------------------------
@x [49] m.1218 l.22749 - e-TeX protected
  q:=scan_toks(true,e); define(p,call+(a mod 4),def_ref);
@y
  q:=scan_toks(true,e);
  if j<>0 then
    begin q:=get_avail; info(q):=j; link(q):=link(def_ref);
    link(def_ref):=q;
    end;
  define(p,call+(a mod 4),def_ref);
@z
%---------------------------------------
@x [49] m.1224 l.22840 - e-TeX sparse arrays
  othercases begin scan_eight_bit_int;
@y
  othercases begin scan_register_num;
    if cur_val>255 then
      begin j:=n-count_def_code; {|int_val..box_val|}
      if j>mu_val then j:=tok_val; {|int_val..mu_val| or |tok_val|}
      find_sa_element(j,cur_val,true); add_sa_ref(cur_ptr);
      if j=tok_val then j:=toks_register@+else j:=register;
      define(p,j,cur_ptr);
      end
    else
@z
%---------------------------------------
@x [49] m.1225 l.22853 - e-TeX read_line
read_to_cs: begin scan_int; n:=cur_val;
@y
read_to_cs: begin j:=cur_chr; scan_int; n:=cur_val;
@z
%---------------------------------------
@x [49] m.1225 l.22861 - e-TeX read_line
  p:=cur_cs; read_toks(n,p); define(p,call,cur_val);
@y
  p:=cur_cs; read_toks(n,p,j); define(p,call,cur_val);
@z
%---------------------------------------
@x [49] m.1226 l.22870 - e-TeX sparse arrays
  if cur_cmd=toks_register then
    begin scan_eight_bit_int; p:=toks_base+cur_val;
    end
  else p:=cur_chr; {|p=every_par_loc| or |output_routine_loc| or \dots}
@y
  e:=false; {just in case, will be set |true| for sparse array elements}
  if cur_cmd=toks_register then
    if cur_chr=mem_bot then
      begin scan_register_num;
      if cur_val>255 then
        begin find_sa_element(tok_val,cur_val,true);
        cur_chr:=cur_ptr; e:=true;
        end
      else cur_chr:=toks_base+cur_val;
      end
    else e:=true;
  p:=cur_chr; {|p=every_par_loc| or |output_routine_loc| or \dots}
@z
%---------------------------------------
@x [49] m.1226 l.22880 - e-TeX sparse arrays
    begin define(p,undefined_cs,null); free_avail(def_ref);
    end
  else  begin if p=output_routine_loc then {enclose in curlies}
@y
    begin sa_define(p,null)(p,undefined_cs,null); free_avail(def_ref);
    end
  else  begin if (p=output_routine_loc)and not e then {enclose in curlies}
@z
%---------------------------------------
@x [49] m.1226 l.22888 - e-TeX sparse arrays
    define(p,call,def_ref);
@y
    sa_define(p,def_ref)(p,call,def_ref);
@z
%---------------------------------------
@x [49] m.1227 l.22893 - e-TeX sparse arrays
begin if cur_cmd=toks_register then
  begin scan_eight_bit_int; cur_cmd:=assign_toks; cur_chr:=toks_base+cur_val;
  end;
if cur_cmd=assign_toks then
  begin q:=equiv(cur_chr);
  if q=null then define(p,undefined_cs,null)
  else  begin add_token_ref(q); define(p,call,q);
    end;
  goto done;
  end;
end
@y
if (cur_cmd=toks_register)or(cur_cmd=assign_toks) then
  begin if cur_cmd=toks_register then
    if cur_chr=mem_bot then
      begin scan_register_num;
      if cur_val<256 then q:=equiv(toks_base+cur_val)
      else  begin find_sa_element(tok_val,cur_val,false);
        if cur_ptr=null then q:=null
        else q:=sa_ptr(cur_ptr);
        end;
      end
    else q:=sa_ptr(cur_ptr)
  else q:=equiv(cur_chr);
  if q=null then sa_define(p,null)(p,undefined_cs,null)
  else  begin add_token_ref(q); sa_define(p,q)(p,call,q);
    end;
  goto done;
  end
@z
%---------------------------------------
@x [49] m.1236 l.23009 do_register_command - e-TeX sparse arrays
begin q:=cur_cmd;
@y
@!e:boolean; {does |l| refer to a sparse array element?}
@!w:integer; {integer or dimen value of |l|}
begin q:=cur_cmd;
e:=false; {just in case, will be set |true| for sparse array elements}
@z
%---------------------------------------
@x [49] m.1236 l.23024 do_register_command - e-TeX sparse arrays
if p<glue_val then word_define(l,cur_val)
else  begin trap_zero_glue; define(l,glue_ref,cur_val);
@y
if p<glue_val then sa_word_define(l,cur_val)
else  begin trap_zero_glue; sa_define(l,cur_val)(l,glue_ref,cur_val);
@z
%---------------------------------------
@x [49] m.1237 l.23046 - e-TeX sparse arrays
p:=cur_chr; scan_eight_bit_int;
@y
if (cur_chr<mem_bot)or(cur_chr>lo_mem_stat_max) then
  begin l:=cur_chr; p:=sa_type(l); e:=true;
  end
else  begin p:=cur_chr-mem_bot; scan_register_num;
  if cur_val>255 then
    begin find_sa_element(p,cur_val,true); l:=cur_ptr; e:=true;
    end
  else
@z
%---------------------------------------
@x [49] m.1237 l.23053 - e-TeX sparse arrays
end;
found:
@y
  end;
end;
found: if p<glue_val then@+if e then w:=sa_int(l)@+else w:=new_eqtb_int(l);
else if e then s:=sa_ptr(l)@+else s:=equiv(l)
@z
%---------------------------------------
@x [49] m.1238 l.23059 - e-TeX sparse arrays
  if q=advance then cur_val:=cur_val+new_eqtb_int(l);
@y
  if q=advance then cur_val:=cur_val+w;
@z
%---------------------------------------
@x [49] m.1239 l.23066 - e-TeX sparse arrays
begin q:=new_spec(cur_val); r:=equiv(l);
@y
begin q:=new_spec(cur_val); r:=s;
@z
%---------------------------------------
@x [49] m.1240 l.23086 - e-TeX sparse arrays
    if p=int_val then cur_val:=mult_integers(new_eqtb_int(l),cur_val)
    else cur_val:=nx_plus_y(new_eqtb_int(l),cur_val,0)
  else cur_val:=x_over_n(new_eqtb_int(l),cur_val)
else  begin s:=equiv(l); r:=new_spec(s);
@y
    if p=int_val then cur_val:=mult_integers(w,cur_val)
    else cur_val:=nx_plus_y(w,cur_val,0)
  else cur_val:=x_over_n(w,cur_val)
else  begin r:=new_spec(s);
@z
%---------------------------------------
@x [49] m.1241 l.23108 - e-TeX sparse arrays
set_box: begin scan_eight_bit_int;
  if global then n:=number_regs+cur_val@+else n:=cur_val;
  scan_optional_equals;
  if set_box_allowed then scan_box(box_flag+n)
@y
set_box: begin scan_register_num;
  if global then n:=global_box_flag+cur_val@+else n:=box_flag+cur_val;
  scan_optional_equals;
  if set_box_allowed then scan_box(n)
@z
%---------------------------------------
@x [49] m.1246 l.23179 alter_integer - e-TeX interaction_mode
var c:0..1; {0 for \.{\\deadcycles}, 1 for \.{\\insertpenalties}}
begin c:=cur_chr; scan_optional_equals; scan_int;
if c=0 then dead_cycles:=cur_val
@y
var c:small_number;
  {0 for \.{\\deadcycles}, 1 for \.{\\insertpenalties}, etc.}
begin c:=cur_chr; scan_optional_equals; scan_int;
if c=0 then dead_cycles:=cur_val
@/@<Cases for |alter_integer|@>@/
@z
%---------------------------------------
@x [49] m.1247 l.23188 alter_box_dimen - e-TeX sparse arrays
@!b:eight_bits; {box number}
begin c:=cur_chr; scan_eight_bit_int; b:=cur_val; scan_optional_equals;
scan_normal_dimen;
if box(b)<>null then mem[box(b)+c].sc:=cur_val;
@y
@!b:pointer; {box register}
begin c:=cur_chr; scan_register_num; fetch_box(b); scan_optional_equals;
scan_normal_dimen;
if b<>null then mem[b+c].sc:=cur_val;
@z
%---------------------------------------
@x [49] m.1248 l.23197 - e-TeX penalties
set_shape: begin scan_optional_equals; scan_int; n:=cur_val;
  if n<=0 then p:=null
@y
set_shape: begin q:=cur_chr; scan_optional_equals; scan_int; n:=cur_val;
  if n<=0 then p:=null
  else if q>par_shape_loc then
    begin n:=(cur_val div 2)+1; p:=get_node(2*n+1); info(p):=n;
    n:=cur_val; mem[p+1].int:=n; {number of penalties}
    for j:=p+2 to p+n+1 do
      begin scan_int; mem[j].int:=cur_val; {penalty values}
      end;
    if not odd(n) then mem[p+n+2].int:=0; {unused}
    end
@z
%---------------------------------------
@x [49] m.1248 l.23207 - e-TeX penalties
  define(par_shape_loc,shape_ref,p);
@y
  define(q,shape_ref,p);
@z
%---------------------------------------
@x [49] m.1292 l.23581 - e-TeX show_groups
  show_lists_code:print_esc("showlists");
@y
  show_lists_code:print_esc("showlists");
  @<Cases of |xray| for |print_cmd_chr|@>@;@/
@z
%---------------------------------------
@x [49] m.1293 l.23588 show_whatever - e-TeX show_ifs
var p:pointer; {tail of a token list to show}
@y
var p:pointer; {tail of a token list to show}
@!t:small_number; {type of conditional being shown}
@!m:normal..or_code; {upper bound on |fi_or_else| codes}
@!l:integer; {line where that conditional began}
@!n:integer; {level of \.{\\if...\\fi} nesting}
@z
%---------------------------------------
@x [49] m.1293 l.23594 show_whatever - e-TeX show_groups
othercases @<Show the current value of some parameter or register,
@y
@<Cases for |show_whatever|@>@;@/
othercases @<Show the current value of some parameter or register,
@z
%---------------------------------------
@x [49] m.1295 l.23629 - e-TeX protected
call: print("macro");
long_call: print_esc("long macro");
outer_call: print_esc("outer macro");
long_outer_call: begin print_esc("long"); print_esc("outer macro");
@y
call,long_call,outer_call,long_outer_call: begin n:=cmd-call;
  if info(link(chr_code))=protected_token then n:=n+4;
  if odd(n div 4) then print_esc("protected");
  if odd(n) then print_esc("long");
  if odd(n div 2) then print_esc("outer");
  if n>0 then print_char(" ");
  print("macro");
@z
%---------------------------------------
@x [49] m.1296 l.23637 - e-TeX sparse arrays
begin scan_eight_bit_int; begin_diagnostic;
print_nl("> \box"); print_int(cur_val); print_char("=");
if box(cur_val)=null then print("void")
else show_box(box(cur_val));
@y
begin scan_register_num; fetch_box(p); begin_diagnostic;
print_nl("> \box"); print_int(cur_val); print_char("=");
if p=null then print("void")@+else show_box(p);
@z
%---------------------------------------
@x [50] m.1307 l.23779 - e-TeX basic
dump_int(@$);@/
@y
dump_int(@$);@/
@<Dump the \eTeX\ state@>@/
@z
%---------------------------------------
@x [50] m.1308 l.23794 - e-TeX basic
if x<>@$ then goto bad_fmt; {check that strings are the same}
@y
if x<>@$ then goto bad_fmt; {check that strings are the same}
@/@<Undump the \eTeX\ state@>@/
@z
%---------------------------------------
@x [50] m.1311 l.23848 - e-TeX sparse arrays
dump_int(lo_mem_max); dump_int(rover);
@y
dump_int(lo_mem_max); dump_int(rover);
if eTeX_ex then for k:=int_val to tok_val do dump_int(sa_root[k]);
@z
%---------------------------------------
@x [50] m.1312 l.23871 - e-TeX sparse arrays
undump(lo_mem_stat_max+1)(lo_mem_max)(rover);
@y
undump(lo_mem_stat_max+1)(lo_mem_max)(rover);
if eTeX_ex then for k:=int_val to tok_val do
  undump(null)(lo_mem_max)(sa_root[k]);
@z
%---------------------------------------
@x [50] m.1324 l.24065 - e-TeX hyph_codes
dump_int(trie_max);
@y
dump_int(trie_max);
dump_int(hyph_start);
@z
%---------------------------------------
@x [50] m.1325 l.24093 - e-TeX hyph_codes
undump_size(0)(trie_size)('trie size')(j); @+init trie_max:=j;@+tini
@y
undump_size(0)(trie_size)('trie size')(j); @+init trie_max:=j;@+tini
undump(0)(j)(hyph_start);
@z
%---------------------------------------
@x [51] m.1335 l.24313 final_cleanup - tracing
  print_int(cur_level-level_one); print_char(")");
@y
  print_int(cur_level-level_one); print_char(")");
  if eTeX_ex then show_save_groups;
@z
%---------------------------------------
@x [51] m.1335 l.24336 final_cleanup - e-TeX marks, saved_items
    if cur_mark[c]<>null then delete_token_ref(cur_mark[c]);
@y
    if cur_mark[c]<>null then delete_token_ref(cur_mark[c]);
  if sa_mark<>null then
    if do_marks(destroy_marks,0,sa_mark) then sa_mark:=null;
  for c:=last_box_code to vsplit_code do flush_node_list(disc_ptr[c]);
@z
%---------------------------------------
@x [51] m.1336 l.24345 ] m.1336 l.24340 init_prim - e-TeX basic
begin no_new_control_sequence:=false;
@y
begin no_new_control_sequence:=false;
first:=0;
@z
%---------------------------------------
@x [51] m.1337 l.24360 - e-TeX basic
if (format_ident=0)or(buffer[loc]="&") then
@y
@<Enable \eTeX, if requested@>@;@/
if (format_ident=0)or(buffer[loc]="&") then
@z
%---------------------------------------
@x [51] m.1337 l.24368 - e-TeX basic
  end;
@y
  end;
if eTeX_ex then wterm_ln('entering extended mode');
@z
%---------------------------------------
@x [53] m.1363 l.24715 - e-TeX hyph_codes
adv_past(s)
@y
if subtype(s)=language_node then
  begin cur_lang:=what_lang(s); l_hyf:=what_lhm(s); r_hyf:=what_rhm(s);
  set_hyph_index;
  end
@z
%---------------------------------------
@x [54] m.1379 l.24903 - e-TeX additions
@* \[54] System-dependent changes.
@y
@* \[53a] The extended features of \eTeX.
The program has two modes of operation:  (1)~In \TeX\ compatibility mode
it fully deserves the name \TeX\ and there are neither extended features
nor additional primitive commands.  There are, however, a few
modifications that would be legitimate in any implementation of \TeX\
such as, e.g., preventing inadequate results of the glue to \.{DVI}
unit conversion during |ship_out|.  (2)~In extended mode there are
additional primitive commands and the extended features of \eTeX\ are
available.

The distinction between these two modes of operation initially takes
place when a `virgin' \.{eINITEX} starts without reading a format file.
Later on the values of all \eTeX\ state variables are inherited when
\.{eVIRTEX} (or \.{eINITEX}) reads a format file.

The code below is designed to work for cases where `$|init|\ldots|tini|$'
is a run-time switch.

@<Enable \eTeX, if requested@>=
@!init if (buffer[loc]="*")and(format_ident=" (INITEX)") then
  begin no_new_control_sequence:=false;
  @<Generate all \eTeX\ primitives@>@;
  incr(loc); eTeX_mode:=1; {enter extended mode}
  @<Initialize variables for \eTeX\ extended mode@>@;
  end;
tini@;@/
if not no_new_control_sequence then {just entered extended mode ?}
  no_new_control_sequence:=true@+else

@ The \eTeX\ features available in extended mode are grouped into two
categories:  (1)~Some of them are permanently enabled and have no
semantic effect as long as none of the additional primitives are
executed.  (2)~The remaining \eTeX\ features are optional and can be
individually enabled and disabled.  For each optional feature there is
an \eTeX\ state variable named \.{\\...state}; the feature is enabled,
resp.\ disabled by assigning a positive, resp.\ non-positive value to
that integer.

@d eTeX_state_base=int_base+eTeX_state_code
@d eTeX_state(#)==eqtb[eTeX_state_base+#].int {an \eTeX\ state variable}
@#
@d eTeX_version_code=eTeX_int {code for \.{\\eTeXversion}}
@d eTeX_revision_code=6 {command code for \.{\\eTeXrevision}}

@<Generate all \eTeX...@>=
primitive("lastnodetype",last_item,last_node_type_code);
@!@:last_node_type_}{\.{\\lastnodetype} primitive@>
primitive("eTeXversion",last_item,eTeX_version_code);
@!@:eTeX_version_}{\.{\\eTeXversion} primitive@>
primitive("eTeXrevision",convert,eTeX_revision_code);@/
@!@:eTeX_revision_}{\.{\\eTeXrevision} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
last_node_type_code: print_esc("lastnodetype");
eTeX_version_code: print_esc("eTeXversion");

@ @<Cases for fetching an integer value@>=
eTeX_version_code: cur_val:=eTeX_version;

@ @<Cases of |convert| for |print_cmd_chr|@>=
eTeX_revision_code: print_esc("eTeXrevision");

@ @<Cases of `Scan the argument for command |c|'@>=
eTeX_revision_code: do_nothing;

@ @<Cases of `Print the result of command |c|'@>=
eTeX_revision_code: print(eTeX_revision);

@ @d eTeX_ex==(eTeX_mode=1) {is this extended mode?}

@<Glob...@>=
@!eTeX_mode: 0..1; {identifies compatibility and extended mode}

@ @<Initialize table entries...@>=
eTeX_mode:=0; {initially we are in compatibility mode}
@<Initialize variables for \eTeX\ compatibility mode@>@;

@ @<Dump the \eTeX\ state@>=
dump_int(eTeX_mode);
for j:=0 to eTeX_states-1 do eTeX_state(j):=0; {disable all enhancements}

@ @<Undump the \eTeX\ state@>=
undump(0)(1)(eTeX_mode);
if eTeX_ex then
  begin @<Initialize variables for \eTeX\ extended mode@>@;
  end
else  begin @<Initialize variables for \eTeX\ compatibility mode@>@;
  end;

@ The |eTeX_enabled| function simply returns its first argument as
result.  This argument is |true| if an optional \eTeX\ feature is
currently enabled; otherwise, if the argument is |false|, the function
gives an error message.

@<Declare \eTeX\ procedures for use...@>=
function eTeX_enabled(@!b:boolean;@!j:quarterword;@!k:halfword):boolean;
begin if not b then
  begin print_err("Improper "); print_cmd_chr(j,k);
  help1("Sorry, this optional e-TeX feature has been disabled."); error;
  end;
eTeX_enabled:=b;
end;

@ First we implement the additional \eTeX\ parameters in the table of
equivalents.

@<Generate all \eTeX...@>=
primitive("everyeof",assign_toks,every_eof_loc);
@!@:every_eof_}{\.{\\everyeof} primitive@>
primitive("tracingassigns",assign_int,int_base+tracing_assigns_code);@/
@!@:tracing_assigns_}{\.{\\tracingassigns} primitive@>
primitive("tracinggroups",assign_int,int_base+tracing_groups_code);@/
@!@:tracing_groups_}{\.{\\tracinggroups} primitive@>
primitive("tracingifs",assign_int,int_base+tracing_ifs_code);@/
@!@:tracing_ifs_}{\.{\\tracingifs} primitive@>
primitive("tracingscantokens",assign_int,int_base+tracing_scan_tokens_code);@/
@!@:tracing_scan_tokens_}{\.{\\tracingscantokens} primitive@>
primitive("tracingnesting",assign_int,int_base+tracing_nesting_code);@/
@!@:tracing_nesting_}{\.{\\tracingnesting} primitive@>
{ FIXME: the next two should be commented }
primitive("predisplaydirection",
  assign_int,int_base+pre_display_direction_code);@/
@!@:pre_display_direction_}{\.{\\predisplaydirection} primitive@>
primitive("lastlinefit",assign_int,int_base+last_line_fit_code);@/
@!@:last_line_fit_}{\.{\\lastlinefit} primitive@>
primitive("savingvdiscards",assign_int,int_base+saving_vdiscards_code);@/
@!@:saving_vdiscards_}{\.{\\savingvdiscards} primitive@>
primitive("savinghyphcodes",assign_int,int_base+saving_hyph_codes_code);@/
@!@:saving_hyph_codes_}{\.{\\savinghyphcodes} primitive@>

@ @d every_eof==equiv(every_eof_loc)

@<Cases of |assign_toks| for |print_cmd_chr|@>=
every_eof_loc: print_esc("everyeof");

@ @<Cases for |print_param|@>=
tracing_assigns_code:print_esc("tracingassigns");
tracing_groups_code:print_esc("tracinggroups");
tracing_ifs_code:print_esc("tracingifs");
tracing_scan_tokens_code:print_esc("tracingscantokens");
tracing_nesting_code:print_esc("tracingnesting");
{ FIXME: the next one should be commented }
pre_display_direction_code:print_esc("predisplaydirection");
last_line_fit_code:print_esc("lastlinefit");
saving_vdiscards_code:print_esc("savingvdiscards");
saving_hyph_codes_code:print_esc("savinghyphcodes");

@ In order to handle \.{\\everyeof} we need an array |eof_seen| of
boolean variables.

@<Glob...@>=
@!eof_seen : array[1..max_in_open] of boolean; {has eof been seen?}

@ The |print_group| procedure prints the current level of grouping and
the name corresponding to |cur_group|.

@<Declare \eTeX\ procedures for tr...@>=
procedure print_group(@!e:boolean);
label exit;
begin case cur_group of
  bottom_level: begin print("bottom level"); return;
    end;
  simple_group,semi_simple_group:
    begin if cur_group=semi_simple_group then print("semi ");
    print("simple");
    end;
  hbox_group,adjusted_hbox_group:
    begin if cur_group=adjusted_hbox_group then print("adjusted ");
    print("hbox");
    end;
  vbox_group: print("vbox");
  vtop_group: print("vtop");
  align_group,no_align_group:
    begin if cur_group=no_align_group then print("no ");
    print("align");
    end;
  output_group: print("output");
  disc_group: print("disc");
  insert_group: print("insert");
  vcenter_group: print("vcenter");
  math_group,math_choice_group,math_shift_group,math_left_group:
    begin print("math");
    if cur_group=math_choice_group then print(" choice")
    else if cur_group=math_shift_group then print(" shift")
    else if cur_group=math_left_group then print(" left");
    end;
  end; {there are no other cases}
print(" group (level "); print_int(qo(cur_level)); print_char(")");
if saved(-1)<>0 then
  begin if e then print(" entered at line ") else print(" at line ");
  print_int(saved(-1));
  end;
exit:end;

@ The |group_trace| procedure is called when a new level of grouping
begins (|e=false|) or ends (|e=true|) with |saved(-1)| containing the
line number.

@<Declare \eTeX\ procedures for tr...@>=
@!stat procedure group_trace(@!e:boolean);
begin begin_diagnostic; print_char("{");
if e then print("leaving ") else print("entering ");
print_group(e); print_char("}"); end_diagnostic(false);
end;
tats

@ The \.{\\currentgrouplevel} and \.{\\currentgrouptype} commands return
the current level of grouping and the type of the current group
respectively.

@d current_group_level_code=eTeX_int+1 {code for \.{\\currentgrouplevel}}
@d current_group_type_code=eTeX_int+2 {code for \.{\\currentgrouptype}}

@<Generate all \eTeX...@>=
primitive("currentgrouplevel",last_item,current_group_level_code);
@!@:current_group_level_}{\.{\\currentgrouplevel} primitive@>
primitive("currentgrouptype",last_item,current_group_type_code);
@!@:current_group_type_}{\.{\\currentgrouptype} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
current_group_level_code: print_esc("currentgrouplevel");
current_group_type_code: print_esc("currentgrouptype");

@ @<Cases for fetching an integer value@>=
current_group_level_code: cur_val:=cur_level-level_one;
current_group_type_code: cur_val:=cur_group;

@ The \.{\\currentiflevel}, \.{\\currentiftype}, and
\.{\\currentifbranch} commands return the current level of conditionals
and the type and branch of the current conditional.

@d current_if_level_code=eTeX_int+3 {code for \.{\\currentiflevel}}
@d current_if_type_code=eTeX_int+4 {code for \.{\\currentiftype}}
@d current_if_branch_code=eTeX_int+5 {code for \.{\\currentifbranch}}

@<Generate all \eTeX...@>=
primitive("currentiflevel",last_item,current_if_level_code);
@!@:current_if_level_}{\.{\\currentiflevel} primitive@>
primitive("currentiftype",last_item,current_if_type_code);
@!@:current_if_type_}{\.{\\currentiftype} primitive@>
primitive("currentifbranch",last_item,current_if_branch_code);
@!@:current_if_branch_}{\.{\\currentifbranch} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
current_if_level_code: print_esc("currentiflevel");
current_if_type_code: print_esc("currentiftype");
current_if_branch_code: print_esc("currentifbranch");

@ @<Cases for fetching an integer value@>=
current_if_level_code: begin q:=cond_ptr; cur_val:=0;
  while q<>null do
    begin incr(cur_val); q:=link(q);
    end;
  end;
current_if_type_code: if cond_ptr=null then cur_val:=0
  else if cur_if<unless_code then cur_val:=cur_if+1
  else cur_val:=-(cur_if-unless_code+1);
current_if_branch_code:
  if (if_limit=or_code)or(if_limit=else_code) then cur_val:=1
  else if if_limit=fi_code then cur_val:=-1
  else cur_val:=0;

@ The \.{\\fontcharwd}, \.{\\fontcharht}, \.{\\fontchardp}, and
\.{\\fontcharic} commands return information about a character in a
font.

@d font_char_wd_code=eTeX_dim {code for \.{\\fontcharwd}}
@d font_char_ht_code=eTeX_dim+1 {code for \.{\\fontcharht}}
@d font_char_dp_code=eTeX_dim+2 {code for \.{\\fontchardp}}
@d font_char_ic_code=eTeX_dim+3 {code for \.{\\fontcharic}}

@<Generate all \eTeX...@>=
primitive("fontcharwd",last_item,font_char_wd_code);
@!@:font_char_wd_}{\.{\\fontcharwd} primitive@>
primitive("fontcharht",last_item,font_char_ht_code);
@!@:font_char_ht_}{\.{\\fontcharht} primitive@>
primitive("fontchardp",last_item,font_char_dp_code);
@!@:font_char_dp_}{\.{\\fontchardp} primitive@>
primitive("fontcharic",last_item,font_char_ic_code);
@!@:font_char_ic_}{\.{\\fontcharic} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
font_char_wd_code: print_esc("fontcharwd");
font_char_ht_code: print_esc("fontcharht");
font_char_dp_code: print_esc("fontchardp");
font_char_ic_code: print_esc("fontcharic");

@ @<Cases for fetching a dimension value@>=
font_char_wd_code,
font_char_ht_code,
font_char_dp_code,
font_char_ic_code: begin scan_font_ident; q:=cur_val; scan_char_num;
  if (font_bc(q)<=cur_val)and(font_ec(q)>=cur_val) then
    begin i:=char_info(q)(qi(cur_val));
    case m of
    font_char_wd_code: cur_val:=char_width(q)(i);
    font_char_ht_code: cur_val:=char_height(q)(height_depth(i));
    font_char_dp_code: cur_val:=char_depth(q)(height_depth(i));
    font_char_ic_code: cur_val:=char_italic(q)(i);
    end; {there are no other cases}
    end
  else cur_val:=0;
  end;

@ The \.{\\parshapedimen}, \.{\\parshapeindent}, and \.{\\parshapelength}
commands return the indent and length parameters of the current
\.{\\parshape} specification.

@d par_shape_length_code=eTeX_dim+4 {code for \.{\\parshapelength}}
@d par_shape_indent_code=eTeX_dim+5 {code for \.{\\parshapeindent}}
@d par_shape_dimen_code=eTeX_dim+6 {code for \.{\\parshapedimen}}

@<Generate all \eTeX...@>=
primitive("parshapelength",last_item,par_shape_length_code);
@!@:par_shape_length_}{\.{\\parshapelength} primitive@>
primitive("parshapeindent",last_item,par_shape_indent_code);
@!@:par_shape_indent_}{\.{\\parshapeindent} primitive@>
primitive("parshapedimen",last_item,par_shape_dimen_code);
@!@:par_shape_dimen_}{\.{\\parshapedimen} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
par_shape_length_code: print_esc("parshapelength");
par_shape_indent_code: print_esc("parshapeindent");
par_shape_dimen_code: print_esc("parshapedimen");

@ @<Cases for fetching a dimension value@>=
par_shape_length_code,
par_shape_indent_code,
par_shape_dimen_code: begin q:=cur_chr-par_shape_length_code; scan_int;
  if (par_shape_ptr=null)or(cur_val<=0) then cur_val:=0
  else  begin if q=2 then
      begin q:=cur_val mod 2; cur_val:=(cur_val+q)div 2;
      end;
    if cur_val>info(par_shape_ptr) then cur_val:=info(par_shape_ptr);
    cur_val:=mem[par_shape_ptr+2*cur_val-q].sc;
    end;
  cur_val_level:=dimen_val;
  end;

@ The \.{\\showgroups} command displays all currently active grouping
levels.

@d show_groups=4 { \.{\\showgroups} }

@<Generate all \eTeX...@>=
primitive("showgroups",xray,show_groups);
@!@:show_groups_}{\.{\\showgroups} primitive@>

@ @<Cases of |xray| for |print_cmd_chr|@>=
show_groups:print_esc("showgroups");

@ @<Cases for |show_whatever|@>=
show_groups: begin begin_diagnostic; show_save_groups;
  end;

@ @<Types...@>=
@!save_pointer=0..save_size; {index into |save_stack|}

@ The modifications of \TeX\ required for the display produced by the
|show_save_groups| procedure were first discussed by Donald~E. Knuth in
{\sl TUGboat\/} {\bf 11}, 165--170 and 499--511, 1990.
@^Knuth, Donald Ervin@>

In order to understand a group type we also have to know its mode.
Since unrestricted horizontal modes are not associated with grouping,
they are skipped when traversing the semantic nest.

@<Declare \eTeX\ procedures for use...@>=
procedure show_save_groups;
label found1,found2,found,done;
var p:0..nest_size; {index into |nest|}
@!m:-mmode..mmode; {mode}
@!v:save_pointer; {saved value of |save_ptr|}
@!l:quarterword; {saved value of |cur_level|}
@!c:group_code; {saved value of |cur_group|}
@!a:-1..1; {to keep track of alignments}
@!i:integer;
@!j:quarterword;
@!s:str_number;
begin p:=nest_ptr; nest[p]:=cur_list; {put the top level into the array}
v:=save_ptr; l:=cur_level; c:=cur_group;
save_ptr:=cur_boundary; decr(cur_level);@/
a:=1;
print_nl(""); print_ln;
loop@+begin print_nl("### "); print_group(true);
  if cur_group=bottom_level then goto done;
  repeat m:=nest[p].mode_field;
  if p>0 then decr(p) else m:=vmode;
  until m<>hmode;
  print(" (");
  case cur_group of
    simple_group: begin incr(p); goto found2;
      end;
    hbox_group,adjusted_hbox_group: s:="hbox";
    vbox_group: s:="vbox";
    vtop_group: s:="vtop";
    align_group: if a=0 then
        begin if m=-vmode then s:="halign" else s:="valign";
        a:=1; goto found1;
        end
      else  begin if a=1 then print("align entry") else print_esc("cr");
        if p>=a then p:=p-a;
        a:=0; goto found;
        end;
    no_align_group:
      begin incr(p); a:=-1; print_esc("noalign"); goto found2;
      end;
    output_group:
      begin print_esc("output"); goto found;
      end;
    math_group: goto found2;
    disc_group,math_choice_group:
      begin if cur_group=disc_group then print_esc("discretionary")
      else print_esc("mathchoice");
      for i:=1 to 3 do if i<=saved(-2) then print("{}");
      goto found2;
      end;
    insert_group:
      begin if saved(-2)=255 then print_esc("vadjust")
      else  begin print_esc("insert"); print_int(saved(-2));
        end;
      goto found2;
      end;
    vcenter_group: begin s:="vcenter"; goto found1;
      end;
    semi_simple_group: begin incr(p); print_esc("begingroup"); goto found;
      end;
    math_shift_group:
      begin if m=mmode then print_char("$")
      else if nest[p].mode_field=mmode then
        begin print_cmd_chr(eq_no,saved(-2)); goto found;
        end;
      print_char("$"); goto found;
      end;
    math_left_group:
      begin if type(nest[p+1].eTeX_aux_field)=left_noad then print_esc("left")
      else print_esc("middle");
      goto found;
      end;
    end; {there are no other cases}
  @<Show the box context@>;
  found1: print_esc(s); @<Show the box packaging info@>;
  found2: print_char("{");
  found: print_char(")"); decr(cur_level);
  cur_group:=save_level(save_ptr); save_ptr:=save_index(save_ptr)
  end;
done: save_ptr:=v; cur_level:=l; cur_group:=c;
end;

@ @<Show the box packaging info@>=
if saved(-2)<>0 then
  begin print_char(" ");
  if saved(-3)=exactly then print("to") else print("spread");
  print_scaled(saved(-2)); print("pt");
  end

@ @<Show the box context@>=
i:=saved(-4);
if i<>0 then
  if i<box_flag then
    begin if abs(nest[p].mode_field)=vmode then j:=hmove else j:=vmove;
    if i>0 then print_cmd_chr(j,0) else print_cmd_chr(j,1);
    print_scaled(abs(i)); print("pt");
    end
  else if i<ship_out_flag then
    begin if i>=global_box_flag then
      begin print_esc("global"); i:=i-(global_box_flag-box_flag);
      end;
    print_esc("setbox"); print_int(i-box_flag); print_char("=");
    end
  else print_cmd_chr(leader_ship,i-(leader_flag-a_leaders))

@ The |scan_general_text| procedure is much like |scan_toks(false,false)|,
but will be invoked via |expand|, i.e., recursively.
@^recursion@>

@<Declare \eTeX\ procedures for sc...@>=
procedure@?scan_general_text; forward;@t\2@>

@ The token list (balanced text) created by |scan_general_text| begins
at |link(temp_head)| and ends at |cur_val|.  (If |cur_val=temp_head|,
the list is empty.)

@<Declare \eTeX\ procedures for tok...@>=
procedure scan_general_text;
label found;
var s:normal..absorbing; {to save |scanner_status|}
@!w:pointer; {to save |warning_index|}
@!d:pointer; {to save |def_ref|}
@!p:pointer; {tail of the token list being built}
@!q:pointer; {new node being added to the token list via |store_new_token|}
@!unbalance:halfword; {number of unmatched left braces}
begin s:=scanner_status; w:=warning_index; d:=def_ref;
scanner_status:=absorbing; warning_index:=cur_cs;
def_ref:=get_avail; token_ref_count(def_ref):=null; p:=def_ref;
scan_left_brace; {remove the compulsory left brace}
unbalance:=1;
loop@+  begin get_token;
  if cur_tok<right_brace_limit then
    if cur_cmd<right_brace then incr(unbalance)
    else  begin decr(unbalance);
      if unbalance=0 then goto found;
      end;
  store_new_token(cur_tok);
  end;
found: q:=link(def_ref); free_avail(def_ref); {discard reference count}
if q=null then cur_val:=temp_head @+ else cur_val:=p;
link(temp_head):=q;
scanner_status:=s; warning_index:=w; def_ref:=d;
end;

@ The \.{\\showtokens} command displays a token list.

@d show_tokens=5 { \.{\\showtokens} , must be odd! }

@<Generate all \eTeX...@>=
primitive("showtokens",xray,show_tokens);
@!@:show_tokens_}{\.{\\showtokens} primitive@>

@ @<Cases of |xray| for |print_cmd_chr|@>=
show_tokens:print_esc("showtokens");

@ The \.{\\unexpanded} primitive prevents expansion of tokens much as
the result from \.{\\the} applied to a token variable.  The
\.{\\detokenize} primitive converts a token list into a list of
character tokens much as if the token list were written to a file.  We
use the fact that the command modifiers for \.{\\unexpanded} and
\.{\\detokenize} are odd whereas those for \.{\\the} and \.{\\showthe}
are even.

@<Generate all \eTeX...@>=
primitive("unexpanded",the,1);@/
@!@:unexpanded_}{\.{\\unexpanded} primitive@>
primitive("detokenize",the,show_tokens);@/
@!@:detokenize_}{\.{\\detokenize} primitive@>

@ @<Cases of |the| for |print_cmd_chr|@>=
else if chr_code=1 then print_esc("unexpanded")
else print_esc("detokenize")

@ @<Handle \.{\\unexpanded} or \.{\\detokenize} and |return|@>=
if odd(cur_chr) then
  begin c:=cur_chr; scan_general_text;
  if c=1 then the_toks:=cur_val
  else begin old_setting:=selector; selector:=new_string; b:=pool_ptr;
    p:=get_avail; link(p):=link(temp_head);
    token_show(p); flush_list(p);
    selector:=old_setting; the_toks:=str_toks(b);
    end;
  return;
  end

@ The \.{\\showifs} command displays all currently active conditionals.

@d show_ifs=6 { \.{\\showifs} }

@<Generate all \eTeX...@>=
primitive("showifs",xray,show_ifs);
@!@:show_ifs_}{\.{\\showifs} primitive@>

@ @<Cases of |xray| for |print_cmd_chr|@>=
show_ifs:print_esc("showifs");

@
@d print_if_line(#)==if #<>0 then
  begin print(" entered on line "); print_int(#);
  end

@<Cases for |show_whatever|@>=
show_ifs: begin begin_diagnostic; print_nl(""); print_ln;
  if cond_ptr=null then
    begin print_nl("### "); print("no active conditionals");
    end
  else  begin p:=cond_ptr; n:=0;
    repeat incr(n); p:=link(p);@+until p=null;
    p:=cond_ptr; t:=cur_if; l:=if_line; m:=if_limit;
    repeat print_nl("### level "); print_int(n); print(": ");
    print_cmd_chr(if_test,t);
    if m=fi_code then print_esc("else");
    print_if_line(l);
    decr(n); t:=subtype(p); l:=if_line_field(p); m:=type(p); p:=link(p);
    until p=null;
    end;
  end;

@ The \.{\\interactionmode} primitive allows to query and set the
interaction mode.

@<Generate all \eTeX...@>=
primitive("interactionmode",set_page_int,2);
@!@:interaction_mode_}{\.{\\interactionmode} primitive@>

@ @<Cases of |set_page_int| for |print_cmd_chr|@>=
else if chr_code=2 then print_esc("interactionmode")

@ @<Cases for `Fetch the |dead_cycles| or the |insert_penalties|'@>=
else if m=2 then cur_val:=interaction

@ @<Declare \eTeX\ procedures for use...@>=
procedure@?new_interaction; forward;@t\2@>

@ @<Cases for |alter_integer|@>=
else if c=2 then
  begin if (cur_val<batch_mode)or(cur_val>error_stop_mode) then
    begin print_err("Bad interaction mode");
@.Bad interaction mode@>
    help2("Modes are 0=batch, 1=nonstop, 2=scroll, and")@/
    ("3=errorstop. Proceed, and I'll ignore this case.");
    int_error(cur_val);
    end
  else  begin cur_chr:=cur_val; new_interaction;
    end;
  end

@ The |middle| feature of \eTeX\ allows one ore several \.{\\middle}
delimiters to appear between \.{\\left} and \.{\\right}.

@<Generate all \eTeX...@>=
primitive("middle",left_right,middle_noad);
@!@:middle_}{\.{\\middle} primitive@>

@ @<Cases of |left_right| for |print_cmd_chr|@>=
else if chr_code=middle_noad then print_esc("middle")

@ The |scan_tokens| feature of \eTeX\ defines the \.{\\scantokens}
primitive.

@<Generate all \eTeX...@>=
primitive("scantokens",input,2);
@!@:scan_tokens_}{\.{\\scantokens} primitive@>

@ @<Cases of |input| for |print_cmd_chr|@>=
else if chr_code=2 then print_esc("scantokens")

@ @<Cases for |input|@>=
else if cur_chr=2 then pseudo_start

@ The global variable |pseudo_files| is used to maintain a stack of
pseudo files.  The |info| field of each pseudo file points to a linked
list of variable size nodes representing lines not yet processed: the
|info| field of the first word contains the size of this node, all the
following words contain ASCII codes.

@<Glob...@>=
@!pseudo_files:pointer; {stack of pseudo files}

@ @<Set init...@>=
pseudo_files:=null;

@ The |pseudo_start| procedure initiates reading from a pseudo file.

@<Declare \eTeX\ procedures for ex...@>=
procedure@?pseudo_start; forward;@t\2@>

@ @<Declare \eTeX\ procedures for tok...@>=
procedure pseudo_start;
var old_setting:0..max_selector; {holds |selector| setting}
@!s:str_number; {string to be converted into a pseudo file}
@!l,@!m:pool_pointer; {indices into |str_pool|}
@!p,@!q,@!r:pointer; {for list construction}
@!w: four_quarters; {four ASCII codes}
@!nl,@!sz:integer;
begin scan_general_text;
old_setting:=selector; selector:=new_string;
token_show(temp_head); selector:=old_setting;
flush_list(link(temp_head));
str_room(1); s:=make_string;
@<Convert string |s| into a new pseudo file@>;
flush_string;
@<Initiate input from new pseudo file@>;
end;

@ @<Convert string |s| into a new pseudo file@>=
str_pool[pool_ptr]:=si(" "); l:=str_start(s);
nl:=si(new_line_char);
p:=get_avail; q:=p;
while l<pool_ptr do
  begin m:=l;
  while (l<pool_ptr)and(str_pool[l]<>nl) do incr(l);
  sz:=(l-m+7)div 4;
  if sz=1 then sz:=2;
  r:=get_node(sz); link(q):=r; q:=r; info(q):=hi(sz);
  while sz>2 do
    begin decr(sz); incr(r);
    w.b0:=qi(so(str_pool[m])); w.b1:=qi(so(str_pool[m+1]));
    w.b2:=qi(so(str_pool[m+2])); w.b3:=qi(so(str_pool[m+3]));
    mem[r].qqqq:=w; m:=m+4;
    end;
  w.b0:=qi(" "); w.b1:=qi(" "); w.b2:=qi(" "); w.b3:=qi(" ");
  if l>m then
    begin w.b0:=qi(so(str_pool[m]));
    if l>m+1 then
      begin  w.b1:=qi(so(str_pool[m+1]));
      if l>m+2 then
        begin  w.b2:=qi(so(str_pool[m+2]));
        if l>m+3 then w.b3:=qi(so(str_pool[m+3]));
        end;
      end;
    end;
  mem[r+1].qqqq:=w;
  if str_pool[l]=nl then incr(l);
  end;
info(p):=link(p); link(p):=pseudo_files; pseudo_files:=p

@ @<Initiate input from new pseudo file@>=
begin_file_reading; {set up |cur_file| and new level of input}
line:=0; limit:=start; loc:=limit+1; {force line read}
if tracing_scan_tokens>0 then
  begin if term_offset>max_print_line-3 then print_ln
  else if (term_offset>0)or(file_offset>0) then print_char(" ");
  name:=19; print("( "); incr(open_parens); update_terminal;
  end
else name:=18

@ Here we read a line from the current pseudo file into |buffer|.

@<Declare \eTeX\ procedures for tr...@>=
function pseudo_input: boolean; {inputs the next line or returns |false|}
var p:pointer; {current line from pseudo file}
@!sz:integer; {size of node |p|}
@!w:four_quarters; {four ASCII codes}
@!r:pointer; {loop index}
begin last:=first; {cf.\ Matthew 19\thinspace:\thinspace30}
p:=info(pseudo_files);
if p=null then pseudo_input:=false
else  begin info(pseudo_files):=link(p); sz:=ho(info(p));
  if 4*sz-3>=buf_size-last then
    @<Report overflow of the input buffer, and abort@>;
  last:=first;
  for r:=p+1 to p+sz-1 do
    begin w:=mem[r].qqqq;
    buffer[last]:=w.b0; buffer[last+1]:=w.b1;
    buffer[last+2]:=w.b2; buffer[last+3]:=w.b3;
    last:=last+4;
    end;
  if last>=max_buf_stack then max_buf_stack:=last+1;
  while (last>first)and(buffer[last-1]=" ") do decr(last);
  free_node(p,sz);
  pseudo_input:=true;
  end;
end;

@ When we are done with a pseudo file we `close' it.

@<Declare \eTeX\ procedures for tr...@>=
procedure pseudo_close; {close the top level pseudo file}
var p,@!q: pointer;
begin p:=link(pseudo_files); q:=info(pseudo_files);
free_avail(pseudo_files); pseudo_files:=p;
while q<>null do
  begin p:=q; q:=link(p); free_node(p,ho(info(p)));
  end;
end;

@ @<Dump the \eTeX\ state@>=
while pseudo_files<>null do pseudo_close; {flush pseudo files}

@ @<Generate all \eTeX...@>=
primitive("readline",read_to_cs,1);@/
@!@:read_line_}{\.{\\readline} primitive@>

@ @<Cases of |read| for |print_cmd_chr|@>=
else print_esc("readline")

@ @<Handle \.{\\readline} and |goto done|@>=
if j=1 then
  begin while loc<=limit do {current line not yet finished}
    begin cur_chr:=buffer[loc]; incr(loc);
    if cur_chr=" " then cur_tok:=space_token
    @+else cur_tok:=cur_chr+other_token;
    store_new_token(cur_tok);
    end;
  goto done;
  end

@ Here we define the additional conditionals of \eTeX\ as well as the
\.{\\unless} prefix.

@d if_def_code=17 { `\.{\\ifdefined}' }
@d if_cs_code=18 { `\.{\\ifcsname}' }
@d if_font_char_code=19 { `\.{\\iffontchar}' }

@<Generate all \eTeX...@>=
primitive("unless",expand_after,1);@/
@!@:unless_}{\.{\\unless} primitive@>
primitive("ifdefined",if_test,if_def_code);
@!@:if_defined_}{\.{\\ifdefined} primitive@>
primitive("ifcsname",if_test,if_cs_code);
@!@:if_cs_name_}{\.{\\ifcsname} primitive@>
primitive("iffontchar",if_test,if_font_char_code);
@!@:if_font_char_}{\.{\\iffontchar} primitive@>

@ @<Cases of |expandafter| for |print_cmd_chr|@>=
else print_esc("unless")

@ @<Cases of |if_test| for |print_cmd_chr|@>=
if_def_code:print_esc("ifdefined");
if_cs_code:print_esc("ifcsname");
if_font_char_code:print_esc("iffontchar");

@ The result of a boolean condition is reversed when the conditional is
preceded by \.{\\unless}.

@<Negate a boolean conditional and |goto reswitch|@>=
begin get_token;
if (cur_cmd=if_test)and(cur_chr<>if_case_code) then
  begin cur_chr:=cur_chr+unless_code; goto reswitch;
  end;
print_err("You can't use `"); print_esc("unless"); print("' before `");
@.You can't use \\unless...@>
print_cmd_chr(cur_cmd,cur_chr); print_char("'");
help1("Continue, and I'll forget that it ever happened.");
back_error;
end

@ The conditional \.{\\ifdefined} tests if a control sequence is
defined.

We need to reset |scanner_status|, since \.{\\outer} control sequences
are allowed, but we might be scanning a macro definition or preamble.

@<Cases for |conditional|@>=
if_def_code:begin save_scanner_status:=scanner_status;
  scanner_status:=normal;
  get_next; b:=(cur_cmd<>undefined_cs);
  scanner_status:=save_scanner_status;
  end;

@ The conditional \.{\\ifcsname} is equivalent to \.{\{\\expandafter}
\.{\}\\expandafter} \.{\\ifdefined} \.{\\csname}, except that no new
control sequence will be entered into the hash table (once all tokens
preceding the mandatory \.{\\endcsname} have been expanded).

@<Cases for |conditional|@>=
if_cs_code:begin n:=get_avail; p:=n; {head of the list of characters}
  repeat get_x_token;
  if cur_cs=0 then store_new_token(cur_tok);
  until cur_cs<>0;
  if cur_cmd<>end_cs_name then @<Complain about missing \.{\\endcsname}@>;
  @<Look up the characters of list |n| in the hash table, and set |cur_cs|@>;
  flush_list(n);
  b:=(eq_type(cur_cs)<>undefined_cs);
  end;

@ @<Look up the characters of list |n| in the hash table...@>=
m:=first; p:=link(n);
while p<>null do
  begin if m>=max_buf_stack then
    begin max_buf_stack:=m+1;
    if max_buf_stack=buf_size then
      overflow("buffer size",buf_size);
@:TeX capacity exceeded buffer size}{\quad buffer size@>
    end;
  buffer[m]:=info(p) mod @'400; incr(m); p:=link(p);
  end;
if m>first+1 then
  cur_cs:=id_lookup(first,m-first) {|no_new_control_sequence| is |true|}
else if m=first then cur_cs:=null_cs {the list is empty}
else cur_cs:=single_base+buffer[first] {the list has length one}

@ The conditional \.{\\iffontchar} tests the existence of a character in
a font.

@<Cases for |conditional|@>=
if_font_char_code:begin scan_font_ident; n:=cur_val; scan_char_num;
  if (font_bc(n)<=cur_val)and(font_ec(n)>=cur_val) then
    b:=char_exists(char_info(n)(qi(cur_val)))
  else b:=false;
  end;

@ The |protected| feature of \eTeX\ defines the \.{\\protected} prefix
command for macro definitions.  Such macros are protected against
expansions when lists of expanded tokens are built, e.g., for \.{\\edef}
or during \.{\\write}.

@<Generate all \eTeX...@>=
primitive("protected",prefix,8);
@!@:protected_}{\.{\\protected} primitive@>

@ @<Cases of |prefix| for |print_cmd_chr|@>=
else if chr_code=8 then print_esc("protected")

@ The |get_x_or_protected| procedure is like |get_x_token| except that
protected macros are not expanded.

@<Declare \eTeX\ procedures for sc...@>=
procedure get_x_or_protected; {sets |cur_cmd|, |cur_chr|, |cur_tok|,
  and expands non-protected macros}
label exit;
begin loop@+begin get_token;
  if cur_cmd<=max_command then return;
  if (cur_cmd>=call)and(cur_cmd<end_template) then
    if info(link(cur_chr))=protected_token then return;
  expand;
  end;
exit:end;

@ A group entered (or a conditional started) in one file may end in a
different file.  Such slight anomalies, although perfectly legitimate,
may cause errors that are difficult to locate.  In order to be able to
give a warning message when such anomalies occur, \eTeX\ uses the
|grp_stack| and |if_stack| arrays to record the initial |cur_boundary|
and |cond_ptr| values for each input file.

@<Glob...@>=
@!grp_stack : array[0..max_in_open] of save_pointer; {initial |cur_boundary|}
@!if_stack : array[0..max_in_open] of pointer; {initial |cond_ptr|}

@ When a group ends that was apparently entered in a different input
file, the |group_warning| procedure is invoked in order to update the
|grp_stack|.  If moreover \.{\\tracingnesting} is positive we want to
give a warning message.  The situation is, however, somewhat complicated
by two facts:  (1)~There may be |grp_stack| elements without a
corresponding \.{\\input} file or \.{\\scantokens} pseudo file (e.g.,
error insertions from the terminal); and (2)~the relevant information is
recorded in the |name_field| of the |input_stack| only loosely
synchronized with the |in_open| variable indexing |grp_stack|.

@<Declare \eTeX\ procedures for tr...@>=
procedure group_warning;
var i:0..max_in_open; {index into |grp_stack|}
@!w:boolean; {do we need a warning?}
begin base_ptr:=input_ptr; input_stack[base_ptr]:=cur_input;
  {store current state}
i:=in_open; w:=false;
while (grp_stack[i]=cur_boundary)and(i>0) do
  begin @<Set variable |w| to indicate if this case should be reported@>;
  grp_stack[i]:=save_index(save_ptr); decr(i);
  end;
if w then
  begin print_nl("Warning: end of "); print_group(true);
@.Warning: end of...@>
  print(" of a different file"); print_ln;
  if tracing_nesting>1 then show_context;
  if history=spotless then history:=warning_issued;
  end;
end;

@ This code scans the input stack in order to determine the type of the
current input file.

@<Set variable |w| to...@>=
if tracing_nesting>0 then
  begin while (input_stack[base_ptr].state_field=token_list)or@|
    (input_stack[base_ptr].index_field>i) do decr(base_ptr);
  if input_stack[base_ptr].name_field>17 then w:=true;
  end

@ When a conditional ends that was apparently started in a different
input file, the |if_warning| procedure is invoked in order to update the
|if_stack|.  If moreover \.{\\tracingnesting} is positive we want to
give a warning message (with the same complications as above).

@<Declare \eTeX\ procedures for tr...@>=
procedure if_warning;
var i:0..max_in_open; {index into |if_stack|}
@!w:boolean; {do we need a warning?}
begin base_ptr:=input_ptr; input_stack[base_ptr]:=cur_input;
  {store current state}
i:=in_open; w:=false;
while if_stack[i]=cond_ptr do
  begin @<Set variable |w| to...@>;
  if_stack[i]:=link(cond_ptr); decr(i);
  end;
if w then
  begin print_nl("Warning: end of "); print_cmd_chr(if_test,cur_if);
@.Warning: end of...@>
  print_if_line(if_line); print(" of a different file"); print_ln;
  if tracing_nesting>1 then show_context;
  if history=spotless then history:=warning_issued;
  end;
end;

@ Conversely, the |file_warning| procedure is invoked when a file ends
and some groups entered or conditionals started while reading from that
file are still incomplete.

@<Declare \eTeX\ procedures for tr...@>=
procedure file_warning;
var p:pointer; {saved value of |save_ptr| or |cond_ptr|}
@!l:quarterword; {saved value of |cur_level| or |if_limit|}
@!c:quarterword; {saved value of |cur_group| or |cur_if|}
@!i:integer; {saved value of |if_line|}
begin p:=save_ptr; l:=cur_level; c:=cur_group; save_ptr:=cur_boundary;
while grp_stack[in_open]<>save_ptr do
  begin decr(cur_level);
  print_nl("Warning: end of file when ");
@.Warning: end of file when...@>
  print_group(true); print(" is incomplete");@/
  cur_group:=save_level(save_ptr); save_ptr:=save_index(save_ptr)
  end;
save_ptr:=p; cur_level:=l; cur_group:=c; {restore old values}
p:=cond_ptr; l:=if_limit; c:=cur_if; i:=if_line;
while if_stack[in_open]<>cond_ptr do
  begin print_nl("Warning: end of file when ");
@.Warning: end of file when...@>
  print_cmd_chr(if_test,cur_if);
  if if_limit=fi_code then print_esc("else");
  print_if_line(if_line); print(" is incomplete");@/
  if_line:=if_line_field(cond_ptr); cur_if:=subtype(cond_ptr);
  if_limit:=type(cond_ptr); cond_ptr:=link(cond_ptr);
  end;
cond_ptr:=p; if_limit:=l; cur_if:=c; if_line:=i; {restore old values}
print_ln;
if tracing_nesting>1 then show_context;
if history=spotless then history:=warning_issued;
end;

@ Here are the additional \eTeX\ primitives for expressions.

@<Generate all \eTeX...@>=
primitive("numexpr",last_item,eTeX_expr-int_val+int_val);
@!@:num_expr_}{\.{\\numexpr} primitive@>
primitive("dimexpr",last_item,eTeX_expr-int_val+dimen_val);
@!@:dim_expr_}{\.{\\dimexpr} primitive@>
primitive("glueexpr",last_item,eTeX_expr-int_val+glue_val);
@!@:glue_expr_}{\.{\\glueexpr} primitive@>
primitive("muexpr",last_item,eTeX_expr-int_val+mu_val);
@!@:mu_expr_}{\.{\\muexpr} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
eTeX_expr-int_val+int_val: print_esc("numexpr");
eTeX_expr-int_val+dimen_val: print_esc("dimexpr");
eTeX_expr-int_val+glue_val: print_esc("glueexpr");
eTeX_expr-int_val+mu_val: print_esc("muexpr");

@ This code for reducing |cur_val_level| and\slash or negating the
result is similar to the one for all the other cases of
|scan_something_internal|, with the difference that |scan_expr| has
already increased the reference count of a glue specification.

@<Process an expression and |return|@>=
begin if m<eTeX_mu then
  begin case m of
  @/@<Cases for fetching a glue value@>@/
  end; {there are no other cases}
  cur_val_level:=glue_val;
  end
else if m<eTeX_expr then
  begin case m of
  @/@<Cases for fetching a mu value@>@/
  end; {there are no other cases}
  cur_val_level:=mu_val;
  end
else  begin cur_val_level:=m-eTeX_expr+int_val; scan_expr;
  end;
while cur_val_level>level do
  begin if cur_val_level=glue_val then
    begin m:=cur_val; cur_val:=width(m); delete_glue_ref(m);
    end
  else if cur_val_level=mu_val then mu_error;
  decr(cur_val_level);
  end;
if negative then
  if cur_val_level>=glue_val then
    begin m:=cur_val; cur_val:=new_spec(m); delete_glue_ref(m);
    @<Negate all three glue components of |cur_val|@>;
    end
  else negate(cur_val);
return;
end

@ @<Declare \eTeX\ procedures for sc...@>=
procedure@?scan_expr; forward;@t\2@>

@ The |scan_expr| procedure scans and evaluates an expression.

@<Declare procedures needed for expressions@>=
@t\4@>@<Declare subprocedures for |scan_expr|@>
procedure scan_expr; {scans and evaluates an expression}
label restart, continue, found;
var a,@!b:boolean; {saved values of |arith_error|}
@!l:small_number; {type of expression}
@!r:small_number; {state of expression so far}
@!s:small_number; {state of term so far}
@!o:small_number; {next operation or type of next factor}
@!e:integer; {expression so far}
@!t:integer; {term so far}
@!f:integer; {current factor}
@!n:integer; {numerator of combined multiplication and division}
@!p:pointer; {top of expression stack}
@!q:pointer; {for stack manipulations}
begin l:=cur_val_level; a:=arith_error; b:=false; p:=null;
incr(expand_depth_count);
if expand_depth_count>=expand_depth then overflow("expansion depth",expand_depth);
@<Scan and evaluate an expression |e| of type |l|@>;
decr(expand_depth_count);
if b then
  begin print_err("Arithmetic overflow");
@.Arithmetic overflow@>
  help2("I can't evaluate this expression,")@/
    ("since the result is out of range.");
  error;
  if l>=glue_val then
    begin delete_glue_ref(e); e:=zero_glue; add_glue_ref(e);
    end
  else e:=0;
  end;
arith_error:=a; cur_val:=e; cur_val_level:=l;
end;

@ Evaluating an expression is a recursive process:  When the left
parenthesis of a subexpression is scanned we descend to the next level
of recursion; the previous level is resumed with the matching right
parenthesis.

@d expr_none=0 {\.( seen, or \.( $\langle\it expr\rangle$ \.) seen}
@d expr_add=1 {\.( $\langle\it expr\rangle$ \.+ seen}
@d expr_sub=2 {\.( $\langle\it expr\rangle$ \.- seen}
@d expr_mult=3 {$\langle\it term\rangle$ \.* seen}
@d expr_div=4 {$\langle\it term\rangle$ \./ seen}
@d expr_scale=5 {$\langle\it term\rangle$ \.*
  $\langle\it factor\rangle$ \./ seen}

@<Scan and eval...@>=
restart: r:=expr_none; e:=0; s:=expr_none; t:=0; n:=0;
continue: if s=expr_none then o:=l@+else o:=int_val;
@<Scan a factor |f| of type |o| or start a subexpression@>;
found: @<Scan the next operator and set |o|@>;
arith_error:=b;
@<Make sure that |f| is in the proper range@>;
case s of @<Cases for evaluation of the current term@>@;
end; {there are no other cases}
if o>expr_sub then s:=o@+else @<Evaluate the current expression@>;
b:=arith_error;
if o<>expr_none then goto continue;
if p<>null then @<Pop the expression stack and |goto found|@>

@ @<Scan the next op...@>=
@<Get the next non-blank non-call token@>;
if cur_tok=other_token+"+" then o:=expr_add
else if cur_tok=other_token+"-" then o:=expr_sub
else if cur_tok=other_token+"*" then o:=expr_mult
else if cur_tok=other_token+"/" then o:=expr_div
else  begin o:=expr_none;
  if p=null then
    begin if cur_cmd<>relax then back_input;
    end
  else if cur_tok<>other_token+")" then
    begin print_err("Missing ) inserted for expression");
@.Missing ) inserted@>
    help1("I was expecting to see `+', `-', `*', `/', or `)'. Didn't.");
    back_error;
    end;
  end

@ @<Scan a factor...@>=
@<Get the next non-blank non-call token@>;
if cur_tok=other_token+"(" then
  @<Push the expression stack and |goto restart|@>;
back_input;
if o=int_val then scan_int
else if o=dimen_val then scan_normal_dimen
else if o=glue_val then scan_normal_glue
else scan_mu_glue;
f:=cur_val

@ @<Declare \eTeX\ procedures for sc...@>=
procedure@?scan_normal_glue; forward;@t\2@>@/
procedure@?scan_mu_glue; forward;@t\2@>

@ Here we declare two trivial procedures in order to avoid mutually
recursive procedures with parameters.

@<Declare procedures needed for expressions@>=
procedure scan_normal_glue;
begin scan_glue(glue_val);
end;
@#
procedure scan_mu_glue;
begin scan_glue(mu_val);
end;

@ Parenthesized subexpressions can be inside expressions, and this
nesting has a stack.  Seven local variables represent the top of the
expression stack:  |p| points to pushed-down entries, if any; |l|
specifies the type of expression currently beeing evaluated; |e| is the
expression so far and |r| is the state of its evaluation; |t| is the
term so far and |s| is the state of its evaluation; finally |n| is the
numerator for a combined multiplication and division, if any.

@d expr_node_size=4 {number of words in stack entry for subexpressions}
@d expr_e_field(#)==mem[#+1].int {saved expression so far}
@d expr_t_field(#)==mem[#+2].int {saved term so far}
@d expr_n_field(#)==mem[#+3].int {saved numerator}

@<Push the expression...@>=
begin q:=get_node(expr_node_size); link(q):=p; type(q):=l;
subtype(q):=4*s+r;
expr_e_field(q):=e; expr_t_field(q):=t; expr_n_field(q):=n;
p:=q; l:=o; goto restart;
end

@ @<Pop the expression...@>=
begin f:=e; q:=p;
e:=expr_e_field(q); t:=expr_t_field(q); n:=expr_n_field(q);
s:=subtype(q) div 4; r:=subtype(q) mod 4;
l:=type(q); p:=link(q); free_node(q,expr_node_size);
goto found;
end

@ We want to make sure that each term and (intermediate) result is in
the proper range.  Integer values must not exceed |infinity|
($2^{31}-1$) in absolute value, dimensions must not exceed |max_dimen|
($2^{30}-1$).  We avoid the absolute value of an integer, because this
might fail for the value $-2^{31}$ using 32-bit arithmetic.

@d num_error(#)== {clear a number or dimension and set |arith_error|}
  begin arith_error:=true; #:=0;
  end
@d glue_error(#)== {clear a glue spec and set |arith_error|}
  begin arith_error:=true; delete_glue_ref(#); #:=new_spec(zero_glue);
  end

@<Make sure that |f|...@>=
if (l=int_val)or(s>expr_sub) then
  begin if (f>infinity)or(f<-infinity) then num_error(f);
  end
else if l=dimen_val then
  begin if abs(f)>max_dimen then num_error(f);
  end
else  begin if (abs(width(f))>max_dimen)or@|
   (abs(stretch(f))>max_dimen)or@|
   (abs(shrink(f))>max_dimen) then glue_error(f);
  end

@ Applying the factor |f| to the partial term |t| (with the operator
|s|) is delayed until the next operator |o| has been scanned.  Here we
handle the first factor of a partial term.  A glue spec has to be copied
unless the next operator is a right parenthesis; this allows us later on
to simply modify the glue components.

@d normalize_glue(#)==
  if stretch(#)=0 then stretch_order(#):=normal;
  if shrink(#)=0 then shrink_order(#):=normal

@<Cases for evaluation of the current term@>=
expr_none: if (l>=glue_val)and(o<>expr_none) then
    begin t:=new_spec(f); delete_glue_ref(f); normalize_glue(t);
    end
  else t:=f;

@ When a term |t| has been completed it is copied to, added to, or
subtracted from the expression |e|.

@d expr_add_sub(#)==add_or_sub(#,r=expr_sub)
@d expr_a(#)==expr_add_sub(#,max_dimen)

@<Evaluate the current expression@>=
begin s:=expr_none;
if r=expr_none then e:=t
else if l=int_val then e:=expr_add_sub(e,t,infinity)
else if l=dimen_val then e:=expr_a(e,t)
else @<Compute the sum or difference of two glue specs@>;
r:=o;
end

@ The function |add_or_sub(x,y,max_answer,negative)| computes the sum
(for |negative=false|) or difference (for |negative=true|) of |x| and
|y|, provided the absolute value of the result does not exceed
|max_answer|.

@<Declare subprocedures for |scan_expr|@>=
function add_or_sub(@!x,@!y,@!max_answer:integer;@!negative:boolean):integer;
var a:integer; {the answer}
begin if negative then negate(y);
if x>=0 then
  if y<=max_answer-x then a:=x+y@+else num_error(a)
else if y>=-max_answer-x then a:=x+y@+else num_error(a);
add_or_sub:=a;
end;

@ We know that |stretch_order(e)>normal| implies |stretch(e)<>0| and
|shrink_order(e)>normal| implies |shrink(e)<>0|.

@<Compute the sum or diff...@>=
begin width(e):=expr_a(width(e),width(t));
if stretch_order(e)=stretch_order(t) then
  stretch(e):=expr_a(stretch(e),stretch(t))
else if (stretch_order(e)<stretch_order(t))and(stretch(t)<>0) then
  begin stretch(e):=stretch(t); stretch_order(e):=stretch_order(t);
  end;
if shrink_order(e)=shrink_order(t) then
  shrink(e):=expr_a(shrink(e),shrink(t))
else if (shrink_order(e)<shrink_order(t))and(shrink(t)<>0) then
  begin shrink(e):=shrink(t); shrink_order(e):=shrink_order(t);
  end;
delete_glue_ref(t); normalize_glue(e);
end

@ If a multiplication is followed by a division, the two operations are
combined into a `scaling' operation.  Otherwise the term |t| is
multiplied by the factor |f|.

@d expr_m(#)==#:=nx_plus_y(#,f,0)

@<Cases for evaluation of the current term@>=
expr_mult: if o=expr_div then
    begin n:=f; o:=expr_scale;
    end
  else if l=int_val then t:=mult_integers(t,f)
  else if l=dimen_val then expr_m(t)
  else  begin expr_m(width(t)); expr_m(stretch(t)); expr_m(shrink(t));
    end;

@ Here we divide the term |t| by the factor |f|.

@d expr_d(#)==#:=quotient(#,f)

@<Cases for evaluation of the current term@>=
expr_div: if l<glue_val then expr_d(t)
  else  begin expr_d(width(t)); expr_d(stretch(t)); expr_d(shrink(t));
    end;

@ The function |quotient(n,d)| computes the rounded quotient
$q=\lfloor n/d+{1\over2}\rfloor$, when $n$ and $d$ are positive.

@<Declare subprocedures for |scan_expr|@>=
function quotient(@!n,@!d:integer):integer;
var negative:boolean; {should the answer be negated?}
@!a:integer; {the answer}
begin if d=0 then num_error(a)
else  begin if d>0 then negative:=false
  else  begin negate(d); negative:=true;
    end;
  if n<0 then
    begin negate(n); negative:=not negative;
    end;
  a:=n div d; n:=n-a*d; d:=n-d; {avoid certain compiler optimizations!}
  if d+n>=0 then incr(a);
  if negative then negate(a);
  end;
quotient:=a;
end;

@ Here the term |t| is multiplied by the quotient $n/f$.

@d expr_s(#)==#:=fract(#,n,f,max_dimen)

@<Cases for evaluation of the current term@>=
expr_scale: if l=int_val then t:=fract(t,n,f,infinity)
  else if l=dimen_val then expr_s(t)
  else  begin expr_s(width(t)); expr_s(stretch(t)); expr_s(shrink(t));
    end;

@ Finally, the function |fract(x,n,d,max_answer)| computes the integer
$q=\lfloor xn/d+{1\over2}\rfloor$, when $x$, $n$, and $d$ are positive
and the result does not exceed |max_answer|.  We can't use floating
point arithmetic since the routine must produce identical results in all
cases; and it would be too dangerous to multiply by~|n| and then divide
by~|d|, in separate operations, since overflow might well occur.  Hence
this subroutine simulates double precision arithmetic, somewhat
analogous to \MF's |make_fraction| and |take_fraction| routines.

@d too_big=88 {go here when the result is too big}

@<Declare subprocedures for |scan_expr|@>=
function fract(@!x,@!n,@!d,@!max_answer:integer):integer;
label found, found1, too_big, done;
var negative:boolean; {should the answer be negated?}
@!a:integer; {the answer}
@!f:integer; {a proper fraction}
@!h:integer; {smallest integer such that |2*h>=d|}
@!r:integer; {intermediate remainder}
@!t:integer; {temp variable}
begin if d=0 then goto too_big;
a:=0;
if d>0 then negative:=false
else  begin negate(d); negative:=true;
  end;
if x<0 then
  begin negate(x); negative:=not negative;
  end
else if x=0 then goto done;
if n<0 then
  begin negate(n); negative:=not negative;
  end;
t:=n div d;
if t>max_answer div x then goto too_big;
a:=t*x; n:=n-t*d;
if n=0 then goto found;
t:=x div d;
if t>(max_answer-a) div n then goto too_big;
a:=a+t*n; x:=x-t*d;
if x=0 then goto found;
if x<n then
  begin t:=x; x:=n; n:=t;
  end; {now |0<n<=x<d|}
@<Compute \(f)$f=\lfloor xn/d+{1\over2}\rfloor$@>@;
if f>(max_answer-a) then goto too_big;
a:=a+f;
found: if negative then negate(a);
goto done;
too_big: num_error(a);
done: fract:=a;
end;

@ The loop here preserves the following invariant relations
between |f|, |x|, |n|, and~|r|:
(i)~$f+\lfloor(xn+(r+d))/d\rfloor=\lfloor x_0n_0/d+{1\over2}\rfloor$;
(ii)~|-d<=r<0<n<=x<d|, where $x_0$, $n_0$ are the original values of~$x$
and $n$.

Notice that the computation specifies |(x-d)+x| instead of |(x+x)-d|,
because the latter could overflow.

@<Compute \(f)$f=\lfloor xn/d+{1\over2}\rfloor$@>=
f:=0; r:=(d div 2)-d; h:=-r;
loop@+begin if odd(n) then
    begin r:=r+x;
    if r>=0 then
      begin r:=r-d; incr(f);
      end;
    end;
  n:=n div 2;
  if n=0 then goto found1;
  if x<h then x:=x+x
  else  begin t:=x-d; x:=t+x; f:=f+n;
      if x<n then
        begin if x=0 then goto found1;
        t:=x; x:=n; n:=t;
        end;
    end;
  end;
found1:

@ The \.{\\gluestretch}, \.{\\glueshrink}, \.{\\gluestretchorder}, and
\.{\\glueshrinkorder} commands return the stretch and shrink components
and their orders of ``infinity'' of a glue specification.

@d glue_stretch_order_code=eTeX_int+6 {code for \.{\\gluestretchorder}}
@d glue_shrink_order_code=eTeX_int+7 {code for \.{\\glueshrinkorder}}
@d glue_stretch_code=eTeX_dim+7 {code for \.{\\gluestretch}}
@d glue_shrink_code=eTeX_dim+8 {code for \.{\\glueshrink}}

@<Generate all \eTeX...@>=
primitive("gluestretchorder",last_item,glue_stretch_order_code);
@!@:glue_stretch_order_}{\.{\\gluestretchorder} primitive@>
primitive("glueshrinkorder",last_item,glue_shrink_order_code);
@!@:glue_shrink_order_}{\.{\\glueshrinkorder} primitive@>
primitive("gluestretch",last_item,glue_stretch_code);
@!@:glue_stretch_}{\.{\\gluestretch} primitive@>
primitive("glueshrink",last_item,glue_shrink_code);
@!@:glue_shrink_}{\.{\\glueshrink} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
glue_stretch_order_code: print_esc("gluestretchorder");
glue_shrink_order_code: print_esc("glueshrinkorder");
glue_stretch_code: print_esc("gluestretch");
glue_shrink_code: print_esc("glueshrink");

@ @<Cases for fetching an integer value@>=
glue_stretch_order_code, glue_shrink_order_code:
  begin scan_normal_glue; q:=cur_val;
  if m=glue_stretch_order_code then cur_val:=stretch_order(q)
  else cur_val:=shrink_order(q);
  delete_glue_ref(q);
  end;

@ @<Cases for fetching a dimension value@>=
glue_stretch_code, glue_shrink_code:
  begin scan_normal_glue; q:=cur_val;
  if m=glue_stretch_code then cur_val:=stretch(q)
  else cur_val:=shrink(q);
  delete_glue_ref(q);
  end;

@ The \.{\\mutoglue} and \.{\\gluetomu} commands convert ``math'' glue
into normal glue and vice versa; they allow to manipulate math glue with
\.{\\gluestretch} etc.

@d mu_to_glue_code=eTeX_glue {code for \.{\\mutoglue}}
@d glue_to_mu_code=eTeX_mu {code for \.{\\gluetomu}}

@<Generate all \eTeX...@>=
primitive("mutoglue",last_item,mu_to_glue_code);
@!@:mu_to_glue_}{\.{\\mutoglue} primitive@>
primitive("gluetomu",last_item,glue_to_mu_code);
@!@:glue_to_mu_}{\.{\\gluetomu} primitive@>

@ @<Cases of |last_item| for |print_cmd_chr|@>=
mu_to_glue_code: print_esc("mutoglue");
glue_to_mu_code: print_esc("gluetomu");

@ @<Cases for fetching a glue value@>=
mu_to_glue_code: scan_mu_glue;

@ @<Cases for fetching a mu value@>=
glue_to_mu_code: scan_normal_glue;

{ FIXME: next chapter is about sparse arrays; how does it cope with Omega? }

@ \eTeX\ (in extended mode) supports 32768 (i.e., $2^{15}$) count,
dimen, skip, muskip, box, and token registers.  As in \TeX\ the first
256 registers of each kind are realized as arrays in the table of
equivalents; the additional registers are realized as tree structures
built from variable-size nodes with individual registers existing only
when needed.  Default values are used for nonexistent registers:  zero
for count and dimen values, |zero_glue| for glue (skip and muskip)
values, void for boxes, and |null| for token lists (and current marks
discussed below).

Similarly there are 32768 mark classes; the command \.{\\marks}|n|
creates a mark node for a given mark class |0<=n<=32767| (where
\.{\\marks0} is synonymous to \.{\\mark}).  The page builder (actually
the |fire_up| routine) and the |vsplit| routine maintain the current
values of |top_mark|, |first_mark|, |bot_mark|, |split_first_mark|, and
|split_bot_mark| for each mark class.  They are accessed as
\.{\\topmarks}|n| etc., and \.{\\topmarks0} is again synonymous to
\.{\\topmark}.  As in \TeX\ the five current marks for mark class zero
are realized as |cur_mark| array.  The additional current marks are
again realized as tree structure with individual mark classes existing
only when needed.

@<Generate all \eTeX...@>=
primitive("marks",mark,marks_code);
@!@:marks_}{\.{\\marks} primitive@>
primitive("topmarks",top_bot_mark,top_mark_code+marks_code);
@!@:top_marks_}{\.{\\topmarks} primitive@>
primitive("firstmarks",top_bot_mark,first_mark_code+marks_code);
@!@:first_marks_}{\.{\\firstmarks} primitive@>
primitive("botmarks",top_bot_mark,bot_mark_code+marks_code);
@!@:bot_marks_}{\.{\\botmarks} primitive@>
primitive("splitfirstmarks",top_bot_mark,split_first_mark_code+marks_code);
@!@:split_first_marks_}{\.{\\splitfirstmarks} primitive@>
primitive("splitbotmarks",top_bot_mark,split_bot_mark_code+marks_code);
@!@:split_bot_marks_}{\.{\\splitbotmarks} primitive@>

@ The |scan_register_num| procedure scans a register number that must
not exceed 255 in compatibility mode resp.\ 32767 in extended mode.

@<Declare \eTeX\ procedures for ex...@>=
procedure@?scan_register_num; forward;@t\2@>

@ @<Declare procedures that scan restricted classes of integers@>=
procedure scan_register_num;
begin scan_int;
if (cur_val<0)or(cur_val>max_reg_num) then
  begin print_err("Bad register code");
@.Bad register code@>
  help2(max_reg_help_line)("I changed this one to zero.");
  int_error(cur_val); cur_val:=0;
  end;
end;

@ @<Initialize variables for \eTeX\ comp...@>=
max_reg_num:=255;
max_reg_help_line:="A register number must be between 0 and 255.";

@ @<Initialize variables for \eTeX\ ext...@>=
max_reg_num:=32767;
max_reg_help_line:="A register number must be between 0 and 32767.";

@ @<Glob...@>=
@!max_reg_num: halfword; {largest allowed register number}
@!max_reg_help_line: str_number; {first line of help message}

@ There are seven almost identical doubly linked trees, one for the
sparse array of the up to 32512 additional registers of each kind and
one for the sparse array of the up to 32767 additional mark classes.
The root of each such tree, if it exists, is an index node containing 16
pointers to subtrees for 4096 consecutive array elements.  Similar index
nodes are the starting points for all nonempty subtrees for 4096, 256,
and 16 consecutive array elements.  These four levels of index nodes are
followed by a fifth level with nodes for the individual array elements.

Each index node is nine words long.  The pointers to the 16 possible
subtrees or are kept in the |info| and |link| fields of the last eight
words.  (It would be both elegant and efficient to declare them as
array, unfortunately \PASCAL\ doesn't allow this.)

The fields in the first word of each index node and in the nodes for the
array elements are closely related.  The |link| field points to the next
lower index node and the |sa_index| field contains four bits (one
hexadecimal digit) of the register number or mark class.  For the lowest
index node the |link| field is |null| and the |sa_index| field indicates
the type of quantity (|int_avl|, |dimen_val|, |glue_val|, |mu_val|,
|box_val|, |tok_val|, or |mark_val|).  The |sa_used| field in the index
nodes counts how many of the 16 pointers are non-null.

The |sa_index| field in the nodes for array elements contains the four
bits plus 16 times the type.  Therefore such a node represents a count
or dimen register if and only if |sa_index<dimen_val_limit|; it
represents a skip or muskip register if and only if
|dimen_val_limit<=sa_index<mu_val_limit|; it represents a box register
if and only if |mu_val_limit<=sa_index<box_val_limit|; it represents a
token list register if and only if
|box_val_limit<=sa_index<tok_val_limit|; finally it represents a mark
class if and only if |tok_val_limit<=sa_index|.

The |new_index| procedure creates an index node (returned in |cur_ptr|)
having given contents of the |sa_index| and |link| fields.

@d box_val==4 {the additional box registers}
@d mark_val=6 {the additional mark classes}
@#
@d dimen_val_limit=@"20 {$2^4\cdot(|dimen_val|+1)$}
@d mu_val_limit=@"40 {$2^4\cdot(|mu_val|+1)$}
@d box_val_limit=@"50 {$2^4\cdot(|box_val|+1)$}
@d tok_val_limit=@"60 {$2^4\cdot(|tok_val|+1)$}
@#
@d index_node_size=9 {size of an index node}
@d sa_index==type {a four-bit address or a type or both}
@d sa_used==subtype {count of non-null pointers}

@<Declare \eTeX\ procedures for ex...@>=
procedure new_index(@!i:quarterword; @!q:pointer);
var k:small_number; {loop index}
begin cur_ptr:=get_node(index_node_size); sa_index(cur_ptr):=i;
sa_used(cur_ptr):=0; link(cur_ptr):=q;
for k:=1 to index_node_size-1 do {clear all 16 pointers}
  mem[cur_ptr+k]:=sa_null;
end;

@ The roots of the seven trees for the additional registers and mark
classes are kept in the |sa_root| array.  The first six locations must
be dumped and undumped; the last one is also known as |sa_mark|.

@d sa_mark==sa_root[mark_val] {root for mark classes}

@<Glob...@>=
@!sa_root:array[int_val..mark_val] of pointer; {roots of sparse arrays}
@!cur_ptr:pointer; {value returned by |new_index| and |find_sa_element|}
@!sa_null:memory_word; {two |null| pointers}

@ @<Set init...@>=
sa_mark:=null; sa_null.hh.lh:=null; sa_null.hh.rh:=null;

@ @<Initialize table...@>=
for i:=int_val to tok_val do sa_root[i]:=null;

@ Given a type |t| and a sixteen-bit number |n|, the |find_sa_element|
procedure returns (in |cur_ptr|) a pointer to the node for the
corresponding array element, or |null| when no such element exists.  The
third parameter |w| is set |true| if the element must exist, e.g.,
because it is about to be modified.  The procedure has two main
branches:  one follows the existing tree structure, the other (only used
when |w| is |true|) creates the missing nodes.

We use macros to extract the four-bit pieces from a sixteen-bit register
number or mark class and to fetch or store one of the 16 pointers from
an index node.

@d if_cur_ptr_is_null_then_return_or_goto(#)== {some tree element is missing}
  begin if cur_ptr=null then
    if w then goto #@+else return;
  end
@#
@d hex_dig1(#)==# div 4096 {the fourth lowest hexadecimal digit}
@d hex_dig2(#)==(# div 256) mod 16 {the third lowest hexadecimal digit}
@d hex_dig3(#)==(# div 16) mod 16 {the second lowest hexadecimal digit}
@d hex_dig4(#)==# mod 16 {the lowest hexadecimal digit}
@#
@d get_sa_ptr==if odd(i) then cur_ptr:=link(q+(i div 2)+1)
  else cur_ptr:=info(q+(i div 2)+1)
    {set |cur_ptr| to the pointer indexed by |i| from index node |q|}
@d put_sa_ptr(#)==if odd(i) then link(q+(i div 2)+1):=#
  else info(q+(i div 2)+1):=#
    {store the pointer indexed by |i| in index node |q|}
@d add_sa_ptr==begin put_sa_ptr(cur_ptr); incr(sa_used(q));
  end {add |cur_ptr| as the pointer indexed by |i| in index node |q|}
@d delete_sa_ptr==begin put_sa_ptr(null); decr(sa_used(q));
  end {delete the pointer indexed by |i| in index node |q|}

@<Declare \eTeX\ procedures for ex...@>=
procedure find_sa_element(@!t:small_number;@!n:halfword;@!w:boolean);
  {sets |cur_val| to sparse array element location or |null|}
label not_found,not_found1,not_found2,not_found3,not_found4,exit;
var q:pointer; {for list manipulations}
@!i:small_number; {a four bit index}
begin cur_ptr:=sa_root[t];
if_cur_ptr_is_null_then_return_or_goto(not_found);@/
q:=cur_ptr; i:=hex_dig1(n); get_sa_ptr;
if_cur_ptr_is_null_then_return_or_goto(not_found1);@/
q:=cur_ptr; i:=hex_dig2(n); get_sa_ptr;
if_cur_ptr_is_null_then_return_or_goto(not_found2);@/
q:=cur_ptr; i:=hex_dig3(n); get_sa_ptr;
if_cur_ptr_is_null_then_return_or_goto(not_found3);@/
q:=cur_ptr; i:=hex_dig4(n); get_sa_ptr;
if (cur_ptr=null)and w then goto not_found4;
return;
not_found: new_index(t,null); {create first level index node}
sa_root[t]:=cur_ptr; q:=cur_ptr; i:=hex_dig1(n);
not_found1: new_index(i,q); {create second level index node}
add_sa_ptr; q:=cur_ptr; i:=hex_dig2(n);
not_found2: new_index(i,q); {create third level index node}
add_sa_ptr; q:=cur_ptr; i:=hex_dig3(n);
not_found3: new_index(i,q); {create fourth level index node}
add_sa_ptr; q:=cur_ptr; i:=hex_dig4(n);
not_found4: @<Create a new array element of type |t| with index |i|@>;
link(cur_ptr):=q; add_sa_ptr;
exit:end;

@ The array elements for registers are subject to grouping and have an
|sa_lev| field (quite analogous to |eq_level|) instead of |sa_used|.
Since saved values as well as shorthand definitions (created by e.g.,
\.{\\countdef}) refer to the location of the respective array element,
we need a reference count that is kept in the |sa_ref| field.  An array
element can be deleted (together with all references to it) when its
|sa_ref| value is |null| and its value is the default value.
@^reference counts@>

Skip, muskip, box, and token registers use two word nodes, their values
are stored in the |sa_ptr| field.
Count and dimen registers use three word nodes, their
values are stored in the |sa_int| resp.\ |sa_dim| field in the third
word; the |sa_ptr| field is used under the name |sa_num| to store
the register number.  Mark classes use four word nodes.  The last three
words contain the five types of current marks

@d sa_lev==sa_used {grouping level for the current value}
@d pointer_node_size=2 {size of an element with a pointer value}
@d sa_type(#)==(sa_index(#) div 16) {type part of combined type/index}
@d sa_ref(#)==info(#+1) {reference count of a sparse array element}
@d sa_ptr(#)==link(#+1) {a pointer value}
@#
@d word_node_size=3 {size of an element with a word value}
@d sa_num==sa_ptr {the register number}
@d sa_int(#)==mem[#+2].int {an integer}
@d sa_dim(#)==mem[#+2].sc {a dimension (a somewhat esotheric distinction)}
@#
@d mark_class_node_size=4 {size of an element for a mark class}
@#
@d fetch_box(#)== {fetch |box(cur_val)|}
  if cur_val<256 then #:=box(cur_val)
  else  begin find_sa_element(box_val,cur_val,false);
    if cur_ptr=null then #:=null@+else #:=sa_ptr(cur_ptr);
    end

@<Create a new array element...@>=
if t=mark_val then {a mark class}
  begin cur_ptr:=get_node(mark_class_node_size);
  mem[cur_ptr+1]:=sa_null; mem[cur_ptr+2]:=sa_null; mem[cur_ptr+3]:=sa_null;
  end
else  begin if t<=dimen_val then {a count or dimen register}
    begin cur_ptr:=get_node(word_node_size); sa_int(cur_ptr):=0;
    sa_num(cur_ptr):=n;
    end
  else  begin cur_ptr:=get_node(pointer_node_size);
    if t<=mu_val then {a skip or muskip register}
      begin sa_ptr(cur_ptr):=zero_glue; add_glue_ref(zero_glue);
      end
    else sa_ptr(cur_ptr):=null; {a box or token list register}
    end;
  sa_ref(cur_ptr):=null; {all registers have a reference count}
  end;
sa_index(cur_ptr):=16*t+i; sa_lev(cur_ptr):=level_one

@ The |delete_sa_ref| procedure is called when a pointer to an array
element representing a register is being removed; this means that the
reference count should be decreased by one.  If the reduced reference
count is |null| and the register has been (globally) assigned its
default value the array element should disappear, possibly together with
some index nodes.  This procedure will never be used for mark class
nodes.
@^reference counts@>

@d add_sa_ref(#)==incr(sa_ref(#)) {increase reference count}
@#
@d change_box(#)== {change |box(cur_val)|, the |eq_level| stays the same}
  if cur_val<256 then set_equiv(box_base+cur_val,#)@+else set_sa_box(#)
@#

{ FIXME: needs debugging (sparse arrays) }
@d set_sa_box(#)==begin find_sa_element(box_val,cur_val,false);
  if cur_ptr<>0 then
    begin
      set_equiv(sa_ptr(cur_ptr),#);
      add_sa_ref(cur_ptr);
      delete_sa_ref(cur_ptr);
    end;
  end

@<Declare \eTeX\ procedures for tr...@>=
procedure delete_sa_ref(@!q:pointer); {reduce reference count}
label exit;
var p:pointer; {for list manipulations}
@!i:small_number; {a four bit index}
@!s:small_number; {size of a node}
begin decr(sa_ref(q));
if sa_ref(q)<>null then return;
if sa_index(q)<dimen_val_limit then
 if sa_int(q)=0 then s:=word_node_size
 else return
else  begin if sa_index(q)<mu_val_limit then
    if sa_ptr(q)=zero_glue then delete_glue_ref(zero_glue)
    else return
  else if sa_ptr(q)<>null then return;
  s:=pointer_node_size;
  end;
repeat i:=hex_dig4(sa_index(q)); p:=q; q:=link(p); free_node(p,s);
if q=null then {the whole tree has been freed}
  begin sa_root[i]:=null; return;
  end;
delete_sa_ptr; s:=index_node_size; {node |q| is an index node}
until sa_used(q)>0;
exit:end;

@ The |print_sa_num| procedure prints the register number corresponding
to an array element.

@<Basic print...@>=
procedure print_sa_num(@!q:pointer); {print register number}
var @!n:halfword; {the register number}
begin if sa_index(q)<dimen_val_limit then n:=sa_num(q) {the easy case}
else  begin n:=hex_dig4(sa_index(q)); q:=link(q); n:=n+16*sa_index(q);
  q:=link(q); n:=n+256*(sa_index(q)+16*sa_index(link(q)));
  end;
print_int(n);
end;

@ Here is a procedure that displays the contents of an array element
symbolically.  It is used under similar circumstances as is
|restore_trace| (together with |show_eqtb|) for the quantities kept in
the |eqtb| array.

@<Declare \eTeX\ procedures for tr...@>=
@!stat procedure show_sa(@!p:pointer;@!s:str_number);
var t:small_number; {the type of element}
begin begin_diagnostic; print_char("{"); print(s); print_char(" ");
if p=null then print_char("?") {this can't happen}
else  begin t:=sa_type(p);
  if t<box_val then print_cmd_chr(register,p)
  else if t=box_val then
    begin print_esc("box"); print_sa_num(p);
    end
  else if t=tok_val then print_cmd_chr(toks_register,p)
  else print_char("?"); {this can't happen either}
  print_char("=");
  if t=int_val then print_int(sa_int(p))
  else if t=dimen_val then
    begin print_scaled(sa_dim(p)); print("pt");
    end
  else  begin p:=sa_ptr(p);
    if t=glue_val then print_spec(p,"pt")
    else if t=mu_val then print_spec(p,"mu")
    else if t=box_val then
      if p=null then print("void")
      else  begin depth_threshold:=0; breadth_max:=1; show_node_list(p);
        end
    else if t=tok_val then
      begin if p<>null then show_token_list(link(p),null,32);
      end
    else print_char("?"); {this can't happen either}
    end;
  end;
print_char("}"); end_diagnostic(false);
end;
tats

@ Here we compute the pointer to the current mark of type |t| and mark
class |cur_val|.

@<Compute the mark pointer...@>=
begin find_sa_element(mark_val,cur_val,false);
if cur_ptr<>null then
  if odd(t) then cur_ptr:=link(cur_ptr+(t div 2)+1)
  else cur_ptr:=info(cur_ptr+(t div 2)+1);
end

@ The current marks for all mark classes are maintained by the |vsplit|
and |fire_up| routines and are finally destroyed (for \.{INITEX} only)
@.INITEX@>
by the |final_cleanup| routine.  Apart from updating the current marks
when mark nodes are encountered, these routines perform certain actions
on all existing mark classes.  The recursive |do_marks| procedure walks
through the whole tree or a subtree of existing mark class nodes and
preforms certain actions indicted by its first parameter |a|, the action
code.  The second parameter |l| indicates the level of recursion (at
most four); the third parameter points to a nonempty tree or subtree.
The result is |true| if the complete tree or subtree has been deleted.

@d vsplit_init==0 {action code for |vsplit| initialization}
@d fire_up_init==1 {action code for |fire_up| initialization}
@d fire_up_done==2 {action code for |fire_up| completion}
@d destroy_marks==3 {action code for |final_cleanup|}
@#
@d sa_top_mark(#)==info(#+1) {\.{\\topmarks}|n|}
@d sa_first_mark(#)==link(#+1) {\.{\\firstmarks}|n|}
@d sa_bot_mark(#)==info(#+2) {\.{\\botmarks}|n|}
@d sa_split_first_mark(#)==link(#+2) {\.{\\splitfirstmarks}|n|}
@d sa_split_bot_mark(#)==info(#+3) {\.{\\splitbotmarks}|n|}

@<Declare the function called |do_marks|@>=
function do_marks(@!a,@!l:small_number;@!q:pointer):boolean;
var i:small_number; {a four bit index}
begin if l<4 then {|q| is an index node}
  begin for i:=0 to 15 do
    begin get_sa_ptr;
    if cur_ptr<>null then if do_marks(a,l+1,cur_ptr) then delete_sa_ptr;
    end;
  if sa_used(q)=0 then
    begin free_node(q,index_node_size); q:=null;
    end;
  end
else {|q| is the node for a mark class}
  begin case a of
  @<Cases for |do_marks|@>@;
  end; {there are no other cases}
  if sa_bot_mark(q)=null then if sa_split_bot_mark(q)=null then
    begin free_node(q,mark_class_node_size); q:=null;
    end;
  end;
do_marks:=(q=null);
end;

@ At the start of the |vsplit| routine the existing |split_fist_mark|
and |split_bot_mark| are discarded.

@<Cases for |do_marks|@>=
vsplit_init: if sa_split_first_mark(q)<>null then
  begin delete_token_ref(sa_split_first_mark(q)); sa_split_first_mark(q):=null;
  delete_token_ref(sa_split_bot_mark(q)); sa_split_bot_mark(q):=null;
  end;

@ We use again the fact that |split_first_mark=null| if and only if
|split_bot_mark=null|.

@<Update the current marks for |vsplit|@>=
begin find_sa_element(mark_val,mark_class(p),true);
if sa_split_first_mark(cur_ptr)=null then
  begin sa_split_first_mark(cur_ptr):=mark_ptr(p);
  add_token_ref(mark_ptr(p));
  end
else delete_token_ref(sa_split_bot_mark(cur_ptr));
sa_split_bot_mark(cur_ptr):=mark_ptr(p);
add_token_ref(mark_ptr(p));
end

@ At the start of the |fire_up| routine the old |top_mark| and
|first_mark| are discarded, whereas the old |bot_mark| becomes the new
|top_mark|.  An empty new |top_mark| token list is, however, discarded
as well in order that mark class nodes can eventually be released.  We
use again the fact that |bot_mark<>null| implies |first_mark<>null|; it
also knows that |bot_mark=null| implies |top_mark=first_mark=null|.

@<Cases for |do_marks|@>=
fire_up_init: if sa_bot_mark(q)<>null then
  begin if sa_top_mark(q)<>null then delete_token_ref(sa_top_mark(q));
  delete_token_ref(sa_first_mark(q)); sa_first_mark(q):=null;
  if link(sa_bot_mark(q))=null then {an empty token list}
    begin delete_token_ref(sa_bot_mark(q)); sa_bot_mark(q):=null;
    end
  else add_token_ref(sa_bot_mark(q));
  sa_top_mark(q):=sa_bot_mark(q);
  end;

@ @<Cases for |do_marks|@>=
fire_up_done: if (sa_top_mark(q)<>null)and(sa_first_mark(q)=null) then
  begin sa_first_mark(q):=sa_top_mark(q); add_token_ref(sa_top_mark(q));
  end;

@ @<Update the current marks for |fire_up|@>=
begin find_sa_element(mark_val,mark_class(p),true);
if sa_first_mark(cur_ptr)=null then
  begin sa_first_mark(cur_ptr):=mark_ptr(p);
  add_token_ref(mark_ptr(p));
  end;
if sa_bot_mark(cur_ptr)<>null then delete_token_ref(sa_bot_mark(cur_ptr));
sa_bot_mark(cur_ptr):=mark_ptr(p); add_token_ref(mark_ptr(p));
end

@ Here we use the fact that the five current mark pointers in a mark
class node occupy the same locations as the first five pointers of
an index node.  For systems using a run-time switch to distinguish
between \.{VIRTEX} and \.{INITEX}, the codewords `$|init|\ldots|tini|$'
surrounding the following piece of code should be removed.
@.INITEX@>
@^system dependencies@>

@<Cases for |do_marks|@>=
@!init destroy_marks: for i:=top_mark_code to split_bot_mark_code do
  begin get_sa_ptr;
  if cur_ptr<>null then
    begin delete_token_ref(cur_ptr); put_sa_ptr(null);
    end;
  end;
tini

@ The command code |register| is used for `\.{\\count}', `\.{\\dimen}',
etc., as well as for references to sparse array elements defined by
`\.{\\countdef}', etc.

@<Cases of |register| for |print_cmd_chr|@>=
begin if (chr_code<mem_bot)or(chr_code>lo_mem_stat_max) then
  cmd:=sa_type(chr_code)
else  begin cmd:=chr_code-mem_bot; chr_code:=null;
  end;
if cmd=int_val then print_esc("count")
else if cmd=dimen_val then print_esc("dimen")
else if cmd=glue_val then print_esc("skip")
else print_esc("muskip");
if chr_code<>null then print_sa_num(chr_code);
end

@ Similarly the command code |toks_register| is used for `\.{\\toks}' as
well as for references to sparse array elements defined by
`\.{\\toksdef}'.

@<Cases of |toks_register| for |print_cmd_chr|@>=
begin print_esc("toks");
if chr_code<>mem_bot then print_sa_num(chr_code);
end

@ When a shorthand definition for an element of one of the sparse arrays
is destroyed, we must reduce the reference count.

@<Cases for |eq_destroy|@>=
toks_register,register:
  if (equiv_field(w)<mem_bot)or(equiv_field(w)>lo_mem_stat_max) then
    delete_sa_ref(equiv_field(w));

@ The task to maintain (change, save, and restore) register values is
essentially the same when the register is realized as sparse array
element or entry in |eqtb|.  The global variable |sa_chain| is the head
of a linked list of entries saved at the topmost level |sa_level|; the
lists for lowel levels are kept in special save stack entries.

@<Glob...@>=
@!sa_chain: pointer; {chain of saved sparse array entries}
@!sa_level: quarterword; {group level for |sa_chain|}

@ @<Set init...@>=
sa_chain:=null; sa_level:=level_zero;

@ The individual saved items are kept in pointer or word nodes similar
to those used for the array elements: a word node with value zero is,
however, saved as pointer node with the otherwise impossible |sa_index|
value |tok_val_limit|.

@d sa_loc==sa_ref {location of saved item}

@<Declare \eTeX\ procedures for tr...@>=
procedure sa_save(@!p:pointer); {saves value of |p|}
var q:pointer; {the new save node}
@!i:quarterword; {index field of node}
begin if cur_level<>sa_level then
  begin check_full_save_stack; save_type(save_ptr):=restore_sa;
  save_level(save_ptr):=sa_level; save_index(save_ptr):=sa_chain;
  incr(save_ptr); sa_chain:=null; sa_level:=cur_level;
  end;
i:=sa_index(p);
if i<dimen_val_limit then
  begin if sa_int(p)=0 then
    begin q:=get_node(pointer_node_size); i:=tok_val_limit;
    end
  else  begin q:=get_node(word_node_size); sa_int(q):=sa_int(p);
    end;
  sa_ptr(q):=null;
  end
else  begin q:=get_node(pointer_node_size); sa_ptr(q):=sa_ptr(p);
  end;
sa_loc(q):=p; sa_index(q):=i; sa_lev(q):=sa_lev(p);
link(q):=sa_chain; sa_chain:=q; add_sa_ref(p);
end;

@ @<Declare \eTeX\ procedures for tr...@>=
procedure sa_destroy(@!p:pointer); {destroy value of |p|}
begin if sa_index(p)<mu_val_limit then delete_glue_ref(sa_ptr(p))
else if sa_ptr(p)<>null then
  if sa_index(p)<box_val_limit then flush_node_list(sa_ptr(p))
  else delete_token_ref(sa_ptr(p));
end;

@ The procedure |sa_def| assigns a new value to sparse array elements,
and saves the former value if appropriate.  This procedure is used only
for skip, muskip, box, and token list registers.  The counterpart of
|sa_def| for count and dimen registers is called |sa_w_def|.

@d sa_define(#)==if e then
    if global then gsa_def(#)@+else sa_def(#)
  else define
@#
@d sa_def_box== {assign |cur_box| to |box(cur_val)|}
  begin find_sa_element(box_val,cur_val,true);
  if global then gsa_def(cur_ptr,cur_box)@+else sa_def(cur_ptr,cur_box);
  end
@#
@d sa_word_define(#)==if e then
    if global then gsa_w_def(#)@+else sa_w_def(#)
  else word_define(#)

@<Declare \eTeX\ procedures for tr...@>=
procedure sa_def(@!p:pointer;@!e:halfword);
  {new data for sparse array elements}
begin add_sa_ref(p);
if sa_ptr(p)=e then
  begin @!stat if tracing_assigns>0 then show_sa(p,"reassigning");@+tats@;@/
  sa_destroy(p);
  end
else  begin @!stat if tracing_assigns>0 then show_sa(p,"changing");@+tats@;@/
  if sa_lev(p)=cur_level then sa_destroy(p)@+else sa_save(p);
  sa_lev(p):=cur_level; sa_ptr(p):=e;
  @!stat if tracing_assigns>0 then show_sa(p,"into");@+tats@;@/
  end;
delete_sa_ref(p);
end;
@#
procedure sa_w_def(@!p:pointer;@!w:integer);
begin add_sa_ref(p);
if sa_int(p)=w then
  begin @!stat if tracing_assigns>0 then show_sa(p,"reassigning");@+tats@;@/
  end
else  begin @!stat if tracing_assigns>0 then show_sa(p,"changing");@+tats@;@/
  if sa_lev(p)<>cur_level then sa_save(p);
  sa_lev(p):=cur_level; sa_int(p):=w;
  @!stat if tracing_assigns>0 then show_sa(p,"into");@+tats@;@/
  end;
delete_sa_ref(p);
end;

@ The |sa_def| and |sa_w_def| routines take care of local definitions.
@^global definitions@>
Global definitions are done in almost the same way, but there is no need
to save old values, and the new value is associated with |level_one|.

@<Declare \eTeX\ procedures for tr...@>=
procedure gsa_def(@!p:pointer;@!e:halfword); {global |sa_def|}
begin add_sa_ref(p);
@!stat if tracing_assigns>0 then show_sa(p,"globally changing");@+tats@;@/
sa_destroy(p); sa_lev(p):=level_one; sa_ptr(p):=e;
@!stat if tracing_assigns>0 then show_sa(p,"into");@+tats@;@/
delete_sa_ref(p);
end;
@#
procedure gsa_w_def(@!p:pointer;@!w:integer); {global |sa_w_def|}
begin add_sa_ref(p);
@!stat if tracing_assigns>0 then show_sa(p,"globally changing");@+tats@;@/
sa_lev(p):=level_one; sa_int(p):=w;
@!stat if tracing_assigns>0 then show_sa(p,"into");@+tats@;@/
delete_sa_ref(p);
end;

@ The |sa_restore| procedure restores the sparse array entries pointed
at by |sa_chain|

@<Declare \eTeX\ procedures for tr...@>=
procedure sa_restore;
var p:pointer; {sparse array element}
begin repeat p:=sa_loc(sa_chain);
if sa_lev(p)=level_one then
  begin if sa_index(p)>=dimen_val_limit then sa_destroy(sa_chain);
  @!stat if tracing_restores>0 then show_sa(p,"retaining");@+tats@;@/
  end
else  begin if sa_index(p)<dimen_val_limit then
    if sa_index(sa_chain)<dimen_val_limit then sa_int(p):=sa_int(sa_chain)
    else sa_int(p):=0
  else  begin sa_destroy(p); sa_ptr(p):=sa_ptr(sa_chain);
    end;
  sa_lev(p):=sa_lev(sa_chain);
  @!stat if tracing_restores>0 then show_sa(p,"restoring");@+tats@;@/
  end;
delete_sa_ref(p);
p:=sa_chain; sa_chain:=link(p);
if sa_index(p)<dimen_val_limit then free_node(p,word_node_size)
else free_node(p,pointer_node_size);
until sa_chain=null;
end;

{FIXME: |last_line_fit| might be incompatible with Omega}

@ When the value of |last_line_fit| is positive, the last line of a
(partial) paragraph is treated in a special way and we need additional
fields in the active nodes.

@d active_node_size_extended=5 {number of words in extended active nodes}
@d active_short(#)==mem[#+3].sc {|shortfall| of this line}
@d active_glue(#)==mem[#+4].sc {corresponding glue stretch or shrink}

@<Glob...@>=
@!last_line_fill:pointer; {the |par_fill_skip| glue node of the new paragraph}
@!do_last_line_fit:boolean; {special algorithm for last line of paragraph?}
@!active_node_size:small_number; {number of words in active nodes}
@!fill_width:array[0..2] of scaled; {infinite stretch components of
  |par_fill_skip|}
@!best_pl_short:array[very_loose_fit..tight_fit] of scaled; {|shortfall|
  corresponding to |minimal_demerits|}
@!best_pl_glue:array[very_loose_fit..tight_fit] of scaled; {corresponding
  glue stretch or shrink}

@ The new algorithm for the last line requires that the stretchability
|par_fill_skip| is infinite and the stretchability of |left_skip| plus
|right_skip| is finite.

@<Check for special...@>=
do_last_line_fit:=false; active_node_size:=active_node_size_normal;
  {just in case}
if last_line_fit>0 then
  begin q:=glue_ptr(last_line_fill);
  if (stretch(q)>0)and(stretch_order(q)>normal) then
    if (background[3]=0)and(background[4]=0)and(background[5]=0) then
    begin do_last_line_fit:=true;
    active_node_size:=active_node_size_extended;
    fill_width[0]:=0; fill_width[1]:=0; fill_width[2]:=0;
    fill_width[stretch_order(q)-1]:=stretch(q);
    end;
  end

@ @<Other local variables for |try_break|@>=
@!g:scaled; {glue stretch or shrink of test line, adjustment for last line}

@ Here we initialize the additional fields of the first active node
representing the beginning of the paragraph.

@<Initialize additional fields of the first active node@>=
begin active_short(q):=0; active_glue(q):=0;
end

@ Here we compute the adjustment |g| and badness |b| for a line from |r|
to the end of the paragraph.  When any of the criteria for adjustment is
violated we fall through to the normal algorithm.

The last line must be too short, and have infinite stretch entirely due
to |par_fill_skip|.

@<Perform computations for last line and |goto found|@>=
begin if (active_short(r)=0)or(active_glue(r)<=0) then goto not_found;
  {previous line was neither stretched nor shrunk, or was infinitely bad}
if (cur_active_width[3]<>fill_width[0])or@|
  (cur_active_width[4]<>fill_width[1])or@|
  (cur_active_width[5]<>fill_width[2]) then goto not_found;
  {infinite stretch of this line not entirely due to |par_fill_skip|}
if active_short(r)>0 then g:=cur_active_width[2]
else g:=cur_active_width[6];
if g<=0 then goto not_found; {no finite stretch resp.\ no shrink}
arith_error:=false; g:=fract(g,active_short(r),active_glue(r),max_dimen);
if last_line_fit<1000 then g:=fract(g,last_line_fit,1000,max_dimen);
if arith_error then
  if active_short(r)>0 then g:=max_dimen@+else g:=-max_dimen;
if g>0 then
  @<Set the value of |b| to the badness of the last line for stretching,
    compute the corresponding |fit_class, and |goto found||@>
else if g<0 then
  @<Set the value of |b| to the badness of the last line for shrinking,
    compute the corresponding |fit_class, and |goto found||@>;
not_found:end

@ These badness computations are rather similar to those of the standard
algorithm, with the adjustment amount |g| replacing the |shortfall|.

@<Set the value of |b| to the badness of the last line for str...@>=
begin if g>shortfall then g:=shortfall;
if g>7230584 then if cur_active_width[2]<1663497 then
  begin b:=inf_bad; fit_class:=very_loose_fit; goto found;
  end;
b:=badness(g,cur_active_width[2]);
if b>12 then
  if b>99 then fit_class:=very_loose_fit
  else fit_class:=loose_fit
else fit_class:=decent_fit;
goto found;
end

@ @<Set the value of |b| to the badness of the last line for shr...@>=
begin if -g>cur_active_width[6] then g:=-cur_active_width[6];
b:=badness(-g,cur_active_width[6]);
if b>12 then fit_class:=tight_fit@+else fit_class:=decent_fit;
goto found;
end

@ Vanishing values of |shortfall| and |g| indicate that the last line is
not adjusted.

@<Adjust \(t)the additional data for last line@>=
begin if cur_p=null then shortfall:=0;
if shortfall>0 then g:=cur_active_width[2]
else if shortfall<0 then g:=cur_active_width[6]
else g:=0;
end

@ For each feasible break we record the shortfall and glue stretch or
shrink (or adjustment).

@<Store \(a)additional data for this feasible break@>=
begin best_pl_short[fit_class]:=shortfall; best_pl_glue[fit_class]:=g;
end

@ Here we save these data in the active node representing a potential
line break.

@<Store \(a)additional data in the new active node@>=
begin active_short(q):=best_pl_short[fit_class];
active_glue(q):=best_pl_glue[fit_class];
end

@ @<Print additional data in the new active node@>=
begin print(" s="); print_scaled(active_short(q));
if cur_p=null then print(" a=")@+else print(" g=");
print_scaled(active_glue(q));
end

@ Here we either reset |do_last_line_fit| or adjust the |par_fill_skip|
glue.

@<Adjust \(t)the final line of the paragraph@>=
if active_short(best_bet)=0 then do_last_line_fit:=false
else  begin q:=new_spec(glue_ptr(last_line_fill));
  delete_glue_ref(glue_ptr(last_line_fill));
  width(q):=width(q)+active_short(best_bet)-active_glue(best_bet);
  stretch(q):=0; glue_ptr(last_line_fill):=q;
  end

@ When reading \.{\\patterns} while \.{\\savinghyphcodes} is positive
the current |lc_code| values are stored together with the hyphenation
patterns for the current language.  They will later be used instead of
the |lc_code| values for hyphenation purposes.

The |lc_code| values are stored in the linked trie analogous to patterns
$p_1$ of length~1, with |hyph_root=trie_r[0]| replacing |trie_root| and
|lc_code(p_1)| replacing the |trie_op| code.  This allows to compress
and pack them together with the patterns with minimal changes to the
existing code.

@d hyph_root==trie_r[0] {root of the linked trie for |hyph_codes|}

@<Initialize table entries...@>=
hyph_root:=0; hyph_start:=0;

@ @<Store hyphenation codes for current language@>=
begin c:=cur_lang; first_child:=false; p:=0;
repeat q:=p; p:=trie_r[q];
until (p=0)or(c<=so(trie_c[p]));
if (p=0)or(c<so(trie_c[p])) then
  @<Insert a new trie node between |q| and |p|, and
    make |p| point to it@>;
q:=p; {now node |q| represents |cur_lang|}
@<Store all current |lc_code| values@>;
end

@ We store all nonzero |lc_code| values, overwriting any previously
stored values (and possibly wasting a few trie nodes that were used
previously and are not needed now).  We always store at least one
|lc_code| value such that |hyph_index| (defined below) will not be zero.

@<Store all current |lc_code| values@>=
p:=trie_l[q]; first_child:=true;
for c:=0 to 255 do
  if (lc_code(c)>0)or((c=255)and first_child) then
    begin if p=0 then
      @<Insert a new trie node between |q| and |p|, and
        make |p| point to it@>
    else trie_c[p]:=si(c);
    trie_o[p]:=qi(lc_code(c));
    q:=p; p:=trie_r[q]; first_child:=false;
    end;
if first_child then trie_l[q]:=0@+else trie_r[q]:=0

@ We must avoid to ``take'' location~1, in order to distinguish between
|lc_code| values and patterns.

@<Pack all stored |hyph_codes|@>=
begin if trie_root=0 then for p:=0 to 255 do trie_min[p]:=p+2;
first_fit(hyph_root); trie_pack(hyph_root);
hyph_start:=trie_ref[hyph_root];
end

@ The global variable |hyph_index| will point to the hyphenation codes
for the current language.

@d set_hyph_index== {set |hyph_index| for current language}
  if trie_char(hyph_start+cur_lang)<>qi(cur_lang)
    then hyph_index:=0 {no hyphenation codes for |cur_lang|}
  else hyph_index:=trie_link(hyph_start+cur_lang)
@#
@d set_lc_code(#)== {set |hc[0]| to hyphenation or lc code for |#|}
  if hyph_index=0 then hc[0]:=lc_code(#)
  else if trie_char(hyph_index+#)<>qi(#) then hc[0]:=0
  else hc[0]:=qo(trie_op(hyph_index+#))

@<Glob...@>=
@!hyph_start:trie_pointer; {root of the packed trie for |hyph_codes|}
@!hyph_index:trie_pointer; {pointer to hyphenation codes for |cur_lang|}

@ When |saving_vdiscards| is positive then the glue, kern, and penalty
nodes removed by the page builder or by \.{\\vsplit} from the top of a
vertical list are saved in special lists instead of being discarded.

@d tail_page_disc==disc_ptr[copy_code] {last item removed by page builder}
@d page_disc==disc_ptr[last_box_code] {first item removed by page builder}
@d split_disc==disc_ptr[vsplit_code] {first item removed by \.{\\vsplit}}

@<Glob...@>=
@!disc_ptr:array[copy_code..vsplit_code] of pointer; {list pointers}

@ @<Set init...@>=
page_disc:=null; split_disc:=null;

@ The \.{\\pagediscards} and \.{\\splitdiscards} commands share the
command code |un_vbox| with \.{\\unvbox} and \.{\\unvcopy}, they are
distinguished by their |chr_code| values |last_box_code| and
|vsplit_code|.  These |chr_code| values are larger than |box_code| and
|copy_code|.

@<Generate all \eTeX...@>=
primitive("pagediscards",un_vbox,last_box_code);@/
@!@:page_discards_}{\.{\\pagediscards} primitive@>
primitive("splitdiscards",un_vbox,vsplit_code);@/
@!@:split_discards_}{\.{\\splitdiscards} primitive@>

@ @<Cases of |un_vbox| for |print_cmd_chr|@>=
else if chr_code=last_box_code then print_esc("pagediscards")
else if chr_code=vsplit_code then print_esc("splitdiscards")

@ @<Handle saved items and |goto done|@>=
begin link(tail):=disc_ptr[cur_chr]; disc_ptr[cur_chr]:=null;
goto done;
end

@ The \.{\\interlinepenalties}, \.{\\clubpenalties}, \.{\\widowpenalties},
and \.{\\displaywidowpenalties} commands allow to define arrays of
penalty values to be used instead of the corresponding single values.

@d inter_line_penalties_ptr==equiv(inter_line_penalties_loc)
@d club_penalties_ptr==equiv(club_penalties_loc)
@d widow_penalties_ptr==equiv(widow_penalties_loc)
@d display_widow_penalties_ptr==equiv(display_widow_penalties_loc)

@<Generate all \eTeX...@>=
primitive("interlinepenalties",set_shape,inter_line_penalties_loc);@/
@!@:inter_line_penalties_}{\.{\\interlinepenalties} primitive@>
primitive("clubpenalties",set_shape,club_penalties_loc);@/
@!@:club_penalties_}{\.{\\clubpenalties} primitive@>
primitive("widowpenalties",set_shape,widow_penalties_loc);@/
@!@:widow_penalties_}{\.{\\widowpenalties} primitive@>
primitive("displaywidowpenalties",set_shape,display_widow_penalties_loc);@/
@!@:display_widow_penalties_}{\.{\\displaywidowpenalties} primitive@>

@ @<Cases of |set_shape| for |print_cmd_chr|@>=
inter_line_penalties_loc: print_esc("interlinepenalties");
club_penalties_loc: print_esc("clubpenalties");
widow_penalties_loc: print_esc("widowpenalties");
display_widow_penalties_loc: print_esc("displaywidowpenalties");

@ @<Fetch a penalties array element@>=
begin scan_int;
if (equiv(m)=null)or(cur_val<0) then cur_val:=0
else  begin if cur_val>penalty(equiv(m)) then cur_val:=penalty(equiv(m));
  cur_val:=penalty(equiv(m)+cur_val);
  end;
end

@* \[54] System-dependent changes.
@z
%---------------------------------------
