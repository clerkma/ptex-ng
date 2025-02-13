/*

Copyright 2009-2011 Taco Hoekwater <taco@luatex.org>

This file is part of LuaTeX.

LuaTeX is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

LuaTeX is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU General Public License along
with LuaTeX; if not, see <http://www.gnu.org/licenses/>.

*/

#include "ptexlib.h"

/*tex

    Here we implement subroutines for work with objects and related things. Some
    of them are used in former parts too, so we need to declare them forward.
    Memory will grow dynamically.

*/

void init_dest_names(PDF pdf)
{
    pdf->dest_names_size = inf_dest_names_size;
    pdf->dest_names = xmallocarray(dest_name_entry, inf_dest_names_size);
}

void append_dest_name(PDF pdf, char *s, int n)
{
    int a;
    if (pdf->dest_names_ptr == sup_dest_names_size)
        overflow("number of destination names (dest_names_size)",(unsigned) pdf->dest_names_size);
    if (pdf->dest_names_ptr == pdf->dest_names_size) {
        a = pdf->dest_names_size / 5;
        if (pdf->dest_names_size < sup_dest_names_size - a)
            pdf->dest_names_size = pdf->dest_names_size + a;
        else
            pdf->dest_names_size = sup_dest_names_size;
        pdf->dest_names = xreallocarray(pdf->dest_names, dest_name_entry, (unsigned) pdf->dest_names_size);
    }
    pdf->dest_names[pdf->dest_names_ptr].objname = xstrdup(s);
    pdf->dest_names[pdf->dest_names_ptr].objnum = n;
    pdf->dest_names_ptr++;
}

/*tex

    When a destination is created we need to check whether another destination
    with the same identifier already exists and give a warning if needed.

*/

static void warn_dest_dup(int id, small_number byname)
{
    if (byname > 0) {
        char *ss = tokenlist_to_cstring(id, true, NULL);
        formatted_warning("pdf backend", "ignoring duplicate destination with the name '%s'",ss);
    } else {
        formatted_warning("pdf backend", "ignoring duplicate destination with the num '%d'",id);
    }
}

/*tex

    In |do_dest|, structure destinations and regular destinations use separate object
    types for |pdf_get_obj| but share an object type for |addto_page_resources|.
    This allows |write_out_pdf_mark_destinations| to handle both kinds of destinations
    in a uniform way, while still allowing to use the same name for both kinds of
    destinations. This is a useful feature since it's rather common to have both kinds
    of destinations for the same object and therefore they will often have the same name.

*/
void do_dest(PDF pdf, halfword p, halfword parent_box, scaledpos cur)
{
    scaledpos pos = pdf->posstruct->pos;
    scaled_whd alt_rule;
    int k;
    if (global_shipping_mode == SHIPPING_FORM)
        normal_error("pdf backend", "destinations cannot be inside an xform");
    if (doing_leaders)
        return;
    k = pdf_get_obj(pdf,
            pdf_dest_objnum(p) == null ? obj_type_dest : obj_type_struct_dest,
            pdf_dest_id(p), pdf_dest_named_id(p));
    if (obj_dest_ptr(pdf, k) != null) {
        warn_dest_dup(pdf_dest_id(p), (small_number) pdf_dest_named_id(p));
        return;
    }
    obj_dest_ptr(pdf, k) = p;
    addto_page_resources(pdf, obj_type_dest, k);
    alt_rule.wd = width(p);
    alt_rule.ht = height(p);
    alt_rule.dp = depth(p);
    /*tex
        The different branches for matrixused is somewhat strange and should
        always be used
    */
    pdf_ann_margin(p) = pdf_dest_margin;
    switch (pdf_dest_type(p)) {
    case pdf_dest_xyz:
        if (matrixused())
            set_rect_dimens(pdf, p, parent_box, cur, alt_rule);
        else {
            pdf_ann_left(p) = pos.h;
            pdf_ann_top(p) = pos.v;
        }
        break;
    case pdf_dest_fith:
    case pdf_dest_fitbh:
        if (matrixused())
            set_rect_dimens(pdf, p, parent_box, cur, alt_rule);
        else
            pdf_ann_top(p) = pos.v;
        break;
    case pdf_dest_fitv:
    case pdf_dest_fitbv:
        if (matrixused())
            set_rect_dimens(pdf, p, parent_box, cur, alt_rule);
        else
            pdf_ann_left(p) = pos.h;
        break;
    case pdf_dest_fit:
    case pdf_dest_fitb:
        break;
    case pdf_dest_fitr:
        set_rect_dimens(pdf, p, parent_box, cur, alt_rule);
        break;
    }
}

void write_out_pdf_mark_destinations(PDF pdf)
{
    pdf_object_list *k;
    if ((k = get_page_resources_list(pdf, obj_type_dest)) != NULL) {
        while (k != NULL) {
            if (is_obj_written(pdf, k->info)) {
                normal_error("pdf backend","destination has been already written (this shouldn't happen)");
            } else {
                int i;
                int objnum;
                i = obj_dest_ptr(pdf, k->info);
                objnum = pdf_dest_objnum(i);
                pdf_begin_obj(pdf, k->info, OBJSTM_ALWAYS);
                if (pdf_dest_named_id(i) > 0 && objnum == null) {
                    pdf_begin_dict(pdf);
                    pdf_add_name(pdf, "D");
                }
                pdf_begin_array(pdf);
                pdf_add_ref(pdf, objnum == null ? pdf->last_page : objnum);
                switch (pdf_dest_type(i)) {
                    case pdf_dest_xyz:
                        pdf_add_name(pdf, "XYZ");
                        pdf_add_bp(pdf, pdf_ann_left(i));
                        pdf_add_bp(pdf, pdf_ann_top(i));
                        if (pdf_dest_xyz_zoom(i) == null) {
                            pdf_add_null(pdf);
                        } else {
                            pdf_check_space(pdf);
                            pdf_print_int(pdf, pdf_dest_xyz_zoom(i) / 1000);
                            pdf_out(pdf, '.');
                            pdf_print_int(pdf, (pdf_dest_xyz_zoom(i) % 1000));
                            pdf_set_space(pdf);
                        }
                        break;
                    case pdf_dest_fit:
                        pdf_add_name(pdf, "Fit");
                        break;
                    case pdf_dest_fith:
                        pdf_add_name(pdf, "FitH");
                        pdf_add_bp(pdf, pdf_ann_top(i));
                        break;
                    case pdf_dest_fitv:
                        pdf_add_name(pdf, "FitV");
                        pdf_add_bp(pdf, pdf_ann_left(i));
                        break;
                    case pdf_dest_fitb:
                        pdf_add_name(pdf, "FitB");
                        break;
                    case pdf_dest_fitbh:
                        pdf_add_name(pdf, "FitBH");
                        pdf_add_bp(pdf, pdf_ann_top(i));
                        break;
                    case pdf_dest_fitbv:
                        pdf_add_name(pdf, "FitBV");
                        pdf_add_bp(pdf, pdf_ann_left(i));
                        break;
                    case pdf_dest_fitr:
                        pdf_add_name(pdf, "FitR");
                        pdf_add_rect_spec(pdf, i);
                        break;
                    default:
                        normal_error("pdf backend", "unknown dest type");
                        break;
                }
                pdf_end_array(pdf);
                if (pdf_dest_named_id(i) > 0 && objnum == null)
                    pdf_end_dict(pdf);
                pdf_end_obj(pdf);
            }
            k = k->link;
        }
    }
}

void scan_pdfdest(PDF pdf)
{
    halfword q;
    int k;
    int obj_type;
    str_number i;
    scaled_whd alt_rule;
    q = cur_list.tail_field;
    new_whatsit(pdf_dest_node);
    if (scan_keyword("struct")) {
        scan_int();
        if (cur_val <= 0)
            normal_error("pdf backend", "struct identifier must be positive");
        pdf_dest_objnum(cur_list.tail_field) = cur_val;
        obj_type = obj_type_struct_dest;
    } else {
        pdf_dest_objnum(cur_list.tail_field) = null;
        obj_type = obj_type_dest;
    }
    if (scan_keyword("num")) {
        scan_int();
        if (cur_val <= 0)
            normal_error("pdf backend", "num identifier must be positive");
        if (cur_val > max_halfword)
            normal_error("pdf backend", "number too big");
        set_pdf_dest_id(cur_list.tail_field, cur_val);
        set_pdf_dest_named_id(cur_list.tail_field, 0);
    } else if (scan_keyword("name")) {
        scan_toks(false, true);
        set_pdf_dest_id(cur_list.tail_field, def_ref);
        set_pdf_dest_named_id(cur_list.tail_field, 1);
    } else {
        normal_error("pdf backend", "identifier type missing");
    }
    if (scan_keyword("xyz")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_xyz);
        if (scan_keyword("zoom")) {
            scan_int();
            if (cur_val > max_halfword)
                normal_error("pdf backend", "number too big");
            set_pdf_dest_xyz_zoom(cur_list.tail_field, cur_val);
        } else {
            set_pdf_dest_xyz_zoom(cur_list.tail_field, null);
        }
    } else if (scan_keyword("fitbh")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fitbh);
    } else if (scan_keyword("fitbv")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fitbv);
    } else if (scan_keyword("fitb")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fitb);
    } else if (scan_keyword("fith")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fith);
    } else if (scan_keyword("fitv")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fitv);
    } else if (scan_keyword("fitr")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fitr);
    } else if (scan_keyword("fit")) {
        set_pdf_dest_type(cur_list.tail_field, pdf_dest_fit);
    } else {
        normal_error("pdf backend", "destination type missing");
    }
    /*tex Scan an optional space. */
    get_x_token();
    if (cur_cmd != spacer_cmd)
        back_input();
    if (pdf_dest_type(cur_list.tail_field) == pdf_dest_fitr) {
        alt_rule = scan_alt_rule();
        set_width(cur_list.tail_field, alt_rule.wd);
        set_height(cur_list.tail_field, alt_rule.ht);
        set_depth(cur_list.tail_field, alt_rule.dp);
    }
    if (pdf_dest_named_id(cur_list.tail_field) != 0) {
        i = tokens_to_string(pdf_dest_id(cur_list.tail_field));
        k = find_obj(pdf, obj_type, i, true);
        flush_str(i);
    } else {
        k = find_obj(pdf, obj_type, pdf_dest_id(cur_list.tail_field), false);
    }
    if ((k != 0) && (obj_dest_ptr(pdf, k) != null)) {
        warn_dest_dup(pdf_dest_id(cur_list.tail_field),(small_number) pdf_dest_named_id(cur_list.tail_field));
        flush_node_list(cur_list.tail_field);
        cur_list.tail_field = q;
        vlink(q) = null;
    }
}

/*tex 
  Sort |dest_names| by names: we have to consider the lexical meaning of 
  the literal strings, as well as the hexadecimal strings as byte stream.
*/

static int unescapebuf(char *buf,const char *src, size_t *newsize){
  const char *pch = src;
  unsigned char bs = 0;
  int bal = 0;

  *newsize=0;
  while (*pch!='\0'){
    switch (*pch) {
    case '\\':
      if (bs==0){
	bs=1;
      }else if (bs>=1){
	/* seen '\\' i.e. a backslash */
	*buf++ = '\\';
	(*newsize)++;
	bs=0;
      }
      break;
    case '\r': 
      if(bs==1){/* drop '\\','\r' */ pch++;if(*pch!='\n'){ pch--;}; /* drop '\\', '\n' */ bs=0;} else { pch++; if(*pch!='\n'){*buf++='\n';(*newsize)++;}; pch--;/* next cycle will eventually manage \n */}  
      break; 
    case '\n': 
      if(bs==1){/* drop '\\','\n' */ bs=0;} else { *buf++ = '\n';(*newsize)++;} 
      break; 
   case 'n':
     if(bs==1){ *buf++ = '\n'; bs=0;} else { *buf++ = 'n';}
     (*newsize)++;
     break;
    case 'r':
      if(bs==1){ *buf++ = '\r'; bs=0;} else { *buf++ = 'r';}
      (*newsize)++;
      break;
    case 't':
      if(bs==1){ *buf++ = '\t';	bs=0;} else { *buf++ = 't';}
      (*newsize)++;
      break;
    case 'b':
      if(bs==1){ *buf++ = '\b';	bs=0;} else { *buf++ = 'b';}
      (*newsize)++;
      break;
    case 'f':
      if(bs==1){ *buf++ = '\f'; bs=0;} else { *buf++ = 'f';}
      (*newsize)++;
      break;
    case '(': case ')':
      if(bs==0){if( *pch=='(' ) bal++; else bal--;}; /* we keep track of unescaped '(' or ')' because they can be unbalanced. */
      bs=0;  
      *buf++ = *pch;
      (*newsize)++;
      break;
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
      if(bs==1){
	/* seen \o */
	char last_seen[]={'\0','\0','\0'};
	last_seen[0]=*pch-'0';
	pch++;
	if ('0'<=*pch && *pch<='7'){
	  /* seen \oo */
	  last_seen[1]=*pch-'0';
	  pch++;
	  if ('0'<=*pch && *pch<='7'){
	      /* seen \ooo */
	      last_seen[2] = last_seen[0]*64+last_seen[1]*8+(*pch-'0'); 
	      *buf++ = last_seen[2];
	      (*newsize)++;
	    } else {
	      /* seen \ooO */
	      pch--; /* next cycle will manage O */
	      last_seen[2] = last_seen[0]*8+last_seen[1] ;
	      *buf++ = last_seen[2];
	      (*newsize)++;
	    }
	}else {
	  /*seen \oO */
	  pch--; /* next cycle will manage O */
	  *buf++ = last_seen[0];
	  (*newsize)++;
	}
	bs=0;
      }else {
	*buf++ = *pch;
	(*newsize)++;
      }
      break;
    default:
      bs=0; /* skip isolated backslash */
      *buf++ = *pch;
      (*newsize)++;
      break;
    }
    pch++;
  }
  return bal;
}


static int dest_cmp(const void *a, const void *b)
{
    dest_name_entry aa = *(const dest_name_entry *) a;
    dest_name_entry bb = *(const dest_name_entry *) b;

    size_t la=strlen(aa.objname), lb=strlen(bb.objname);

    boolean aa_is_hexstring=false, bb_is_hexstring=false;
    boolean aa_is_empty=false, bb_is_empty=false;
    boolean free_cmp_aa=false,free_cmp_bb=false;
    
    char *cmp_aa = NULL;
    char *cmp_bb = NULL;
    size_t cmp_aa_size,cmp_bb_size;
    int res;

    aa_is_empty = (la==0) || ( la==2? (aa.objname[0] == '<') && (aa.objname[1] == '>'): false);
    bb_is_empty = (lb==0) || ( lb==2? (bb.objname[0] == '<') && (bb.objname[1] == '>'): false);
    
    
    if (aa_is_empty && bb_is_empty) {
      formatted_warning("pdf backend", "both entries are empty");
      return 0;
    }

    if (aa_is_empty && !(bb_is_empty))
      return -1;

    if (!(aa_is_empty) && bb_is_empty)
      return 1;
    
    
    if ((aa.objname[0] == '<') && (aa.objname[la-1] == '>') && !(odd(la))) {
      /* perhaps a hexadecimal string */
      cmp_aa_size = (la/2)-1 ;
      cmp_aa = malloc(cmp_aa_size);
      if (cmp_aa) {
	aa_is_hexstring = true;
	free_cmp_aa = true;
	size_t j;
	for (j=1;j<(la-1);j+=2){
	  unsigned char c1=aa.objname[j];
	  unsigned char c0=aa.objname[j+1];
	  if( isxdigit(c1) && isxdigit(c0) ){
	    if (isdigit(c1)) c1=c1-'0'; else c1=(tolower(c1)-'a')+10;
	    if (isdigit(c0)) c0=c0-'0'; else c0=(tolower(c0)-'a')+10;
	    cmp_aa[(j-1)/2]=c1*16+c0;
	  }else {
	    aa_is_hexstring = false;
	    free_cmp_aa = false;
	    cmp_aa_size = 0;
	    xfree(cmp_aa);
	    break;
	  }
	}
      } else {
	/* something went wrong with malloc */
	free_cmp_aa = false;
	cmp_aa_size = 0;
	cmp_aa = NULL;
      }
    }

    if ( !(aa_is_hexstring) && (strchr(aa.objname,'\\')!=NULL || strchr(aa.objname,'\n')!=NULL || strchr(aa.objname,'\r')!=NULL)){
      size_t l = la; /* strlen(aa.objname) */
      int res1 = 0;
      cmp_aa=malloc(l);
      if (cmp_aa) {
	free_cmp_aa = true;
	memset(cmp_aa,'\0',l);
	res1 = unescapebuf(cmp_aa,aa.objname,&cmp_aa_size);
	if (res1)
	  formatted_warning("pdf backend", "unbalanced () in (%s)",aa.objname);
	if (0<cmp_aa_size && cmp_aa_size<l){
	  cmp_aa = realloc(cmp_aa, cmp_aa_size);
	}
      } else {
	/* something went wrong with malloc */
	free_cmp_aa = false;
	cmp_aa_size = 0;
	cmp_aa = NULL;
      }
    }


    if ((bb.objname[0] == '<') && (bb.objname[lb-1] == '>') && !(odd(lb))) {
      /* perhaps a hexadecimal string */
      cmp_bb_size =(lb/2)-1 ;
      cmp_bb = malloc(cmp_bb_size);
      if (cmp_bb) {
	size_t j;
	for (j=1;j<(lb-1);j+=2){
	  unsigned char c1=bb.objname[j];
	  unsigned char c0=bb.objname[j+1];
	  if( isxdigit(c1) && isxdigit(c0) ){
	    if (isdigit(c1)) c1=c1-'0'; else c1=(tolower(c1)-'a')+10;
	    if (isdigit(c0)) c0=c0-'0'; else c0=(tolower(c0)-'a')+10;
	    cmp_bb[(j-1)/2]=c1*16+c0;
	  }else {
	    bb_is_hexstring = false;
	    free_cmp_bb = false;
	    cmp_bb_size = 0;
	    xfree(cmp_bb);
	    break;
	  }
	}
      } else {
	/* something went wrong with malloc */
	free_cmp_bb = false;
	cmp_bb_size = 0;
	cmp_bb = NULL;
      }
    }

    if ( !(bb_is_hexstring) && (strchr(bb.objname,'\\')!=NULL || strchr(bb.objname,'\n')!=NULL || strchr(bb.objname,'\r')!=NULL)){
      size_t l = lb; /* strlen(bb.objname) */
      int res1 = 0;
      cmp_bb=malloc(l);
      if (cmp_bb) {
	free_cmp_bb = true;
	memset(cmp_bb,'\0',l);
	res1 = unescapebuf(cmp_bb,bb.objname,&cmp_bb_size);
	if (res1)
	  formatted_warning("pdf backend", "unbalanced () in (%s)",bb.objname);
	if (0<cmp_bb_size && cmp_bb_size<l){
	  cmp_bb = realloc(cmp_bb, cmp_bb_size);
	}
      } else {
	/* something went wrong with malloc */
	free_cmp_bb = false;
	cmp_bb_size = 0;
	cmp_bb = NULL;
      }
    }
        
    if (cmp_aa==NULL && cmp_bb==NULL){
      res = strcmp(aa.objname, bb.objname);
      if (res==0)
	formatted_warning("pdf backend", "duplicate entry %s",aa.objname);

    }
    if (cmp_aa == NULL) {
      cmp_aa_size= la;
      cmp_aa = aa.objname;
    }
    if (cmp_bb == NULL) {
      cmp_bb_size= lb;
      cmp_bb = bb.objname;
    }

    res = memcmp(cmp_aa,cmp_bb, cmp_aa_size<cmp_bb_size? cmp_aa_size : cmp_bb_size);
    if (res==0) {
      if (cmp_aa_size==cmp_bb_size)
	formatted_warning("pdf backend", "entries %s and %s  are equivalent to %s",aa.objname,bb.objname,cmp_aa);
      else {
	if (cmp_aa_size<cmp_bb_size)
	  res=-1;
	else
	  res=1;
      }
    }
    if (free_cmp_aa)
      free(cmp_aa);
    if (free_cmp_bb)
      free(cmp_bb);
    return res;
}

void sort_dest_names(PDF pdf)
{
  qsort(pdf->dest_names, (size_t) pdf->dest_names_ptr, sizeof(dest_name_entry), dest_cmp);
}

/*tex

    Output the name tree. The tree nature of the destination list forces the
    storing of intermediate data in |obj_info| and |obj_aux| fields, which is
    further uglified by the fact that |obj_tab| entries do not accept char
    pointers.

*/

int output_name_tree(PDF pdf)
{
    /*tex A flag for name tree output: is it |/Names| or |/Kids|: */
    boolean is_names = true;
    /*tex
        The index of current child of |l|; if |k < pdf_dest_names_ptr| then this
        is pointer to |dest_names| array; otherwise it is the pointer to
        |obj_tab| (object number).
    */
    int k = 0;
    int b = 0;
    int m, j, l;
    int dests = 0;
    int names_head = 0;
    int names_tail = 0;
    if (pdf->dest_names_ptr == 0) {
        goto DONE;
    }
    sort_dest_names(pdf);
    while (true) {
        do {
            /*tex Create a new node: */
            l = pdf_create_obj(pdf, obj_type_others, 0);
            if (b == 0) {
                /*tex First in this level: */
                b = l;
            }
            if (names_head == 0) {
                names_head = l;
                names_tail = l;
            } else {
                set_obj_link(pdf, names_tail, l);
                names_tail = l;
            }
            set_obj_link(pdf, names_tail, 0);
            /*tex Output the current node in this level. */
            pdf_begin_obj(pdf, l, OBJSTM_ALWAYS);
            pdf_begin_dict(pdf);
            j = 0;
            if (is_names) {
                set_obj_start(pdf, l, pdf->dest_names[k].objname);
                pdf_add_name(pdf, "Names");
                pdf_begin_array(pdf);
                do {
                    pdf_add_string(pdf, pdf->dest_names[k].objname);
                    pdf_add_ref(pdf, pdf->dest_names[k].objnum);
                    j++;
                    k++;
                } while (j != name_tree_kids_max && k != pdf->dest_names_ptr);
                pdf_end_array(pdf);
                /*tex For later use: */
                set_obj_stop(pdf, l, pdf->dest_names[k - 1].objname);
                if (k == pdf->dest_names_ptr) {
                    is_names = false;
                    k = names_head;
                    b = 0;
                }
            } else {
                set_obj_start(pdf, l, obj_start(pdf, k));
                pdf_add_name(pdf, "Kids");
                pdf_begin_array(pdf);
                do {
                    pdf_add_ref(pdf, k);
                    set_obj_stop(pdf, l, obj_stop(pdf, k));
                    k = obj_link(pdf, k);
                    j++;
                } while (j != name_tree_kids_max && k != b
                         && obj_link(pdf, k) != 0);
                pdf_end_array(pdf);
                if (k == b)
                    b = 0;
            }
            pdf_add_name(pdf, "Limits");
            pdf_begin_array(pdf);
            pdf_add_string(pdf, obj_start(pdf, l));
            pdf_add_string(pdf, obj_stop(pdf, l));
            pdf_end_array(pdf);
            pdf_end_dict(pdf);
            pdf_end_obj(pdf);
        } while (b != 0);
        if (k == l) {
            dests = l;
            goto DONE;
        }
    }
  DONE:
    if ((dests != 0) || (pdf_names_toks != null)) {
        m = pdf_create_obj(pdf, obj_type_others, 0);
        pdf_begin_obj(pdf, m, OBJSTM_ALWAYS);
        pdf_begin_dict(pdf);
        if (dests != 0)
            pdf_dict_add_ref(pdf, "Dests", dests);
        if (pdf_names_toks != null) {
            pdf_print_toks(pdf, pdf_names_toks);
            delete_token_ref(pdf_names_toks);
            pdf_names_toks = null;
        }
        print_pdf_table_string(pdf, "names");
        pdf_end_dict(pdf);
        pdf_end_obj(pdf);
        return m;
    } else {
        return 0;
    }
}
