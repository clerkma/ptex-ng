/*
   Copyright 2017, 2018 Clerk Ma
 
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
 
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA.
*/

/*
  Reference:
    * https://www.microsoft.com/en-us/Typography/OpenTypeSpecification.aspx
    * https://developer.apple.com/fonts/TrueType-Reference-Manual/
    * https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Fonts
    * https://developer.mozilla.org/en-US/docs/Web/CSS/font-feature-settings
    * https://developer.mozilla.org/en-US/docs/Web/CSS/font-language-override
 
  Note on OpenType and TrueType (AAT)
    * support of OpenType is done with libotf.
    * support of TrueType/AAT will to be implemented in future.

  Name Syntax (OLD, regular expression based) :
    \jfont\t=name
    name        = "ot:" file_name file_index? script_lang? gsub_spec? ":" jfm_name
    file_index  = "[" number "]"
    script_lang = "(" script? lang? ")"
    script      = script_tag
    lang        = "/" lang_tag
    gsub_spec   = ";" fea_list
    fea_list    = (fea_tag ",")* [fea_tag | "*"]

    script_tag: https://www.microsoft.com/typography/otspec/scripttags.htm
    lang_tag:   https://www.microsoft.com/typography/otspec/languagetags.htm
    fea_tag:    https://www.microsoft.com/typography/otspec/featuretags.htm

    '*' for all gsub featurs
  Examples:
    \jfont\t=ot:yumin.ttf;jp90,hojo:upjisr-h
    \tfont\t=ot:yumin.ttf;vert:upjisr-v
    \jfont\t=ot:simsun.ttc[1]:upjisr-h
    \jfont\t=ot:SourceHanSansTC-Normal.otf:uprml-h
  
  Name Syntax (NEW, inline yaml syntax):
    KEY 'jfm'
    KEY 'jvf'
    KEY 'font-name'
    KEY 'font-index'
    KEY 'font-engine'
    KEY 'baseline-shift'
    KEY 'font-features' 
  Example:
    \jfont\t="ot: {jfm: upjisr-h, font-name: yumin.ttf, font-features: [jp90, aalt: 2], baseline-shift: 20}"
  Check Name with Ruby:
    """
      require "yaml"
      spec = YAML.load("ot: {jfm: upjisr-h, font-name: yumin.ttf, font-features: [jp90, aalt: 2], baseline-shift: 20}")
      p spec
    """
  Check Name with Python:
    """
      import yaml
      spec = yaml.load("ot: {jfm: upjisr-h, font-name: yumin.ttf, font-features: [jp90, aalt: 2], baseline-shift: 20}")
      print(spec)
    """
*/

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "yaml.h"

void aptex_validate_yaml_fontspec (char * spec)
{
  yaml_parser_t spec_parser;
  yaml_document_t spec_document;
  yaml_node_t * spec_root;

  yaml_parser_initialize(&spec_parser);
  yaml_parser_set_input_string(&spec_parser, spec, strlen(spec));

  if (yaml_parser_load(&spec_parser, &spec_document))
  {
    spec_root = yaml_document_get_root_node(&spec_document);
    if (spec_root->type == YAML_MAPPING_NODE)
    {
      yaml_node_pair_t * root_pair;
      for (root_pair = spec_root->data.mapping.pairs.start; root_pair < spec_root->data.mapping.pairs.top; root_pair++)
      {
        yaml_node_t * node_key = yaml_document_get_node(&spec_document, root_pair->key);
        yaml_node_t * node_val = yaml_document_get_node(&spec_document, root_pair->value);
        if (strcmp("ot", node_key->data.scalar.value) == 0 && node_val->type == YAML_MAPPING_NODE)
        {
          yaml_node_pair_t * spec_pair;
          for (spec_pair = node_val->data.mapping.pairs.start; spec_pair < node_val->data.mapping.pairs.top; spec_pair++)
          {
            yaml_node_t * spec_key = yaml_document_get_node(&spec_document, spec_pair->key);
            printf("KEY '%s'\n", spec_key->data.scalar.value);
          }
        }
      }
    }
  }

  yaml_parser_delete(&spec_parser);
}

int main (void)
{
  aptex_validate_yaml_fontspec("ot: {jfm: upjisr-h, font-name: yumin.ttf, font-features: [jp90, aalt: 2], baseline-shift: 20}");

  return 0;
}
