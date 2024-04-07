/************************************************************************

  This file is a part of the wrapper program for launching scripts
  and programs in TeX Live on Windows. See readme.txt for more details.
  
  This file was originally written in 2009 by Tomasz M. Trzeciak and
  was placed in the Public Domain.
 
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#define IS_WHITESPACE(c) ((c == ' ') || (c == '\t'))
#define MAX_MSG 2*MAX_PATH
#define DIE(...) { _snprintf( msg_buf, MAX_MSG - 1, __VA_ARGS__ ); goto DIE; }

char module_name[] = "runscript.dll";
char script_name[] = "runscript.tlu";
char texlua_name[] = "texlua"; // just a bare name, luatex strips the rest anyway
char subsys_mode[] = "CUI_MODE\n";
char err_env_var[] = "RUNSCRIPT_ERROR_MESSAGE";
char msg_buf[MAX_MSG];

__declspec(dllimport) int dllluatexmain( int argc, char *argv[] );

__declspec(dllexport) int dllrunscript( int argc, char *argv[] ) 
{
  static char own_path[MAX_PATH];
  static char fpath[MAX_PATH];
  char *fname, *argline, **lua_argv;
  int k, quoted, lua_argc;
  HMODULE module_handle = NULL;

/* change argv[0] into long file name */

  char buff[260];
  char infile[260];
  char *fp;

  if (argc) {
    char *pp;
    char ppp[260];
    strcpy(ppp, argv[0]);
    pp = strrchr(ppp, '.');
    if (!pp || stricmp(pp, ".exe")) {
      strcat(ppp, ".exe");
    }
    argv[0] = ppp;
    k = SearchPath(NULL, argv[0], NULL, 260, infile, &fp);
    if (!k)
      DIE("cannot find %s\n", argv[0]);
    k = GetLongPathNameA(infile, buff, 260);
    if (!k)
      DIE("cannot find %s\n", argv[0]);
    argv[0] = buff;
  }

  // file path of the executable
  k = (int) GetModuleFileName(NULL, own_path, MAX_PATH);
  if ( !k || (k == MAX_PATH) ) 
    DIE("cannot get own path (may be too long): %s\n", own_path);

  // script path (the dir of this library)
  module_handle = GetModuleHandle(module_name); 
  // if ( module_handle == NULL ) exe path will be used, which is OK too
  k = (int) GetModuleFileName(module_handle, fpath, MAX_PATH);
  if ( !k || (k == MAX_PATH) ) 
    DIE("cannot get module path (may be too long): %s\n", fpath);
  fname = strrchr(fpath, '\\');
  if ( fname == NULL ) DIE("no directory part in module path: %s\n", fpath);
  fname++;
  if ( fname + strlen(script_name) >=  fpath + MAX_PATH - 1 ) 
    DIE("path too long: %s\n", fpath);
  strcpy(fname, script_name);
  if ( GetFileAttributes(fpath) == INVALID_FILE_ATTRIBUTES ) 
    DIE("main lua script not found: %s\n", fpath);

  // get command line of this process
  argline = GetCommandLine();
  if ( argline == NULL ) DIE("failed to retrieve command line string\n");
  // skip over argv[0] (it can contain embedded double quotes if launched from cmd.exe!)
  for ( quoted = 0; (*argline) && ( !IS_WHITESPACE(*argline) || quoted ); argline++ )
    if ( *argline == '"' ) quoted = !quoted;
  while ( IS_WHITESPACE(*argline) ) argline++; // remove leading whitespace if any

  // set up argument list for texlua script
  lua_argv = (char **)malloc( (argc + 6) * sizeof(char *) );
  lua_argv[lua_argc=0] = texlua_name;
  lua_argv[++lua_argc] = fpath; // script to execute
  for ( k = 1; k < argc; k++ ) lua_argv[++lua_argc] = argv[k]; // copy argument list
  lua_argv[++lua_argc] = subsys_mode; // sentinel argument
  lua_argv[++lua_argc] = argc ? argv[0] : own_path; // original argv[0]
  lua_argv[++lua_argc] = argline; // unparsed arguments
  lua_argv[++lua_argc] = NULL;

  // call texlua interpreter
  // dllluatexmain  never returns, but we pretend that it does
  k = dllluatexmain( lua_argc, lua_argv );
  if (lua_argv) free(lua_argv);
  return k;

DIE:
  if (*subsys_mode != 'G') {
    fprintf(stderr, "%s: ", module_name);
    fprintf(stderr, msg_buf);
  }
  if (*subsys_mode == 'G')
    MessageBox( NULL, msg_buf, module_name, MB_ICONERROR | MB_SETFOREGROUND );
  return 1;
}

void finalize( void )
{
  // check for and display error message if any
  char *err_msg;
  if (*subsys_mode == 'G') {
    if ( err_msg = (char *) getenv(err_env_var) )
      MessageBox( NULL, err_msg, script_name, MB_ICONERROR | MB_SETFOREGROUND );
  }
  if (*subsys_mode != 'G') {
    if ( err_msg = (char *) getenv(err_env_var) ) {
      fprintf(stderr, "%s: ", script_name);
      fprintf(stderr, "%s\n", err_msg);
    }
  }
}

__declspec(dllexport) int dllwrunscript( 
  HINSTANCE hInstance,
  HINSTANCE hPrevInstance,
  char *argline,
  int winshow 
) {
  // set sentinel argument (G for GUI_MODE)
  *subsys_mode = 'G';
  // clear error var in case it exists already
  SetEnvironmentVariable(err_env_var, NULL);
  // register atexit handler to recover control before terminating
  atexit( finalize );
  // call the console entry point routine
#ifdef __MSVCRT__
  // WinMain doesn't provide argc & argv, call MSVCRT proc to get them
	int argc = 0; 
  char **argv, **env; 
  int expand_wildcards = 0;
  int new_mode;
  __getmainargs(&argc, &argv, &env, expand_wildcards, &new_mode);
  return dllrunscript( argc, argv );
#else
  return dllrunscript( 0, NULL );
#endif
}
