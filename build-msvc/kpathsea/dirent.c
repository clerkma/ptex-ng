/*
Public domain. No warranty. Akira Kakuto
*/

/*
DIR * opendir(char *name);
struct dirent * readdir(DIR * dirp);
int closedir(DIR * dirp);

 to be used on VC++ 2.0
 ( by A. Kakuto, 1995 )
 rewritten on Jan. 18, 1997 ( A.K. )

 dirent.h should be included
*/

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "kpathsea/dirent.h"

static int find(DIR *dp, char *name)
{
  HANDLE h;
  WIN32_FIND_DATA *pfd;

  pfd = (WIN32_FIND_DATA *)(dp->_d_buf);
  if((h = FindFirstFile(name,pfd)) == INVALID_HANDLE_VALUE)
    return -1;
  dp->_d_hdir = h;
  pfd->dwReserved1 = strlen(pfd->cFileName);
  dp->_d_magic = pfd->dwFileAttributes;
  pfd->dwReserved0 = (dp->_d_magic & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
  dp->_d_direntptr = (struct dirent *)&(pfd->dwReserved0);
  dp->_d_nfiles = 0;
  return 0;
}

static int findn(DIR *dp)
{
  BOOL res;
  HANDLE h;
  WIN32_FIND_DATA *pfd;

  h = dp->_d_hdir;
  pfd = (WIN32_FIND_DATA *)(dp->_d_buf);
  if((res = FindNextFile(h,pfd)) == FALSE)
    return -1;
  pfd->dwReserved1 = strlen(pfd->cFileName);
  dp->_d_magic = pfd->dwFileAttributes;
  pfd->dwReserved0 = (dp->_d_magic & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
  dp->_d_direntptr = (struct dirent *)&(pfd->dwReserved0);

  return 0;
}

DIR *opendir(char *fname)
{
  DIR *dirptr;
  char nbuf[512];
  char *name, *lastpos;

  if(!(dirptr = (DIR *)malloc(sizeof(DIR))))
    return NULL;
  strcpy(nbuf,fname);
  name = nbuf;

  if(*name != '\0' && *(name+1) == ':')
    name += 2;

  if(*name == '\0') {
    *name='/'; *(name+1)='\0'; goto add_ast;
    }
  if(*(name+1) == '\0' && (*name == '/' || *name == '\\')) {
    *name='/'; goto add_ast;
    }

  lastpos = nbuf;
  while(*lastpos)
    lastpos++;
  lastpos--;
  if(*lastpos == '/' || *lastpos == '\\') {
    *lastpos = '\0'; lastpos--;
    }

  if(*name == '.' && name[1] == '\0') {
    name[1]='/'; name[2]='\0'; goto add_ast;
    }
  if(*name == '.' && name[1] == '.' && name[2] == '\0') {
    name[2]='/'; name[3]='\0'; goto add_ast;
    }
  if(*lastpos == '.' && (*(lastpos-1) == '/' || *(lastpos-1) == '\\')) {
    *(lastpos+1)='/'; *(lastpos+2)='\0'; goto add_ast;
    }
  if(*lastpos == '.' && *(lastpos-1) == '.' &&
    (*(lastpos-2) == '/' || *(lastpos-2) == '\\')) {
    *(lastpos+1)='/'; *(lastpos+2)='\0'; goto add_ast;
    }

  if(find(dirptr, nbuf) != 0) {
    free(dirptr);
    return NULL;
  }
  if(strpbrk(name,"*?") == NULL) {
    if(dirptr->_d_magic & FILE_ATTRIBUTE_DIRECTORY) {
      FindClose(dirptr->_d_hdir);
      strcat(name, "/");
    add_ast:
      strcat(name,"*");
      if(find(dirptr,nbuf) != 0) {
        free(dirptr);
        return NULL;
      }
    }
  }
  dirptr->_d_nfiles = 0;
  return dirptr;
}

int closedir(DIR *dirp)
{
  HANDLE h;
  BOOL res;

  h = dirp->_d_hdir;
  res = FindClose(h);
  if( res == FALSE) {
    free(dirp);
    return -1;
  }
  free(dirp);
  return 0;
}

struct dirent * readdir(DIR *dirp)
{
  if(dirp == NULL)
    return NULL;
  if(++(dirp->_d_nfiles) == 1) {
    return (dirp->_d_direntptr);
  }
  if(findn(dirp) != 0)
    return NULL;
  return (dirp->_d_direntptr);
}
