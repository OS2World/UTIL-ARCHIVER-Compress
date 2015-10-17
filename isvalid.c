/* ISVALID.C
 *
 * Autor:    Kai Uwe Rommel
 * Datum:    Thu 15-Nov-1990
 *
 * Compiler: MS C ab 6.00
 * System:   OS/2 ab 1.2
 */

#define LABEL    "isvalid.c"
#define VERSION  "1.0"


#define INCL_NOPM
#define INCL_DOSERRORS
#include <os2.h>
#include <stdlib.h>


int IsFileNameValid(char *name)
{
  HFILE hf;
  USHORT usAction;

  switch( DosOpen(name, &hf, &usAction, 0L, 0, FILE_OPEN,
                  OPEN_ACCESS_READONLY | OPEN_SHARE_DENYNONE, 0L) )
  {
  case ERROR_INVALID_NAME:
  case ERROR_FILENAME_EXCED_RANGE:
    return FALSE;
  case NO_ERROR:
    DosClose(hf);
  default:
    return TRUE;
  }
}


int IsFileSystemDumb(char *dir)
{                         
  char drive[5], path[256], name[256];
  
  _splitpath(dir, drive, path, NULL, NULL);
  _makepath(name, drive, path, ".DUMB.TEST.NAME", NULL);
  
  return !IsFileNameValid(name);
}


void UnixFileName(char *name)
{
  for ( ; *name; name++ )
    if ( *name == '\\' )
      *name = '/';
}



/* Ende ISVALID.C */
