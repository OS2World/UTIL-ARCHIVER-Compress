Here is a version of compress 4.0 hacked for MSDOS. A makefile is provided
which will compile it using Microsoft C, Turbo C, or Zortech C. The makefile
will need editing if other than the Microsoft compiler is used.
The program requires about 400K to run. It takes the same command line
args as does the UNIX program of the same name, and should be compatible
in all ways with that program. It will decode a 16 bit compressed file,
and can generate the same. On my machine, it decodes about twice as quickly
as the "u16" decompress program posted earlier to c.s.m.

Doug Graham.
uunet!mitel!sce!tsmith!graham


Ported to OS/2. Only the memory allocation and some usage().. stuff
was changed, -k (keep) option added.
Under OS/2 and if a family mode application is to build, the huge segment
allocation functions of the DOS layer have to be used carefully instead
of the MS C library routines for big segments.
The system calls DosAllocHuge() and DosGetHugeShift() are used.

Compile with:
  CL -AS -G2s -Oltn -Zep -W3 -Lp -Fb -F 2000 -DOS2 -DMSC -DPROTO -Di8088
     compress.c compress.def setargv.obj -link /noe
or:
  SET CL=-AS -G2s -Oltn -Zep -W3 -Lp -Fb -F 2000
  SET LINK=/NOIG /NOE
  CL  -DOS2 -DMSC -DPROTO -Di8088 compress.c compress.def setargv.obj

You have to use Microsoft C 5.1 or above.


Kai Uwe Rommel
rommel@lan.informatik.tu-muenchen.dbp.de


Compress now supports the HPFS file names. On a HPFS, it now appends .Z
like under Unix instead of replacing the extension with .Z like it
does on a FAT file system.

Kai Uwe Rommel
