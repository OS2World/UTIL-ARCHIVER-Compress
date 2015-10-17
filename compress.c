/*
 * Compress - data compression program
 */
static char rcs_ident[] =
 "@(#) compress, v 4.2 (DOS, OS/2) 91/02/12 23:49:00 doug/kur Release $";

/*
 * compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Authors: Spencer W. Thomas    (decvax!harpo!utah-cs!utah-gr!thomas)
 *          Jim McKie            (decvax!mcvax!jim)
 *          Steve Davies         (decvax!vax135!petsd!peora!srd)
 *          Ken Turkowski        (decvax!decwrl!turtlevax!ken)
 *          James A. Woods       (decvax!ihnp4!ames!jaw)
 *          Joe Orost            (decvax!vax135!petsd!joe)
 *          Doug Graham          (uunet!mitel!sce!tsmith!graham)
 *          Kai Uwe Rommel       (rommel@lan.informatik.tu-muenchen.dbp.de)
 *
 * Revision 4.2 (DOS, OS/2) 89/11/10 02:43:00 kur
 * Ported to OS/2. Only the memory allocation and some usage().. stuff
 * was changed, -k (keep) option added.
 * Under OS/2 and if a family mode application is to build, the huge segment
 * allocation functions of the DOS layer have to be used carefully instead
 * of the MS C library routines for big segments.
 * The system calls DosAllocHuge() and DosGetHugeShift() are used.
 *
 * Revision 4.1 (DOS) 89/11/10 02:43:00 doug
 * Ported to MSDOS. Still works elsewhere, but maybe not as quickly.
 * Removed as much long arithmetic as possible for speed on 16 bit machines.
 * Use unsigned short's instead. Changed secondary hashing function to limit
 * hash table size to 64K. This means table indexes can be 16 bit shorts.
 * This compress will not generate codes from MAXMAXCODE (0xf000) thru
 * 0xffff. Doesn't appear to hurt compression much. Removed speed hacks for
 * other machines so I could understand the code. Added some for the i8088.
 * Send CLEAR immediately when hash table fills instead of waiting for the
 * compression ratio to drop. This is faster, and in some cases improves
 * compression (but more often reduces it slightly). Junked the variable
 * size hash table stuff because I am depending on 16 bit unsigned integer
 * wrap around for indexing into hash table, so the table must have 2^16
 * entries. Took out the XENIX_16 stuff. The DOS way ought to work on Xenix
 * as well, and should be faster, but I don't have access to Xenix in order
 * to find out. Added some extra error checking on decompression to try to
 * avoid blowing the machine out of the water when decompressing a corrupt
 * file. Add "okunlink" to avoid the problem of losing the output file as
 * well as the input file if ^C is hit at the wrong time. Lot's of other
 * cosmetic changes.
 *
 * Revision 4.0  85/07/30  12:50:00  joe
 * Removed ferror() calls in output routine on every output except first.
 * Prepared for release to the world.
 *
 * Revision 3.6  85/07/04  01:22:21  joe
 * Remove much wasted storage by overlaying hash table with the tables
 * used by decompress: tab_suffix[1<<BITS], stack[8000].  Updated USERMEM
 * computations.  Fixed dump_tab() DEBUG routine.
 *
 * Revision 3.5  85/06/30  20:47:21  jaw
 * Change hash function to use exclusive-or.  Rip out hash cache.  These
 * speedups render the megamemory version defunct, for now.  Make decoder
 * stack global.  Parts of the RCS trunks 2.7, 2.6, and 2.1 no longer apply.
 *
 * Revision 3.4  85/06/27  12:00:00  ken
 * Get rid of all floating-point calculations by doing all compression ratio
 * calculations in fixed point.
 *
 * Revision 3.3  85/06/24  21:53:24  joe
 * Incorporate portability suggestion for M_XENIX.  Got rid of text on #else
 * and #endif lines.  Cleaned up #ifdefs for vax and interdata.
 *
 * Revision 3.2  85/06/06  21:53:24  jaw
 * Incorporate portability suggestions for Z8000, IBM PC/XT from mailing list.
 * Default to "quiet" output (no compression statistics).
 *
 * Revision 3.1  85/05/12  18:56:13  jaw
 * Integrate decompress() stack speedups (from early pointer mods by McKie).
 * Repair multi-file USERMEM gaffe.  Unify 'force' flags to mimic semantics
 * of SVR2 'pack'.  Streamline block-compress table clear logic.  Increase
 * output byte count by magic number size.
 *
 * Revision 3.0   84/11/27  11:50:00  petsd!joe
 * Set HSIZE depending on BITS.  Set BITS depending on USERMEM.  Unrolled
 * loops in clear routines.  Added "-C" flag for 2.0 compatibility.  Used
 * unsigned compares on Perkin-Elmer.  Fixed foreground check.
 *
 * Revision 2.7   84/11/16  19:35:39  ames!jaw
 * Cache common hash codes based on input statistics; this improves
 * performance for low-density raster images.  Pass on #ifdef bundle
 * from Turkowski.
 *
 * Revision 2.6   84/11/05  19:18:21  ames!jaw
 * Vary size of hash tables to reduce time for small files.
 * Tune PDP-11 hash function.
 *
 * Revision 2.5   84/10/30  20:15:14  ames!jaw
 * Junk chaining; replace with the simpler (and, on the VAX, faster)
 * double hashing, discussed within.  Make block compression standard.
 *
 * Revision 2.4   84/10/16  11:11:11  ames!jaw
 * Introduce adaptive reset for block compression, to boost the rate
 * another several percent.  (See mailing list notes.)
 *
 * Revision 2.3   84/09/22  22:00:00  petsd!joe
 * Implemented "-B" block compress.  Implemented REVERSE sorting of tab_next.
 * Bug fix for last bits.  Changed fwrite to putchar loop everywhere.
 *
 * Revision 2.2   84/09/18  14:12:21  ames!jaw
 * Fold in news changes, small machine typedef from thomas,
 * #ifdef interdata from joe.
 *
 * Revision 2.1   84/09/10  12:34:56  ames!jaw
 * Configured fast table lookup for 32-bit machines.
 * This cuts user time in half for b <= FBITS, and is useful for news batching
 * from VAX to PDP sites.  Also sped up decompress() [fwrite->putc] and
 * added signal catcher [plus beef in writeerr()] to delete effluvia.
 *
 * Revision 2.0   84/08/28  22:00:00  petsd!joe
 * Add check for foreground before prompting user.  Insert maxbits into
 * compressed file.  Force file being uncompressed to end with ".Z".
 * Added "-c" flag and "zcat".  Prepared for release.
 *
 * Revision 1.10  84/08/24  18:28:00  turtlevax!ken
 * Will only compress regular files (no directories), added a magic number
 * header (plus an undocumented -n flag to handle old files without headers),
 * added -f flag to force overwriting of possibly existing destination file,
 * otherwise the user is prompted for a response.  Will tack on a .Z to a
 * filename if it doesn't have one when decompressing.  Will only replace
 * file if it was compressed.
 *
 * Revision 1.9  84/08/16  17:28:00  turtlevax!ken
 * Removed scanargs(), getopt(), added .Z extension and unlimited number of
 * filenames to compress.  Flags may be clustered (-Ddvb12) or separated
 * (-D -d -v -b 12), or combination thereof.  Modes and other status is
 * copied with copystat().  -O bug for 4.2 seems to have disappeared with
 * 1.8.
 *
 * Revision 1.8  84/08/09  23:15:00  joe
 * Made it compatible with vax version, installed jim's fixes/enhancements
 *
 * Revision 1.6  84/08/01  22:08:00  joe
 * Sped up algorithm significantly by sorting the compress chain.
 *
 * Revision 1.5  84/07/13  13:11:00  srd
 * Added C version of vax asm routines.  Changed structure to arrays to
 * save much memory.  Do unsigned compares where possible (faster on
 * Perkin-Elmer)
 *
 * Revision 1.4  84/07/05  03:11:11  thomas
 * Clean up the code a little and lint it.  (Lint complains about all
 * the regs used in the asm, but I'm not going to "fix" this.)
 *
 * Revision 1.3  84/07/05  02:06:54  thomas
 * Minor fixes.
 *
 * Revision 1.2  84/07/05  00:27:27  thomas
 * Add variable bit length output.
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef __ZTC__
#include <malloc.h>
#endif
#ifndef BSD4_2
#include <stdlib.h>
#include <io.h>
#endif
#include <string.h>
#include <fcntl.h>
#ifdef MSDOS
#ifdef OS2
#define INCL_NOPM
#define INCL_DOSMEMMGR
#include <os2.h>
int IsFileNameValid(char *name);
#else
#include <dos.h>
#endif
#endif

#ifdef PROTO
/*
 * Zortech appears to be missing this prototype, and MSC uses some
 * silly structure as the second arg. Turbo C doesn't support this
 * call at all.
 */
extern int utime(char *path, time_t times[]);
#endif

#define BITS		16		/* max number of bits/code */
#define INIT_BITS	9		/* initial number of bits/code */

#define MAXCODE(n_bits)		((code_t)((1L << (n_bits)) - 1))

/*
 * Magic numbers which should appear at the beginning of a compressed file.
 */
#define MAGIC0	0x1f
#define MAGIC1	0x9d

/*
 * Defines for third byte of header
 */
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80

#if 0
#define CHECK_GAP	10000		/* ratio check interval */
#endif

/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */
#define FIRST	257		/* first free entry */
#define	CLEAR	256		/* table clear output code */

#define DE_STACKLEN	8192	/* Size of decoder stack */

#define HSIZE	(1L << 16)	/* Size of the hash table. Don't change this */

typedef unsigned char	uchar;
typedef unsigned long	ulong;
typedef unsigned short	code_t;
typedef	unsigned short	hash_t;

#ifdef PROTO
#define ARGS(x)	x
#else
#define ARGS(x)	()
#endif

void		main ARGS((int argc, char **argv));
void		Usage ARGS((void));
void		version ARGS((void));
void		compress ARGS((void));
void		decompress ARGS((void));
void		copystat ARGS((void));
void		writeerr ARGS((void));
void		cl_hash ARGS((void));
void		putcode ARGS((code_t code));
void		prratio ARGS((long num, long den));
int		ofopen ARGS((char *filename));
int		ifopen ARGS((char *filename));
int		check_magic ARGS((void));
int		need_clear ARGS((void));
void            onintr ARGS((void));
void            oops ARGS((void));
int		taballoc ARGS((void));
void		clearhash ARGS((void));

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
int		block_compress = BLOCK_MASK;

int		maxbits = BITS;		/* user settable max # bits/code */
int		magic = 1;		/* 3-byte magic number header */
int		zcat_flg = 0;		/* Output on stdout */
int		verbose = 0;		/* don't tell me about compression */
int		force = 0;		/* Force overwrite of output file */
int		do_decomp = 0;		/* Decompress rather than compress. */
char		ofname[100];		/* Output file name */
int		foreground;		/* Running in foreground? */
int		exit_stat = 0;		/* Exit status */
uchar		bitbuf[BITS+2];		/* For (dis)assembling code bytes */
int		okunlink;		/* OK for sig handler to unlink output file */
int             keep = 0;               /* keep input files ? */
char		*ifname;

#ifdef i8088

uchar		*de_stack;
uchar far	*charptr1;
uchar far	*codeptrs1[2];
uchar far	*codeptrs2[2];

#define de_suffixof(i)	charptr1[i]
#define de_prefixof(i)	(*(code_t far *)&codeptrs1[i&1][i&~1])

#define en_hashchar(i)	charptr1[i]
#define en_hashent(i)	(*(code_t far *)&codeptrs1[i&1][i&~1])
#define en_hashcode(i)	(*(code_t far *)&codeptrs2[i&1][i&~1])

#ifndef MK_FP
#define MK_FP(seg, ofs) \
	((void far *)(((ulong)(seg) << 16) | (unsigned)(ofs)))
#endif

#define	PARA	16		/* Size of a paragraph */

/*
 * Return a segment address which is the segment part of the normalized
 * version of "fp" rounded upwards.
 * I use this on the far pointers returned by "farmalloc". While
 * they are probably already normalized, I have never seen this
 * stated anywhere in the doc's.
 *
 * There is a lot of junk below which would be unecessary if only
 * there were a reasonably compiler independent way of allocating
 * a given number of PARAGRAPHS (like TC's allocmem). I can't find
 * one though.
 */

#define FP_SEGCEIL(fp)   (FP_SEG(fp) + (FP_OFF(fp) + PARA - 1) / PARA)

/*
 * Allocate space for the tables used in {en,de}coding. These tables
 * reside in the far heap. It may seem inefficient to be using far pointers
 * for the base of these tables, because the offset portion will always be zero.
 * We could just keep the segment address of the base, and then do something
 * like:
 *		 *MK_FP(baseseg, offset) = blahblah;
 *
 * whenever we need to access the table. This SHOULD be more efficient,
 * but the compilers do not appear to generate very efficient code in this
 * case. Huge pointers are not used, because they are slow, and because
 * Zortech does not support them.
 */

#ifdef MSC
#define farmalloc(n)	halloc(n, 1)
#endif

int taballoc()
{
#ifdef OS2
        ULONG size;
        USHORT shift;
        SEL sel;

        DosGetHugeShift(&shift);
#else
	char far *X;
#endif

	if (do_decomp) {
		if ((de_stack = malloc(DE_STACKLEN)) == 0)
			return (0);
	}
	else {
#ifdef OS2
                size = HSIZE * sizeof(code_t);

                if ( DosAllocHuge(HIUSHORT(size), LOUSHORT(size),
                                  &sel, 0, SEG_NONSHARED) )
                        return (0);

                codeptrs2[0] = MAKEP(sel, 0);
                codeptrs2[1] = MAKEP(sel + (1 << shift), 0);
#else
		if ((X = farmalloc((HSIZE + PARA) * sizeof(code_t))) == 0)
                        return (0);

		codeptrs2[0] = MK_FP(FP_SEGCEIL(X), 0);
                codeptrs2[1] = MK_FP(FP_SEGCEIL(X) + HSIZE/PARA, 0);
#endif
	}

#ifdef OS2
        size = HSIZE * sizeof(char);

        if ( DosAllocHuge(HIUSHORT(size), LOUSHORT(size),
                          &sel, 0, SEG_NONSHARED) )
                return (0);

        charptr1 = MAKEP(sel, 0);
#else
	if ((X = farmalloc((HSIZE + PARA) * sizeof(char))) == 0)
                return (0);

	charptr1 = MK_FP(FP_SEGCEIL(X), 0);
#endif

#ifdef OS2
        size = HSIZE * sizeof(code_t);

        if ( DosAllocHuge(HIUSHORT(size), LOUSHORT(size),
                          &sel, 0, SEG_NONSHARED) )
                return (0);

        codeptrs1[0] = MAKEP(sel, 0);
        codeptrs1[1] = MAKEP(sel + (1 << shift), 0);
#else
	if ((X = farmalloc((HSIZE + PARA) * sizeof(code_t))) == 0)
                return (0);

	codeptrs1[0] = MK_FP(FP_SEGCEIL(X), 0);
	codeptrs1[1] = MK_FP(FP_SEGCEIL(X) + HSIZE/PARA, 0);
#endif

	return (1);
}

#else

uchar	chartab1[HSIZE];
code_t	codetab1[HSIZE];
code_t	codetab2[HSIZE];

#define de_suffixof(i)	chartab1[i]
#define de_prefixof(i)	codetab1[i]
#define de_stack	(uchar *)codetab2

#define en_hashchar(i)	chartab1[i]
#define en_hashent(i)	codetab1[i]
#define en_hashcode(i)	codetab2[i]

#endif

void Usage()
{
  printf("\nUsage: COMPRESS [-dfvcVnC] [-b maxbits] [file ...]\n\n");
  printf("  -V   print Version\n");
  printf("  -h   print usage\n");
  printf("  -v   verbose\n");
  printf("  -q   quiet (default)\n");
  printf("  -d   decompress\n");
  printf("  -c   cat all output to stdout\n");
  printf("  -k   keep input files\n");
  printf("  -f   force overwrite of output files\n");
  printf("  -n   no header: useful to uncompress old files\n");
  printf("  -C   generate output compatible with compress 2.0.\n");
  printf("  -b maxbits   maxbits. Default %d\n", BITS);
}

/*****************************************************************
 * TAG( main )
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Usage: compress [-dfvc] [-b bits] [file ...]
 * Inputs:
 *	-d:	    If given, decompression is done instead.
 *
 *      -c:         Write output on stdout, don't remove original.
 *
 *      -b:         Parameter limits the max number of bits/code.
 *
 *	-f:	    Forces output file to be generated, even if one already
 *		    exists, and even if no space is saved by compressing.
 *		    If -f is not used, the user will be prompted if stdin is
 *		    a tty, otherwise, the output file will not be overwritten.
 *
 *      -v:	    Write compression statistics
 *
 * 	file ...:   Files to be compressed.  If none specified, stdin
 *		    is used.
 * Outputs:
 *	file.Z:	    Compressed form of file with same mode, owner, and utimes
 * 	or stdout   (if stdin used as input)
 *
 * Assumptions:
 *	When filenames are given, replaces with the compressed version
 *	(.Z suffix) only if the file decreases in size.
 * Algorithm:
 * 	Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 */

#ifdef __ZTC__
#include <int.h>
int silly_nonsense(struct INT_DATA *foo) {raise(SIGINT); return 1;}
#endif

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

void main(argc, argv)
int argc;
char **argv;
{
	char tempname[100], *cp;

	if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
		signal(SIGINT, onintr);
#ifdef __ZTC__
		/*
		 * The "signal" call above isn't good enough for Zortech
		 */
		int_intercept(0x23, silly_nonsense, 256);
#endif
#ifdef SIGSEGV
		signal(SIGSEGV, oops);
#endif
		if (isatty(2))
			foreground = 1;
	}

#ifndef MSDOS
	if ((cp = strrchr(argv[0], '/')) != 0)
		cp++;
	else
		cp = argv[0];
#else
	for (cp = argv[0]; *cp; cp++)
		if (*cp == '/' || *cp == '\\')
			argv[0] = cp + 1;
	cp = strlwr(argv[0]);
#endif
	/* Limited to 8 char filenames under DOS */
	if (strncmp(cp, "uncompress", 8) == 0)
		do_decomp = 1;
	else if (strncmp(cp, "zcat", 4) == 0) {
		do_decomp = 1;
		zcat_flg = 1;
	}

#ifdef BSD4_2
	/* 4.2BSD dependent - take it out if not */
	setlinebuf(stderr);
#endif /* BSD4_2 */

	for (argc--, argv++; argc > 0 && **argv == '-'; argc--, argv++) {
		while (*++(*argv)) {	/* Process all flags in this arg */
			switch (**argv) {
			case 'V':
                                version();
				break;
			case 'v':
				verbose = 1;
				break;
			case 'd':
				do_decomp = 1;
				break;
			case 'f':
			case 'F':
				force = 1;
				break;
			case 'n':
				magic = 0;
				break;
			case 'C':
				block_compress = 0;
				break;
			case 'b':
				if (!ARGVAL()) {
					fprintf(stderr, "Missing maxbits\n");
					Usage();
					exit(1);
				}
				maxbits = atoi(*argv);
				goto nextarg;
			case 'c':
				zcat_flg = 1;
				break;
			case 'q':
				verbose = 0;
                                break;
                        case 'k':
                                keep = 1;
				break;
                        case 'h':
                                Usage();
				exit(1);
			default:
				fprintf(stderr, "Unknown flag: '%c'; ", **argv);
				Usage();
				exit(1);
			}
		}
nextarg:;
	}

#ifdef i8088
	if (! taballoc()) {
		fprintf(stderr, "compress: out of memory\n");
		exit(1);
	}
#endif
	/*
	 * If no filename args, do standard input.
	 */
        if (argc <= 0) {
                if ( isatty(fileno(stdin)) )
                {
                  version();
                  Usage();
                  exit(1);
                }

		if (! ifopen((char *)0) || ! ofopen((char *)0))
			exit(1);

		ifname = "stdin";

		if (do_decomp) {
			if (!check_magic())
				exit(1);
			decompress();
		}
		else {
			compress();
			if (verbose)
				putc('\n', stderr);
		}
		exit(exit_stat);
	}

	while (--argc >= 0) {
		char *suf;

		ifname = *argv++;
		suf = strrchr(ifname, '.');

		exit_stat = 0;
		okunlink = 0;

		if (do_decomp) {		/* DECOMPRESSION */
			if (!suf || (strcmp(suf, ".Z") && strcmp(suf, ".z"))) {
                                strcpy(tempname, ifname);
				strcat(tempname, ".Z");
#ifdef MSDOS
#ifdef OS2
                                if ( !IsFileNameValid(tempname) )
#endif
                                {
                                  strcpy(tempname, ifname);
                                  if ( suf = strrchr(tempname, '.') )
                                    if ( suf > strrchr(tempname, '\\') )
                                      *suf = 0;
				  strcat(tempname, ".Z");
                                }
#endif
				ifname = tempname;
			}
			if (! ifopen(ifname) || !check_magic())
				continue;
			if (zcat_flg)
				ofname[0] = '\0';
			else {
				strcpy(ofname, ifname);
				ofname[strlen(ifname) - 2] = '\0';
			}
			if (!ofopen(ofname))
				continue;
			if (!zcat_flg && verbose)
				fprintf(stderr, "%s: ", ifname);
			decompress();
		}
		else {				/* COMPRESSION */
			if (suf && (!strcmp(suf, ".Z") || !strcmp(suf, ".z"))) {
				fprintf(stderr, "%s: already has .Z suffix -- no change\n",
				    ifname);
				continue;
			}
			if (! ifopen(ifname))
				continue;
			if (zcat_flg)
				ofname[0] = 0;
			else {
				strcpy(ofname, ifname);
#ifdef MSDOS
				strcat(ofname, ".Z");

#ifdef OS2
                                if ( !IsFileNameValid(ofname) )
#endif
                                {
                                  strcpy(ofname, ifname);

                                  if ( suf = strrchr(ofname, '.') )
                                    if ( suf > strrchr(ofname, '\\') )
                                      *suf = 0;

  				  strcat(ofname, ".Z");
                                }
#else   	/* We'll let ofopen do the complaining */
#ifndef BSD4_2
				if ((cp = strrchr(ofname, '/')) != NULL)
					cp++;
				else
					cp = ofname;
				if (strlen(cp) > 12) {
					fprintf(stderr,"%s: filename too long to tack on .Z\n",cp);
					continue;
				}
#endif
				strcat(ofname, ".Z");
#endif
			}
			if (! ofopen(ofname))
				continue;
			if (! zcat_flg && verbose)
				fprintf(stderr, "%s: ", ifname);
			compress();
		}

		if (! zcat_flg) {
			copystat();
			if ((exit_stat == 1) || verbose)
				putc('\n', stderr);
		}
	}
	exit(exit_stat);
}

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 *
 * Secondary hash function changed slightly for DOS. Hash table used to be
 * > 64K. This is slow on a 16 bit machine because it means long arithmetic,
 * and more complicated addressing of tables in the far address space.
 * We now restrict the table size to 64K, and, so that the table does
 * not overfill, restrict the codes that we will generate to MAXMAXCODE.
 * This causes slightly poorer compression in some cases, but, interestingly
 * enough, also causes better compression ratios in certain other cases.
 * Yes, this is all compatible with other compresses.
 */
static long	in_count;		/* length of input */
static long	out_count;		/* length of compressed output */
static long	ratio;			/* in_count/out_count * 256 */
static int	n_bits;			/* number of bits/code */
static int	n_bits8;		/* bits/code times 8 */
static int	bitoffset;		/* Offset into bitbuf */

#define NOENT		((code_t)0xffff)
#define MAXMAXCODE	((code_t)0xf000)

/*
 * Clear out the hash table. We try to do this as quickly as possible, because
 * it's running time dominates for small files. For big files, it doesn't matter
 * much because it doesn't get called often. Now I understand why the original
 * had a variable size hash table.
 */
void clearhash()
{
#ifdef i8088
	register unsigned i;
	code_t far *hp;

	hp = (code_t far *)codeptrs1[0];
	i = (unsigned)(HSIZE/2);
	do
		*hp++ = NOENT;
	while (--i > 0);

	hp = (code_t far *)codeptrs1[1];
	i = (unsigned)(HSIZE/2);
	do
		*hp++ = NOENT;
	while (--i > 0);
#else
	/*
	 * WARNING: assumes that NOENT == 0xffff
	 */
	memset((char *)codetab1, 0xff, HSIZE*sizeof(code_t));
#endif
}

/*
 * Compress stdin to stdout.
 */
void compress()
{
	register hash_t	i;
	register code_t	ent;
	hash_t		disp;
	int		c;
	code_t		freecode;	/* first unused entry */
	code_t		maxcode;	/* maximum code, given n_bits */
	code_t		maxmaxcode;
	code_t		k;
#ifdef CHECK_GAP
	long		checkpoint = 0;
#endif

	if (maxbits < INIT_BITS)
		maxbits = INIT_BITS;
	if (maxbits > BITS)
		maxbits = BITS;

	if (magic) {
		putchar(MAGIC0); putchar(MAGIC1);
		putchar(maxbits | block_compress);
		if (ferror(stdout))
			writeerr();
	}

	bitbuf[bitoffset = 0] = 0;
	out_count = 3;			/* includes 3-byte header mojo */
	ratio = 0;
	in_count = 1;

	n_bits = INIT_BITS;
	n_bits8 = INIT_BITS << 3;
	maxcode = MAXCODE(INIT_BITS);
	maxmaxcode = MAXCODE(maxbits);
	if (maxmaxcode > MAXMAXCODE)
		maxmaxcode = MAXMAXCODE;

	freecode = ((block_compress) ? FIRST : 256);

	clearhash();

	ent = getchar();

	while ((c = getchar()) != EOF) {
		in_count++;

		i = (hash_t)(c << 8) ^ ent;		/* xor hashing */

		if ((k = en_hashent(i)) == ent && en_hashchar(i) == (uchar)c) {
			ent = en_hashcode(i);
			goto Continue;
		}

		if (k != NOENT) {
			/*
			 * New secondary hash for 64K table.
			 * Experiment shows that the shift by 6 works well.
			 * Beats me why. "disp" must be relatively
			 * prime to the table size. Since the table size is a
			 * power of 2, this means "disp" must be odd.
			 *
			 * Note that we do not do a range check before doing
			 * "i -= disp". It is assumed that the hash table size
			 * (HSIZE) is 64K, and that the type "hash_t" (which
			 * is unsigned short) is 16 bits. Thus it is impossible
			 * for "i" to be out of range. On a machine with something
			 * other than 16 bit shorts, this would have to change.
			 */
			disp = ((hash_t)(c << 6) ^ ent) | 1;
			do {
				i -= disp;
				if ((k = en_hashent(i)) == ent &&
				    en_hashchar(i) == (uchar)c) {
					ent = en_hashcode(i);
					goto Continue;
				}
			} while (k != NOENT);
		}

		putcode(ent);

		if (freecode <= maxmaxcode) {
			/*
			 * Add the new entry.
			 */
			en_hashchar(i) = (uchar)c;
			en_hashent(i) = ent;
			en_hashcode(i) = freecode;

			/*
			 * If the next entry is going to be too big for the
			 * code size, then increase it, if possible.
			 */
			if (freecode++ > maxcode) {
				while (bitoffset)
					putcode(0);
				++n_bits;
				n_bits8 += 8;
				maxcode = MAXCODE(n_bits);
			}
		}
#ifdef CHECK_GAP
		else if (in_count >= checkpoint && block_compress) {
			checkpoint = in_count + CHECK_GAP;
			if (need_clear()) {
#else
		else if (block_compress) {
			if (1) {
#endif
				putcode(CLEAR);
				while (bitoffset > 0)
					putcode(0);
				clearhash();
				freecode = FIRST;
				maxcode = MAXCODE(INIT_BITS);
				n_bits = INIT_BITS;
				n_bits8 = n_bits << 3;
			}
		}
		ent = c;
Continue:;
	}
	/*
	 * Put out the final code.
	 */
	putcode(ent);

	/*
	 * At EOF, write the rest of the buffer.
	 */
	if (bitoffset > 0)
		fwrite(bitbuf, 1, (bitoffset + 7) / 8, stdout);
	out_count += (bitoffset + 7) / 8;
	fflush(stdout);
	if (ferror(stdout))
		writeerr();

	/*
	 * Print out stats on stderr
	 */
	if (! zcat_flg && verbose) {
		fprintf(stderr, "Compression: ");
		prratio(in_count - out_count, in_count);
	}
	if (out_count > in_count)	/* exit(2) if no savings */
		exit_stat = 2;
}

/*
 * Output the given code. Assumes that chars are 8 bits.
 * "n_bits" output bytes (containing 8 codes) are assembled
 * in in "bitbuf", and then written out.
 */
void putcode(code)
code_t code;
{
	register int i;
	register uchar *bp;

	bp = &bitbuf[(bitoffset >> 3)];
	i = bitoffset & 7;
	bp[0] |= (uchar)(code << i);
	bp[1] = (uchar)(code >>= (8 - i));
	bp[2] = (uchar)(code >> 8);

	if ((bitoffset += n_bits) == n_bits8) {
		bp = bitbuf;
		i = n_bits;
		out_count += i;
		do
			putchar(*bp++);
		while (--i);
		bitbuf[bitoffset = 0] = 0;
	}
}

#ifdef CHECK_GAP
/*
 * Compute the current compression ratio, and return non-zero if
 * it is has decreased since the last we checked.
 *
 * Don't use this anymore. Whenever the hash table fills,
 * we send a CLEAR immediately (if block_compress). This is faster,
 * and doesn't appear to affect the compression ratio much.
 */
int need_clear()
{
	long rat;

	if (in_count > 0x007fffffL) {		/* shift will overflow */
		rat = out_count >> 8;
		if (rat == 0)	     		/* Don't divide by zero */
			rat = 0x7fffffffL;
		else
			rat = in_count / rat;
	} else
		rat = (in_count << 8) / out_count;

	if (rat > ratio) {
		ratio = rat;
		return (0);
	}
	else {
		ratio = 0;
		return (1);
	}
}
#endif

/*
 * Decompress stdin to stdout. This code assumes that chars are 8 bits.
 */
void decompress()
{
	register uchar	*stackp;
	register code_t	code;
	code_t		oldcode, incode;
	code_t		codemask;
	code_t		freecode;		/* first unused entry */
	code_t		maxcode;		/* maximum code, given n_bits */
	code_t		maxmaxcode;
	int		finchar;
	int		size;			/* #bits in bitbuf */
	int		bitoff;			/* Offset into bitbuf */
	int		n_bits;			/* number of bits/code */
#ifndef i8088
	register uchar	*bp;
#endif

	n_bits = INIT_BITS;
	maxcode = MAXCODE(INIT_BITS) - 1;
	codemask = MAXCODE(INIT_BITS);
	freecode = ((block_compress) ? FIRST : 256) - 1;
	maxmaxcode = MAXCODE(maxbits);

	/*
	 * Read the first code into "oldcode"
	 */
	if ((size = fread(bitbuf, 1, n_bits, stdin)) <= 0)
		return;
	size = (size << 3) - (n_bits - 1);
	oldcode = (bitbuf[0] | (bitbuf[1] << 8)) & codemask;
	bitoff = n_bits;

	/*
	 * First code must be 8 bits == char. Write it, and die
	 * if it can't be written.
	 */
	putchar(finchar = oldcode);
	if (ferror(stdout))
		writeerr();

	stackp = de_stack;

	for ( ; ; ) {
		if (bitoff >= size) {
			if ((size = fread(bitbuf, 1, n_bits, stdin)) <= 0)
				break;
			/* Round size down to integral number of codes */
			size = (size << 3) - (n_bits - 1);
			bitoff = 0;
		}
		/*
		 * Read the next code into "code". On the 8088,
		 * a slight speedup is possible because it has the right byte
		 * order, and no alignment restrictions.
		 */
#ifdef i8088
		code = ((code_t)(*(long *)&bitbuf[(bitoff >> 3)] >>
			 (bitoff&7))) & codemask;
#else
		bp = &bitbuf[(bitoff >> 3)];
		code = (code_t)(((bp[0] | (code_t)bp[1] << 8) |
		     (ulong)bp[2] << 16) >> (bitoff & 7)) & codemask;
#endif
		bitoff += n_bits;

		if ((code == CLEAR) && block_compress) {
			n_bits = INIT_BITS;
    			maxcode = MAXCODE(INIT_BITS) - 1;
			codemask = MAXCODE(INIT_BITS);
			freecode = (FIRST - 1) - 1;
			size = 0;
			continue;
		}
		incode = code;

		/*
		 * Special case for KwKwK string.
		 */
		if (code > freecode) {
			if (code != freecode + 1)
				oops();
        		*stackp++ = (uchar)finchar;
			code = oldcode;
		}

		/*
		 * Generate output characters in reverse order
		 */
		while (code >= 256) {
			*stackp++ = de_suffixof(code);
			code = de_prefixof(code);
		}

		/*
		 * And write them out in the forward order.
		 */
		putchar(finchar = code);
		for (code = (stackp - de_stack) + 1; --code != 0; )
			putchar(*--stackp);

		/*
		 * Generate the new entry.
		 */
		if (freecode < maxmaxcode) {
			if (++freecode > maxcode) {
				if (++n_bits == maxbits)
					maxcode = maxmaxcode;
				else
					maxcode = MAXCODE(n_bits) - 1;
				size = 0;
				codemask = MAXCODE(n_bits);
			}
			de_prefixof(freecode) = oldcode;
			de_suffixof(freecode) = (uchar)finchar;
		}
		/*
		 * Remember previous code.
		 */
		oldcode = incode;
	}
	fflush(stdout);
	if (ferror(stdout))
		writeerr();
}

/*
 * Check a compressed file to make sure it has the proper magic number
 * at the beginning. Also read the third byte to determine "maxbits",
 * and "block_compress".
 */
int check_magic()
{
	if (! magic)
		return (1);
	if ((getchar() != MAGIC0) || (getchar() != MAGIC1)) {
		fprintf(stderr, "%s: not in compressed format\n", ifname);
		return (0);
	}
	maxbits = getchar();	/* set -b from file */
	block_compress = maxbits & BLOCK_MASK;
	maxbits &= BIT_MASK;
	if (maxbits > BITS) {
		fprintf(stderr,
		   "%s: compressed with %d bits, can only handle %d bits\n",
		    ifname, maxbits, BITS);
		return (0);
	}
	return (1);
}

void writeerr()
{
	perror(ofname);
	fclose(stdout);
	unlink(ofname);
	exit(1);
}

/*
 * Copy the permissions and file times from the input file to the
 * output.
 */
void copystat()
{
	struct stat statbuf;
	int mode;
	void (* ss)();
#ifndef __TURBOC__
	time_t timep[2];
#else
	struct ftime filetime;
	int fd;
#endif

	fclose(stdout);
	if (stat(ifname, &statbuf)) {		/* Get stat on input file */
		perror(ifname);
		return;
	}
	if ((statbuf.st_mode & S_IFMT) != S_IFREG) {
		if (! verbose)
		    	fprintf(stderr, "%s: ", ifname);
		fprintf(stderr, " -- not a regular file: unchanged");
		exit_stat = 1;
	}
	else if (statbuf.st_nlink > 1) {
		if (! verbose)
			fprintf(stderr, "%s: ", ifname);
		fprintf(stderr, " -- has %d other links: unchanged",
			statbuf.st_nlink - 1);
		exit_stat = 1;
	}
	else if (exit_stat == 2 && !force) { /* No compression: remove file.Z */
		if (verbose)
			fprintf(stderr, " -- file unchanged");
	}
	else {			/* ***** Successful Compression ***** */
		exit_stat = 0;
		mode = statbuf.st_mode & 07777;
#ifndef __ZTC__
		if (chmod(ofname, mode))		/* Copy modes */
			perror(ofname);
#endif
#ifndef MSDOS
		chown(ofname, statbuf.st_uid, statbuf.st_gid);	/* Copy ownership */
#endif
#ifndef __TURBOC__
		timep[0] = statbuf.st_atime;
		timep[1] = statbuf.st_mtime;
		utime(ofname, timep);
#else
		if ((fd = open(ofname, O_RDONLY)) >= 0) {
			if (getftime(fileno(stdin), &filetime) == 0)
				setftime(fd, &filetime);
			close(fd);
		}
#endif
		fclose(stdin);
		ss = signal(SIGINT, SIG_IGN);
		okunlink = 0;
                /* ^C here would leave both input, and output files around */
                if ( !keep )
                {
                  if (unlink(ifname))     /* Remove input file */
                          perror(ifname);
                  signal(SIGINT, ss);
                  if (verbose)
                          fprintf(stderr, " -- replaced with %s", ofname);
                }
		return;		/* Successful return */
	}

	/* Unsuccessful return -- one of the tests failed */

	if (unlink(ofname))
		perror(ofname);
}

void onintr(void)
{
	fclose(stdout);
	if (okunlink)
		unlink(ofname);
	exit(1);
}

void oops(void)     /* wild pointer -- assume bad input */
{
	if (do_decomp)
		fprintf (stderr, "uncompress: %s is corrupt.\n", ifname);
	fclose(stdout);
	if (okunlink)
		unlink(ofname);
	exit(1);
}

void prratio(num, den)
long int num, den;
{
	register int q;				/* Doesn't need to be long */

	if (num > 214748L)			/* 2147483647/10000 */
		q = (int)(num / (den / 10000L));
	else
		q = (int)(10000L * num / den);	/* Long calculations, though */
	if (q < 0) {
		putc('-', stderr);
		q = -q;
	}
	fprintf(stderr, "%d.%02d%%", q / 100, q % 100);
}

void version()
{
        static shown = 0;

        if ( shown == 0 )
        {
          fprintf(stderr, "\n%s\n", rcs_ident + 5);
          fprintf(stderr, "BITS = %d\n", BITS);
          shown = 1;
        }
}

/*
 * Open the file "ofname" for binary output with possible check
 * for overwrite. If all goes well, return non-zero, else zero.
 */
int ofopen(filename)
char *filename;
{
	static char IOoutbuf[8192];
	struct stat statbuf;

	if (filename && !*filename)
		filename = 0;

	/*
	 * Check for overwrite of existing file
	 */
	if (filename && !force && stat(filename, &statbuf) == 0) {
		char response[2];
		response[0] = 'n';
		fprintf(stderr, "%s already exists;", filename);
		if (foreground) {
			fprintf(stderr, " do you wish to overwrite %s (y or n)? ", filename);
			fflush(stderr);
			read(2, response, 2);
			while (response[1] != '\n') {
				if (read(2, response+1, 1) < 0)	{ /* Ack! */
					perror("stderr");
					break;
				}
			}
		}
		if (response[0] != 'y') {
			fprintf(stderr, "\tnot overwritten\n");
			return (0);
		}
	}

	okunlink = 1;
	/*
	 * Open the output file.
	 */
	if (filename && !freopen(filename, "wb", stdout)) {
		perror(filename);
		return (0);
	}
#ifdef O_BINARY
	setmode(fileno(stdout), O_BINARY);
#else
#ifdef __ZTC__
	/*
	 * I'm sure there must be a better way in Zortech C to change the
	 * mode of an already opened file, but I can't find it. It doesn't
	 * have a "setmode" call it seems.
	 */
	stdout->_flag &= ~_IOTRAN;
#endif
#endif
	setvbuf(stdout, IOoutbuf, _IOFBF, sizeof(IOoutbuf));
	return (1);
}

ifopen(filename)
char *filename;
{
	static char IOinbuf[8192];

	if (filename && !freopen(filename, "rb", stdin)) {
		perror(filename);
		return (0);
	}
#ifdef O_BINARY
	setmode(fileno(stdin), O_BINARY);
#else
#ifdef __ZTC__
	stdin->_flag &= ~_IOTRAN;
#endif
#endif
	setvbuf(stdin, IOinbuf, _IOFBF, sizeof(IOinbuf));
	return (1);
}
