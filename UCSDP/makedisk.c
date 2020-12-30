#include <stdio.h>
#include <io.h>
#include <ctype.h>
#include <stddef.h>
#include <string.h>

#include <fcntl.h>    // O_RDWR...
#include <sys/stat.h> // S_IWRITE

#define MAKEDISK "UCSD p-System Disk Image Converter V1.2 for Z80-MBC2, (C) 2019-20 by GmEsoft"

typedef unsigned int uint32_t;

void help()
{
	puts(
		"Makedisk -M:outfile [-L:lrl] [-SI:input track size] [-SO:output track size]\n"
		"         [ [-I:infile] [-XI:input interleave] [-KI:input skew]\n"
		"            -F:[T]from -T:[T]to -D:[T]dest ... ]\n"
		"         -P:[T]padsize\n"
		"  T for whole tracks, else sectors\n"
		"Defaults:\n"
		"  -L:128   bytes per sector\n"
		"  -SI:26   sectors per side for input (8inch format)\n"
		"  -SO:128  sectors per side for output (Z80-MBC2 image format)\n"
		"  -XI:1    (no interleave)\n"
		"  -KI:0    (no skew)\n"
		"Example:\n"
		"  makedisk -M:DS2N22.DSK -I:ucsd-advx.dsk -F:T0 -T:T1 -D:T0 -XI:2 -KI:6 -F:T1 -T:T77 -D:T1 -P:T77"
	);
}

void copy( int out, int in, uint32_t from, uint32_t to, uint32_t dest, int lrl, int spt, int intfact, int skew )
{
	char buf[1024];
	int map[256];
	int rmap[256];
	int i, j, t;

	if ( !spt )
		spt = 1;
	if ( !intfact )
		intfact = 1;

	for ( i=0; i<spt; ++i )
		map[i] = -1;

	j=0;

	for ( i=0; i<spt; ++i )
	{
		map[j] = i;
		j += intfact;
		if ( j >= spt )
			j = 0;
		while ( map[j] != -1 )
			++j;
	}

	for ( i=0; i<spt; ++i )
		rmap[map[i]] = i;

	printf( "Sect Map:\nFrom: " );
	for ( i=0; i<spt; ++i )
		printf( "%3d", map[i] );
	printf( "\n  To: " );
	for ( i=0; i<spt; ++i )
		printf( "%3d", i );
	printf( "\n" );

	lseek( out, dest*lrl, SEEK_SET );

	t = 0;
	for ( ; from<to; from+=spt )
	{
		for ( i=0; i<spt && i<to-from; ++i )
		{
			j = rmap[i] + t;
			while ( j>=spt )
				j -= spt;
			//printf( "%3d ", j );
			lseek( in, ( from + j ) * lrl, SEEK_SET );
			read( in, buf, lrl );
			write( out, buf, lrl );
		}
		//puts("");
		t += skew;
		while ( t >= spt )
			t -= spt;
	}
}

int main( int argc, char* argv[] )
{
	int infile=0, outfile=0;

	uint32_t	from=0;
	uint32_t	to=0;
	uint32_t	dest=0;
	uint32_t	mult;
	uint32_t	lrl=128;
	uint32_t	intrksize=26;
	uint32_t	outtrksize=128;
	uint32_t	intfact=1;
	uint32_t	skew=0;
	int		i;

	puts( MAKEDISK "\n\n" );

	for ( i=1; i<argc; ++i )
	{
		char *s = argv[i];
		char c = 0;

		//puts(s);
		if ( *s == '-' )
			++s;
		switch ( toupper( *s ) )
		{
		case 'M':
		case 'O':
			++s;
			if ( *s == ':' )
				++s;
			printf( "Creating: %s\n", s );
			outfile = open( s, _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY, _S_IWRITE );
			break;
		case 'I':
			++s;
			if ( *s == ':' )
				++s;
			if ( infile )
				close( infile );
			printf( "Reading: %s\n", s );
			infile = open( (const char *)s, _O_RDONLY | _O_BINARY, _S_IREAD );
			break;
		case 'L':
			++s;
			if ( *s == ':' )
				++s;
			sscanf( s, "%d", &lrl );
			break;
		case 'S':
			++s;
			if ( *s != ':' )
				c = toupper( *s ), ++s;
			if ( *s == ':' )
				++s;
			switch ( c )
			{
			case 'I':
				sscanf( s, "%d", &intrksize );
				break;
			case 'O':
				sscanf( s, "%d", &outtrksize );
				break;
			default:
				printf( "Unrecognized option: -S%c:%s", c, s );
				return 1;
			}
			break;
		case 'X':
			++s;
			if ( *s != ':' )
				c = toupper( *s ), ++s;
			if ( *s == ':' )
				++s;
			switch ( c )
			{
			case 'I':
				sscanf( s, "%d", &intfact );
				break;
			default:
				printf( "Unrecognized option: -X%c:%s", c, s );
				return 1;
			}
			break;
		case 'K':
			++s;
			if ( *s != ':' )
				c = toupper( *s ), ++s;
			if ( *s == ':' )
				++s;
			switch ( c )
			{
			case 'I':
				sscanf( s, "%d", &skew );
				break;
			default:
				printf( "Unrecognized option: -K%c:%s", c, s );
				return 1;
			}
			break;
		case 'F':
			++s;
			if ( *s == ':' )
				++s;
			mult = 1;
			if ( toupper( *s ) == 'T' )
				mult = intrksize, ++s;
			sscanf( s, "%d", &from );
			from *= mult;
			break;
		case 'T':
			++s;
			if ( *s == ':' )
				++s;
			mult = 1;
			if ( toupper( *s ) == 'T' )
				mult = intrksize, ++s;
			sscanf( s, "%d", &to );
			to *= mult;
			break;
		case 'D':
			++s;
			if ( *s == ':' )
				++s;
			mult = 1;
			if ( toupper( *s ) == 'T' )
				mult = outtrksize, ++s;
			sscanf( s, "%d", &dest );
			dest *= mult;
			printf( "[%06X-%06X) -> %06X (int=%d,skew=%d)\n", from*lrl, to*lrl, dest*lrl, intfact, skew );
			copy( outfile, infile, from, to, dest, lrl, intrksize, intfact, skew );
			break;
		case 'P':
			++s;
			if ( *s == ':' )
				++s;
			mult = 1;
			if ( toupper( *s ) == 'T' )
				mult = outtrksize, ++s;
			sscanf( s, "%d", &dest );
			dest *= mult;
			printf( "Padding to %06X\n", dest*lrl);
			chsize( outfile, dest*lrl );
			break;
		case '?':
			help();
			return 0;
		default:
			printf( "Unrecognized switch: -%s\n", s );
			printf( "makedisk -? for help.\n" );
			return 1;
		}

		if ( errno )
		{
			puts( strerror( errno ) );
			return 1;
		}
	}
	return 0;
}
