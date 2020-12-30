#include <stdio.h>
#include <ctype.h>
#include <time.h>

char *months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

int main( int argc, char *argv[] )
{
	struct tm *ltm;
	time_t ltime;
	char *prefix;

	prefix = argc > 1 ? argv[1] : "";
	time( &ltime );
	ltm = localtime( &ltime );
	printf(
		"%sDATE	MACRO\r\n"
		"\tDB	\"%s %02d %04d\"\r\n"
		"\tENDM\r\n"
		"%sTIME	MACRO\r\n"
		"\tDB	\"%02d:%02d:%02d\"\r\n"
		"\tENDM\r\n",
		prefix, months[ltm->tm_mon], ltm->tm_mday, ltm->tm_year+1900,
		prefix, ltm->tm_hour, ltm->tm_min, ltm->tm_sec );
	return 0;
}