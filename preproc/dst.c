/* This function receives the month, day, and year from the calling program
and returns a flag which is set to be true (1) if the date falls within 
daylight savings time or false (0) if date falls within EST. This function
simply loads all of the date and time varibles contained within the structure
the_time.  For additional information see UNIX manpage mktime */

#include <time.h>

int
#ifdef hpux
 daylight_saving_time
#else
 daylight_saving_time_
#endif
(int *mo, int *day, int *year)
{
  struct tm the_time, *r;
  time_t sec_since_epoch;

  the_time.tm_sec = 0;
  the_time.tm_min = 0;
  the_time.tm_hour = 0;
  the_time.tm_mday = *day;
  the_time.tm_mon = *mo;
  the_time.tm_year = *year;
  the_time.tm_wday = 0;
  the_time.tm_yday = 0;
  the_time.tm_isdst = -1;
						  
  sec_since_epoch = mktime(&the_time);
  r = localtime(&sec_since_epoch);
  return r->tm_isdst;
}
