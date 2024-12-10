/* This function returns the current date */
#include <time.h>

#ifdef hpux
 get_current_date
#else
 get_current_date_
#endif
(int *mon, int *day, int *year)
{
  struct tm *the_time;
  time_t tt;
  char date[8];

  tt = time(NULL);
  the_time = localtime(&tt);
  
  *mon = the_time->tm_mon + 1;
  *day = the_time->tm_mday;
  *year= the_time->tm_year +1900;

  return; 
}

/*int main (int ac, char **av)
{

   int m,d,y;

   get_current_date(&m, &d, &y);

   printf("The date is: %2.2d/%2.2d/%4.4d\n", m,d,y);

   exit(0);
}*/

