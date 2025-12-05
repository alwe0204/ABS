/***************************************************************************
  timer.c
  Zeitmessung

Entnommen der bim-1.0 Software (bip matching)

Copyright (c) 1997, 1998, NEC Research Institute Inc.  All rights reserved.
Copyright (c) 1997, 1998, Joao C. Setubal. All rights reserved.
Copyright (c) 1997, 1998, Jorge Stolfi.  All rights reserved.
***************************************************************************/

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

float timer () {
/*********************************************************************/
/*                                                                   */
/* current processor time in seconds                                 */
/* difference between two calls is processor time spent by your code */
/* needs: <sys/types.h>, <sys/times.h>                               */
/* depends on compiler and OS                                        */
/*                                                                   */
/*********************************************************************/
  struct rusage r;

  getrusage(0, &r);
  return (float)(r.ru_utime.tv_sec+r.ru_utime.tv_usec/(float)1000000);
}
