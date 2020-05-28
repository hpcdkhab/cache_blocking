#include <stdio.h>
#ifndef __USE_GNU
#define __USE_GNU
#endif
#include <sched.h>
#include <errno.h>
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif
#include <string.h>
#ifndef __USE_POSIX199309
#define __USE_POSIX199309
#include <time.h>
#undef __USE_POSIX199309
#else
#include <time.h>
#endif
#include <stdlib.h>
#include <unistd.h>

long getrdtsc(void)
{
  unsigned hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return (long)(( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 ));
}

double get_time(void)
{
    struct timespec time_now;
    double result_sec_now;
 
    clock_gettime(CLOCK_REALTIME,&time_now);
    result_sec_now=(time_now.tv_sec)+(time_now.tv_nsec)*1e-9;
    return result_sec_now;
}

void get_time_stamp(long long* seconds,long long* nanoseconds)
{
    struct timespec time_now;

    clock_gettime(CLOCK_REALTIME,&time_now);
    *seconds=time_now.tv_sec;
    *nanoseconds=time_now.tv_nsec;
}

double sec_tsc_cycle()
{

  double time_start, time_end;
  long tick_start, tick_end;

  time_start = get_time();
  tick_start = getrdtsc();
  usleep(100000);
  tick_end = getrdtsc();
  time_end = get_time();

  return (time_end-time_start)/(double)(tick_end-tick_start);
}

int pin_thread(int* thread_id)
{
    int err;
    int core;
    core=*thread_id;
    cpu_set_t processor_mask;
    CPU_ZERO(&processor_mask);
    CPU_SET(core,&processor_mask);
    err = sched_setaffinity( 0, sizeof(cpu_set_t), &processor_mask );
    if(err<0)
    {
      printf("pin_thread error:%s;%d\n",strerror(errno),core);
    }
    return err;
}

