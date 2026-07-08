#include "./math.HC"
#include "./defs.HH"
#include "./memory.HC"

#define CDATE_YEAR_DAYS		365.24225
#define CDATE_YEAR_DAYS_INT	36524225
#define CDATE_BASE_DAY_OF_WEEK	0

U64 ModU64(U64 *k, U64 m);

public class timeval
{ // c compatability
  I64 tv_sec;   /* seconds since Jan. 1, 1970 */
  I32 tv_usec;  /* and microseconds */
};

public class tm
{ // c compatability
	I32	tm_sec;		/* seconds after the minute [0-60] */
	I32	tm_min;		/* minutes after the hour [0-59] */
	I32	tm_hour;	/* hours since midnight [0-23] */
	I32	tm_mday;	/* day of the month [1-31] */
	I32	tm_mon;		/* months since January [0-11] */
	I32	tm_year;	/* years since 1900 */
	I32	tm_wday;	/* days since Sunday [0-6] */
	I32	tm_yday;	/* days since January 1 [0-365] */
	I32	tm_isdst;	/* Daylight Savings Time flag */
	I64 tm_gmtoff;	/* offset from UTC in seconds */
	I8  *tm_zone;	/* timezone abbreviation */
};

extern "c" tm *localtime(I64 *clock);
extern "c" I32 gettimeofday(timeval *tp, U0 *tzp);

public I64 class CDate
{
  U32 time;
  I32 date;
};

public class CDateStruct
{
  U64 sec10000,sec100,sec,min,hour,
	day_of_week,day_of_mon,mon;
  I32 year;
  I64 zone_diff;
};

U64 mon_start_days1[12]={
  0,31,59,90,120,151,181,212,243,273,304,334};
U64 mon_start_days2[12]={
  0,31,60,91,121,152,182,213,244,274,305,335};


U64 NowMilliseconds()
{
  timeval tm;
  gettimeofday(&tm,NULL);
  return (tm.tv_sec*1000) +
         (tm.tv_usec/1000);
}

U0 NowDateTimeStruct(CDateStruct *_ds)
{
  timeval _tv;
  tm *ptm;
  U64 milliseconds,seconds;

  gettimeofday(&_tv,NULL);
  milliseconds = (_tv.tv_sec*1000) +
                 (_tv.tv_usec/1000);
  seconds = milliseconds / 1000;
  ptm = localtime(&seconds);

  _ds->sec10000 = _tv.tv_usec/100;
  _ds->sec100 = _tv.tv_usec/10000;
  _ds->sec  = ptm->tm_sec(U64);
  _ds->min  = ptm->tm_min(U64);
  _ds->hour = ptm->tm_hour(U64);
  _ds->year = ptm->tm_year(U64);
  _ds->mon  = ptm->tm_mon(U64);
  _ds->mon += 1;
  _ds->day_of_week = ptm->tm_wday(U64);
  _ds->day_of_mon = ptm->tm_mday(U64);
  _ds->year += 1900;
}

I64 YearStartDate(I64 year)
{//32-bit day since AD 0, given year number.
  I64 y1=year-1,yd4000=y1/4000,yd400=y1/400,yd100=y1/100,yd4=y1/4;
  return year*365+yd4-yd100+yd400-yd4000;
}

I64 DayOfWeek(I64 i)
{//Day of week, given 32-bit day since AD 0.
  i+=CDATE_BASE_DAY_OF_WEEK;
  if (i>=0)
    return i % 7;
  else
    return 6-(6-i)%7;
}

U0 Date2Struct(CDateStruct *_ds,CDate cdt)
{//Cvt CDate to CDateStruct.
  I64 i,k,date=cdt.date;
  _ds->day_of_week=DayOfWeek(date);
  _ds->year=(date+1)*100000/CDATE_YEAR_DAYS_INT;
  i=YearStartDate(_ds->year);
  while (i>date) {
    _ds->year--;
    i=YearStartDate(_ds->year);
  }
  date-=i;
  if (YearStartDate(_ds->year+1)-i==365) {
    k=0;
    while (date>=mon_start_days1[k+1] && k<11)
      k++;
    date-=mon_start_days1[k];
  } else {
    k=0;
    while (date>=mon_start_days2[k+1] && k<11)
      k++;
    date-=mon_start_days2[k];
  }
  _ds->mon=k+1;
  _ds->day_of_mon=date+1;
  k=((625*15*15*3*cdt.time)>>21)+1;
  _ds->sec10000=ModU64(&k,100);
  _ds->sec100=ModU64(&k,100);
  _ds->sec=ModU64(&k,60);
  _ds->min=ModU64(&k,60);
  _ds->hour  =k;
}


CDate Struct2Date(CDateStruct *_ds)
{//Cvt CDateStruct to CDate.
  CDate cdt;
  I64 i1,i2;
  i1=YearStartDate(_ds->year);
  i2=YearStartDate(_ds->year+1);
  if (i2-i1==365) {
    i1+=mon_start_days1[_ds->mon-1];
  } else {
    i1+=mon_start_days2[_ds->mon-1];
  }
  cdt.date=i1+_ds->day_of_mon-1;
  cdt.time=((_ds->sec10000+100*(_ds->sec100+100*(_ds->sec
	+60*(_ds->min+60*_ds->hour))))<<21)/(15*15*3*625);
  return cdt;
}

I32 FirstDayOfMon(I64 i)
{//First day of month, given 32-bit day since AD 0.
  CDateStruct ds;
  CDate cdt=0;
  cdt.date=i;
  Date2Struct(&ds,cdt);
  cdt=Struct2Date(&ds);
  return cdt.date;
}

I64 LastDayOfMon(I64 i)
{//Last day of month, given 32-bit day since AD 0.
  CDateStruct ds;
  CDate cdt=0;
  cdt.date=i;
  Date2Struct(&ds,cdt);
  ds.mon++;
  if (ds.mon==13) {
    ds.mon=0;
    ds.year++;
  }
  ds.day_of_mon=1;
  cdt=Struct2Date(&ds);
  return cdt.date-1;
}

I32 FirstDayOfYear(I64 i)
{//First day of year, given 32-bit day since AD 0.
  CDateStruct ds;
  CDate cdt=0;
  cdt.date=i;
  Date2Struct(&ds,cdt);
  ds.day_of_mon=1;
  ds.mon=1;
  cdt=Struct2Date(&ds);
  return cdt.date;
}

I64 LastDayOfYear(I64 i)
{//Last day of year, given 32-bit day since AD 0.
  CDateStruct ds;
  CDate cdt=0;
  cdt.date=i;
  Date2Struct(&ds,cdt);
  ds.day_of_mon=1;
  ds.mon=1;
  ds.year++;
  cdt=Struct2Date(&ds);
  return cdt.date-1;
}

CDate Now()
{//Current datetime.
  CDateStruct ds;
  CDate cdt = 0;
  NowDateTimeStruct(&ds);
  cdt=Struct2Date(&ds);
  return cdt;
}

U0 CivilFromDays(I64 z, I64 *y, I64 *m, I64 *d)
{
  z += 719468; // shift epoch to 0000-03-01
  I64 era;

  if (z >= 0) {
    era = z / 146097;
  } else {
    era = (z - 146096) / 146097;
  }

  I64 doe = z - era * 146097; // [0, 146096]
  I64 yoe = (doe - doe/1460 + doe/36524 - doe/146096) / 365;
  I64 yr  = yoe + era * 400;
  I64 doy = doe - (365*yoe + yoe/4 - yoe/100); // [0, 365]
  I64 mp  = (5*doy + 2) / 153;
  *d = doy - (153*mp + 2)/5 + 1; // [1, 31]
  // [1, 12]
  if (mp < 10) {
    *m = mp + 3;
  } else {
    *m = mp - 9;
  }
  *y = yr + (*m <= 2);
}

// Writes e.g. 2026-07-05T14:30:00.123456789Z  (UTC)
U0 IsoFromTimespec(cTimespec *ts, U8 *buf)
{
  I64 sec = ts->tv_sec, nsec = ts->tv_nsec;
  I64 days = sec / 86400, rem = sec % 86400;
  if (rem < 0) {
    rem += 86400;
    days--;
  }// floored div, for pre-1970
  I64 hh = rem/3600, mm = rem%3600/60, ss = rem%60;
  I64 y, mo, d;
  CivilFromDays(days, &y, &mo, &d);
  StrPrint(buf, "%04d-%02d-%02dT%02d:%02d:%02d.%09dZ", y, mo, d, hh, mm, ss, nsec);
}
