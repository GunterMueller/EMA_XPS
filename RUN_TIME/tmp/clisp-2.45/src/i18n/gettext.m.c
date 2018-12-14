#line 1 "gettext.c"
/*
 * ======= General internationalization, for Lisp programs too =======
 * Copyright (C) 1990-2005 Bruno Haible
 * Copyright (C) 1998-2006 Sam Steingold
 * GPL2
 */

#if defined(_WIN32)
/* get ASCII functions */
# undef UNICODE
#endif

#include "clisp.h"
#include "config.h"

#include <string.h>             /* strncpy() */
#include <locale.h>
#if defined(HAVE_LANGINFO_H)
# include <langinfo.h>
#endif
#include <limits.h>            /* for CHAR_MAX */

#ifdef CLISP_UNICODE
# define if_UNICODE(statement)  statement
#else
# define if_UNICODE(statement)  /*nothing*/
#endif

DEFMODULE(i18n,"I18N")

#define O(varname) module__i18n__object_tab._##varname
#define F(varname) subr_tab_ptr_as_object(&(module__i18n__subr_tab._##varname))

struct module__i18n__object_tab_t {
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  gcv_object_t _object_i18n__mk_locale_conv;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_1))
  gcv_object_t _object_Kabday_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_2))
  gcv_object_t _object_Kabday_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_3))
  gcv_object_t _object_Kabday_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_4))
  gcv_object_t _object_Kabday_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_5))
  gcv_object_t _object_Kabday_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_6))
  gcv_object_t _object_Kabday_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_7))
  gcv_object_t _object_Kabday_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_1))
  gcv_object_t _object_Kabmon_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_10))
  gcv_object_t _object_Kabmon_10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_11))
  gcv_object_t _object_Kabmon_11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_12))
  gcv_object_t _object_Kabmon_12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_2))
  gcv_object_t _object_Kabmon_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_3))
  gcv_object_t _object_Kabmon_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_4))
  gcv_object_t _object_Kabmon_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_5))
  gcv_object_t _object_Kabmon_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_6))
  gcv_object_t _object_Kabmon_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_7))
  gcv_object_t _object_Kabmon_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_8))
  gcv_object_t _object_Kabmon_8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_9))
  gcv_object_t _object_Kabmon_9;
#endif
#if defined(LC_ADDRESS)
  gcv_object_t _object_Kaddress;
#endif
#if defined(LC_ALL)
  gcv_object_t _object_Kall;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ALT_DIGITS))
  gcv_object_t _object_Kalt_digits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(AM_STR))
  gcv_object_t _object_Kam_str;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(CODESET))
  gcv_object_t _object_Kcodeset;
#endif
#if defined(LC_COLLATE)
  gcv_object_t _object_Kcollate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(CRNCYSTR))
  gcv_object_t _object_Kcrncystr;
#endif
#if defined(LC_CTYPE)
  gcv_object_t _object_Kctype;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_1))
  gcv_object_t _object_Kday_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_2))
  gcv_object_t _object_Kday_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_3))
  gcv_object_t _object_Kday_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_4))
  gcv_object_t _object_Kday_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_5))
  gcv_object_t _object_Kday_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_6))
  gcv_object_t _object_Kday_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_7))
  gcv_object_t _object_Kday_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_FMT))
  gcv_object_t _object_Kd_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_MD_ORDER))
  gcv_object_t _object_Kd_md_order;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_T_FMT))
  gcv_object_t _object_Kd_t_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA))
  gcv_object_t _object_Kera;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_D_FMT))
  gcv_object_t _object_Kera_d_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_D_T_FMT))
  gcv_object_t _object_Kera_d_t_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_T_FMT))
  gcv_object_t _object_Kera_t_fmt;
#endif
#if defined(LC_IDENTIFICATION)
  gcv_object_t _object_Kidentification;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_FONTSIGNATURE))
  gcv_object_t _object_Klocale_fontsignature;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICALENDARTYPE))
  gcv_object_t _object_Klocale_icalendartype;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICENTURY))
  gcv_object_t _object_Klocale_icentury;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICOUNTRY))
  gcv_object_t _object_Klocale_icountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICURRDIGITS))
  gcv_object_t _object_Klocale_icurrdigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICURRENCY))
  gcv_object_t _object_Klocale_icurrency;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDATE))
  gcv_object_t _object_Klocale_idate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDAYLZERO))
  gcv_object_t _object_Klocale_idaylzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTANSICODEPAGE))
  gcv_object_t _object_Klocale_idefaultansicodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTCODEPAGE))
  gcv_object_t _object_Klocale_idefaultcodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTCOUNTRY))
  gcv_object_t _object_Klocale_idefaultcountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTEBCDICCODEPAGE))
  gcv_object_t _object_Klocale_idefaultebcdiccodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTLANGUAGE))
  gcv_object_t _object_Klocale_idefaultlanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTMACCODEPAGE))
  gcv_object_t _object_Klocale_idefaultmaccodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDIGITS))
  gcv_object_t _object_Klocale_idigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDIGITSUBSTITUTION))
  gcv_object_t _object_Klocale_idigitsubstitution;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IFIRSTDAYOFWEEK))
  gcv_object_t _object_Klocale_ifirstdayofweek;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IFIRSTWEEKOFYEAR))
  gcv_object_t _object_Klocale_ifirstweekofyear;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IINTLCURRDIGITS))
  gcv_object_t _object_Klocale_iintlcurrdigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILANGUAGE))
  gcv_object_t _object_Klocale_ilanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILDATE))
  gcv_object_t _object_Klocale_ildate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILZERO))
  gcv_object_t _object_Klocale_ilzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IMEASURE))
  gcv_object_t _object_Klocale_imeasure;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IMONLZERO))
  gcv_object_t _object_Klocale_imonlzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGCURR))
  gcv_object_t _object_Klocale_inegcurr;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGNUMBER))
  gcv_object_t _object_Klocale_inegnumber;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSEPBYSPACE))
  gcv_object_t _object_Klocale_inegsepbyspace;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSIGNPOSN))
  gcv_object_t _object_Klocale_inegsignposn;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSYMPRECEDES))
  gcv_object_t _object_Klocale_inegsymprecedes;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IOPTIONALCALENDAR))
  gcv_object_t _object_Klocale_ioptionalcalendar;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPAPERSIZE))
  gcv_object_t _object_Klocale_ipapersize;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSEPBYSPACE))
  gcv_object_t _object_Klocale_ipossepbyspace;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSIGNPOSN))
  gcv_object_t _object_Klocale_ipossignposn;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSYMPRECEDES))
  gcv_object_t _object_Klocale_ipossymprecedes;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ITIMEMARKPOSN))
  gcv_object_t _object_Klocale_itimemarkposn;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ITLZERO))
  gcv_object_t _object_Klocale_itlzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_S1159))
  gcv_object_t _object_Klocale_s1159;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_S2359))
  gcv_object_t _object_Klocale_s2359;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVCTRYNAME))
  gcv_object_t _object_Klocale_sabbrevctryname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME1))
  gcv_object_t _object_Klocale_sabbrevdayname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME2))
  gcv_object_t _object_Klocale_sabbrevdayname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME3))
  gcv_object_t _object_Klocale_sabbrevdayname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME4))
  gcv_object_t _object_Klocale_sabbrevdayname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME5))
  gcv_object_t _object_Klocale_sabbrevdayname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME6))
  gcv_object_t _object_Klocale_sabbrevdayname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME7))
  gcv_object_t _object_Klocale_sabbrevdayname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVLANGNAME))
  gcv_object_t _object_Klocale_sabbrevlangname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME1))
  gcv_object_t _object_Klocale_sabbrevmonthname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME10))
  gcv_object_t _object_Klocale_sabbrevmonthname10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME11))
  gcv_object_t _object_Klocale_sabbrevmonthname11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME12))
  gcv_object_t _object_Klocale_sabbrevmonthname12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME13))
  gcv_object_t _object_Klocale_sabbrevmonthname13;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME2))
  gcv_object_t _object_Klocale_sabbrevmonthname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME3))
  gcv_object_t _object_Klocale_sabbrevmonthname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME4))
  gcv_object_t _object_Klocale_sabbrevmonthname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME5))
  gcv_object_t _object_Klocale_sabbrevmonthname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME6))
  gcv_object_t _object_Klocale_sabbrevmonthname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME7))
  gcv_object_t _object_Klocale_sabbrevmonthname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME8))
  gcv_object_t _object_Klocale_sabbrevmonthname8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME9))
  gcv_object_t _object_Klocale_sabbrevmonthname9;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SCOUNTRY))
  gcv_object_t _object_Klocale_scountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SCURRENCY))
  gcv_object_t _object_Klocale_scurrency;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDATE))
  gcv_object_t _object_Klocale_sdate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME1))
  gcv_object_t _object_Klocale_sdayname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME2))
  gcv_object_t _object_Klocale_sdayname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME3))
  gcv_object_t _object_Klocale_sdayname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME4))
  gcv_object_t _object_Klocale_sdayname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME5))
  gcv_object_t _object_Klocale_sdayname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME6))
  gcv_object_t _object_Klocale_sdayname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME7))
  gcv_object_t _object_Klocale_sdayname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDECIMAL))
  gcv_object_t _object_Klocale_sdecimal;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGCOUNTRY))
  gcv_object_t _object_Klocale_sengcountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGCURRNAME))
  gcv_object_t _object_Klocale_sengcurrname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGLANGUAGE))
  gcv_object_t _object_Klocale_senglanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SGROUPING))
  gcv_object_t _object_Klocale_sgrouping;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SINTLSYMBOL))
  gcv_object_t _object_Klocale_sintlsymbol;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SISO3166CTRYNAME))
  gcv_object_t _object_Klocale_siso3166ctryname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SISO639LANGNAME))
  gcv_object_t _object_Klocale_siso639langname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLANGUAGE))
  gcv_object_t _object_Klocale_slanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLIST))
  gcv_object_t _object_Klocale_slist;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLONGDATE))
  gcv_object_t _object_Klocale_slongdate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONDECIMALSEP))
  gcv_object_t _object_Klocale_smondecimalsep;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONGROUPING))
  gcv_object_t _object_Klocale_smongrouping;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME1))
  gcv_object_t _object_Klocale_smonthname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME10))
  gcv_object_t _object_Klocale_smonthname10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME11))
  gcv_object_t _object_Klocale_smonthname11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME12))
  gcv_object_t _object_Klocale_smonthname12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME13))
  gcv_object_t _object_Klocale_smonthname13;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME2))
  gcv_object_t _object_Klocale_smonthname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME3))
  gcv_object_t _object_Klocale_smonthname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME4))
  gcv_object_t _object_Klocale_smonthname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME5))
  gcv_object_t _object_Klocale_smonthname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME6))
  gcv_object_t _object_Klocale_smonthname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME7))
  gcv_object_t _object_Klocale_smonthname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME8))
  gcv_object_t _object_Klocale_smonthname8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME9))
  gcv_object_t _object_Klocale_smonthname9;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHOUSANDSEP))
  gcv_object_t _object_Klocale_smonthousandsep;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVECTRYNAME))
  gcv_object_t _object_Klocale_snativectryname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVECURRNAME))
  gcv_object_t _object_Klocale_snativecurrname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVEDIGITS))
  gcv_object_t _object_Klocale_snativedigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVELANGNAME))
  gcv_object_t _object_Klocale_snativelangname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNEGATIVESIGN))
  gcv_object_t _object_Klocale_snegativesign;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SPOSITIVESIGN))
  gcv_object_t _object_Klocale_spositivesign;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SSHORTDATE))
  gcv_object_t _object_Klocale_sshortdate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SSORTNAME))
  gcv_object_t _object_Klocale_ssortname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STHOUSAND))
  gcv_object_t _object_Klocale_sthousand;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STIME))
  gcv_object_t _object_Klocale_stime;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STIMEFORMAT))
  gcv_object_t _object_Klocale_stimeformat;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SYEARMONTH))
  gcv_object_t _object_Klocale_syearmonth;
#endif
#if defined(LC_MEASUREMENT)
  gcv_object_t _object_Kmeasurement;
#endif
#if defined(LC_MESSAGES)
  gcv_object_t _object_Kmessages;
#endif
#if defined(LC_MONETARY)
  gcv_object_t _object_Kmonetary;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_1))
  gcv_object_t _object_Kmon_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_10))
  gcv_object_t _object_Kmon_10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_11))
  gcv_object_t _object_Kmon_11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_12))
  gcv_object_t _object_Kmon_12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_2))
  gcv_object_t _object_Kmon_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_3))
  gcv_object_t _object_Kmon_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_4))
  gcv_object_t _object_Kmon_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_5))
  gcv_object_t _object_Kmon_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_6))
  gcv_object_t _object_Kmon_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_7))
  gcv_object_t _object_Kmon_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_8))
  gcv_object_t _object_Kmon_8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_9))
  gcv_object_t _object_Kmon_9;
#endif
#if defined(LC_NAME)
  gcv_object_t _object_Kname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(NOEXPR))
  gcv_object_t _object_Knoexpr;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(NOSTR))
  gcv_object_t _object_Knostr;
#endif
#if defined(LC_NUMERIC)
  gcv_object_t _object_Knumeric;
#endif
#if defined(LC_PAPER)
  gcv_object_t _object_Kpaper;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(PM_STR))
  gcv_object_t _object_Kpm_str;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(RADIXCHAR))
  gcv_object_t _object_Kradixchar;
#endif
#if defined(LC_TELEPHONE)
  gcv_object_t _object_Ktelephone;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(THOUSEP))
  gcv_object_t _object_Kthousep;
#endif
#if defined(LC_TIME)
  gcv_object_t _object_Ktime;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(T_FMT))
  gcv_object_t _object_Kt_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(T_FMT_AMPM))
  gcv_object_t _object_Kt_fmt_ampm;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(YESEXPR))
  gcv_object_t _object_Kyesexpr;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(YESSTR))
  gcv_object_t _object_Kyesstr;
#endif
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  gcv_object_t _object___28or_20integer_20_28member_defined_28codeset_29_Kcodeset_defined_28d_t_fmt_29_Kd_t_fmt_defined_28d_fmt_29_Kd_fmt_defined_28t_fmt_29_Kt_fmt_defined_28t_fmt_ampm_29_Kt_fmt_ampm_defined_28am_str_29_Kam_str_defined_28pm_str_29_Kpm_str_defined_28day_1_29_Kday_1_defined_28day_2_29_Kday_2_defined_28day_3_29_Kday_3_defined_28day_4_29_Kday_4_defined_28day_5_29_Kday_5_defined_28day_6_29_Kday_6_defined_28day_7_29_Kday_7_defined_28abday_1_29_Kabday_1_defined_28abday_2_29_Kabday_2_defined_28abday_3_29_Kabday_3_defined_28abday_4_29_Kabday_4_defined_28abday_5_29_Kabday_5_defined_28abday_6_29_Kabday_6_defined_28abday_7_29_Kabday_7_defined_28mon_1_29_Kmon_1_defined_28mon_2_29_Kmon_2_defined_28mon_3_29_Kmon_3_defined_28mon_4_29_Kmon_4_defined_28mon_5_29_Kmon_5_defined_28mon_6_29_Kmon_6_defined_28mon_7_29_Kmon_7_defined_28mon_8_29_Kmon_8_defined_28mon_9_29_Kmon_9_defined_28mon_10_29_Kmon_10_defined_28mon_11_29_Kmon_11_defined_28mon_12_29_Kmon_12_defined_28abmon_1_29_Kabmon_1_defined_28abmon_2_29_Kabmon_2_defined_28abmon_3_29_Kabmon_3_defined_28abmon_4_29_Kabmon_4_defined_28abmon_5_29_Kabmon_5_defined_28abmon_6_29_Kabmon_6_defined_28abmon_7_29_Kabmon_7_defined_28abmon_8_29_Kabmon_8_defined_28abmon_9_29_Kabmon_9_defined_28abmon_10_29_Kabmon_10_defined_28abmon_11_29_Kabmon_11_defined_28abmon_12_29_Kabmon_12_defined_28era_29_Kera_defined_28era_d_fmt_29_Kera_d_fmt_defined_28era_d_t_fmt_29_Kera_d_t_fmt_defined_28era_t_fmt_29_Kera_t_fmt_defined_28alt_digits_29_Kalt_digits_defined_28radixchar_29_Kradixchar_defined_28thousep_29_Kthousep_defined_28yesexpr_29_Kyesexpr_defined_28noexpr_29_Knoexpr_defined_28yesstr_29_Kyesstr_defined_28nostr_29_Knostr_defined_28crncystr_29_Kcrncystr_defined_28d_md_order_29_Kd_md_order_defined_28locale_fontsignature_29_Klocale_fontsignature_defined_28locale_icalendartype_29_Klocale_icalendartype_defined_28locale_icentury_29_Klocale_icentury_defined_28locale_icountry_29_Klocale_icountry_defined_28locale_icurrdigits_29_Klocale_icurrdigits_defined_28locale_icu;
#endif
  gcv_object_t _object___28or_20null_20integer_20_28member_defined_28lc_all_29_Kall_defined_28lc_collate_29_Kcollate_defined_28lc_ctype_29_Kctype_defined_28lc_messages_29_Kmessages_defined_28lc_monetary_29_Kmonetary_defined_28lc_numeric_29_Knumeric_defined_28lc_time_29_Ktime_defined_28lc_paper_29_Kpaper_defined_28lc_name_29_Kname_defined_28lc_address_29_Kaddress_defined_28lc_telephone_29_Ktelephone_defined_28lc_measurement_29_Kmeasurement_defined_28lc_identification_29_Kidentification__29_29;
} module__i18n__object_tab;
uintC module__i18n__object_tab_size = sizeof(module__i18n__object_tab)/sizeof(gcv_object_t);

struct module__i18n__object_tab_initdata_t {
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  object_initdata_t _object_i18n__mk_locale_conv;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_1))
  object_initdata_t _object_Kabday_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_2))
  object_initdata_t _object_Kabday_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_3))
  object_initdata_t _object_Kabday_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_4))
  object_initdata_t _object_Kabday_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_5))
  object_initdata_t _object_Kabday_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_6))
  object_initdata_t _object_Kabday_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_7))
  object_initdata_t _object_Kabday_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_1))
  object_initdata_t _object_Kabmon_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_10))
  object_initdata_t _object_Kabmon_10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_11))
  object_initdata_t _object_Kabmon_11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_12))
  object_initdata_t _object_Kabmon_12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_2))
  object_initdata_t _object_Kabmon_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_3))
  object_initdata_t _object_Kabmon_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_4))
  object_initdata_t _object_Kabmon_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_5))
  object_initdata_t _object_Kabmon_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_6))
  object_initdata_t _object_Kabmon_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_7))
  object_initdata_t _object_Kabmon_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_8))
  object_initdata_t _object_Kabmon_8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_9))
  object_initdata_t _object_Kabmon_9;
#endif
#if defined(LC_ADDRESS)
  object_initdata_t _object_Kaddress;
#endif
#if defined(LC_ALL)
  object_initdata_t _object_Kall;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ALT_DIGITS))
  object_initdata_t _object_Kalt_digits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(AM_STR))
  object_initdata_t _object_Kam_str;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(CODESET))
  object_initdata_t _object_Kcodeset;
#endif
#if defined(LC_COLLATE)
  object_initdata_t _object_Kcollate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(CRNCYSTR))
  object_initdata_t _object_Kcrncystr;
#endif
#if defined(LC_CTYPE)
  object_initdata_t _object_Kctype;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_1))
  object_initdata_t _object_Kday_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_2))
  object_initdata_t _object_Kday_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_3))
  object_initdata_t _object_Kday_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_4))
  object_initdata_t _object_Kday_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_5))
  object_initdata_t _object_Kday_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_6))
  object_initdata_t _object_Kday_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_7))
  object_initdata_t _object_Kday_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_FMT))
  object_initdata_t _object_Kd_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_MD_ORDER))
  object_initdata_t _object_Kd_md_order;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_T_FMT))
  object_initdata_t _object_Kd_t_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA))
  object_initdata_t _object_Kera;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_D_FMT))
  object_initdata_t _object_Kera_d_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_D_T_FMT))
  object_initdata_t _object_Kera_d_t_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_T_FMT))
  object_initdata_t _object_Kera_t_fmt;
#endif
#if defined(LC_IDENTIFICATION)
  object_initdata_t _object_Kidentification;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_FONTSIGNATURE))
  object_initdata_t _object_Klocale_fontsignature;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICALENDARTYPE))
  object_initdata_t _object_Klocale_icalendartype;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICENTURY))
  object_initdata_t _object_Klocale_icentury;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICOUNTRY))
  object_initdata_t _object_Klocale_icountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICURRDIGITS))
  object_initdata_t _object_Klocale_icurrdigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICURRENCY))
  object_initdata_t _object_Klocale_icurrency;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDATE))
  object_initdata_t _object_Klocale_idate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDAYLZERO))
  object_initdata_t _object_Klocale_idaylzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTANSICODEPAGE))
  object_initdata_t _object_Klocale_idefaultansicodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTCODEPAGE))
  object_initdata_t _object_Klocale_idefaultcodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTCOUNTRY))
  object_initdata_t _object_Klocale_idefaultcountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTEBCDICCODEPAGE))
  object_initdata_t _object_Klocale_idefaultebcdiccodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTLANGUAGE))
  object_initdata_t _object_Klocale_idefaultlanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTMACCODEPAGE))
  object_initdata_t _object_Klocale_idefaultmaccodepage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDIGITS))
  object_initdata_t _object_Klocale_idigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDIGITSUBSTITUTION))
  object_initdata_t _object_Klocale_idigitsubstitution;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IFIRSTDAYOFWEEK))
  object_initdata_t _object_Klocale_ifirstdayofweek;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IFIRSTWEEKOFYEAR))
  object_initdata_t _object_Klocale_ifirstweekofyear;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IINTLCURRDIGITS))
  object_initdata_t _object_Klocale_iintlcurrdigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILANGUAGE))
  object_initdata_t _object_Klocale_ilanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILDATE))
  object_initdata_t _object_Klocale_ildate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILZERO))
  object_initdata_t _object_Klocale_ilzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IMEASURE))
  object_initdata_t _object_Klocale_imeasure;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IMONLZERO))
  object_initdata_t _object_Klocale_imonlzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGCURR))
  object_initdata_t _object_Klocale_inegcurr;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGNUMBER))
  object_initdata_t _object_Klocale_inegnumber;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSEPBYSPACE))
  object_initdata_t _object_Klocale_inegsepbyspace;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSIGNPOSN))
  object_initdata_t _object_Klocale_inegsignposn;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSYMPRECEDES))
  object_initdata_t _object_Klocale_inegsymprecedes;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IOPTIONALCALENDAR))
  object_initdata_t _object_Klocale_ioptionalcalendar;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPAPERSIZE))
  object_initdata_t _object_Klocale_ipapersize;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSEPBYSPACE))
  object_initdata_t _object_Klocale_ipossepbyspace;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSIGNPOSN))
  object_initdata_t _object_Klocale_ipossignposn;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSYMPRECEDES))
  object_initdata_t _object_Klocale_ipossymprecedes;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ITIMEMARKPOSN))
  object_initdata_t _object_Klocale_itimemarkposn;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ITLZERO))
  object_initdata_t _object_Klocale_itlzero;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_S1159))
  object_initdata_t _object_Klocale_s1159;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_S2359))
  object_initdata_t _object_Klocale_s2359;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVCTRYNAME))
  object_initdata_t _object_Klocale_sabbrevctryname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME1))
  object_initdata_t _object_Klocale_sabbrevdayname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME2))
  object_initdata_t _object_Klocale_sabbrevdayname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME3))
  object_initdata_t _object_Klocale_sabbrevdayname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME4))
  object_initdata_t _object_Klocale_sabbrevdayname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME5))
  object_initdata_t _object_Klocale_sabbrevdayname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME6))
  object_initdata_t _object_Klocale_sabbrevdayname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME7))
  object_initdata_t _object_Klocale_sabbrevdayname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVLANGNAME))
  object_initdata_t _object_Klocale_sabbrevlangname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME1))
  object_initdata_t _object_Klocale_sabbrevmonthname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME10))
  object_initdata_t _object_Klocale_sabbrevmonthname10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME11))
  object_initdata_t _object_Klocale_sabbrevmonthname11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME12))
  object_initdata_t _object_Klocale_sabbrevmonthname12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME13))
  object_initdata_t _object_Klocale_sabbrevmonthname13;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME2))
  object_initdata_t _object_Klocale_sabbrevmonthname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME3))
  object_initdata_t _object_Klocale_sabbrevmonthname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME4))
  object_initdata_t _object_Klocale_sabbrevmonthname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME5))
  object_initdata_t _object_Klocale_sabbrevmonthname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME6))
  object_initdata_t _object_Klocale_sabbrevmonthname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME7))
  object_initdata_t _object_Klocale_sabbrevmonthname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME8))
  object_initdata_t _object_Klocale_sabbrevmonthname8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME9))
  object_initdata_t _object_Klocale_sabbrevmonthname9;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SCOUNTRY))
  object_initdata_t _object_Klocale_scountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SCURRENCY))
  object_initdata_t _object_Klocale_scurrency;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDATE))
  object_initdata_t _object_Klocale_sdate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME1))
  object_initdata_t _object_Klocale_sdayname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME2))
  object_initdata_t _object_Klocale_sdayname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME3))
  object_initdata_t _object_Klocale_sdayname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME4))
  object_initdata_t _object_Klocale_sdayname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME5))
  object_initdata_t _object_Klocale_sdayname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME6))
  object_initdata_t _object_Klocale_sdayname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME7))
  object_initdata_t _object_Klocale_sdayname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDECIMAL))
  object_initdata_t _object_Klocale_sdecimal;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGCOUNTRY))
  object_initdata_t _object_Klocale_sengcountry;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGCURRNAME))
  object_initdata_t _object_Klocale_sengcurrname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGLANGUAGE))
  object_initdata_t _object_Klocale_senglanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SGROUPING))
  object_initdata_t _object_Klocale_sgrouping;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SINTLSYMBOL))
  object_initdata_t _object_Klocale_sintlsymbol;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SISO3166CTRYNAME))
  object_initdata_t _object_Klocale_siso3166ctryname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SISO639LANGNAME))
  object_initdata_t _object_Klocale_siso639langname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLANGUAGE))
  object_initdata_t _object_Klocale_slanguage;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLIST))
  object_initdata_t _object_Klocale_slist;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLONGDATE))
  object_initdata_t _object_Klocale_slongdate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONDECIMALSEP))
  object_initdata_t _object_Klocale_smondecimalsep;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONGROUPING))
  object_initdata_t _object_Klocale_smongrouping;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME1))
  object_initdata_t _object_Klocale_smonthname1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME10))
  object_initdata_t _object_Klocale_smonthname10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME11))
  object_initdata_t _object_Klocale_smonthname11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME12))
  object_initdata_t _object_Klocale_smonthname12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME13))
  object_initdata_t _object_Klocale_smonthname13;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME2))
  object_initdata_t _object_Klocale_smonthname2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME3))
  object_initdata_t _object_Klocale_smonthname3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME4))
  object_initdata_t _object_Klocale_smonthname4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME5))
  object_initdata_t _object_Klocale_smonthname5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME6))
  object_initdata_t _object_Klocale_smonthname6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME7))
  object_initdata_t _object_Klocale_smonthname7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME8))
  object_initdata_t _object_Klocale_smonthname8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME9))
  object_initdata_t _object_Klocale_smonthname9;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHOUSANDSEP))
  object_initdata_t _object_Klocale_smonthousandsep;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVECTRYNAME))
  object_initdata_t _object_Klocale_snativectryname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVECURRNAME))
  object_initdata_t _object_Klocale_snativecurrname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVEDIGITS))
  object_initdata_t _object_Klocale_snativedigits;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVELANGNAME))
  object_initdata_t _object_Klocale_snativelangname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNEGATIVESIGN))
  object_initdata_t _object_Klocale_snegativesign;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SPOSITIVESIGN))
  object_initdata_t _object_Klocale_spositivesign;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SSHORTDATE))
  object_initdata_t _object_Klocale_sshortdate;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SSORTNAME))
  object_initdata_t _object_Klocale_ssortname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STHOUSAND))
  object_initdata_t _object_Klocale_sthousand;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STIME))
  object_initdata_t _object_Klocale_stime;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STIMEFORMAT))
  object_initdata_t _object_Klocale_stimeformat;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SYEARMONTH))
  object_initdata_t _object_Klocale_syearmonth;
#endif
#if defined(LC_MEASUREMENT)
  object_initdata_t _object_Kmeasurement;
#endif
#if defined(LC_MESSAGES)
  object_initdata_t _object_Kmessages;
#endif
#if defined(LC_MONETARY)
  object_initdata_t _object_Kmonetary;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_1))
  object_initdata_t _object_Kmon_1;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_10))
  object_initdata_t _object_Kmon_10;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_11))
  object_initdata_t _object_Kmon_11;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_12))
  object_initdata_t _object_Kmon_12;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_2))
  object_initdata_t _object_Kmon_2;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_3))
  object_initdata_t _object_Kmon_3;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_4))
  object_initdata_t _object_Kmon_4;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_5))
  object_initdata_t _object_Kmon_5;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_6))
  object_initdata_t _object_Kmon_6;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_7))
  object_initdata_t _object_Kmon_7;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_8))
  object_initdata_t _object_Kmon_8;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_9))
  object_initdata_t _object_Kmon_9;
#endif
#if defined(LC_NAME)
  object_initdata_t _object_Kname;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(NOEXPR))
  object_initdata_t _object_Knoexpr;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(NOSTR))
  object_initdata_t _object_Knostr;
#endif
#if defined(LC_NUMERIC)
  object_initdata_t _object_Knumeric;
#endif
#if defined(LC_PAPER)
  object_initdata_t _object_Kpaper;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(PM_STR))
  object_initdata_t _object_Kpm_str;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(RADIXCHAR))
  object_initdata_t _object_Kradixchar;
#endif
#if defined(LC_TELEPHONE)
  object_initdata_t _object_Ktelephone;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(THOUSEP))
  object_initdata_t _object_Kthousep;
#endif
#if defined(LC_TIME)
  object_initdata_t _object_Ktime;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(T_FMT))
  object_initdata_t _object_Kt_fmt;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(T_FMT_AMPM))
  object_initdata_t _object_Kt_fmt_ampm;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(YESEXPR))
  object_initdata_t _object_Kyesexpr;
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(YESSTR))
  object_initdata_t _object_Kyesstr;
#endif
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  object_initdata_t _object___28or_20integer_20_28member_defined_28codeset_29_Kcodeset_defined_28d_t_fmt_29_Kd_t_fmt_defined_28d_fmt_29_Kd_fmt_defined_28t_fmt_29_Kt_fmt_defined_28t_fmt_ampm_29_Kt_fmt_ampm_defined_28am_str_29_Kam_str_defined_28pm_str_29_Kpm_str_defined_28day_1_29_Kday_1_defined_28day_2_29_Kday_2_defined_28day_3_29_Kday_3_defined_28day_4_29_Kday_4_defined_28day_5_29_Kday_5_defined_28day_6_29_Kday_6_defined_28day_7_29_Kday_7_defined_28abday_1_29_Kabday_1_defined_28abday_2_29_Kabday_2_defined_28abday_3_29_Kabday_3_defined_28abday_4_29_Kabday_4_defined_28abday_5_29_Kabday_5_defined_28abday_6_29_Kabday_6_defined_28abday_7_29_Kabday_7_defined_28mon_1_29_Kmon_1_defined_28mon_2_29_Kmon_2_defined_28mon_3_29_Kmon_3_defined_28mon_4_29_Kmon_4_defined_28mon_5_29_Kmon_5_defined_28mon_6_29_Kmon_6_defined_28mon_7_29_Kmon_7_defined_28mon_8_29_Kmon_8_defined_28mon_9_29_Kmon_9_defined_28mon_10_29_Kmon_10_defined_28mon_11_29_Kmon_11_defined_28mon_12_29_Kmon_12_defined_28abmon_1_29_Kabmon_1_defined_28abmon_2_29_Kabmon_2_defined_28abmon_3_29_Kabmon_3_defined_28abmon_4_29_Kabmon_4_defined_28abmon_5_29_Kabmon_5_defined_28abmon_6_29_Kabmon_6_defined_28abmon_7_29_Kabmon_7_defined_28abmon_8_29_Kabmon_8_defined_28abmon_9_29_Kabmon_9_defined_28abmon_10_29_Kabmon_10_defined_28abmon_11_29_Kabmon_11_defined_28abmon_12_29_Kabmon_12_defined_28era_29_Kera_defined_28era_d_fmt_29_Kera_d_fmt_defined_28era_d_t_fmt_29_Kera_d_t_fmt_defined_28era_t_fmt_29_Kera_t_fmt_defined_28alt_digits_29_Kalt_digits_defined_28radixchar_29_Kradixchar_defined_28thousep_29_Kthousep_defined_28yesexpr_29_Kyesexpr_defined_28noexpr_29_Knoexpr_defined_28yesstr_29_Kyesstr_defined_28nostr_29_Knostr_defined_28crncystr_29_Kcrncystr_defined_28d_md_order_29_Kd_md_order_defined_28locale_fontsignature_29_Klocale_fontsignature_defined_28locale_icalendartype_29_Klocale_icalendartype_defined_28locale_icentury_29_Klocale_icentury_defined_28locale_icountry_29_Klocale_icountry_defined_28locale_icurrdigits_29_Klocale_icurrdigits_defined_28locale_icu;
#endif
  object_initdata_t _object___28or_20null_20integer_20_28member_defined_28lc_all_29_Kall_defined_28lc_collate_29_Kcollate_defined_28lc_ctype_29_Kctype_defined_28lc_messages_29_Kmessages_defined_28lc_monetary_29_Kmonetary_defined_28lc_numeric_29_Knumeric_defined_28lc_time_29_Ktime_defined_28lc_paper_29_Kpaper_defined_28lc_name_29_Kname_defined_28lc_address_29_Kaddress_defined_28lc_telephone_29_Ktelephone_defined_28lc_measurement_29_Kmeasurement_defined_28lc_identification_29_Kidentification__29_29;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__i18n__object_tab_initdata = {
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  { "I18N::MK-LOCALE-CONV" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_1))
  { ":ABDAY_1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_2))
  { ":ABDAY_2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_3))
  { ":ABDAY_3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_4))
  { ":ABDAY_4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_5))
  { ":ABDAY_5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_6))
  { ":ABDAY_6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABDAY_7))
  { ":ABDAY_7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_1))
  { ":ABMON_1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_10))
  { ":ABMON_10" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_11))
  { ":ABMON_11" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_12))
  { ":ABMON_12" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_2))
  { ":ABMON_2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_3))
  { ":ABMON_3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_4))
  { ":ABMON_4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_5))
  { ":ABMON_5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_6))
  { ":ABMON_6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_7))
  { ":ABMON_7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_8))
  { ":ABMON_8" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ABMON_9))
  { ":ABMON_9" },
#endif
#if defined(LC_ADDRESS)
  { ":ADDRESS" },
#endif
#if defined(LC_ALL)
  { ":ALL" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ALT_DIGITS))
  { ":ALT_DIGITS" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(AM_STR))
  { ":AM_STR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(CODESET))
  { ":CODESET" },
#endif
#if defined(LC_COLLATE)
  { ":COLLATE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(CRNCYSTR))
  { ":CRNCYSTR" },
#endif
#if defined(LC_CTYPE)
  { ":CTYPE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_1))
  { ":DAY_1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_2))
  { ":DAY_2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_3))
  { ":DAY_3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_4))
  { ":DAY_4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_5))
  { ":DAY_5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_6))
  { ":DAY_6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(DAY_7))
  { ":DAY_7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_FMT))
  { ":D_FMT" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_MD_ORDER))
  { ":D_MD_ORDER" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(D_T_FMT))
  { ":D_T_FMT" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA))
  { ":ERA" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_D_FMT))
  { ":ERA_D_FMT" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_D_T_FMT))
  { ":ERA_D_T_FMT" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(ERA_T_FMT))
  { ":ERA_T_FMT" },
#endif
#if defined(LC_IDENTIFICATION)
  { ":IDENTIFICATION" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_FONTSIGNATURE))
  { ":LOCALE_FONTSIGNATURE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICALENDARTYPE))
  { ":LOCALE_ICALENDARTYPE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICENTURY))
  { ":LOCALE_ICENTURY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICOUNTRY))
  { ":LOCALE_ICOUNTRY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICURRDIGITS))
  { ":LOCALE_ICURRDIGITS" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ICURRENCY))
  { ":LOCALE_ICURRENCY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDATE))
  { ":LOCALE_IDATE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDAYLZERO))
  { ":LOCALE_IDAYLZERO" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTANSICODEPAGE))
  { ":LOCALE_IDEFAULTANSICODEPAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTCODEPAGE))
  { ":LOCALE_IDEFAULTCODEPAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTCOUNTRY))
  { ":LOCALE_IDEFAULTCOUNTRY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTEBCDICCODEPAGE))
  { ":LOCALE_IDEFAULTEBCDICCODEPAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTLANGUAGE))
  { ":LOCALE_IDEFAULTLANGUAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDEFAULTMACCODEPAGE))
  { ":LOCALE_IDEFAULTMACCODEPAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDIGITS))
  { ":LOCALE_IDIGITS" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IDIGITSUBSTITUTION))
  { ":LOCALE_IDIGITSUBSTITUTION" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IFIRSTDAYOFWEEK))
  { ":LOCALE_IFIRSTDAYOFWEEK" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IFIRSTWEEKOFYEAR))
  { ":LOCALE_IFIRSTWEEKOFYEAR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IINTLCURRDIGITS))
  { ":LOCALE_IINTLCURRDIGITS" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILANGUAGE))
  { ":LOCALE_ILANGUAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILDATE))
  { ":LOCALE_ILDATE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ILZERO))
  { ":LOCALE_ILZERO" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IMEASURE))
  { ":LOCALE_IMEASURE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IMONLZERO))
  { ":LOCALE_IMONLZERO" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGCURR))
  { ":LOCALE_INEGCURR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGNUMBER))
  { ":LOCALE_INEGNUMBER" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSEPBYSPACE))
  { ":LOCALE_INEGSEPBYSPACE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSIGNPOSN))
  { ":LOCALE_INEGSIGNPOSN" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_INEGSYMPRECEDES))
  { ":LOCALE_INEGSYMPRECEDES" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IOPTIONALCALENDAR))
  { ":LOCALE_IOPTIONALCALENDAR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPAPERSIZE))
  { ":LOCALE_IPAPERSIZE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSEPBYSPACE))
  { ":LOCALE_IPOSSEPBYSPACE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSIGNPOSN))
  { ":LOCALE_IPOSSIGNPOSN" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_IPOSSYMPRECEDES))
  { ":LOCALE_IPOSSYMPRECEDES" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ITIMEMARKPOSN))
  { ":LOCALE_ITIMEMARKPOSN" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_ITLZERO))
  { ":LOCALE_ITLZERO" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_S1159))
  { ":LOCALE_S1159" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_S2359))
  { ":LOCALE_S2359" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVCTRYNAME))
  { ":LOCALE_SABBREVCTRYNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME1))
  { ":LOCALE_SABBREVDAYNAME1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME2))
  { ":LOCALE_SABBREVDAYNAME2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME3))
  { ":LOCALE_SABBREVDAYNAME3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME4))
  { ":LOCALE_SABBREVDAYNAME4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME5))
  { ":LOCALE_SABBREVDAYNAME5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME6))
  { ":LOCALE_SABBREVDAYNAME6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVDAYNAME7))
  { ":LOCALE_SABBREVDAYNAME7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVLANGNAME))
  { ":LOCALE_SABBREVLANGNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME1))
  { ":LOCALE_SABBREVMONTHNAME1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME10))
  { ":LOCALE_SABBREVMONTHNAME10" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME11))
  { ":LOCALE_SABBREVMONTHNAME11" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME12))
  { ":LOCALE_SABBREVMONTHNAME12" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME13))
  { ":LOCALE_SABBREVMONTHNAME13" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME2))
  { ":LOCALE_SABBREVMONTHNAME2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME3))
  { ":LOCALE_SABBREVMONTHNAME3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME4))
  { ":LOCALE_SABBREVMONTHNAME4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME5))
  { ":LOCALE_SABBREVMONTHNAME5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME6))
  { ":LOCALE_SABBREVMONTHNAME6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME7))
  { ":LOCALE_SABBREVMONTHNAME7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME8))
  { ":LOCALE_SABBREVMONTHNAME8" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SABBREVMONTHNAME9))
  { ":LOCALE_SABBREVMONTHNAME9" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SCOUNTRY))
  { ":LOCALE_SCOUNTRY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SCURRENCY))
  { ":LOCALE_SCURRENCY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDATE))
  { ":LOCALE_SDATE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME1))
  { ":LOCALE_SDAYNAME1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME2))
  { ":LOCALE_SDAYNAME2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME3))
  { ":LOCALE_SDAYNAME3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME4))
  { ":LOCALE_SDAYNAME4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME5))
  { ":LOCALE_SDAYNAME5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME6))
  { ":LOCALE_SDAYNAME6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDAYNAME7))
  { ":LOCALE_SDAYNAME7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SDECIMAL))
  { ":LOCALE_SDECIMAL" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGCOUNTRY))
  { ":LOCALE_SENGCOUNTRY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGCURRNAME))
  { ":LOCALE_SENGCURRNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SENGLANGUAGE))
  { ":LOCALE_SENGLANGUAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SGROUPING))
  { ":LOCALE_SGROUPING" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SINTLSYMBOL))
  { ":LOCALE_SINTLSYMBOL" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SISO3166CTRYNAME))
  { ":LOCALE_SISO3166CTRYNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SISO639LANGNAME))
  { ":LOCALE_SISO639LANGNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLANGUAGE))
  { ":LOCALE_SLANGUAGE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLIST))
  { ":LOCALE_SLIST" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SLONGDATE))
  { ":LOCALE_SLONGDATE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONDECIMALSEP))
  { ":LOCALE_SMONDECIMALSEP" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONGROUPING))
  { ":LOCALE_SMONGROUPING" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME1))
  { ":LOCALE_SMONTHNAME1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME10))
  { ":LOCALE_SMONTHNAME10" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME11))
  { ":LOCALE_SMONTHNAME11" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME12))
  { ":LOCALE_SMONTHNAME12" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME13))
  { ":LOCALE_SMONTHNAME13" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME2))
  { ":LOCALE_SMONTHNAME2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME3))
  { ":LOCALE_SMONTHNAME3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME4))
  { ":LOCALE_SMONTHNAME4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME5))
  { ":LOCALE_SMONTHNAME5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME6))
  { ":LOCALE_SMONTHNAME6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME7))
  { ":LOCALE_SMONTHNAME7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME8))
  { ":LOCALE_SMONTHNAME8" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHNAME9))
  { ":LOCALE_SMONTHNAME9" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SMONTHOUSANDSEP))
  { ":LOCALE_SMONTHOUSANDSEP" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVECTRYNAME))
  { ":LOCALE_SNATIVECTRYNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVECURRNAME))
  { ":LOCALE_SNATIVECURRNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVEDIGITS))
  { ":LOCALE_SNATIVEDIGITS" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNATIVELANGNAME))
  { ":LOCALE_SNATIVELANGNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SNEGATIVESIGN))
  { ":LOCALE_SNEGATIVESIGN" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SPOSITIVESIGN))
  { ":LOCALE_SPOSITIVESIGN" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SSHORTDATE))
  { ":LOCALE_SSHORTDATE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SSORTNAME))
  { ":LOCALE_SSORTNAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STHOUSAND))
  { ":LOCALE_STHOUSAND" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STIME))
  { ":LOCALE_STIME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_STIMEFORMAT))
  { ":LOCALE_STIMEFORMAT" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(LOCALE_SYEARMONTH))
  { ":LOCALE_SYEARMONTH" },
#endif
#if defined(LC_MEASUREMENT)
  { ":MEASUREMENT" },
#endif
#if defined(LC_MESSAGES)
  { ":MESSAGES" },
#endif
#if defined(LC_MONETARY)
  { ":MONETARY" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_1))
  { ":MON_1" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_10))
  { ":MON_10" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_11))
  { ":MON_11" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_12))
  { ":MON_12" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_2))
  { ":MON_2" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_3))
  { ":MON_3" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_4))
  { ":MON_4" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_5))
  { ":MON_5" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_6))
  { ":MON_6" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_7))
  { ":MON_7" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_8))
  { ":MON_8" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(MON_9))
  { ":MON_9" },
#endif
#if defined(LC_NAME)
  { ":NAME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(NOEXPR))
  { ":NOEXPR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(NOSTR))
  { ":NOSTR" },
#endif
#if defined(LC_NUMERIC)
  { ":NUMERIC" },
#endif
#if defined(LC_PAPER)
  { ":PAPER" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(PM_STR))
  { ":PM_STR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(RADIXCHAR))
  { ":RADIXCHAR" },
#endif
#if defined(LC_TELEPHONE)
  { ":TELEPHONE" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(THOUSEP))
  { ":THOUSEP" },
#endif
#if defined(LC_TIME)
  { ":TIME" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(T_FMT))
  { ":T_FMT" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(T_FMT_AMPM))
  { ":T_FMT_AMPM" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(YESEXPR))
  { ":YESEXPR" },
#endif
#if (defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)) && (defined(YESSTR))
  { ":YESSTR" },
#endif
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(CODESET)
    " " ":CODESET"
#  endif
#  if defined(D_T_FMT)
    " " ":D_T_FMT"
#  endif
#  if defined(D_FMT)
    " " ":D_FMT"
#  endif
#  if defined(T_FMT)
    " " ":T_FMT"
#  endif
#  if defined(T_FMT_AMPM)
    " " ":T_FMT_AMPM"
#  endif
#  if defined(AM_STR)
    " " ":AM_STR"
#  endif
#  if defined(PM_STR)
    " " ":PM_STR"
#  endif
#  if defined(DAY_1)
    " " ":DAY_1"
#  endif
#  if defined(DAY_2)
    " " ":DAY_2"
#  endif
#  if defined(DAY_3)
    " " ":DAY_3"
#  endif
#  if defined(DAY_4)
    " " ":DAY_4"
#  endif
#  if defined(DAY_5)
    " " ":DAY_5"
#  endif
#  if defined(DAY_6)
    " " ":DAY_6"
#  endif
#  if defined(DAY_7)
    " " ":DAY_7"
#  endif
#  if defined(ABDAY_1)
    " " ":ABDAY_1"
#  endif
#  if defined(ABDAY_2)
    " " ":ABDAY_2"
#  endif
#  if defined(ABDAY_3)
    " " ":ABDAY_3"
#  endif
#  if defined(ABDAY_4)
    " " ":ABDAY_4"
#  endif
#  if defined(ABDAY_5)
    " " ":ABDAY_5"
#  endif
#  if defined(ABDAY_6)
    " " ":ABDAY_6"
#  endif
#  if defined(ABDAY_7)
    " " ":ABDAY_7"
#  endif
#  if defined(MON_1)
    " " ":MON_1"
#  endif
#  if defined(MON_2)
    " " ":MON_2"
#  endif
#  if defined(MON_3)
    " " ":MON_3"
#  endif
#  if defined(MON_4)
    " " ":MON_4"
#  endif
#  if defined(MON_5)
    " " ":MON_5"
#  endif
#  if defined(MON_6)
    " " ":MON_6"
#  endif
#  if defined(MON_7)
    " " ":MON_7"
#  endif
#  if defined(MON_8)
    " " ":MON_8"
#  endif
#  if defined(MON_9)
    " " ":MON_9"
#  endif
#  if defined(MON_10)
    " " ":MON_10"
#  endif
#  if defined(MON_11)
    " " ":MON_11"
#  endif
#  if defined(MON_12)
    " " ":MON_12"
#  endif
#  if defined(ABMON_1)
    " " ":ABMON_1"
#  endif
#  if defined(ABMON_2)
    " " ":ABMON_2"
#  endif
#  if defined(ABMON_3)
    " " ":ABMON_3"
#  endif
#  if defined(ABMON_4)
    " " ":ABMON_4"
#  endif
#  if defined(ABMON_5)
    " " ":ABMON_5"
#  endif
#  if defined(ABMON_6)
    " " ":ABMON_6"
#  endif
#  if defined(ABMON_7)
    " " ":ABMON_7"
#  endif
#  if defined(ABMON_8)
    " " ":ABMON_8"
#  endif
#  if defined(ABMON_9)
    " " ":ABMON_9"
#  endif
#  if defined(ABMON_10)
    " " ":ABMON_10"
#  endif
#  if defined(ABMON_11)
    " " ":ABMON_11"
#  endif
#  if defined(ABMON_12)
    " " ":ABMON_12"
#  endif
#  if defined(ERA)
    " " ":ERA"
#  endif
#  if defined(ERA_D_FMT)
    " " ":ERA_D_FMT"
#  endif
#  if defined(ERA_D_T_FMT)
    " " ":ERA_D_T_FMT"
#  endif
#  if defined(ERA_T_FMT)
    " " ":ERA_T_FMT"
#  endif
#  if defined(ALT_DIGITS)
    " " ":ALT_DIGITS"
#  endif
#  if defined(RADIXCHAR)
    " " ":RADIXCHAR"
#  endif
#  if defined(THOUSEP)
    " " ":THOUSEP"
#  endif
#  if defined(YESEXPR)
    " " ":YESEXPR"
#  endif
#  if defined(NOEXPR)
    " " ":NOEXPR"
#  endif
#  if defined(YESSTR)
    " " ":YESSTR"
#  endif
#  if defined(NOSTR)
    " " ":NOSTR"
#  endif
#  if defined(CRNCYSTR)
    " " ":CRNCYSTR"
#  endif
#  if defined(D_MD_ORDER)
    " " ":D_MD_ORDER"
#  endif
#  if defined(LOCALE_FONTSIGNATURE)
    " " ":LOCALE_FONTSIGNATURE"
#  endif
#  if defined(LOCALE_ICALENDARTYPE)
    " " ":LOCALE_ICALENDARTYPE"
#  endif
#  if defined(LOCALE_ICENTURY)
    " " ":LOCALE_ICENTURY"
#  endif
#  if defined(LOCALE_ICOUNTRY)
    " " ":LOCALE_ICOUNTRY"
#  endif
#  if defined(LOCALE_ICURRDIGITS)
    " " ":LOCALE_ICURRDIGITS"
#  endif
#  if defined(LOCALE_ICURRENCY)
    " " ":LOCALE_ICURRENCY"
#  endif
#  if defined(LOCALE_IDATE)
    " " ":LOCALE_IDATE"
#  endif
#  if defined(LOCALE_IDAYLZERO)
    " " ":LOCALE_IDAYLZERO"
#  endif
#  if defined(LOCALE_IDEFAULTANSICODEPAGE)
    " " ":LOCALE_IDEFAULTANSICODEPAGE"
#  endif
#  if defined(LOCALE_IDEFAULTCODEPAGE)
    " " ":LOCALE_IDEFAULTCODEPAGE"
#  endif
#  if defined(LOCALE_IDEFAULTCOUNTRY)
    " " ":LOCALE_IDEFAULTCOUNTRY"
#  endif
#  if defined(LOCALE_IDEFAULTEBCDICCODEPAGE)
    " " ":LOCALE_IDEFAULTEBCDICCODEPAGE"
#  endif
#  if defined(LOCALE_IDEFAULTLANGUAGE)
    " " ":LOCALE_IDEFAULTLANGUAGE"
#  endif
#  if defined(LOCALE_IDEFAULTMACCODEPAGE)
    " " ":LOCALE_IDEFAULTMACCODEPAGE"
#  endif
#  if defined(LOCALE_IDIGITS)
    " " ":LOCALE_IDIGITS"
#  endif
#  if defined(LOCALE_IDIGITSUBSTITUTION)
    " " ":LOCALE_IDIGITSUBSTITUTION"
#  endif
#  if defined(LOCALE_IFIRSTDAYOFWEEK)
    " " ":LOCALE_IFIRSTDAYOFWEEK"
#  endif
#  if defined(LOCALE_IFIRSTWEEKOFYEAR)
    " " ":LOCALE_IFIRSTWEEKOFYEAR"
#  endif
#  if defined(LOCALE_IINTLCURRDIGITS)
    " " ":LOCALE_IINTLCURRDIGITS"
#  endif
#  if defined(LOCALE_ILANGUAGE)
    " " ":LOCALE_ILANGUAGE"
#  endif
#  if defined(LOCALE_ILDATE)
    " " ":LOCALE_ILDATE"
#  endif
#  if defined(LOCALE_ILZERO)
    " " ":LOCALE_ILZERO"
#  endif
#  if defined(LOCALE_IMEASURE)
    " " ":LOCALE_IMEASURE"
#  endif
#  if defined(LOCALE_IMONLZERO)
    " " ":LOCALE_IMONLZERO"
#  endif
#  if defined(LOCALE_INEGCURR)
    " " ":LOCALE_INEGCURR"
#  endif
#  if defined(LOCALE_INEGNUMBER)
    " " ":LOCALE_INEGNUMBER"
#  endif
#  if defined(LOCALE_INEGSEPBYSPACE)
    " " ":LOCALE_INEGSEPBYSPACE"
#  endif
#  if defined(LOCALE_INEGSIGNPOSN)
    " " ":LOCALE_INEGSIGNPOSN"
#  endif
#  if defined(LOCALE_INEGSYMPRECEDES)
    " " ":LOCALE_INEGSYMPRECEDES"
#  endif
#  if defined(LOCALE_IOPTIONALCALENDAR)
    " " ":LOCALE_IOPTIONALCALENDAR"
#  endif
#  if defined(LOCALE_IPAPERSIZE)
    " " ":LOCALE_IPAPERSIZE"
#  endif
#  if defined(LOCALE_IPOSSEPBYSPACE)
    " " ":LOCALE_IPOSSEPBYSPACE"
#  endif
#  if defined(LOCALE_IPOSSIGNPOSN)
    " " ":LOCALE_IPOSSIGNPOSN"
#  endif
#  if defined(LOCALE_IPOSSYMPRECEDES)
    " " ":LOCALE_IPOSSYMPRECEDES"
#  endif
#  if defined(LOCALE_ITIMEMARKPOSN)
    " " ":LOCALE_ITIMEMARKPOSN"
#  endif
#  if defined(LOCALE_ITLZERO)
    " " ":LOCALE_ITLZERO"
#  endif
#  if defined(LOCALE_S1159)
    " " ":LOCALE_S1159"
#  endif
#  if defined(LOCALE_S2359)
    " " ":LOCALE_S2359"
#  endif
#  if defined(LOCALE_SABBREVCTRYNAME)
    " " ":LOCALE_SABBREVCTRYNAME"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME1)
    " " ":LOCALE_SABBREVDAYNAME1"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME2)
    " " ":LOCALE_SABBREVDAYNAME2"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME3)
    " " ":LOCALE_SABBREVDAYNAME3"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME4)
    " " ":LOCALE_SABBREVDAYNAME4"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME5)
    " " ":LOCALE_SABBREVDAYNAME5"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME6)
    " " ":LOCALE_SABBREVDAYNAME6"
#  endif
#  if defined(LOCALE_SABBREVDAYNAME7)
    " " ":LOCALE_SABBREVDAYNAME7"
#  endif
#  if defined(LOCALE_SABBREVLANGNAME)
    " " ":LOCALE_SABBREVLANGNAME"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME1)
    " " ":LOCALE_SABBREVMONTHNAME1"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME2)
    " " ":LOCALE_SABBREVMONTHNAME2"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME3)
    " " ":LOCALE_SABBREVMONTHNAME3"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME4)
    " " ":LOCALE_SABBREVMONTHNAME4"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME5)
    " " ":LOCALE_SABBREVMONTHNAME5"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME6)
    " " ":LOCALE_SABBREVMONTHNAME6"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME7)
    " " ":LOCALE_SABBREVMONTHNAME7"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME8)
    " " ":LOCALE_SABBREVMONTHNAME8"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME9)
    " " ":LOCALE_SABBREVMONTHNAME9"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME10)
    " " ":LOCALE_SABBREVMONTHNAME10"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME11)
    " " ":LOCALE_SABBREVMONTHNAME11"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME12)
    " " ":LOCALE_SABBREVMONTHNAME12"
#  endif
#  if defined(LOCALE_SABBREVMONTHNAME13)
    " " ":LOCALE_SABBREVMONTHNAME13"
#  endif
#  if defined(LOCALE_SCOUNTRY)
    " " ":LOCALE_SCOUNTRY"
#  endif
#  if defined(LOCALE_SCURRENCY)
    " " ":LOCALE_SCURRENCY"
#  endif
#  if defined(LOCALE_SDATE)
    " " ":LOCALE_SDATE"
#  endif
#  if defined(LOCALE_SDAYNAME1)
    " " ":LOCALE_SDAYNAME1"
#  endif
#  if defined(LOCALE_SDAYNAME2)
    " " ":LOCALE_SDAYNAME2"
#  endif
#  if defined(LOCALE_SDAYNAME3)
    " " ":LOCALE_SDAYNAME3"
#  endif
#  if defined(LOCALE_SDAYNAME4)
    " " ":LOCALE_SDAYNAME4"
#  endif
#  if defined(LOCALE_SDAYNAME5)
    " " ":LOCALE_SDAYNAME5"
#  endif
#  if defined(LOCALE_SDAYNAME6)
    " " ":LOCALE_SDAYNAME6"
#  endif
#  if defined(LOCALE_SDAYNAME7)
    " " ":LOCALE_SDAYNAME7"
#  endif
#  if defined(LOCALE_SDECIMAL)
    " " ":LOCALE_SDECIMAL"
#  endif
#  if defined(LOCALE_SENGCOUNTRY)
    " " ":LOCALE_SENGCOUNTRY"
#  endif
#  if defined(LOCALE_SENGCURRNAME)
    " " ":LOCALE_SENGCURRNAME"
#  endif
#  if defined(LOCALE_SENGLANGUAGE)
    " " ":LOCALE_SENGLANGUAGE"
#  endif
#  if defined(LOCALE_SGROUPING)
    " " ":LOCALE_SGROUPING"
#  endif
#  if defined(LOCALE_SINTLSYMBOL)
    " " ":LOCALE_SINTLSYMBOL"
#  endif
#  if defined(LOCALE_SISO3166CTRYNAME)
    " " ":LOCALE_SISO3166CTRYNAME"
#  endif
#  if defined(LOCALE_SISO639LANGNAME)
    " " ":LOCALE_SISO639LANGNAME"
#  endif
#  if defined(LOCALE_SLANGUAGE)
    " " ":LOCALE_SLANGUAGE"
#  endif
#  if defined(LOCALE_SLIST)
    " " ":LOCALE_SLIST"
#  endif
#  if defined(LOCALE_SLONGDATE)
    " " ":LOCALE_SLONGDATE"
#  endif
#  if defined(LOCALE_SMONDECIMALSEP)
    " " ":LOCALE_SMONDECIMALSEP"
#  endif
#  if defined(LOCALE_SMONGROUPING)
    " " ":LOCALE_SMONGROUPING"
#  endif
#  if defined(LOCALE_SMONTHNAME1)
    " " ":LOCALE_SMONTHNAME1"
#  endif
#  if defined(LOCALE_SMONTHNAME2)
    " " ":LOCALE_SMONTHNAME2"
#  endif
#  if defined(LOCALE_SMONTHNAME3)
    " " ":LOCALE_SMONTHNAME3"
#  endif
#  if defined(LOCALE_SMONTHNAME4)
    " " ":LOCALE_SMONTHNAME4"
#  endif
#  if defined(LOCALE_SMONTHNAME5)
    " " ":LOCALE_SMONTHNAME5"
#  endif
#  if defined(LOCALE_SMONTHNAME6)
    " " ":LOCALE_SMONTHNAME6"
#  endif
#  if defined(LOCALE_SMONTHNAME7)
    " " ":LOCALE_SMONTHNAME7"
#  endif
#  if defined(LOCALE_SMONTHNAME8)
    " " ":LOCALE_SMONTHNAME8"
#  endif
#  if defined(LOCALE_SMONTHNAME9)
    " " ":LOCALE_SMONTHNAME9"
#  endif
#  if defined(LOCALE_SMONTHNAME10)
    " " ":LOCALE_SMONTHNAME10"
#  endif
#  if defined(LOCALE_SMONTHNAME11)
    " " ":LOCALE_SMONTHNAME11"
#  endif
#  if defined(LOCALE_SMONTHNAME12)
    " " ":LOCALE_SMONTHNAME12"
#  endif
#  if defined(LOCALE_SMONTHNAME13)
    " " ":LOCALE_SMONTHNAME13"
#  endif
#  if defined(LOCALE_SMONTHOUSANDSEP)
    " " ":LOCALE_SMONTHOUSANDSEP"
#  endif
#  if defined(LOCALE_SNATIVECTRYNAME)
    " " ":LOCALE_SNATIVECTRYNAME"
#  endif
#  if defined(LOCALE_SNATIVECURRNAME)
    " " ":LOCALE_SNATIVECURRNAME"
#  endif
#  if defined(LOCALE_SNATIVEDIGITS)
    " " ":LOCALE_SNATIVEDIGITS"
#  endif
#  if defined(LOCALE_SNATIVELANGNAME)
    " " ":LOCALE_SNATIVELANGNAME"
#  endif
#  if defined(LOCALE_SNEGATIVESIGN)
    " " ":LOCALE_SNEGATIVESIGN"
#  endif
#  if defined(LOCALE_SPOSITIVESIGN)
    " " ":LOCALE_SPOSITIVESIGN"
#  endif
#  if defined(LOCALE_SSHORTDATE)
    " " ":LOCALE_SSHORTDATE"
#  endif
#  if defined(LOCALE_SSORTNAME)
    " " ":LOCALE_SSORTNAME"
#  endif
#  if defined(LOCALE_STHOUSAND)
    " " ":LOCALE_STHOUSAND"
#  endif
#  if defined(LOCALE_STIME)
    " " ":LOCALE_STIME"
#  endif
#  if defined(LOCALE_STIMEFORMAT)
    " " ":LOCALE_STIMEFORMAT"
#  endif
#  if defined(LOCALE_SYEARMONTH)
    " " ":LOCALE_SYEARMONTH"
#  endif
    " " "))"
  },
#endif
  {
    " " "(OR NULL INTEGER (MEMBER"
#  if defined(LC_ALL)
    " " ":ALL"
#  endif
#  if defined(LC_COLLATE)
    " " ":COLLATE"
#  endif
#  if defined(LC_CTYPE)
    " " ":CTYPE"
#  endif
#  if defined(LC_MESSAGES)
    " " ":MESSAGES"
#  endif
#  if defined(LC_MONETARY)
    " " ":MONETARY"
#  endif
#  if defined(LC_NUMERIC)
    " " ":NUMERIC"
#  endif
#  if defined(LC_TIME)
    " " ":TIME"
#  endif
#  if defined(LC_PAPER)
    " " ":PAPER"
#  endif
#  if defined(LC_NAME)
    " " ":NAME"
#  endif
#  if defined(LC_ADDRESS)
    " " ":ADDRESS"
#  endif
#  if defined(LC_TELEPHONE)
    " " ":TELEPHONE"
#  endif
#  if defined(LC_MEASUREMENT)
    " " ":MEASUREMENT"
#  endif
#  if defined(LC_IDENTIFICATION)
    " " ":IDENTIFICATION"
#  endif
    " " "))"
  },
  0
};

struct module__i18n__subr_tab_t {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  subr_t _subr_i18n_gettext;
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  subr_t _subr_i18n_language_information;
#endif
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  subr_t _subr_i18n_locale_conv;
#endif
  subr_t _subr_i18n_ngettext;
  subr_t _subr_i18n_set_locale;
  subr_t _subr_i18n_set_textdomain;
  subr_t _subr_i18n_set_textdomaindir;
  subr_t _subr_i18n_textdomain;
  subr_t _subr_i18n_textdomaindir;
  int _dummy_to_avoid_trailing_comma_in_initializer;
};
extern struct module__i18n__subr_tab_t module__i18n__subr_tab;


static const c_lisp_pair_t check_locale_category_table[] = {
 #ifdef LC_ALL
  { LC_ALL, &(O(object_Kall)) },
 #endif
 #ifdef LC_COLLATE
  { LC_COLLATE, &(O(object_Kcollate)) },
 #endif
 #ifdef LC_CTYPE
  { LC_CTYPE, &(O(object_Kctype)) },
 #endif
 #ifdef LC_MESSAGES
  { LC_MESSAGES, &(O(object_Kmessages)) },
 #endif
 #ifdef LC_MONETARY
  { LC_MONETARY, &(O(object_Kmonetary)) },
 #endif
 #ifdef LC_NUMERIC
  { LC_NUMERIC, &(O(object_Knumeric)) },
 #endif
 #ifdef LC_TIME
  { LC_TIME, &(O(object_Ktime)) },
 #endif
 #ifdef LC_PAPER
  { LC_PAPER, &(O(object_Kpaper)) },
 #endif
 #ifdef LC_NAME
  { LC_NAME, &(O(object_Kname)) },
 #endif
 #ifdef LC_ADDRESS
  { LC_ADDRESS, &(O(object_Kaddress)) },
 #endif
 #ifdef LC_TELEPHONE
  { LC_TELEPHONE, &(O(object_Ktelephone)) },
 #endif
 #ifdef LC_MEASUREMENT
  { LC_MEASUREMENT, &(O(object_Kmeasurement)) },
 #endif
 #ifdef LC_IDENTIFICATION
  { LC_IDENTIFICATION, &(O(object_Kidentification)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_locale_category_map = {
  check_locale_category_table,
  (sizeof(check_locale_category_table)/sizeof(c_lisp_pair_t))-1,
# if defined(LC_MESSAGES)
  LC_MESSAGES,true,
# else
  0,false,
# endif
  true,
  "check_locale_category"
};
#define check_locale_category(a) (int)map_lisp_to_c(a,&check_locale_category_map)
#define check_locale_category_reverse(a) map_c_to_lisp(a,&check_locale_category_map)

#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
static const c_lisp_pair_t check_nl_item_table[] = {
 #ifdef CODESET
  { CODESET, &(O(object_Kcodeset)) },
 #endif
 #ifdef D_T_FMT
  { D_T_FMT, &(O(object_Kd_t_fmt)) },
 #endif
 #ifdef D_FMT
  { D_FMT, &(O(object_Kd_fmt)) },
 #endif
 #ifdef T_FMT
  { T_FMT, &(O(object_Kt_fmt)) },
 #endif
 #ifdef T_FMT_AMPM
  { T_FMT_AMPM, &(O(object_Kt_fmt_ampm)) },
 #endif
 #ifdef AM_STR
  { AM_STR, &(O(object_Kam_str)) },
 #endif
 #ifdef PM_STR
  { PM_STR, &(O(object_Kpm_str)) },
 #endif
 #ifdef DAY_1
  { DAY_1, &(O(object_Kday_1)) },
 #endif
 #ifdef DAY_2
  { DAY_2, &(O(object_Kday_2)) },
 #endif
 #ifdef DAY_3
  { DAY_3, &(O(object_Kday_3)) },
 #endif
 #ifdef DAY_4
  { DAY_4, &(O(object_Kday_4)) },
 #endif
 #ifdef DAY_5
  { DAY_5, &(O(object_Kday_5)) },
 #endif
 #ifdef DAY_6
  { DAY_6, &(O(object_Kday_6)) },
 #endif
 #ifdef DAY_7
  { DAY_7, &(O(object_Kday_7)) },
 #endif
 #ifdef ABDAY_1
  { ABDAY_1, &(O(object_Kabday_1)) },
 #endif
 #ifdef ABDAY_2
  { ABDAY_2, &(O(object_Kabday_2)) },
 #endif
 #ifdef ABDAY_3
  { ABDAY_3, &(O(object_Kabday_3)) },
 #endif
 #ifdef ABDAY_4
  { ABDAY_4, &(O(object_Kabday_4)) },
 #endif
 #ifdef ABDAY_5
  { ABDAY_5, &(O(object_Kabday_5)) },
 #endif
 #ifdef ABDAY_6
  { ABDAY_6, &(O(object_Kabday_6)) },
 #endif
 #ifdef ABDAY_7
  { ABDAY_7, &(O(object_Kabday_7)) },
 #endif
 #ifdef MON_1
  { MON_1, &(O(object_Kmon_1)) },
 #endif
 #ifdef MON_2
  { MON_2, &(O(object_Kmon_2)) },
 #endif
 #ifdef MON_3
  { MON_3, &(O(object_Kmon_3)) },
 #endif
 #ifdef MON_4
  { MON_4, &(O(object_Kmon_4)) },
 #endif
 #ifdef MON_5
  { MON_5, &(O(object_Kmon_5)) },
 #endif
 #ifdef MON_6
  { MON_6, &(O(object_Kmon_6)) },
 #endif
 #ifdef MON_7
  { MON_7, &(O(object_Kmon_7)) },
 #endif
 #ifdef MON_8
  { MON_8, &(O(object_Kmon_8)) },
 #endif
 #ifdef MON_9
  { MON_9, &(O(object_Kmon_9)) },
 #endif
 #ifdef MON_10
  { MON_10, &(O(object_Kmon_10)) },
 #endif
 #ifdef MON_11
  { MON_11, &(O(object_Kmon_11)) },
 #endif
 #ifdef MON_12
  { MON_12, &(O(object_Kmon_12)) },
 #endif
 #ifdef ABMON_1
  { ABMON_1, &(O(object_Kabmon_1)) },
 #endif
 #ifdef ABMON_2
  { ABMON_2, &(O(object_Kabmon_2)) },
 #endif
 #ifdef ABMON_3
  { ABMON_3, &(O(object_Kabmon_3)) },
 #endif
 #ifdef ABMON_4
  { ABMON_4, &(O(object_Kabmon_4)) },
 #endif
 #ifdef ABMON_5
  { ABMON_5, &(O(object_Kabmon_5)) },
 #endif
 #ifdef ABMON_6
  { ABMON_6, &(O(object_Kabmon_6)) },
 #endif
 #ifdef ABMON_7
  { ABMON_7, &(O(object_Kabmon_7)) },
 #endif
 #ifdef ABMON_8
  { ABMON_8, &(O(object_Kabmon_8)) },
 #endif
 #ifdef ABMON_9
  { ABMON_9, &(O(object_Kabmon_9)) },
 #endif
 #ifdef ABMON_10
  { ABMON_10, &(O(object_Kabmon_10)) },
 #endif
 #ifdef ABMON_11
  { ABMON_11, &(O(object_Kabmon_11)) },
 #endif
 #ifdef ABMON_12
  { ABMON_12, &(O(object_Kabmon_12)) },
 #endif
 #ifdef ERA
  { ERA, &(O(object_Kera)) },
 #endif
 #ifdef ERA_D_FMT
  { ERA_D_FMT, &(O(object_Kera_d_fmt)) },
 #endif
 #ifdef ERA_D_T_FMT
  { ERA_D_T_FMT, &(O(object_Kera_d_t_fmt)) },
 #endif
 #ifdef ERA_T_FMT
  { ERA_T_FMT, &(O(object_Kera_t_fmt)) },
 #endif
 #ifdef ALT_DIGITS
  { ALT_DIGITS, &(O(object_Kalt_digits)) },
 #endif
 #ifdef RADIXCHAR
  { RADIXCHAR, &(O(object_Kradixchar)) },
 #endif
 #ifdef THOUSEP
  { THOUSEP, &(O(object_Kthousep)) },
 #endif
 #ifdef YESEXPR
  { YESEXPR, &(O(object_Kyesexpr)) },
 #endif
 #ifdef NOEXPR
  { NOEXPR, &(O(object_Knoexpr)) },
 #endif
 #ifdef YESSTR
  { YESSTR, &(O(object_Kyesstr)) },
 #endif
 #ifdef NOSTR
  { NOSTR, &(O(object_Knostr)) },
 #endif
 #ifdef CRNCYSTR
  { CRNCYSTR, &(O(object_Kcrncystr)) },
 #endif
 #ifdef D_MD_ORDER
  { D_MD_ORDER, &(O(object_Kd_md_order)) },
 #endif
 #ifdef LOCALE_FONTSIGNATURE
  { LOCALE_FONTSIGNATURE, &(O(object_Klocale_fontsignature)) },
 #endif
 #ifdef LOCALE_ICALENDARTYPE
  { LOCALE_ICALENDARTYPE, &(O(object_Klocale_icalendartype)) },
 #endif
 #ifdef LOCALE_ICENTURY
  { LOCALE_ICENTURY, &(O(object_Klocale_icentury)) },
 #endif
 #ifdef LOCALE_ICOUNTRY
  { LOCALE_ICOUNTRY, &(O(object_Klocale_icountry)) },
 #endif
 #ifdef LOCALE_ICURRDIGITS
  { LOCALE_ICURRDIGITS, &(O(object_Klocale_icurrdigits)) },
 #endif
 #ifdef LOCALE_ICURRENCY
  { LOCALE_ICURRENCY, &(O(object_Klocale_icurrency)) },
 #endif
 #ifdef LOCALE_IDATE
  { LOCALE_IDATE, &(O(object_Klocale_idate)) },
 #endif
 #ifdef LOCALE_IDAYLZERO
  { LOCALE_IDAYLZERO, &(O(object_Klocale_idaylzero)) },
 #endif
 #ifdef LOCALE_IDEFAULTANSICODEPAGE
  { LOCALE_IDEFAULTANSICODEPAGE, &(O(object_Klocale_idefaultansicodepage)) },
 #endif
 #ifdef LOCALE_IDEFAULTCODEPAGE
  { LOCALE_IDEFAULTCODEPAGE, &(O(object_Klocale_idefaultcodepage)) },
 #endif
 #ifdef LOCALE_IDEFAULTCOUNTRY
  { LOCALE_IDEFAULTCOUNTRY, &(O(object_Klocale_idefaultcountry)) },
 #endif
 #ifdef LOCALE_IDEFAULTEBCDICCODEPAGE
  { LOCALE_IDEFAULTEBCDICCODEPAGE, &(O(object_Klocale_idefaultebcdiccodepage)) },
 #endif
 #ifdef LOCALE_IDEFAULTLANGUAGE
  { LOCALE_IDEFAULTLANGUAGE, &(O(object_Klocale_idefaultlanguage)) },
 #endif
 #ifdef LOCALE_IDEFAULTMACCODEPAGE
  { LOCALE_IDEFAULTMACCODEPAGE, &(O(object_Klocale_idefaultmaccodepage)) },
 #endif
 #ifdef LOCALE_IDIGITS
  { LOCALE_IDIGITS, &(O(object_Klocale_idigits)) },
 #endif
 #ifdef LOCALE_IDIGITSUBSTITUTION
  { LOCALE_IDIGITSUBSTITUTION, &(O(object_Klocale_idigitsubstitution)) },
 #endif
 #ifdef LOCALE_IFIRSTDAYOFWEEK
  { LOCALE_IFIRSTDAYOFWEEK, &(O(object_Klocale_ifirstdayofweek)) },
 #endif
 #ifdef LOCALE_IFIRSTWEEKOFYEAR
  { LOCALE_IFIRSTWEEKOFYEAR, &(O(object_Klocale_ifirstweekofyear)) },
 #endif
 #ifdef LOCALE_IINTLCURRDIGITS
  { LOCALE_IINTLCURRDIGITS, &(O(object_Klocale_iintlcurrdigits)) },
 #endif
 #ifdef LOCALE_ILANGUAGE
  { LOCALE_ILANGUAGE, &(O(object_Klocale_ilanguage)) },
 #endif
 #ifdef LOCALE_ILDATE
  { LOCALE_ILDATE, &(O(object_Klocale_ildate)) },
 #endif
 #ifdef LOCALE_ILZERO
  { LOCALE_ILZERO, &(O(object_Klocale_ilzero)) },
 #endif
 #ifdef LOCALE_IMEASURE
  { LOCALE_IMEASURE, &(O(object_Klocale_imeasure)) },
 #endif
 #ifdef LOCALE_IMONLZERO
  { LOCALE_IMONLZERO, &(O(object_Klocale_imonlzero)) },
 #endif
 #ifdef LOCALE_INEGCURR
  { LOCALE_INEGCURR, &(O(object_Klocale_inegcurr)) },
 #endif
 #ifdef LOCALE_INEGNUMBER
  { LOCALE_INEGNUMBER, &(O(object_Klocale_inegnumber)) },
 #endif
 #ifdef LOCALE_INEGSEPBYSPACE
  { LOCALE_INEGSEPBYSPACE, &(O(object_Klocale_inegsepbyspace)) },
 #endif
 #ifdef LOCALE_INEGSIGNPOSN
  { LOCALE_INEGSIGNPOSN, &(O(object_Klocale_inegsignposn)) },
 #endif
 #ifdef LOCALE_INEGSYMPRECEDES
  { LOCALE_INEGSYMPRECEDES, &(O(object_Klocale_inegsymprecedes)) },
 #endif
 #ifdef LOCALE_IOPTIONALCALENDAR
  { LOCALE_IOPTIONALCALENDAR, &(O(object_Klocale_ioptionalcalendar)) },
 #endif
 #ifdef LOCALE_IPAPERSIZE
  { LOCALE_IPAPERSIZE, &(O(object_Klocale_ipapersize)) },
 #endif
 #ifdef LOCALE_IPOSSEPBYSPACE
  { LOCALE_IPOSSEPBYSPACE, &(O(object_Klocale_ipossepbyspace)) },
 #endif
 #ifdef LOCALE_IPOSSIGNPOSN
  { LOCALE_IPOSSIGNPOSN, &(O(object_Klocale_ipossignposn)) },
 #endif
 #ifdef LOCALE_IPOSSYMPRECEDES
  { LOCALE_IPOSSYMPRECEDES, &(O(object_Klocale_ipossymprecedes)) },
 #endif
 #ifdef LOCALE_ITIMEMARKPOSN
  { LOCALE_ITIMEMARKPOSN, &(O(object_Klocale_itimemarkposn)) },
 #endif
 #ifdef LOCALE_ITLZERO
  { LOCALE_ITLZERO, &(O(object_Klocale_itlzero)) },
 #endif
 #ifdef LOCALE_S1159
  { LOCALE_S1159, &(O(object_Klocale_s1159)) },
 #endif
 #ifdef LOCALE_S2359
  { LOCALE_S2359, &(O(object_Klocale_s2359)) },
 #endif
 #ifdef LOCALE_SABBREVCTRYNAME
  { LOCALE_SABBREVCTRYNAME, &(O(object_Klocale_sabbrevctryname)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME1
  { LOCALE_SABBREVDAYNAME1, &(O(object_Klocale_sabbrevdayname1)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME2
  { LOCALE_SABBREVDAYNAME2, &(O(object_Klocale_sabbrevdayname2)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME3
  { LOCALE_SABBREVDAYNAME3, &(O(object_Klocale_sabbrevdayname3)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME4
  { LOCALE_SABBREVDAYNAME4, &(O(object_Klocale_sabbrevdayname4)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME5
  { LOCALE_SABBREVDAYNAME5, &(O(object_Klocale_sabbrevdayname5)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME6
  { LOCALE_SABBREVDAYNAME6, &(O(object_Klocale_sabbrevdayname6)) },
 #endif
 #ifdef LOCALE_SABBREVDAYNAME7
  { LOCALE_SABBREVDAYNAME7, &(O(object_Klocale_sabbrevdayname7)) },
 #endif
 #ifdef LOCALE_SABBREVLANGNAME
  { LOCALE_SABBREVLANGNAME, &(O(object_Klocale_sabbrevlangname)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME1
  { LOCALE_SABBREVMONTHNAME1, &(O(object_Klocale_sabbrevmonthname1)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME2
  { LOCALE_SABBREVMONTHNAME2, &(O(object_Klocale_sabbrevmonthname2)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME3
  { LOCALE_SABBREVMONTHNAME3, &(O(object_Klocale_sabbrevmonthname3)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME4
  { LOCALE_SABBREVMONTHNAME4, &(O(object_Klocale_sabbrevmonthname4)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME5
  { LOCALE_SABBREVMONTHNAME5, &(O(object_Klocale_sabbrevmonthname5)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME6
  { LOCALE_SABBREVMONTHNAME6, &(O(object_Klocale_sabbrevmonthname6)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME7
  { LOCALE_SABBREVMONTHNAME7, &(O(object_Klocale_sabbrevmonthname7)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME8
  { LOCALE_SABBREVMONTHNAME8, &(O(object_Klocale_sabbrevmonthname8)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME9
  { LOCALE_SABBREVMONTHNAME9, &(O(object_Klocale_sabbrevmonthname9)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME10
  { LOCALE_SABBREVMONTHNAME10, &(O(object_Klocale_sabbrevmonthname10)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME11
  { LOCALE_SABBREVMONTHNAME11, &(O(object_Klocale_sabbrevmonthname11)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME12
  { LOCALE_SABBREVMONTHNAME12, &(O(object_Klocale_sabbrevmonthname12)) },
 #endif
 #ifdef LOCALE_SABBREVMONTHNAME13
  { LOCALE_SABBREVMONTHNAME13, &(O(object_Klocale_sabbrevmonthname13)) },
 #endif
 #ifdef LOCALE_SCOUNTRY
  { LOCALE_SCOUNTRY, &(O(object_Klocale_scountry)) },
 #endif
 #ifdef LOCALE_SCURRENCY
  { LOCALE_SCURRENCY, &(O(object_Klocale_scurrency)) },
 #endif
 #ifdef LOCALE_SDATE
  { LOCALE_SDATE, &(O(object_Klocale_sdate)) },
 #endif
 #ifdef LOCALE_SDAYNAME1
  { LOCALE_SDAYNAME1, &(O(object_Klocale_sdayname1)) },
 #endif
 #ifdef LOCALE_SDAYNAME2
  { LOCALE_SDAYNAME2, &(O(object_Klocale_sdayname2)) },
 #endif
 #ifdef LOCALE_SDAYNAME3
  { LOCALE_SDAYNAME3, &(O(object_Klocale_sdayname3)) },
 #endif
 #ifdef LOCALE_SDAYNAME4
  { LOCALE_SDAYNAME4, &(O(object_Klocale_sdayname4)) },
 #endif
 #ifdef LOCALE_SDAYNAME5
  { LOCALE_SDAYNAME5, &(O(object_Klocale_sdayname5)) },
 #endif
 #ifdef LOCALE_SDAYNAME6
  { LOCALE_SDAYNAME6, &(O(object_Klocale_sdayname6)) },
 #endif
 #ifdef LOCALE_SDAYNAME7
  { LOCALE_SDAYNAME7, &(O(object_Klocale_sdayname7)) },
 #endif
 #ifdef LOCALE_SDECIMAL
  { LOCALE_SDECIMAL, &(O(object_Klocale_sdecimal)) },
 #endif
 #ifdef LOCALE_SENGCOUNTRY
  { LOCALE_SENGCOUNTRY, &(O(object_Klocale_sengcountry)) },
 #endif
 #ifdef LOCALE_SENGCURRNAME
  { LOCALE_SENGCURRNAME, &(O(object_Klocale_sengcurrname)) },
 #endif
 #ifdef LOCALE_SENGLANGUAGE
  { LOCALE_SENGLANGUAGE, &(O(object_Klocale_senglanguage)) },
 #endif
 #ifdef LOCALE_SGROUPING
  { LOCALE_SGROUPING, &(O(object_Klocale_sgrouping)) },
 #endif
 #ifdef LOCALE_SINTLSYMBOL
  { LOCALE_SINTLSYMBOL, &(O(object_Klocale_sintlsymbol)) },
 #endif
 #ifdef LOCALE_SISO3166CTRYNAME
  { LOCALE_SISO3166CTRYNAME, &(O(object_Klocale_siso3166ctryname)) },
 #endif
 #ifdef LOCALE_SISO639LANGNAME
  { LOCALE_SISO639LANGNAME, &(O(object_Klocale_siso639langname)) },
 #endif
 #ifdef LOCALE_SLANGUAGE
  { LOCALE_SLANGUAGE, &(O(object_Klocale_slanguage)) },
 #endif
 #ifdef LOCALE_SLIST
  { LOCALE_SLIST, &(O(object_Klocale_slist)) },
 #endif
 #ifdef LOCALE_SLONGDATE
  { LOCALE_SLONGDATE, &(O(object_Klocale_slongdate)) },
 #endif
 #ifdef LOCALE_SMONDECIMALSEP
  { LOCALE_SMONDECIMALSEP, &(O(object_Klocale_smondecimalsep)) },
 #endif
 #ifdef LOCALE_SMONGROUPING
  { LOCALE_SMONGROUPING, &(O(object_Klocale_smongrouping)) },
 #endif
 #ifdef LOCALE_SMONTHNAME1
  { LOCALE_SMONTHNAME1, &(O(object_Klocale_smonthname1)) },
 #endif
 #ifdef LOCALE_SMONTHNAME2
  { LOCALE_SMONTHNAME2, &(O(object_Klocale_smonthname2)) },
 #endif
 #ifdef LOCALE_SMONTHNAME3
  { LOCALE_SMONTHNAME3, &(O(object_Klocale_smonthname3)) },
 #endif
 #ifdef LOCALE_SMONTHNAME4
  { LOCALE_SMONTHNAME4, &(O(object_Klocale_smonthname4)) },
 #endif
 #ifdef LOCALE_SMONTHNAME5
  { LOCALE_SMONTHNAME5, &(O(object_Klocale_smonthname5)) },
 #endif
 #ifdef LOCALE_SMONTHNAME6
  { LOCALE_SMONTHNAME6, &(O(object_Klocale_smonthname6)) },
 #endif
 #ifdef LOCALE_SMONTHNAME7
  { LOCALE_SMONTHNAME7, &(O(object_Klocale_smonthname7)) },
 #endif
 #ifdef LOCALE_SMONTHNAME8
  { LOCALE_SMONTHNAME8, &(O(object_Klocale_smonthname8)) },
 #endif
 #ifdef LOCALE_SMONTHNAME9
  { LOCALE_SMONTHNAME9, &(O(object_Klocale_smonthname9)) },
 #endif
 #ifdef LOCALE_SMONTHNAME10
  { LOCALE_SMONTHNAME10, &(O(object_Klocale_smonthname10)) },
 #endif
 #ifdef LOCALE_SMONTHNAME11
  { LOCALE_SMONTHNAME11, &(O(object_Klocale_smonthname11)) },
 #endif
 #ifdef LOCALE_SMONTHNAME12
  { LOCALE_SMONTHNAME12, &(O(object_Klocale_smonthname12)) },
 #endif
 #ifdef LOCALE_SMONTHNAME13
  { LOCALE_SMONTHNAME13, &(O(object_Klocale_smonthname13)) },
 #endif
 #ifdef LOCALE_SMONTHOUSANDSEP
  { LOCALE_SMONTHOUSANDSEP, &(O(object_Klocale_smonthousandsep)) },
 #endif
 #ifdef LOCALE_SNATIVECTRYNAME
  { LOCALE_SNATIVECTRYNAME, &(O(object_Klocale_snativectryname)) },
 #endif
 #ifdef LOCALE_SNATIVECURRNAME
  { LOCALE_SNATIVECURRNAME, &(O(object_Klocale_snativecurrname)) },
 #endif
 #ifdef LOCALE_SNATIVEDIGITS
  { LOCALE_SNATIVEDIGITS, &(O(object_Klocale_snativedigits)) },
 #endif
 #ifdef LOCALE_SNATIVELANGNAME
  { LOCALE_SNATIVELANGNAME, &(O(object_Klocale_snativelangname)) },
 #endif
 #ifdef LOCALE_SNEGATIVESIGN
  { LOCALE_SNEGATIVESIGN, &(O(object_Klocale_snegativesign)) },
 #endif
 #ifdef LOCALE_SPOSITIVESIGN
  { LOCALE_SPOSITIVESIGN, &(O(object_Klocale_spositivesign)) },
 #endif
 #ifdef LOCALE_SSHORTDATE
  { LOCALE_SSHORTDATE, &(O(object_Klocale_sshortdate)) },
 #endif
 #ifdef LOCALE_SSORTNAME
  { LOCALE_SSORTNAME, &(O(object_Klocale_ssortname)) },
 #endif
 #ifdef LOCALE_STHOUSAND
  { LOCALE_STHOUSAND, &(O(object_Klocale_sthousand)) },
 #endif
 #ifdef LOCALE_STIME
  { LOCALE_STIME, &(O(object_Klocale_stime)) },
 #endif
 #ifdef LOCALE_STIMEFORMAT
  { LOCALE_STIMEFORMAT, &(O(object_Klocale_stimeformat)) },
 #endif
 #ifdef LOCALE_SYEARMONTH
  { LOCALE_SYEARMONTH, &(O(object_Klocale_syearmonth)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_nl_item_map = {
  check_nl_item_table,
  (sizeof(check_nl_item_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_nl_item"
};
#define check_nl_item(a) (int)map_lisp_to_c(a,&check_nl_item_map)
#define check_nl_item_reverse(a) map_c_to_lisp(a,&check_nl_item_map)

#endif

#line 30

/* Returns the <locale.h> value corresponding to a LC_... constant. */
#line 34


#ifdef GNU_GETTEXT

static inline object do_gettext (const char* msgid,
                                 const char* domain, int category)
{
  const char* translated_msg;
  if (msgid[0] == '\0') {
    translated_msg = "";  /* Don't return the catalog's header entry. */
  } else {
    begin_system_call();
#  ifdef CLISP_UNICODE
    if (domain != NULL)
      bind_textdomain_codeset(domain,"UTF-8");
#  endif
    translated_msg = dcgettext(domain,msgid,category);
    end_system_call();
  }
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

static inline object do_ngettext (const char* msgid, const char* msgid_plural,
                                  const char* domain, uint32 n, int category)
{
  const char* translated_msg;
  begin_system_call();
# ifdef CLISP_UNICODE
  if (domain != NULL)
    bind_textdomain_codeset(domain,"UTF-8");
# endif
  translated_msg = dcngettext(domain,msgid,msgid_plural,n,category);
  end_system_call();
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

#endif

DEFUNR(I18N:GETTEXT, msgid &optional domain category,(subr_i18n_gettext,seclass_default,1,2,norest,nokey,0,NIL))
{ /* returns the translation of msgid in the given domain,
     depending on the given category. */
  object msgid = check_string(STACK_2);
 #ifdef GNU_GETTEXT
  with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
    object domain = STACK_1;
    if (missingp(domain)) {
      int category = check_locale_category(STACK_0);
      VALUES1(do_gettext(msgid_asciz,NULL,category));
    } else {
      domain = check_string(domain);
      with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
        int category = check_locale_category(STACK_0);
        VALUES1(do_gettext(msgid_asciz,domain_asciz,category));
      });
    }
  });
 #else
  VALUES1(msgid);
 #endif
  skipSTACK(3);
}

DEFUNR(I18N:NGETTEXT,msgid msgid_plural n &optional domain category,(subr_i18n_ngettext,seclass_default,3,2,norest,nokey,0,NIL))
{ /* returns the plural form of the translation for of msgid and n in
     the given domain, depending on the given category. */
  STACK_4 = check_string(STACK_4); /* msgid */
  STACK_3 = check_string(STACK_3); /* msgid_plural */
  {
    object arg = (STACK_2 = check_pos_integer(STACK_2));
    uint32 n;
    if (uint32_p(arg))
      n = I_to_uint32(arg);
    else {
      /* arg is a Bignum. Plural form depends only on (mod arg 1000000). */
      pushSTACK(arg); pushSTACK(fixnum(1000000)); funcall(L(mod),2);
      n = 1000000 + (uint32)posfixnum_to_V(value1);
    }
    {
      object msgid = STACK_4;
      object msgid_plural = STACK_3;
     #ifdef GNU_GETTEXT
      with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
        with_string_0(msgid_plural,Symbol_value(S(ascii)),msgid_plural_asciz, {
          object domain = STACK_1;
          if (missingp(domain)) {
            int category = check_locale_category(STACK_0);
            VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,NULL,
                                n,category));
          } else {
            domain = check_string(domain);
            with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
              int category = check_locale_category(STACK_0);
              VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,domain_asciz,
                                  n,category));
              });
          }
          });
        });
     #else
      VALUES1(n == 1 ? msgid : msgid_plural);
     #endif
    }
  }
  skipSTACK(5);
}

DEFUNR(I18N:TEXTDOMAIN,,(subr_i18n_textdomain,seclass_default,0,0,norest,nokey,0,NIL))
{ /* returns the current default domain. */
 #ifdef GNU_GETTEXT
  const char* domain;
  begin_system_call();
  domain = textdomain(NULL);
  end_system_call();
  VALUES1(asciz_to_string(domain,Symbol_value(S(ascii))));
 #else
  VALUES1(NIL);
 #endif
}

DEFUN(I18N:SET-TEXTDOMAIN, domain,(subr_i18n_set_textdomain,seclass_default,1,0,norest,nokey,0,NIL))
{ /* sets the default domain. */
  object domain = check_string(popSTACK());
 #ifdef GNU_GETTEXT
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    textdomain(domain_asciz);
    if_UNICODE(bind_textdomain_codeset(domain_asciz,"UTF-8"));
    end_system_call();
  });
 #endif
  VALUES1(domain);
}

DEFUN(I18N:TEXTDOMAINDIR, domain,(subr_i18n_textdomaindir,seclass_default,1,0,norest,nokey,0,NIL))
{ /* returns the message catalog directory for the given domain. */
  object domain = check_string(popSTACK());
 #ifdef GNU_GETTEXT
  const char* dir;
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    dir = bindtextdomain(domain_asciz,NULL);
    end_system_call();
  });
  VALUES1(dir != NULL ? OSdir_to_pathname(dir) : (object)NIL);
 #else
  VALUES1(NIL);
 #endif
}

DEFUN(I18N:SET-TEXTDOMAINDIR, domain directory,(subr_i18n_set_textdomaindir,seclass_default,2,0,norest,nokey,0,NIL))
{ /* sets the message catalog directory for the given domain. */
  object domain = (STACK_1=check_string(STACK_1));
 #ifdef GNU_GETTEXT
  /* Check and use default directory, because the bindtextdomain()
     documentation recommends that the argument be an absolute pathname,
     to protect against later chdir() calls. */
  object directory = pathname_to_OSdir(STACK_0,true);
  with_string_0(STACK_1/*domain*/,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    bindtextdomain(domain_asciz,TheAsciz(directory));
    end_system_call();
  });
 #endif
  VALUES1(STACK_0);
  skipSTACK(2);
}


/* ======================== locale ======================== */

DEFUN(I18N:SET-LOCALE, &optional category locale,(subr_i18n_set_locale,seclass_default,0,2,norest,nokey,0,NIL))
{ /* call setlocale(3) */
  gcv_object_t *category = &STACK_1;
  gcv_object_t *locale = &STACK_0;
  char* res;
  if (missingp(*category)) {
    int pos = 0;
    if (missingp(*locale)) {
      for (; pos < check_locale_category_map.size; pos++) {
        begin_system_call();
        res = setlocale(check_locale_category_map.table[pos].c_const,NULL);
        end_system_call();
        pushSTACK(*check_locale_category_map.table[pos].l_const);
        pushSTACK(res ? asciz_to_string(res,GLO(misc_encoding)) : NIL);
      }
    } else {
      *locale = check_string(*locale);
      with_string_0(*locale,GLO(misc_encoding),loc_z,{
          for (; pos < check_locale_category_map.size; pos++) {
            begin_system_call();
            res = setlocale(check_locale_category_map.table[pos].c_const,loc_z);
            end_system_call();
            pushSTACK(*check_locale_category_map.table[pos].l_const);
            pushSTACK(res ? asciz_to_string(res,GLO(misc_encoding)) : NIL);
          }
        });
    }
    VALUES1(listof(2*check_locale_category_map.size));
  } else {
    int cat_value = check_locale_category(*category);
    if (missingp(*locale)) {
      begin_system_call();
      res = setlocale(cat_value,NULL);
      end_system_call();
    } else {
      *locale = check_string(*locale);
      with_string_0(*locale,GLO(misc_encoding),loc_z,{
          begin_system_call();
          res = setlocale(cat_value,loc_z);
          end_system_call();
        });
    }
    VALUES1(res ? asciz_to_string(res,GLO(misc_encoding)) : NIL);
  }
  skipSTACK(2);
}

#if defined(WIN32_NATIVE)
/* call GetLocaleInfo.
 res is a malloc'ed area of size res_size
 it may be increased as a result of calling this function */
# define GET_LOCALE_INFO_BUF_SIZE 256
static void get_locale_info (int what, char**res, int *res_size) {
  int val;
  begin_system_call();
  val = GetLocaleInfo(LOCALE_SYSTEM_DEFAULT,what,*res,*res_size);
  end_system_call();
  if (val == 0) OS_error();
  if (val > *res_size) {
    *res = (char*)my_realloc(*res,val);
    if (*res == NULL) OS_error();
    *res_size = val;
    begin_system_call();
    GetLocaleInfo(LOCALE_SYSTEM_DEFAULT,what,*res,*res_size);
    end_system_call();
  }
}
#endif

#if defined(HAVE_LOCALECONV)
static void thousands_sep_to_STACK (char* sep1000) {
  int ii;
  for (ii=0; sep1000[ii]; ii++) pushSTACK(fixnum(sep1000[ii]));
  pushSTACK(Fixnum_0); value1 = vectorof(ii+1); pushSTACK(value1);
}
static /*maygc*/ object bool_char_lconv(char val) {
  switch (val) {
    case 0: return NIL;
    case 1: return T;
    case CHAR_MAX: return S(Kunspecific);
    default:
      pushSTACK(CLSTEXT("~S: localeconv() returned an invalid value ~S (should be one of ~S, ~S, CHAR_MAX=~S)"));
      pushSTACK(TheSubr(subr_self)->name);
      pushSTACK(fixnum(val));
      pushSTACK(Fixnum_0); pushSTACK(Fixnum_1); pushSTACK(fixnum(CHAR_MAX));
      funcall(S(warn),6);
      return fixnum(val);
  }
}
static object int_char_lconv(char val) {
  switch (val) {
    case CHAR_MAX: return S(Kunspecific);
    default: return fixnum(val);
  }
}
DEFUN(I18N:LOCALE-CONV,,(subr_i18n_locale_conv,seclass_default,0,0,norest,nokey,0,NIL))
{ /* call localeconv(3) */
  struct lconv *lc;
  begin_system_call(); lc = localeconv(); end_system_call();
  pushSTACK(asciz_to_string(lc->decimal_point,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->thousands_sep,GLO(misc_encoding)));
  thousands_sep_to_STACK(lc->grouping);
  pushSTACK(asciz_to_string(lc->int_curr_symbol,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->currency_symbol,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->mon_decimal_point,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->mon_thousands_sep,GLO(misc_encoding)));
  thousands_sep_to_STACK(lc->mon_grouping);
  pushSTACK(asciz_to_string(lc->positive_sign,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->negative_sign,GLO(misc_encoding)));
  pushSTACK(int_char_lconv(lc->int_frac_digits));
  pushSTACK(int_char_lconv(lc->frac_digits));
  pushSTACK(bool_char_lconv(lc->p_cs_precedes));
  pushSTACK(int_char_lconv(lc->p_sep_by_space));
  pushSTACK(bool_char_lconv(lc->n_cs_precedes));
  pushSTACK(int_char_lconv(lc->n_sep_by_space));
  pushSTACK(int_char_lconv(lc->p_sign_posn));
  pushSTACK(int_char_lconv(lc->n_sign_posn));
#if HAVE_STRUCT_LCONV_INT_P_CS_PRECEDES
  pushSTACK(bool_char_lconv(lc->int_p_cs_precedes));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_N_CS_PRECEDES
  pushSTACK(bool_char_lconv(lc->int_n_cs_precedes));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_P_SEP_BY_SPACE
  pushSTACK(int_char_lconv(lc->int_p_sep_by_space));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_N_SEP_BY_SPACE
  pushSTACK(int_char_lconv(lc->int_n_sep_by_space));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_P_SIGN_POSN
  pushSTACK(int_char_lconv(lc->int_p_sign_posn));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_N_SIGN_POSN
  pushSTACK(int_char_lconv(lc->int_n_sign_posn));
#else
  pushSTACK(S(Kunspecific));
#endif
  funcall(O(object_i18n__mk_locale_conv),24);
}
#elif defined(WIN32_NATIVE)
static int my_atoi (char *res) {
  int val;
  begin_system_call(); val = atoi(res); end_system_call();
  return val;
}
static void thousands_sep_to_STACK (int what, char** gres, int* res_size) {
  /* "1;2;3" ==> #(1 2 3) */
  int start = 0, end = 0, count = 0, limit;
  char *res;
  get_locale_info(what,gres,res_size);
  res = *gres; limit = *res_size;
  while (res[end] && (end < limit)) {
    while (res[end] && (res[end] != ';') && (end < limit)) end++;
    pushSTACK(fixnum(my_atoi(res+start))); count++;
    if (!res[end]) break;
    start = ++end;
  }
  value1 = vectorof(count); pushSTACK(value1);
}
static void locale_string_to_STACK (int what, char**res, int* res_size) {
  get_locale_info(what,res,res_size);
  pushSTACK(asciz_to_string(*res,GLO(misc_encoding)));
}
static void locale_int_to_STACK (int what, char**res, int* res_size) {
  get_locale_info(what,res,res_size);
  pushSTACK(fixnum(my_atoi(*res)));
}
static void locale_bool_to_STACK (int what, char**res, int* res_size) {
  get_locale_info(what,res,res_size);
  pushSTACK(my_atoi(*res) ? T : NIL);
}
DEFUN(I18N:LOCALE-CONV,,(subr_i18n_locale_conv,seclass_default,0,0,norest,nokey,0,NIL))
{ /* call GetLocaleInfo(3) */
  int res_size = GET_LOCALE_INFO_BUF_SIZE;
  char *res = my_malloc(res_size);
  locale_string_to_STACK(LOCALE_SDECIMAL,&res,&res_size);
  locale_string_to_STACK(LOCALE_STHOUSAND,&res,&res_size);
  thousands_sep_to_STACK(LOCALE_SGROUPING,&res,&res_size);
  locale_string_to_STACK(LOCALE_SINTLSYMBOL,&res,&res_size);
  locale_string_to_STACK(LOCALE_SCURRENCY,&res,&res_size);
  locale_string_to_STACK(LOCALE_SMONDECIMALSEP,&res,&res_size);
  locale_string_to_STACK(LOCALE_SMONTHOUSANDSEP,&res,&res_size);
  thousands_sep_to_STACK(LOCALE_SMONGROUPING,&res,&res_size);
  locale_string_to_STACK(LOCALE_SPOSITIVESIGN,&res,&res_size);
  locale_string_to_STACK(LOCALE_SNEGATIVESIGN,&res,&res_size);
  locale_int_to_STACK(LOCALE_IINTLCURRDIGITS,&res,&res_size);
  locale_int_to_STACK(LOCALE_ICURRDIGITS,&res,&res_size);
  locale_bool_to_STACK(LOCALE_IPOSSYMPRECEDES,&res,&res_size);
  locale_bool_to_STACK(LOCALE_IPOSSEPBYSPACE,&res,&res_size);
  locale_bool_to_STACK(LOCALE_INEGSYMPRECEDES,&res,&res_size);
  locale_bool_to_STACK(LOCALE_INEGSEPBYSPACE,&res,&res_size);
  locale_bool_to_STACK(LOCALE_IPOSSIGNPOSN,&res,&res_size);
  locale_bool_to_STACK(LOCALE_INEGSIGNPOSN,&res,&res_size);
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  funcall(O(object_i18n__mk_locale_conv),24);
  begin_system_call(); free(res); end_system_call();
}
#endif

#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
#line 473

#if defined(HAVE_NL_LANGINFO)
#line 476
# define get_lang_info(what)                                               begin_system_call(); res = nl_langinfo(what); end_system_call()
# define res_to_obj() (res ? asciz_to_string(res,GLO(misc_encoding)) : NIL)
# define DECLARE_RES  char* res
# define FINISH_RES
#elif defined(WIN32_NATIVE)
# define get_lang_info(what)  get_locale_info(what,&res,&res_size)
# define res_to_obj() (asciz_to_string(res,GLO(misc_encoding)))
# define DECLARE_RES  int res_size=GET_LOCALE_INFO_BUF_SIZE; char *res=(char*)my_malloc(res_size)
# define FINISH_RES   begin_system_call(); free(res); end_system_call()
#endif
DEFUNR(I18N:LANGUAGE-INFORMATION,&optional item,(subr_i18n_language_information,seclass_default,0,1,norest,nokey,0,NIL))
{ /* call nl_langinfo(3) or GetLocaleInfo() */
  object what = popSTACK();
  if (missingp(what)) {         /* everything */
    int pos = 0;
    DECLARE_RES;
    for (; pos < check_nl_item_map.size; pos++) {
      get_lang_info(check_nl_item_map.table[pos].c_const);
      pushSTACK(*check_nl_item_map.table[pos].l_const);
      pushSTACK(res_to_obj());
    }
    FINISH_RES;
    VALUES1(listof(2*check_nl_item_map.size));
  } else {
    int item = check_nl_item(what);
    DECLARE_RES;
    get_lang_info(item);
    VALUES1(res_to_obj());
    FINISH_RES;
  }
}
#endif


struct module__i18n__subr_tab_t module__i18n__subr_tab
  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(__GNUC__)
    __attribute__ ((aligned (varobject_alignment)))
  #endif
  = {
  #if varobjects_misaligned
  { 0 },
  #endif
  LISPFUN_F(subr_i18n_gettext,seclass_default,1,2,norest,nokey,0,NIL)
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  LISPFUN_F(subr_i18n_language_information,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  LISPFUN_F(subr_i18n_locale_conv,seclass_default,0,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_i18n_ngettext,seclass_default,3,2,norest,nokey,0,NIL)
  LISPFUN_F(subr_i18n_set_locale,seclass_default,0,2,norest,nokey,0,NIL)
  LISPFUN_F(subr_i18n_set_textdomain,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_i18n_set_textdomaindir,seclass_default,2,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_i18n_textdomain,seclass_default,0,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_i18n_textdomaindir,seclass_default,1,0,norest,nokey,0,NIL)
  0
};
uintC module__i18n__subr_tab_size = (sizeof(struct module__i18n__subr_tab_t)-varobjects_misaligned-sizeof(int))/sizeof(subr_t);

struct module__i18n__subr_tab_initdata_t {
  subr_initdata_t _subr_i18n_gettext;
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  subr_initdata_t _subr_i18n_language_information;
#endif
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  subr_initdata_t _subr_i18n_locale_conv;
#endif
  subr_initdata_t _subr_i18n_ngettext;
  subr_initdata_t _subr_i18n_set_locale;
  subr_initdata_t _subr_i18n_set_textdomain;
  subr_initdata_t _subr_i18n_set_textdomaindir;
  subr_initdata_t _subr_i18n_textdomain;
  subr_initdata_t _subr_i18n_textdomaindir;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__i18n__subr_tab_initdata = {
  { "I18N", "GETTEXT" },
#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
  { "I18N", "LANGUAGE-INFORMATION" },
#endif
#if (defined(HAVE_LOCALECONV)) || ((!(defined(HAVE_LOCALECONV))) && (defined(WIN32_NATIVE)))
  { "I18N", "LOCALE-CONV" },
#endif
  { "I18N", "NGETTEXT" },
  { "I18N", "SET-LOCALE" },
  { "I18N", "SET-TEXTDOMAIN" },
  { "I18N", "SET-TEXTDOMAINDIR" },
  { "I18N", "TEXTDOMAIN" },
  { "I18N", "TEXTDOMAINDIR" },
  0
};

void module__i18n__init_function_1 (module_t* module);
void module__i18n__init_function_1 (module_t* module)
{
}

void module__i18n__init_function_2 (module_t* module);
void module__i18n__init_function_2 (module_t* module)
{
}

void module__i18n__fini_function (module_t* module);
void module__i18n__fini_function (module_t* module)
{
}
