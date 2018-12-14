#line 1 "calls.c"
/*
 * system calls
 * Copyright (C) 2003-2008 Sam Steingold
 * Copyright (C) 2005 Bruno Haible
 * Copyright (C) 2005 Arseny Slobodyuk
 * GPL2
 */

#if defined(_WIN32)
/* need this for CreateHardLink to work */
# define WINVER 0x0500
/* get ASCII functions */
# undef UNICODE
#endif
#if defined(__CYGWIN__)
# define UNIX_CYGWIN32
# undef UNICODE
#endif

#include "clisp.h"
#include "config.h"

#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
#  include <sys/time.h>
# elif defined(HAVE_TIME_H)
#  include <time.h>
# endif
#endif
#if defined(HAVE_UNISTD_H)
# include <unistd.h>
#endif
#if defined(HAVE_SYS_UNISTD_H)
# include <sys/unistd.h>
#endif
#if defined(HAVE_ERRNO_H)
# include <errno.h>
#endif
#include <sys/types.h>
#if defined(HAVE_SYS_STAT_H)
# include <sys/stat.h>
#endif
#if defined(HAVE_SYS_RESOURCE_H)
# include <sys/resource.h>
#endif
#if defined(HAVE_SYS_STATVFS_H)
# include <sys/statvfs.h>
#endif
#if defined(HAVE_CRYPT_H)
# include <crypt.h>
#endif
#if defined(HAVE_UTIME_H)
# include <utime.h>
#endif
#if defined(HAVE_WCHAR_H)
# include <wchar.h>
#endif
#include <limits.h>
#if !defined(NZERO)             /* should be defined in <limits.h> */
#  define NZERO 20
#endif
#if defined(HAVE_SYSLOG_H)
# include <syslog.h>
#endif
#if defined(HAVE_UTMPX_H)
# include <utmpx.h>
#endif
#if defined(HAVE_SIGNAL_H)
/* bug #[ 1507628 ]: #define unused (void) breaks clisp 2.38 on arm */
# undef unused
# include <signal.h>            /* used unused */
# ifdef GNU     /* see lispbibl.d */
#  define unused  (void)
# else
#  define unused
# endif
#endif

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
#include <initguid.h>
DEFINE_GUID(FMTID_SummaryInformation, 0xF29F85E0, 0x4FF9, 0x1068,
            0xAB, 0x91, 0x08, 0x00, 0x2B, 0x27, 0xB3, 0xD9);
DEFINE_GUID(FMTID_UserDefinedProperties, 0xD5CDD505, 0x2E9C, 0x101B,
            0x93, 0x97, 0x08, 0x00, 0x2B, 0x2C, 0xF9, 0xAE);
#endif

#include <stdio.h>              /* for BUFSIZ */
#include <stdlib.h>
#include <string.h>             /* for strcpy(), strcat() */

/* #define DEBUG */
#if defined(DEBUG)
extern object nobject_out (FILE* stream, object obj);
#line 99
# define XOUT(obj,label)                                                   (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),         obj=nobject_out(stdout,obj), printf("\n"))
#else
# undef OBJECT_OUT
# define OBJECT_OUT(o,l)
# define XOUT(o,l)
#endif

#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
/* we use posix fcntl() on unix and win32 LockFileEx() on win32.
   since cygwin supports fcntl(), we use it there, but another option
   would be to use cygwin get_osfhandle() + win32 LockFileEx(),
   see <http://article.gmane.org/gmane.os.cygwin/35175> */
#if defined(HAVE_FCNTL_H)
# include <fcntl.h>
#endif

#if SIZEOF_PID_T == 8
# define pid_to_I(g)  uint64_to_I(g)
# define I_to_pid(g)  I_to_uint64(g=check_sint64(g))
#else
# define pid_to_I(g)  uint32_to_I(g)
# define I_to_pid(g)  I_to_uint32(g=check_sint32(g))
#endif
#if SIZEOF_UID_T == 8
# define uid_to_I(g)  uint64_to_I(g)
# define I_to_uid(g)  I_to_uint64(g=check_sint64(g))
#else
# define uid_to_I(g)  uint32_to_I(g)
# define I_to_uid(g)  I_to_uint32(g=check_sint32(g))
#endif
#if SIZEOF_GID_T == 8
# define gid_to_I(g)  uint64_to_I(g)
# define I_to_gid(g)  I_to_uint64(g=check_sint64(g))
#else
# define gid_to_I(g)  uint32_to_I(g)
# define I_to_gid(g)  I_to_uint32(g=check_sint32(g))
#endif

/* general convenience macros */
#line 141
#define GETTER(type,call)                                          type##_t id;                                                     begin_system_call(); id = call(); end_system_call();             VALUES1(type##_to_I(id))
#line 147
#define GETTER1(type,call)                                 type##_t id = I_to_##type(STACK_0);                      type##_t ret;                                            begin_system_call(); ret=call(id); end_system_call();    if (ret==(type##_t)-1) OS_error();                       VALUES1(type##_to_I(ret)); skipSTACK(1)
#line 153
#define SETTER(type,call)                                          type##_t id = I_to_##type(STACK_0);                              int status;                                                      begin_system_call(); status = call(id); end_system_call();       if (status) OS_error();                                          VALUES1(popSTACK())
#line 160
#define SETTER2(type,call)                                                 type##_t eid = I_to_##type(STACK_0);                                     type##_t rid = I_to_##type(STACK_1);                                     int status;                                                              begin_system_call(); status = call(rid,eid); end_system_call();          if (status) OS_error();                                                  VALUES0; skipSTACK(2)

/* for COPY-FILE, must come before DEFMODULE for DEFCHECKER to work */
typedef enum {
  COPY_METHOD_COPY,
  COPY_METHOD_SYMLINK,
  COPY_METHOD_HARDLINK,
  COPY_METHOD_RENAME
} copy_method_t;

DEFMODULE(syscalls,"POSIX")

#define O(varname) module__syscalls__object_tab._##varname
#define F(varname) subr_tab_ptr_as_object(&(module__syscalls__subr_tab._##varname))

struct module__syscalls__object_tab_t {
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  gcv_object_t _object_cl_decode_universal_time;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD)))
  gcv_object_t _object_cl_member;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_CHAR_TERM))
  gcv_object_t _object_K2_char_term;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_C_BIND))
  gcv_object_t _object_K2_c_bind;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_C_DEV))
  gcv_object_t _object_K2_c_dev;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_FORT_DEV))
  gcv_object_t _object_K2_fort_dev;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_FORT_RUN))
  gcv_object_t _object_K2_fort_run;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_LOCALEDEF))
  gcv_object_t _object_K2_localedef;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS))
  gcv_object_t _object_K2_pbs;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_ACCOUNTING))
  gcv_object_t _object_K2_pbs_accounting;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_CHECKPOINT))
  gcv_object_t _object_K2_pbs_checkpoint;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_LOCATE))
  gcv_object_t _object_K2_pbs_locate;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_MESSAGE))
  gcv_object_t _object_K2_pbs_message;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_TRACK))
  gcv_object_t _object_K2_pbs_track;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_SW_DEV))
  gcv_object_t _object_K2_sw_dev;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_2_SYMLINKS))
  gcv_object_t _object_K2_symlinks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_UPE))
  gcv_object_t _object_K2_upe;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_VERSION))
  gcv_object_t _object_K2_version;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(ABOVE_NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  gcv_object_t _object_Kabove_normal;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(ACCOUNTING))
  gcv_object_t _object_Kaccounting;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ADVISORY_INFO))
  gcv_object_t _object_Kadvisory_info;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_LISTIO_MAX))
  gcv_object_t _object_Kaio_listio_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_MAX))
  gcv_object_t _object_Kaio_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_PRIO_DELTA_MAX))
  gcv_object_t _object_Kaio_prio_delta_max;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_ALERT))
  gcv_object_t _object_Kalert;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_ALLOC_SIZE_MIN))
  gcv_object_t _object_Kalloc_size_min;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kalpha;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kalt;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_APPEND))
  gcv_object_t _object_Kappend;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kappname;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_ARCHIVE))
  gcv_object_t _object_Karchive;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Karguments;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ARG_MAX))
  gcv_object_t _object_Karg_max;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_AS))
  gcv_object_t _object_Kas;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ASYNCHRONOUS_IO))
  gcv_object_t _object_Kasynchronous_io;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_ASYNC_IO))
  gcv_object_t _object_Kasync_io;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ATEXIT_MAX))
  gcv_object_t _object_Katexit_max;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  gcv_object_t _object_Katime;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_AUTH))
  gcv_object_t _object_Kauth;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kauthor;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_AUTHPRIV))
  gcv_object_t _object_Kauthpriv;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AVPHYS_PAGES))
  gcv_object_t _object_Kavphys_pages;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kbackoffice;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BARRIERS))
  gcv_object_t _object_Kbarriers;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_BASE_MAX))
  gcv_object_t _object_Kbc_base_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_DIM_MAX))
  gcv_object_t _object_Kbc_dim_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_SCALE_MAX))
  gcv_object_t _object_Kbc_scale_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_STRING_MAX))
  gcv_object_t _object_Kbc_string_max;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(BELOW_NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  gcv_object_t _object_Kbelow_normal;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_BINARY))
  gcv_object_t _object_Kbinary;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kblob;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kblock;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kbool;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(BOOT_TIME))
  gcv_object_t _object_Kboot_time;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kbstr;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kbuffered;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kbuilt_in;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kbyref;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kcharcount;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CHILD_MAX))
  gcv_object_t _object_Kchild_max;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_CHOWN_RESTRICTED))
  gcv_object_t _object_Kchown_restricted;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kclipboard_format;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CLK_TCK))
  gcv_object_t _object_Kclk_tck;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CLOCK_SELECTION))
  gcv_object_t _object_Kclock_selection;
#endif
#if (defined(HAVE_FCNTL)) && (defined(FD_CLOEXEC))
  gcv_object_t _object_Kcloexec;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kcodepage;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_COLL_WEIGHTS_MAX))
  gcv_object_t _object_Kcoll_weights_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kcomments;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_COMPRESSED))
  gcv_object_t _object_Kcompressed;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  gcv_object_t _object_Kcons;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kcontrol;
#endif
  gcv_object_t _object_Kcopy;
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_CORE))
  gcv_object_t _object_Kcore;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_CPU))
  gcv_object_t _object_Kcpu;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CPUTIME))
  gcv_object_t _object_Kcputime;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_CREAT))
  gcv_object_t _object_Kcreat;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kcreate_dtm;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_CRIT))
  gcv_object_t _object_Kcrit;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_CRON))
  gcv_object_t _object_Kcron;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kcy;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_DAEMON))
  gcv_object_t _object_Kdaemon;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_DATA))
  gcv_object_t _object_Kdata;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kdatacenter;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kdate;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(DEAD_PROCESS))
  gcv_object_t _object_Kdead_process;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_DEBUG))
  gcv_object_t _object_Kdebug;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_DELAYTIMER_MAX))
  gcv_object_t _object_Kdelaytimer_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kdescription;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_DEVICE))
  gcv_object_t _object_Kdevice;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_DIRECT))
  gcv_object_t _object_Kdirect;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kdirection;
#endif
#if ((defined(HAVE_FCNTL)) && (defined(O_DIRECTORY))) || ((defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_DIRECTORY)))
  gcv_object_t _object_Kdirectory;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kdoc_security;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kdomain_controller;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kedittime;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kelement_type;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_EMERG))
  gcv_object_t _object_Kemerg;
#endif
#if ((defined(HAVE_UTMPX_H)) && (defined(EMPTY))) || (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32))
  gcv_object_t _object_Kempty;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_ENCRYPTED))
  gcv_object_t _object_Kencrypted;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kenterprise;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_ERR))
  gcv_object_t _object_Kerr;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kerror;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_EXCL))
  gcv_object_t _object_Kexcl;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_EXPR_NEST_MAX))
  gcv_object_t _object_Kexpr_nest_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kext;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kexternal_format;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  gcv_object_t _object_Kfacility;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFBLK))
  gcv_object_t _object_Kfblk;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFCHR))
  gcv_object_t _object_Kfchr;
#endif
#if (defined(HAVE_FCNTL)) && (defined(F_GETFD))
  gcv_object_t _object_Kfd;
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFDIR))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_MKDIR))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_CREAT)))
  gcv_object_t _object_Kfdir;
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFIFO))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_MKFIFO)))
  gcv_object_t _object_Kfifo;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_FILESIZEBITS))
  gcv_object_t _object_Kfilesizebits;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kfiletime;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_NAMED_STREAMS))
  gcv_object_t _object_Kfile_named_streams;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_READ_ONLY_VOLUME))
  gcv_object_t _object_Kfile_read_only_volume;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_ENCRYPTION))
  gcv_object_t _object_Kfile_supports_encryption;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_OBJECT_IDS))
  gcv_object_t _object_Kfile_supports_object_ids;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_REPARSE_POINTS))
  gcv_object_t _object_Kfile_supports_reparse_points;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_SPARSE_FILES))
  gcv_object_t _object_Kfile_supports_sparse_files;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_VOLUME_QUOTAS))
  gcv_object_t _object_Kfile_volume_quotas;
#endif
#if (defined(HAVE_FCNTL)) && (defined(F_GETFL))
  gcv_object_t _object_Kfl;
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFREG))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_CREAT)))
  gcv_object_t _object_Kfreg;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_FSIZE))
  gcv_object_t _object_Kfsize;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFSOCK))
  gcv_object_t _object_Kfsock;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_FSYNC))
  gcv_object_t _object_Kfsync;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_CASE_IS_PRESERVED))
  gcv_object_t _object_Kfs_case_is_preserved;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_CASE_SENSITIVE))
  gcv_object_t _object_Kfs_case_sensitive;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_FILE_COMPRESSION))
  gcv_object_t _object_Kfs_file_compression;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_FILE_ENCRYPTION))
  gcv_object_t _object_Kfs_file_encryption;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_PERSISTENT_ACLS))
  gcv_object_t _object_Kfs_persistent_acls;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_UNICODE_STORED_ON_DISK))
  gcv_object_t _object_Kfs_unicode_stored_on_disk;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_VOL_IS_COMPRESSED))
  gcv_object_t _object_Kfs_vol_is_compressed;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_FTP))
  gcv_object_t _object_Kftp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_GETGR_R_SIZE_MAX))
  gcv_object_t _object_Kgetgr_r_size_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_GETPW_R_SIZE_MAX))
  gcv_object_t _object_Kgetpw_r_size_max;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  gcv_object_t _object_Kgid;
#endif
  gcv_object_t _object_Khardlink;
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_HIDDEN))
  gcv_object_t _object_Khidden;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(HIGH_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  gcv_object_t _object_Khigh;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_HOST_NAME_MAX))
  gcv_object_t _object_Khost_name_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Khot_key;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ki1;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ki2;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ki4;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ki8;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kia64;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kicon;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(IDLE_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  gcv_object_t _object_Kidle;
#endif
  gcv_object_t _object_Kif_does_not_exist;
  gcv_object_t _object_Kif_exists;
#if (defined(HAVE_SYSLOG)) && (defined(LOG_INFO))
  gcv_object_t _object_Kinfo;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kinitid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(INIT_PROCESS))
  gcv_object_t _object_Kinit_process;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kint;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kintel;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_IOV_MAX))
  gcv_object_t _object_Kiov_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_IPV6))
  gcv_object_t _object_Kipv6;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_JOB_CONTROL))
  gcv_object_t _object_Kjob_control;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_KERN))
  gcv_object_t _object_Kkern;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kkeywords;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_LARGEFILE))
  gcv_object_t _object_Klargefile;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Klastauthor;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Klastprinted;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Klastsave_dtm;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  gcv_object_t _object_Klength;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_LINE_MAX))
  gcv_object_t _object_Kline_max;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_LINK_MAX))
  gcv_object_t _object_Klink_max;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL0))
  gcv_object_t _object_Klocal0;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL1))
  gcv_object_t _object_Klocal1;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL2))
  gcv_object_t _object_Klocal2;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL3))
  gcv_object_t _object_Klocal3;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL4))
  gcv_object_t _object_Klocal4;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL5))
  gcv_object_t _object_Klocal5;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL6))
  gcv_object_t _object_Klocal6;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL7))
  gcv_object_t _object_Klocal7;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Klocale;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_LOCKS))
  gcv_object_t _object_Klocks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_LOGIN_NAME_MAX))
  gcv_object_t _object_Klogin_name_max;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(LOGIN_PROCESS))
  gcv_object_t _object_Klogin_process;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(LOW_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  gcv_object_t _object_Klow;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LPR))
  gcv_object_t _object_Klpr;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Klpstr;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Klpwstr;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_MAIL))
  gcv_object_t _object_Kmail;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MAPPED_FILES))
  gcv_object_t _object_Kmapped_files;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kmax;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_MAX_CANON))
  gcv_object_t _object_Kmax_canon;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_MAX_INPUT))
  gcv_object_t _object_Kmax_input;
#endif
#if ((defined(HAVE_SYSCONF)) && (defined(_SC_MEMLOCK))) || ((defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_MEMLOCK)))
  gcv_object_t _object_Kmemlock;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MEMLOCK_RANGE))
  gcv_object_t _object_Kmemlock_range;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MEMORY_PROTECTION))
  gcv_object_t _object_Kmemory_protection;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MESSAGE_PASSING))
  gcv_object_t _object_Kmessage_passing;
#endif
  gcv_object_t _object_Kmethod;
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kmin;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kmips;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  gcv_object_t _object_Kmode;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MONOTONIC_CLOCK))
  gcv_object_t _object_Kmonotonic_clock;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MQ_OPEN_MAX))
  gcv_object_t _object_Kmq_open_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MQ_PRIO_MAX))
  gcv_object_t _object_Kmq_prio_max;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  gcv_object_t _object_Kmtime;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_NAME_MAX))
  gcv_object_t _object_Kname_max;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  gcv_object_t _object_Kndelay;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_NEWS))
  gcv_object_t _object_Knews;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(NEW_TIME))
  gcv_object_t _object_Knew_time;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NGROUPS_MAX))
  gcv_object_t _object_Kngroups_max;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOCTTY))
  gcv_object_t _object_Knoctty;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_NOFILE))
  gcv_object_t _object_Knofile;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOFOLLOW))
  gcv_object_t _object_Knofollow;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOINHERIT))
  gcv_object_t _object_Knoinherit;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NONBLOCK))
  gcv_object_t _object_Knonblock;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE))) || ((defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_NORMAL)))
  gcv_object_t _object_Knormal;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_NOTICE))
  gcv_object_t _object_Knotice;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Knotimplemented;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED))
  gcv_object_t _object_Knot_content_indexed;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  gcv_object_t _object_Knowait;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_NO_TRUNC))
  gcv_object_t _object_Kno_trunc;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_NPROC))
  gcv_object_t _object_Knproc;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NPROCESSORS_CONF))
  gcv_object_t _object_Knprocessors_conf;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NPROCESSORS_ONLN))
  gcv_object_t _object_Knprocessors_onln;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Knt;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Knull;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  gcv_object_t _object_Kodelay;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_OFFLINE))
  gcv_object_t _object_Koffline;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(OLD_TIME))
  gcv_object_t _object_Kold_time;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_OPEN_MAX))
  gcv_object_t _object_Kopen_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kpagecount;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PAGESIZE))
  gcv_object_t _object_Kpagesize;
#endif
#if ((defined(HAVE_CONFSTR)) && (defined(_CS_PATH))) || (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32))
  gcv_object_t _object_Kpath;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PATH_MAX))
  gcv_object_t _object_Kpath_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kpersonal;
#endif
#if defined(PRIO_PGRP)
  gcv_object_t _object_Kpgrp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PHYS_PAGES))
  gcv_object_t _object_Kphys_pages;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  gcv_object_t _object_Kpid;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PIPE_BUF))
  gcv_object_t _object_Kpipe_buf;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_CFLAGS))
  gcv_object_t _object_Kposix_v6_ilp32_off32_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_LDFLAGS))
  gcv_object_t _object_Kposix_v6_ilp32_off32_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_LIBS))
  gcv_object_t _object_Kposix_v6_ilp32_off32_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS))
  gcv_object_t _object_Kposix_v6_ilp32_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS))
  gcv_object_t _object_Kposix_v6_ilp32_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_LIBS))
  gcv_object_t _object_Kposix_v6_ilp32_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_CFLAGS))
  gcv_object_t _object_Kposix_v6_lp64_off64_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_LDFLAGS))
  gcv_object_t _object_Kposix_v6_lp64_off64_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_LIBS))
  gcv_object_t _object_Kposix_v6_lp64_off64_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS))
  gcv_object_t _object_Kposix_v6_lpbig_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS))
  gcv_object_t _object_Kposix_v6_lpbig_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_LIBS))
  gcv_object_t _object_Kposix_v6_lpbig_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS))
  gcv_object_t _object_Kposix_v6_width_restricted_envs;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kppc;
#endif
  gcv_object_t _object_Kpreserve;
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PRIORITIZED_IO))
  gcv_object_t _object_Kprioritized_io;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PRIORITY_SCHEDULING))
  gcv_object_t _object_Kpriority_scheduling;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PRIO_IO))
  gcv_object_t _object_Kprio_io;
#endif
#if defined(PRIO_PROCESS)
  gcv_object_t _object_Kprocess;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kr4;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kr8;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RAW_SOCKETS))
  gcv_object_t _object_Kraw_sockets;
#endif
#if defined(HAVE_FCNTL)
  gcv_object_t _object_Krdonly;
#endif
#if defined(HAVE_FCNTL)
  gcv_object_t _object_Krdwr;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_READER_WRITER_LOCKS))
  gcv_object_t _object_Kreader_writer_locks;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_READONLY))
  gcv_object_t _object_Kreadonly;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(REALTIME_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  gcv_object_t _object_Krealtime;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_REALTIME_SIGNALS))
  gcv_object_t _object_Krealtime_signals;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_INCR_XFER_SIZE))
  gcv_object_t _object_Krec_incr_xfer_size;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_MAX_XFER_SIZE))
  gcv_object_t _object_Krec_max_xfer_size;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_MIN_XFER_SIZE))
  gcv_object_t _object_Krec_min_xfer_size;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_XFER_ALIGN))
  gcv_object_t _object_Krec_xfer_align;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_REGEXP))
  gcv_object_t _object_Kregexp;
#endif
  gcv_object_t _object_Krename;
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_REPARSE_POINT))
  gcv_object_t _object_Kreparse_point;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Krevnumber;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RE_DUP_MAX))
  gcv_object_t _object_Kre_dup_max;
#endif
#if defined(S_IRGRP)
  gcv_object_t _object_Krgrp;
#endif
#if defined(S_IROTH)
  gcv_object_t _object_Kroth;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_RSS))
  gcv_object_t _object_Krss;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RTSIG_MAX))
  gcv_object_t _object_Krtsig_max;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(RUN_LVL))
  gcv_object_t _object_Krun_lvl;
#endif
#if defined(S_IRUSR)
  gcv_object_t _object_Krusr;
#endif
#if defined(S_IRWXG)
  gcv_object_t _object_Krwxg;
#endif
#if defined(S_IRWXO)
  gcv_object_t _object_Krwxo;
#endif
#if defined(S_IRWXU)
  gcv_object_t _object_Krwxu;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SAVED_IDS))
  gcv_object_t _object_Ksaved_ids;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEMAPHORES))
  gcv_object_t _object_Ksemaphores;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEM_NSEMS_MAX))
  gcv_object_t _object_Ksem_nsems_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEM_VALUE_MAX))
  gcv_object_t _object_Ksem_value_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kserver;
#endif
#if defined(S_ISGID)
  gcv_object_t _object_Ksgid;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kshared;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SHARED_MEMORY_OBJECTS))
  gcv_object_t _object_Kshared_memory_objects;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SHELL))
  gcv_object_t _object_Kshell;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kshift;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kshow_command;
#endif
#if defined(SIGABRT)
  gcv_object_t _object_Ksigabrt;
#endif
#if defined(SIGALRM)
  gcv_object_t _object_Ksigalrm;
#endif
#if defined(SIGBUS)
  gcv_object_t _object_Ksigbus;
#endif
#if defined(SIGCHLD)
  gcv_object_t _object_Ksigchld;
#endif
#if defined(SIGCONT)
  gcv_object_t _object_Ksigcont;
#endif
#if defined(SIGFPE)
  gcv_object_t _object_Ksigfpe;
#endif
#if defined(SIGHUP)
  gcv_object_t _object_Ksighup;
#endif
#if defined(SIGILL)
  gcv_object_t _object_Ksigill;
#endif
#if defined(SIGINT)
  gcv_object_t _object_Ksigint;
#endif
#if defined(SIGKILL)
  gcv_object_t _object_Ksigkill;
#endif
#if defined(SIGPIPE)
  gcv_object_t _object_Ksigpipe;
#endif
#if defined(SIGPOLL)
  gcv_object_t _object_Ksigpoll;
#endif
#if defined(SIGPROF)
  gcv_object_t _object_Ksigprof;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SIGQUEUE_MAX))
  gcv_object_t _object_Ksigqueue_max;
#endif
#if defined(SIGQUIT)
  gcv_object_t _object_Ksigquit;
#endif
#if defined(SIGSEGV)
  gcv_object_t _object_Ksigsegv;
#endif
#if defined(SIGSTOP)
  gcv_object_t _object_Ksigstop;
#endif
#if defined(SIGSYS)
  gcv_object_t _object_Ksigsys;
#endif
#if defined(SIGTERM)
  gcv_object_t _object_Ksigterm;
#endif
#if defined(SIGTRAP)
  gcv_object_t _object_Ksigtrap;
#endif
#if defined(SIGTSTP)
  gcv_object_t _object_Ksigtstp;
#endif
#if defined(SIGTTIN)
  gcv_object_t _object_Ksigttin;
#endif
#if defined(SIGTTOU)
  gcv_object_t _object_Ksigttou;
#endif
#if defined(SIGURG)
  gcv_object_t _object_Ksigurg;
#endif
#if defined(SIGUSR1)
  gcv_object_t _object_Ksigusr1;
#endif
#if defined(SIGUSR2)
  gcv_object_t _object_Ksigusr2;
#endif
#if defined(SIGVTALRM)
  gcv_object_t _object_Ksigvtalrm;
#endif
#if defined(SIGXCPU)
  gcv_object_t _object_Ksigxcpu;
#endif
#if defined(SIGXFSZ)
  gcv_object_t _object_Ksigxfsz;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ksmallbusiness;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ksmallbusiness_restricted;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SOCK_MAXBUF))
  gcv_object_t _object_Ksock_maxbuf;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_SPARSE_FILE))
  gcv_object_t _object_Ksparse_file;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPAWN))
  gcv_object_t _object_Kspawn;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPIN_LOCKS))
  gcv_object_t _object_Kspin_locks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPORADIC_SERVER))
  gcv_object_t _object_Ksporadic_server;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SS_REPL_MAX))
  gcv_object_t _object_Kss_repl_max;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_STACK))
  gcv_object_t _object_Kstack;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  gcv_object_t _object_Kstart;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_STREAM_MAX))
  gcv_object_t _object_Kstream_max;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_APPEND))
  gcv_object_t _object_Kst_append;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_IMMUTABLE))
  gcv_object_t _object_Kst_immutable;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_MANDLOCK))
  gcv_object_t _object_Kst_mandlock;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOATIME))
  gcv_object_t _object_Kst_noatime;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NODEV))
  gcv_object_t _object_Kst_nodev;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NODIRATIME))
  gcv_object_t _object_Kst_nodiratime;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOEXEC))
  gcv_object_t _object_Kst_noexec;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOSUID))
  gcv_object_t _object_Kst_nosuid;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOTRUNC))
  gcv_object_t _object_Kst_notrunc;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_RDONLY))
  gcv_object_t _object_Kst_rdonly;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_SYNCHRONOUS))
  gcv_object_t _object_Kst_synchronous;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_WRITE))
  gcv_object_t _object_Kst_write;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ksubject;
#endif
#if defined(S_ISUID)
  gcv_object_t _object_Ksuid;
#endif
#if defined(S_ISVTX)
  gcv_object_t _object_Ksvtx;
#endif
  gcv_object_t _object_Ksymlink;
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SYMLINK_MAX))
  gcv_object_t _object_Ksymlink_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SYMLOOP_MAX))
  gcv_object_t _object_Ksymloop_max;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_SYNC))
  gcv_object_t _object_Ksync;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SYNCHRONIZED_IO))
  gcv_object_t _object_Ksynchronized_io;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SYNC_IO))
  gcv_object_t _object_Ksync_io;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_SYSLOG))
  gcv_object_t _object_Ksyslog;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_SYSTEM))
  gcv_object_t _object_Ksystem;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ktemplate;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_TEMPORARY))
  gcv_object_t _object_Ktemporary;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kterminal;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_TEXT))
  gcv_object_t _object_Ktext;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREADS))
  gcv_object_t _object_Kthreads;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_ATTR_STACKADDR))
  gcv_object_t _object_Kthread_attr_stackaddr;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_ATTR_STACKSIZE))
  gcv_object_t _object_Kthread_attr_stacksize;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_CPUTIME))
  gcv_object_t _object_Kthread_cputime;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_DESTRUCTOR_ITERATIONS))
  gcv_object_t _object_Kthread_destructor_iterations;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_KEYS_MAX))
  gcv_object_t _object_Kthread_keys_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIORITY_SCHEDULING))
  gcv_object_t _object_Kthread_priority_scheduling;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIO_INHERIT))
  gcv_object_t _object_Kthread_prio_inherit;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIO_PROTECT))
  gcv_object_t _object_Kthread_prio_protect;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PROCESS_SHARED))
  gcv_object_t _object_Kthread_process_shared;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_SAFE_FUNCTIONS))
  gcv_object_t _object_Kthread_safe_functions;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_SPORADIC_SERVER))
  gcv_object_t _object_Kthread_sporadic_server;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_STACK_MIN))
  gcv_object_t _object_Kthread_stack_min;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_THREADS_MAX))
  gcv_object_t _object_Kthread_threads_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kthumbnail;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMEOUTS))
  gcv_object_t _object_Ktimeouts;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMERS))
  gcv_object_t _object_Ktimers;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMER_MAX))
  gcv_object_t _object_Ktimer_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Ktitle;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE))
  gcv_object_t _object_Ktrace;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_EVENT_FILTER))
  gcv_object_t _object_Ktrace_event_filter;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_EVENT_NAME_MAX))
  gcv_object_t _object_Ktrace_event_name_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_INHERIT))
  gcv_object_t _object_Ktrace_inherit;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_LOG))
  gcv_object_t _object_Ktrace_log;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_NAME_MAX))
  gcv_object_t _object_Ktrace_name_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_SYS_MAX))
  gcv_object_t _object_Ktrace_sys_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_USER_EVENT_MAX))
  gcv_object_t _object_Ktrace_user_event_max;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_TRUNC))
  gcv_object_t _object_Ktrunc;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TTY_NAME_MAX))
  gcv_object_t _object_Ktty_name_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TYPED_MEMORY_OBJECTS))
  gcv_object_t _object_Ktyped_memory_objects;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TZNAME_MAX))
  gcv_object_t _object_Ktzname_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kui1;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kui2;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kui4;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kui8;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  gcv_object_t _object_Kuid;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kuint;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kunknown;
#endif
#if ((defined(HAVE_SYSLOG)) && (defined(LOG_USER))) || (defined(PRIO_USER))
  gcv_object_t _object_Kuser;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kuser_defined;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(USER_PROCESS))
  gcv_object_t _object_Kuser_process;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_UUCP))
  gcv_object_t _object_Kuucp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_ILP32_OFF32))
  gcv_object_t _object_Kv6_ilp32_off32;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_ILP32_OFFBIG))
  gcv_object_t _object_Kv6_ilp32_offbig;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_LP64_OFF64))
  gcv_object_t _object_Kv6_lp64_off64;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_LPBIG_OFFBIG))
  gcv_object_t _object_Kv6_lpbig_offbig;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_VDISABLE))
  gcv_object_t _object_Kvdisable;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_VERSION))
  gcv_object_t _object_Kversion;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_WARNING))
  gcv_object_t _object_Kwarning;
#endif
#if defined(S_IWGRP)
  gcv_object_t _object_Kwgrp;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kwindows;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kwordcount;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kworking_directory;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_Kworkstation;
#endif
#if defined(S_IWOTH)
  gcv_object_t _object_Kwoth;
#endif
#if defined(HAVE_FCNTL)
  gcv_object_t _object_Kwronly;
#endif
#if defined(S_IWUSR)
  gcv_object_t _object_Kwusr;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_ILP32_OFF32))
  gcv_object_t _object_Kxbs5_ilp32_off32;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_CFLAGS))
  gcv_object_t _object_Kxbs5_ilp32_off32_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LDFLAGS))
  gcv_object_t _object_Kxbs5_ilp32_off32_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LIBS))
  gcv_object_t _object_Kxbs5_ilp32_off32_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LINTFLAGS))
  gcv_object_t _object_Kxbs5_ilp32_off32_lintflags;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_ILP32_OFFBIG))
  gcv_object_t _object_Kxbs5_ilp32_offbig;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_CFLAGS))
  gcv_object_t _object_Kxbs5_ilp32_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LDFLAGS))
  gcv_object_t _object_Kxbs5_ilp32_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LIBS))
  gcv_object_t _object_Kxbs5_ilp32_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LINTFLAGS))
  gcv_object_t _object_Kxbs5_ilp32_offbig_lintflags;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_LP64_OFF64))
  gcv_object_t _object_Kxbs5_lp64_off64;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_CFLAGS))
  gcv_object_t _object_Kxbs5_lp64_off64_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LDFLAGS))
  gcv_object_t _object_Kxbs5_lp64_off64_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LIBS))
  gcv_object_t _object_Kxbs5_lp64_off64_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LINTFLAGS))
  gcv_object_t _object_Kxbs5_lp64_off64_lintflags;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_LPBIG_OFFBIG))
  gcv_object_t _object_Kxbs5_lpbig_offbig;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_CFLAGS))
  gcv_object_t _object_Kxbs5_lpbig_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LDFLAGS))
  gcv_object_t _object_Kxbs5_lpbig_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LIBS))
  gcv_object_t _object_Kxbs5_lpbig_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LINTFLAGS))
  gcv_object_t _object_Kxbs5_lpbig_offbig_lintflags;
#endif
#if defined(S_IXGRP)
  gcv_object_t _object_Kxgrp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_CRYPT))
  gcv_object_t _object_Kxopen_crypt;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_ENH_I18N))
  gcv_object_t _object_Kxopen_enh_i18n;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_LEGACY))
  gcv_object_t _object_Kxopen_legacy;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_REALTIME))
  gcv_object_t _object_Kxopen_realtime;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_REALTIME_THREADS))
  gcv_object_t _object_Kxopen_realtime_threads;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_SHM))
  gcv_object_t _object_Kxopen_shm;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_STREAMS))
  gcv_object_t _object_Kxopen_streams;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_UNIX))
  gcv_object_t _object_Kxopen_unix;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_VERSION))
  gcv_object_t _object_Kxopen_version;
#endif
#if defined(S_IXOTH)
  gcv_object_t _object_Kxoth;
#endif
#if defined(S_IXUSR)
  gcv_object_t _object_Kxusr;
#endif
  gcv_object_t _object_posix__copy_file;
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_posix__make_file_info;
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  gcv_object_t _object_posix__make_file_stat;
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  gcv_object_t _object_posix__make_group_info;
#endif
  gcv_object_t _object_posix__make_hostent;
#if defined(HAVE_GETRLIMIT)
  gcv_object_t _object_posix__make_rlimit;
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  gcv_object_t _object_posix__make_service;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_posix__make_shortcut_info;
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  gcv_object_t _object_posix__make_stat_vfs;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_posix__make_system_info;
#endif
#if defined(HAVE_UNAME)
  gcv_object_t _object_posix__make_uname;
#endif
#if defined(HAVE_GETRUSAGE)
  gcv_object_t _object_posix__make_usage;
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  gcv_object_t _object_posix__make_user_info;
#endif
#if defined(HAVE_UTMPX_H)
  gcv_object_t _object_posix__make_utmpx;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_posix__make_version;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object_posix__mkmemstat;
#endif
  gcv_object_t _object_posix__resolve_host_ipaddr;
#if defined(HAVE_SETRLIMIT)
  gcv_object_t _object_posix__rlimit;
#endif
#if defined(HAVE_UTMPX_H)
  gcv_object_t _object_posix__utmpx;
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!(defined(WIN32_NATIVE))) && (!(defined(HAVE_CHMOD)))
  gcv_object_t _object__22chmod_28_29_22;
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!(defined(HAVE_CHOWN)))
  gcv_object_t _object__22chown_28_29_22;
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!defined(WIN32_NATIVE)) && (!(defined(HAVE_UTIME)))
  gcv_object_t _object__22utime_28_29_22;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object__28member_20_alt_20_control_20_ext_20_shift_29;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object__28member_20_normal_20_max_20_min_29;
#endif
  gcv_object_t _object__28or_20null_20integer_20_28member_20_copy_20_symlink_20_hardlink_20_rename_29_29;
#if defined(HAVE_ENCRYPT) || defined(HAVE_SETKEY)
  gcv_object_t _object__28vector_20_28unsigned_byte_208_29_208_29;
#endif
#if defined(HAVE_UTMPX_H)
  gcv_object_t _object___28or_20integer_20_28member_defined_28empty_29_Kempty_defined_28run_lvl_29_Krun_lvl_defined_28boot_time_29_Kboot_time_defined_28old_time_29_Kold_time_defined_28new_time_29_Knew_time_defined_28user_process_29_Kuser_process_defined_28init_process_29_Kinit_process_defined_28login_process_29_Klogin_process_defined_28dead_process_29_Kdead_process_defined_28accounting_29_Kaccounting__29_29;
#endif
#if defined(HAVE_FCNTL)
  gcv_object_t _object___28or_20integer_20_28member_defined_28fd_cloexec_29_Kcloexec__29_29;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  gcv_object_t _object___28or_20integer_20_28member_defined_28file_attribute_archive_29_Karchive_defined_28file_attribute_compressed_29_Kcompressed_defined_28file_attribute_device_29_Kdevice_defined_28file_attribute_directory_29_Kdirectory_defined_28file_attribute_encrypted_29_Kencrypted_defined_28file_attribute_hidden_29_Khidden_defined_28file_attribute_normal_29_Knormal_defined_28file_attribute_not_content_indexed_29_Knot_content_indexed_defined_28file_attribute_offline_29_Koffline_defined_28file_attribute_readonly_29_Kreadonly_defined_28file_attribute_reparse_point_29_Kreparse_point_defined_28file_attribute_sparse_file_29_Ksparse_file_defined_28file_attribute_system_29_Ksystem_defined_28file_attribute_temporary_29_Ktemporary__29_29;
#endif
#if defined(HAVE_FCNTL)
  gcv_object_t _object___28or_20integer_20_28member_defined_28f_getfd_29_Kfd_defined_28f_getfl_29_Kfl__29_29;
#endif
#if defined(HAVE_SYSLOG)
  gcv_object_t _object___28or_20integer_20_28member_defined_28log_emerg_29_Kemerg_defined_28log_alert_29_Kalert_defined_28log_crit_29_Kcrit_defined_28log_err_29_Kerr_defined_28log_warning_29_Kwarning_defined_28log_notice_29_Knotice_defined_28log_info_29_Kinfo_defined_28log_debug_29_Kdebug__29_29;
#endif
#if defined(HAVE_FCNTL)
  gcv_object_t _object___28or_20integer_20_28member_defined_28o_rdonly_29_Krdonly_defined_28o_wronly_29_Kwronly_defined_28o_rdwr_29_Krdwr_defined_28o_append_29_Kappend_defined_28o_creat_29_Kcreat_defined_28o_trunc_29_Ktrunc_defined_28o_excl_29_Kexcl_defined_28o_noctty_29_Knoctty_defined_28o_sync_29_Ksync_defined_28o_nonblock_29_Knonblock_defined_28o_binary_29_Kbinary_defined_28o_text_29_Ktext_defined_28o_noinherit_29_Knoinherit_defined_28o_direct_29_Kdirect_defined_28o_largefile_29_Klargefile_defined_28o_directory_29_Kdirectory_defined_28o_nofollow_29_Knofollow__29_29;
#endif
#if defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)
  gcv_object_t _object___28or_20integer_20_28member_defined_28rlimit_cpu_29_Kcpu_defined_28rlimit_fsize_29_Kfsize_defined_28rlimit_data_29_Kdata_defined_28rlimit_stack_29_Kstack_defined_28rlimit_core_29_Kcore_defined_28rlimit_rss_29_Krss_defined_28rlimit_nofile_29_Knofile_defined_28rlimit_as_29_Kas_defined_28rlimit_nproc_29_Knproc_defined_28rlimit_memlock_29_Kmemlock_defined_28rlimit_locks_29_Klocks__29_29;
#endif
  gcv_object_t _object___28or_20integer_20_28member_defined_28sigabrt_29_Ksigabrt_defined_28sigalrm_29_Ksigalrm_defined_28sigbus_29_Ksigbus_defined_28sigchld_29_Ksigchld_defined_28sigcont_29_Ksigcont_defined_28sigfpe_29_Ksigfpe_defined_28sighup_29_Ksighup_defined_28sigill_29_Ksigill_defined_28sigint_29_Ksigint_defined_28sigkill_29_Ksigkill_defined_28sigpipe_29_Ksigpipe_defined_28sigquit_29_Ksigquit_defined_28sigsegv_29_Ksigsegv_defined_28sigstop_29_Ksigstop_defined_28sigterm_29_Ksigterm_defined_28sigtstp_29_Ksigtstp_defined_28sigttin_29_Ksigttin_defined_28sigttou_29_Ksigttou_defined_28sigusr1_29_Ksigusr1_defined_28sigusr2_29_Ksigusr2_defined_28sigpoll_29_Ksigpoll_defined_28sigprof_29_Ksigprof_defined_28sigsys_29_Ksigsys_defined_28sigtrap_29_Ksigtrap_defined_28sigurg_29_Ksigurg_defined_28sigvtalrm_29_Ksigvtalrm_defined_28sigxcpu_29_Ksigxcpu_defined_28sigxfsz_29_Ksigxfsz__29_29;
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  gcv_object_t _object___28or_20integer_20_28member_defined_28st_rdonly_29_Kst_rdonly_defined_28st_nosuid_29_Kst_nosuid_defined_28st_notrunc_29_Kst_notrunc_defined_28st_nodev_29_Kst_nodev_defined_28st_noexec_29_Kst_noexec_defined_28st_synchronous_29_Kst_synchronous_defined_28st_mandlock_29_Kst_mandlock_defined_28st_write_29_Kst_write_defined_28st_append_29_Kst_append_defined_28st_immutable_29_Kst_immutable_defined_28st_noatime_29_Kst_noatime_defined_28st_nodiratime_29_Kst_nodiratime_defined_28file_named_streams_29_Kfile_named_streams_defined_28file_read_only_volume_29_Kfile_read_only_volume_defined_28file_supports_object_ids_29_Kfile_supports_object_ids_defined_28file_supports_reparse_points_29_Kfile_supports_reparse_points_defined_28file_supports_sparse_files_29_Kfile_supports_sparse_files_defined_28file_volume_quotas_29_Kfile_volume_quotas_defined_28file_supports_encryption_29_Kfile_supports_encryption_defined_28fs_case_is_preserved_29_Kfs_case_is_preserved_defined_28fs_case_sensitive_29_Kfs_case_sensitive_defined_28fs_file_compression_29_Kfs_file_compression_defined_28fs_file_encryption_29_Kfs_file_encryption_defined_28fs_persistent_acls_29_Kfs_persistent_acls_defined_28fs_unicode_stored_on_disk_29_Kfs_unicode_stored_on_disk_defined_28fs_vol_is_compressed_29_Kfs_vol_is_compressed__29_29;
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  gcv_object_t _object___28or_20integer_20_28member_defined_28s_ififo_29_Kfifo_defined_28s_ifsock_29_Kfsock_defined_28s_ifchr_29_Kfchr_defined_28s_ifdir_29_Kfdir_defined_28s_ifblk_29_Kfblk_defined_28s_ifreg_29_Kfreg__29_29;
#endif
  gcv_object_t _object___28or_20integer_20_28member_defined_28s_isuid_29_Ksuid_defined_28s_isgid_29_Ksgid_defined_28s_isvtx_29_Ksvtx_defined_28s_irwxu_29_Krwxu_defined_28s_irusr_29_Krusr_defined_28s_iwusr_29_Kwusr_defined_28s_ixusr_29_Kxusr_defined_28s_irwxg_29_Krwxg_defined_28s_irgrp_29_Krgrp_defined_28s_iwgrp_29_Kwgrp_defined_28s_ixgrp_29_Kxgrp_defined_28s_irwxo_29_Krwxo_defined_28s_iroth_29_Kroth_defined_28s_iwoth_29_Kwoth_defined_28s_ixoth_29_Kxoth__29_29;
#if defined(HAVE_CONFSTR)
  gcv_object_t _object___28or_20integer_20_28member_defined_28_cs_path_29_Kpath_defined_28_cs_posix_v6_ilp32_off32_cflags_29_Kposix_v6_ilp32_off32_cflags_defined_28_cs_posix_v6_ilp32_off32_ldflags_29_Kposix_v6_ilp32_off32_ldflags_defined_28_cs_posix_v6_ilp32_off32_libs_29_Kposix_v6_ilp32_off32_libs_defined_28_cs_posix_v6_ilp32_offbig_cflags_29_Kposix_v6_ilp32_offbig_cflags_defined_28_cs_posix_v6_ilp32_offbig_ldflags_29_Kposix_v6_ilp32_offbig_ldflags_defined_28_cs_posix_v6_ilp32_offbig_libs_29_Kposix_v6_ilp32_offbig_libs_defined_28_cs_posix_v6_lp64_off64_cflags_29_Kposix_v6_lp64_off64_cflags_defined_28_cs_posix_v6_lp64_off64_ldflags_29_Kposix_v6_lp64_off64_ldflags_defined_28_cs_posix_v6_lp64_off64_libs_29_Kposix_v6_lp64_off64_libs_defined_28_cs_posix_v6_lpbig_offbig_cflags_29_Kposix_v6_lpbig_offbig_cflags_defined_28_cs_posix_v6_lpbig_offbig_ldflags_29_Kposix_v6_lpbig_offbig_ldflags_defined_28_cs_posix_v6_lpbig_offbig_libs_29_Kposix_v6_lpbig_offbig_libs_defined_28_cs_posix_v6_width_restricted_envs_29_Kposix_v6_width_restricted_envs_defined_28_cs_xbs5_ilp32_off32_cflags_29_Kxbs5_ilp32_off32_cflags_defined_28_cs_xbs5_ilp32_off32_ldflags_29_Kxbs5_ilp32_off32_ldflags_defined_28_cs_xbs5_ilp32_off32_libs_29_Kxbs5_ilp32_off32_libs_defined_28_cs_xbs5_ilp32_off32_lintflags_29_Kxbs5_ilp32_off32_lintflags_defined_28_cs_xbs5_ilp32_offbig_cflags_29_Kxbs5_ilp32_offbig_cflags_defined_28_cs_xbs5_ilp32_offbig_ldflags_29_Kxbs5_ilp32_offbig_ldflags_defined_28_cs_xbs5_ilp32_offbig_libs_29_Kxbs5_ilp32_offbig_libs_defined_28_cs_xbs5_ilp32_offbig_lintflags_29_Kxbs5_ilp32_offbig_lintflags_defined_28_cs_xbs5_lp64_off64_cflags_29_Kxbs5_lp64_off64_cflags_defined_28_cs_xbs5_lp64_off64_ldflags_29_Kxbs5_lp64_off64_ldflags_defined_28_cs_xbs5_lp64_off64_libs_29_Kxbs5_lp64_off64_libs_defined_28_cs_xbs5_lp64_off64_lintflags_29_Kxbs5_lp64_off64_lintflags_defined_28_cs_xbs5_lpbig_offbig_cflags_29_Kxbs5_lpbig_offbig_cflags_defined_28_cs_xbs5_lpbig_offbig_ldflags_29_Kxbs5_lpbig_offbig_ldflags_defined_28_cs_xbs5_lpbig_off;
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  gcv_object_t _object___28or_20integer_20_28member_defined_28_pc_filesizebits_29_Kfilesizebits_defined_28_pc_link_max_29_Klink_max_defined_28_pc_max_canon_29_Kmax_canon_defined_28_pc_max_input_29_Kmax_input_defined_28_pc_name_max_29_Kname_max_defined_28_pc_path_max_29_Kpath_max_defined_28_pc_pipe_buf_29_Kpipe_buf_defined_28_pc_2_symlinks_29_K2_symlinks_defined_28_pc_alloc_size_min_29_Kalloc_size_min_defined_28_pc_rec_incr_xfer_size_29_Krec_incr_xfer_size_defined_28_pc_rec_max_xfer_size_29_Krec_max_xfer_size_defined_28_pc_rec_min_xfer_size_29_Krec_min_xfer_size_defined_28_pc_rec_xfer_align_29_Krec_xfer_align_defined_28_pc_symlink_max_29_Ksymlink_max_defined_28_pc_chown_restricted_29_Kchown_restricted_defined_28_pc_no_trunc_29_Kno_trunc_defined_28_pc_vdisable_29_Kvdisable_defined_28_pc_async_io_29_Kasync_io_defined_28_pc_prio_io_29_Kprio_io_defined_28_pc_sync_io_29_Ksync_io_defined_28_pc_sock_maxbuf_29_Ksock_maxbuf__29_29;
#endif
#if defined(HAVE_SYSCONF)
  gcv_object_t _object___28or_20integer_20_28member_defined_28_sc_aio_listio_max_29_Kaio_listio_max_defined_28_sc_aio_max_29_Kaio_max_defined_28_sc_aio_prio_delta_max_29_Kaio_prio_delta_max_defined_28_sc_arg_max_29_Karg_max_defined_28_sc_atexit_max_29_Katexit_max_defined_28_sc_bc_base_max_29_Kbc_base_max_defined_28_sc_bc_dim_max_29_Kbc_dim_max_defined_28_sc_bc_scale_max_29_Kbc_scale_max_defined_28_sc_bc_string_max_29_Kbc_string_max_defined_28_sc_child_max_29_Kchild_max_defined_28_sc_clk_tck_29_Kclk_tck_defined_28_sc_coll_weights_max_29_Kcoll_weights_max_defined_28_sc_delaytimer_max_29_Kdelaytimer_max_defined_28_sc_expr_nest_max_29_Kexpr_nest_max_defined_28_sc_host_name_max_29_Khost_name_max_defined_28_sc_iov_max_29_Kiov_max_defined_28_sc_line_max_29_Kline_max_defined_28_sc_login_name_max_29_Klogin_name_max_defined_28_sc_ngroups_max_29_Kngroups_max_defined_28_sc_getgr_r_size_max_29_Kgetgr_r_size_max_defined_28_sc_getpw_r_size_max_29_Kgetpw_r_size_max_defined_28_sc_mq_open_max_29_Kmq_open_max_defined_28_sc_mq_prio_max_29_Kmq_prio_max_defined_28_sc_open_max_29_Kopen_max_defined_28_sc_advisory_info_29_Kadvisory_info_defined_28_sc_barriers_29_Kbarriers_defined_28_sc_asynchronous_io_29_Kasynchronous_io_defined_28_sc_clock_selection_29_Kclock_selection_defined_28_sc_cputime_29_Kcputime_defined_28_sc_fsync_29_Kfsync_defined_28_sc_ipv6_29_Kipv6_defined_28_sc_job_control_29_Kjob_control_defined_28_sc_mapped_files_29_Kmapped_files_defined_28_sc_memlock_29_Kmemlock_defined_28_sc_memlock_range_29_Kmemlock_range_defined_28_sc_memory_protection_29_Kmemory_protection_defined_28_sc_message_passing_29_Kmessage_passing_defined_28_sc_monotonic_clock_29_Kmonotonic_clock_defined_28_sc_prioritized_io_29_Kprioritized_io_defined_28_sc_priority_scheduling_29_Kpriority_scheduling_defined_28_sc_raw_sockets_29_Kraw_sockets_defined_28_sc_reader_writer_locks_29_Kreader_writer_locks_defined_28_sc_realtime_signals_29_Krealtime_signals_defined_28_sc_regexp_29_Kregexp_defined_28_sc_saved_ids_29_Ksaved_ids_defined_28;
#endif
#if defined(HAVE_SYSLOG)
  gcv_object_t _object___28or_20null_20integer_20_28member_defined_28log_kern_29_Kkern_defined_28log_user_29_Kuser_defined_28log_mail_29_Kmail_defined_28log_news_29_Knews_defined_28log_uucp_29_Kuucp_defined_28log_daemon_29_Kdaemon_defined_28log_auth_29_Kauth_defined_28log_cron_29_Kcron_defined_28log_lpr_29_Klpr_defined_28log_syslog_29_Ksyslog_defined_28log_authpriv_29_Kauthpriv_defined_28log_ftp_29_Kftp_defined_28log_local0_29_Klocal0_defined_28log_local1_29_Klocal1_defined_28log_local2_29_Klocal2_defined_28log_local3_29_Klocal3_defined_28log_local4_29_Klocal4_defined_28log_local5_29_Klocal5_defined_28log_local6_29_Klocal6_defined_28log_local7_29_Klocal7__29_29;
#endif
  gcv_object_t _object___28or_20null_20integer_20_28member_defined_28prio_process_29_Kprocess_defined_28prio_pgrp_29_Kpgrp_defined_28prio_user_29_Kuser__29_29;
#if defined(WIN32_NATIVE)
  gcv_object_t _object___28or_20null_20integer_20_28member_defined_28realtime_priority_class_29_Krealtime_defined_28high_priority_class_29_Khigh_defined_28above_normal_priority_class_29_Kabove_normal_defined_28normal_priority_class_29_Knormal_defined_28below_normal_priority_class_29_Kbelow_normal_defined_28low_priority_class_29_Klow_defined_28idle_priority_class_29_Kidle__29_29;
#endif
#if !(defined(WIN32_NATIVE))
  gcv_object_t _object___28or_20null_20integer_20_28member_Krealtime_Khigh_Kabove_normal_Knormal_Kbelow_normal_Klow_Kidle__29_29;
#endif
} module__syscalls__object_tab;
uintC module__syscalls__object_tab_size = sizeof(module__syscalls__object_tab)/sizeof(gcv_object_t);

struct module__syscalls__object_tab_initdata_t {
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  object_initdata_t _object_cl_decode_universal_time;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD)))
  object_initdata_t _object_cl_member;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_CHAR_TERM))
  object_initdata_t _object_K2_char_term;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_C_BIND))
  object_initdata_t _object_K2_c_bind;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_C_DEV))
  object_initdata_t _object_K2_c_dev;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_FORT_DEV))
  object_initdata_t _object_K2_fort_dev;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_FORT_RUN))
  object_initdata_t _object_K2_fort_run;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_LOCALEDEF))
  object_initdata_t _object_K2_localedef;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS))
  object_initdata_t _object_K2_pbs;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_ACCOUNTING))
  object_initdata_t _object_K2_pbs_accounting;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_CHECKPOINT))
  object_initdata_t _object_K2_pbs_checkpoint;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_LOCATE))
  object_initdata_t _object_K2_pbs_locate;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_MESSAGE))
  object_initdata_t _object_K2_pbs_message;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_TRACK))
  object_initdata_t _object_K2_pbs_track;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_SW_DEV))
  object_initdata_t _object_K2_sw_dev;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_2_SYMLINKS))
  object_initdata_t _object_K2_symlinks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_UPE))
  object_initdata_t _object_K2_upe;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_VERSION))
  object_initdata_t _object_K2_version;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(ABOVE_NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  object_initdata_t _object_Kabove_normal;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(ACCOUNTING))
  object_initdata_t _object_Kaccounting;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ADVISORY_INFO))
  object_initdata_t _object_Kadvisory_info;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_LISTIO_MAX))
  object_initdata_t _object_Kaio_listio_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_MAX))
  object_initdata_t _object_Kaio_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_PRIO_DELTA_MAX))
  object_initdata_t _object_Kaio_prio_delta_max;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_ALERT))
  object_initdata_t _object_Kalert;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_ALLOC_SIZE_MIN))
  object_initdata_t _object_Kalloc_size_min;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kalpha;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kalt;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_APPEND))
  object_initdata_t _object_Kappend;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kappname;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_ARCHIVE))
  object_initdata_t _object_Karchive;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Karguments;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ARG_MAX))
  object_initdata_t _object_Karg_max;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_AS))
  object_initdata_t _object_Kas;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ASYNCHRONOUS_IO))
  object_initdata_t _object_Kasynchronous_io;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_ASYNC_IO))
  object_initdata_t _object_Kasync_io;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ATEXIT_MAX))
  object_initdata_t _object_Katexit_max;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  object_initdata_t _object_Katime;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_AUTH))
  object_initdata_t _object_Kauth;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kauthor;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_AUTHPRIV))
  object_initdata_t _object_Kauthpriv;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AVPHYS_PAGES))
  object_initdata_t _object_Kavphys_pages;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kbackoffice;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BARRIERS))
  object_initdata_t _object_Kbarriers;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_BASE_MAX))
  object_initdata_t _object_Kbc_base_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_DIM_MAX))
  object_initdata_t _object_Kbc_dim_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_SCALE_MAX))
  object_initdata_t _object_Kbc_scale_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_STRING_MAX))
  object_initdata_t _object_Kbc_string_max;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(BELOW_NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  object_initdata_t _object_Kbelow_normal;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_BINARY))
  object_initdata_t _object_Kbinary;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kblob;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kblock;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kbool;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(BOOT_TIME))
  object_initdata_t _object_Kboot_time;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kbstr;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kbuffered;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kbuilt_in;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kbyref;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kcharcount;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CHILD_MAX))
  object_initdata_t _object_Kchild_max;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_CHOWN_RESTRICTED))
  object_initdata_t _object_Kchown_restricted;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kclipboard_format;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CLK_TCK))
  object_initdata_t _object_Kclk_tck;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CLOCK_SELECTION))
  object_initdata_t _object_Kclock_selection;
#endif
#if (defined(HAVE_FCNTL)) && (defined(FD_CLOEXEC))
  object_initdata_t _object_Kcloexec;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kcodepage;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_COLL_WEIGHTS_MAX))
  object_initdata_t _object_Kcoll_weights_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kcomments;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_COMPRESSED))
  object_initdata_t _object_Kcompressed;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  object_initdata_t _object_Kcons;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kcontrol;
#endif
  object_initdata_t _object_Kcopy;
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_CORE))
  object_initdata_t _object_Kcore;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_CPU))
  object_initdata_t _object_Kcpu;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CPUTIME))
  object_initdata_t _object_Kcputime;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_CREAT))
  object_initdata_t _object_Kcreat;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kcreate_dtm;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_CRIT))
  object_initdata_t _object_Kcrit;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_CRON))
  object_initdata_t _object_Kcron;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kcy;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_DAEMON))
  object_initdata_t _object_Kdaemon;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_DATA))
  object_initdata_t _object_Kdata;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kdatacenter;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kdate;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(DEAD_PROCESS))
  object_initdata_t _object_Kdead_process;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_DEBUG))
  object_initdata_t _object_Kdebug;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_DELAYTIMER_MAX))
  object_initdata_t _object_Kdelaytimer_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kdescription;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_DEVICE))
  object_initdata_t _object_Kdevice;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_DIRECT))
  object_initdata_t _object_Kdirect;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kdirection;
#endif
#if ((defined(HAVE_FCNTL)) && (defined(O_DIRECTORY))) || ((defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_DIRECTORY)))
  object_initdata_t _object_Kdirectory;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kdoc_security;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kdomain_controller;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kedittime;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kelement_type;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_EMERG))
  object_initdata_t _object_Kemerg;
#endif
#if ((defined(HAVE_UTMPX_H)) && (defined(EMPTY))) || (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32))
  object_initdata_t _object_Kempty;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_ENCRYPTED))
  object_initdata_t _object_Kencrypted;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kenterprise;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_ERR))
  object_initdata_t _object_Kerr;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kerror;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_EXCL))
  object_initdata_t _object_Kexcl;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_EXPR_NEST_MAX))
  object_initdata_t _object_Kexpr_nest_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kext;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kexternal_format;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  object_initdata_t _object_Kfacility;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFBLK))
  object_initdata_t _object_Kfblk;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFCHR))
  object_initdata_t _object_Kfchr;
#endif
#if (defined(HAVE_FCNTL)) && (defined(F_GETFD))
  object_initdata_t _object_Kfd;
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFDIR))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_MKDIR))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_CREAT)))
  object_initdata_t _object_Kfdir;
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFIFO))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_MKFIFO)))
  object_initdata_t _object_Kfifo;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_FILESIZEBITS))
  object_initdata_t _object_Kfilesizebits;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kfiletime;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_NAMED_STREAMS))
  object_initdata_t _object_Kfile_named_streams;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_READ_ONLY_VOLUME))
  object_initdata_t _object_Kfile_read_only_volume;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_ENCRYPTION))
  object_initdata_t _object_Kfile_supports_encryption;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_OBJECT_IDS))
  object_initdata_t _object_Kfile_supports_object_ids;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_REPARSE_POINTS))
  object_initdata_t _object_Kfile_supports_reparse_points;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_SPARSE_FILES))
  object_initdata_t _object_Kfile_supports_sparse_files;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_VOLUME_QUOTAS))
  object_initdata_t _object_Kfile_volume_quotas;
#endif
#if (defined(HAVE_FCNTL)) && (defined(F_GETFL))
  object_initdata_t _object_Kfl;
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFREG))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_CREAT)))
  object_initdata_t _object_Kfreg;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_FSIZE))
  object_initdata_t _object_Kfsize;
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFSOCK))
  object_initdata_t _object_Kfsock;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_FSYNC))
  object_initdata_t _object_Kfsync;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_CASE_IS_PRESERVED))
  object_initdata_t _object_Kfs_case_is_preserved;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_CASE_SENSITIVE))
  object_initdata_t _object_Kfs_case_sensitive;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_FILE_COMPRESSION))
  object_initdata_t _object_Kfs_file_compression;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_FILE_ENCRYPTION))
  object_initdata_t _object_Kfs_file_encryption;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_PERSISTENT_ACLS))
  object_initdata_t _object_Kfs_persistent_acls;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_UNICODE_STORED_ON_DISK))
  object_initdata_t _object_Kfs_unicode_stored_on_disk;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_VOL_IS_COMPRESSED))
  object_initdata_t _object_Kfs_vol_is_compressed;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_FTP))
  object_initdata_t _object_Kftp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_GETGR_R_SIZE_MAX))
  object_initdata_t _object_Kgetgr_r_size_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_GETPW_R_SIZE_MAX))
  object_initdata_t _object_Kgetpw_r_size_max;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  object_initdata_t _object_Kgid;
#endif
  object_initdata_t _object_Khardlink;
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_HIDDEN))
  object_initdata_t _object_Khidden;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(HIGH_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  object_initdata_t _object_Khigh;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_HOST_NAME_MAX))
  object_initdata_t _object_Khost_name_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Khot_key;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ki1;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ki2;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ki4;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ki8;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kia64;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kicon;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(IDLE_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  object_initdata_t _object_Kidle;
#endif
  object_initdata_t _object_Kif_does_not_exist;
  object_initdata_t _object_Kif_exists;
#if (defined(HAVE_SYSLOG)) && (defined(LOG_INFO))
  object_initdata_t _object_Kinfo;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kinitid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(INIT_PROCESS))
  object_initdata_t _object_Kinit_process;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kint;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kintel;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_IOV_MAX))
  object_initdata_t _object_Kiov_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_IPV6))
  object_initdata_t _object_Kipv6;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_JOB_CONTROL))
  object_initdata_t _object_Kjob_control;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_KERN))
  object_initdata_t _object_Kkern;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kkeywords;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_LARGEFILE))
  object_initdata_t _object_Klargefile;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Klastauthor;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Klastprinted;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Klastsave_dtm;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  object_initdata_t _object_Klength;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_LINE_MAX))
  object_initdata_t _object_Kline_max;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_LINK_MAX))
  object_initdata_t _object_Klink_max;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL0))
  object_initdata_t _object_Klocal0;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL1))
  object_initdata_t _object_Klocal1;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL2))
  object_initdata_t _object_Klocal2;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL3))
  object_initdata_t _object_Klocal3;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL4))
  object_initdata_t _object_Klocal4;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL5))
  object_initdata_t _object_Klocal5;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL6))
  object_initdata_t _object_Klocal6;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL7))
  object_initdata_t _object_Klocal7;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Klocale;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_LOCKS))
  object_initdata_t _object_Klocks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_LOGIN_NAME_MAX))
  object_initdata_t _object_Klogin_name_max;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(LOGIN_PROCESS))
  object_initdata_t _object_Klogin_process;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(LOW_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  object_initdata_t _object_Klow;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LPR))
  object_initdata_t _object_Klpr;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Klpstr;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Klpwstr;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_MAIL))
  object_initdata_t _object_Kmail;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MAPPED_FILES))
  object_initdata_t _object_Kmapped_files;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kmax;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_MAX_CANON))
  object_initdata_t _object_Kmax_canon;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_MAX_INPUT))
  object_initdata_t _object_Kmax_input;
#endif
#if ((defined(HAVE_SYSCONF)) && (defined(_SC_MEMLOCK))) || ((defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_MEMLOCK)))
  object_initdata_t _object_Kmemlock;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MEMLOCK_RANGE))
  object_initdata_t _object_Kmemlock_range;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MEMORY_PROTECTION))
  object_initdata_t _object_Kmemory_protection;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MESSAGE_PASSING))
  object_initdata_t _object_Kmessage_passing;
#endif
  object_initdata_t _object_Kmethod;
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kmin;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kmips;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  object_initdata_t _object_Kmode;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MONOTONIC_CLOCK))
  object_initdata_t _object_Kmonotonic_clock;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MQ_OPEN_MAX))
  object_initdata_t _object_Kmq_open_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MQ_PRIO_MAX))
  object_initdata_t _object_Kmq_prio_max;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  object_initdata_t _object_Kmtime;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_NAME_MAX))
  object_initdata_t _object_Kname_max;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  object_initdata_t _object_Kndelay;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_NEWS))
  object_initdata_t _object_Knews;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(NEW_TIME))
  object_initdata_t _object_Knew_time;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NGROUPS_MAX))
  object_initdata_t _object_Kngroups_max;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOCTTY))
  object_initdata_t _object_Knoctty;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_NOFILE))
  object_initdata_t _object_Knofile;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOFOLLOW))
  object_initdata_t _object_Knofollow;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOINHERIT))
  object_initdata_t _object_Knoinherit;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NONBLOCK))
  object_initdata_t _object_Knonblock;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE))) || ((defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_NORMAL)))
  object_initdata_t _object_Knormal;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_NOTICE))
  object_initdata_t _object_Knotice;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Knotimplemented;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED))
  object_initdata_t _object_Knot_content_indexed;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  object_initdata_t _object_Knowait;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_NO_TRUNC))
  object_initdata_t _object_Kno_trunc;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_NPROC))
  object_initdata_t _object_Knproc;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NPROCESSORS_CONF))
  object_initdata_t _object_Knprocessors_conf;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NPROCESSORS_ONLN))
  object_initdata_t _object_Knprocessors_onln;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Knt;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Knull;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  object_initdata_t _object_Kodelay;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_OFFLINE))
  object_initdata_t _object_Koffline;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(OLD_TIME))
  object_initdata_t _object_Kold_time;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_OPEN_MAX))
  object_initdata_t _object_Kopen_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kpagecount;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PAGESIZE))
  object_initdata_t _object_Kpagesize;
#endif
#if ((defined(HAVE_CONFSTR)) && (defined(_CS_PATH))) || (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32))
  object_initdata_t _object_Kpath;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PATH_MAX))
  object_initdata_t _object_Kpath_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kpersonal;
#endif
#if defined(PRIO_PGRP)
  object_initdata_t _object_Kpgrp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PHYS_PAGES))
  object_initdata_t _object_Kphys_pages;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  object_initdata_t _object_Kpid;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PIPE_BUF))
  object_initdata_t _object_Kpipe_buf;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_CFLAGS))
  object_initdata_t _object_Kposix_v6_ilp32_off32_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_LDFLAGS))
  object_initdata_t _object_Kposix_v6_ilp32_off32_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_LIBS))
  object_initdata_t _object_Kposix_v6_ilp32_off32_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS))
  object_initdata_t _object_Kposix_v6_ilp32_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS))
  object_initdata_t _object_Kposix_v6_ilp32_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_LIBS))
  object_initdata_t _object_Kposix_v6_ilp32_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_CFLAGS))
  object_initdata_t _object_Kposix_v6_lp64_off64_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_LDFLAGS))
  object_initdata_t _object_Kposix_v6_lp64_off64_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_LIBS))
  object_initdata_t _object_Kposix_v6_lp64_off64_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS))
  object_initdata_t _object_Kposix_v6_lpbig_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS))
  object_initdata_t _object_Kposix_v6_lpbig_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_LIBS))
  object_initdata_t _object_Kposix_v6_lpbig_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS))
  object_initdata_t _object_Kposix_v6_width_restricted_envs;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kppc;
#endif
  object_initdata_t _object_Kpreserve;
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PRIORITIZED_IO))
  object_initdata_t _object_Kprioritized_io;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PRIORITY_SCHEDULING))
  object_initdata_t _object_Kpriority_scheduling;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PRIO_IO))
  object_initdata_t _object_Kprio_io;
#endif
#if defined(PRIO_PROCESS)
  object_initdata_t _object_Kprocess;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kr4;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kr8;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RAW_SOCKETS))
  object_initdata_t _object_Kraw_sockets;
#endif
#if defined(HAVE_FCNTL)
  object_initdata_t _object_Krdonly;
#endif
#if defined(HAVE_FCNTL)
  object_initdata_t _object_Krdwr;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_READER_WRITER_LOCKS))
  object_initdata_t _object_Kreader_writer_locks;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_READONLY))
  object_initdata_t _object_Kreadonly;
#endif
#if ((defined(WIN32_NATIVE)) && (defined(REALTIME_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  object_initdata_t _object_Krealtime;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_REALTIME_SIGNALS))
  object_initdata_t _object_Krealtime_signals;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_INCR_XFER_SIZE))
  object_initdata_t _object_Krec_incr_xfer_size;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_MAX_XFER_SIZE))
  object_initdata_t _object_Krec_max_xfer_size;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_MIN_XFER_SIZE))
  object_initdata_t _object_Krec_min_xfer_size;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_XFER_ALIGN))
  object_initdata_t _object_Krec_xfer_align;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_REGEXP))
  object_initdata_t _object_Kregexp;
#endif
  object_initdata_t _object_Krename;
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_REPARSE_POINT))
  object_initdata_t _object_Kreparse_point;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Krevnumber;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RE_DUP_MAX))
  object_initdata_t _object_Kre_dup_max;
#endif
#if defined(S_IRGRP)
  object_initdata_t _object_Krgrp;
#endif
#if defined(S_IROTH)
  object_initdata_t _object_Kroth;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_RSS))
  object_initdata_t _object_Krss;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RTSIG_MAX))
  object_initdata_t _object_Krtsig_max;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(RUN_LVL))
  object_initdata_t _object_Krun_lvl;
#endif
#if defined(S_IRUSR)
  object_initdata_t _object_Krusr;
#endif
#if defined(S_IRWXG)
  object_initdata_t _object_Krwxg;
#endif
#if defined(S_IRWXO)
  object_initdata_t _object_Krwxo;
#endif
#if defined(S_IRWXU)
  object_initdata_t _object_Krwxu;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SAVED_IDS))
  object_initdata_t _object_Ksaved_ids;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEMAPHORES))
  object_initdata_t _object_Ksemaphores;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEM_NSEMS_MAX))
  object_initdata_t _object_Ksem_nsems_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEM_VALUE_MAX))
  object_initdata_t _object_Ksem_value_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kserver;
#endif
#if defined(S_ISGID)
  object_initdata_t _object_Ksgid;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kshared;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SHARED_MEMORY_OBJECTS))
  object_initdata_t _object_Kshared_memory_objects;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SHELL))
  object_initdata_t _object_Kshell;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kshift;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kshow_command;
#endif
#if defined(SIGABRT)
  object_initdata_t _object_Ksigabrt;
#endif
#if defined(SIGALRM)
  object_initdata_t _object_Ksigalrm;
#endif
#if defined(SIGBUS)
  object_initdata_t _object_Ksigbus;
#endif
#if defined(SIGCHLD)
  object_initdata_t _object_Ksigchld;
#endif
#if defined(SIGCONT)
  object_initdata_t _object_Ksigcont;
#endif
#if defined(SIGFPE)
  object_initdata_t _object_Ksigfpe;
#endif
#if defined(SIGHUP)
  object_initdata_t _object_Ksighup;
#endif
#if defined(SIGILL)
  object_initdata_t _object_Ksigill;
#endif
#if defined(SIGINT)
  object_initdata_t _object_Ksigint;
#endif
#if defined(SIGKILL)
  object_initdata_t _object_Ksigkill;
#endif
#if defined(SIGPIPE)
  object_initdata_t _object_Ksigpipe;
#endif
#if defined(SIGPOLL)
  object_initdata_t _object_Ksigpoll;
#endif
#if defined(SIGPROF)
  object_initdata_t _object_Ksigprof;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SIGQUEUE_MAX))
  object_initdata_t _object_Ksigqueue_max;
#endif
#if defined(SIGQUIT)
  object_initdata_t _object_Ksigquit;
#endif
#if defined(SIGSEGV)
  object_initdata_t _object_Ksigsegv;
#endif
#if defined(SIGSTOP)
  object_initdata_t _object_Ksigstop;
#endif
#if defined(SIGSYS)
  object_initdata_t _object_Ksigsys;
#endif
#if defined(SIGTERM)
  object_initdata_t _object_Ksigterm;
#endif
#if defined(SIGTRAP)
  object_initdata_t _object_Ksigtrap;
#endif
#if defined(SIGTSTP)
  object_initdata_t _object_Ksigtstp;
#endif
#if defined(SIGTTIN)
  object_initdata_t _object_Ksigttin;
#endif
#if defined(SIGTTOU)
  object_initdata_t _object_Ksigttou;
#endif
#if defined(SIGURG)
  object_initdata_t _object_Ksigurg;
#endif
#if defined(SIGUSR1)
  object_initdata_t _object_Ksigusr1;
#endif
#if defined(SIGUSR2)
  object_initdata_t _object_Ksigusr2;
#endif
#if defined(SIGVTALRM)
  object_initdata_t _object_Ksigvtalrm;
#endif
#if defined(SIGXCPU)
  object_initdata_t _object_Ksigxcpu;
#endif
#if defined(SIGXFSZ)
  object_initdata_t _object_Ksigxfsz;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ksmallbusiness;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ksmallbusiness_restricted;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SOCK_MAXBUF))
  object_initdata_t _object_Ksock_maxbuf;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_SPARSE_FILE))
  object_initdata_t _object_Ksparse_file;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPAWN))
  object_initdata_t _object_Kspawn;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPIN_LOCKS))
  object_initdata_t _object_Kspin_locks;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPORADIC_SERVER))
  object_initdata_t _object_Ksporadic_server;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SS_REPL_MAX))
  object_initdata_t _object_Kss_repl_max;
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_STACK))
  object_initdata_t _object_Kstack;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  object_initdata_t _object_Kstart;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_STREAM_MAX))
  object_initdata_t _object_Kstream_max;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_APPEND))
  object_initdata_t _object_Kst_append;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_IMMUTABLE))
  object_initdata_t _object_Kst_immutable;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_MANDLOCK))
  object_initdata_t _object_Kst_mandlock;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOATIME))
  object_initdata_t _object_Kst_noatime;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NODEV))
  object_initdata_t _object_Kst_nodev;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NODIRATIME))
  object_initdata_t _object_Kst_nodiratime;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOEXEC))
  object_initdata_t _object_Kst_noexec;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOSUID))
  object_initdata_t _object_Kst_nosuid;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOTRUNC))
  object_initdata_t _object_Kst_notrunc;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_RDONLY))
  object_initdata_t _object_Kst_rdonly;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_SYNCHRONOUS))
  object_initdata_t _object_Kst_synchronous;
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_WRITE))
  object_initdata_t _object_Kst_write;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ksubject;
#endif
#if defined(S_ISUID)
  object_initdata_t _object_Ksuid;
#endif
#if defined(S_ISVTX)
  object_initdata_t _object_Ksvtx;
#endif
  object_initdata_t _object_Ksymlink;
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SYMLINK_MAX))
  object_initdata_t _object_Ksymlink_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SYMLOOP_MAX))
  object_initdata_t _object_Ksymloop_max;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_SYNC))
  object_initdata_t _object_Ksync;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SYNCHRONIZED_IO))
  object_initdata_t _object_Ksynchronized_io;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SYNC_IO))
  object_initdata_t _object_Ksync_io;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_SYSLOG))
  object_initdata_t _object_Ksyslog;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_SYSTEM))
  object_initdata_t _object_Ksystem;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ktemplate;
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_TEMPORARY))
  object_initdata_t _object_Ktemporary;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kterminal;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_TEXT))
  object_initdata_t _object_Ktext;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREADS))
  object_initdata_t _object_Kthreads;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_ATTR_STACKADDR))
  object_initdata_t _object_Kthread_attr_stackaddr;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_ATTR_STACKSIZE))
  object_initdata_t _object_Kthread_attr_stacksize;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_CPUTIME))
  object_initdata_t _object_Kthread_cputime;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_DESTRUCTOR_ITERATIONS))
  object_initdata_t _object_Kthread_destructor_iterations;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_KEYS_MAX))
  object_initdata_t _object_Kthread_keys_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIORITY_SCHEDULING))
  object_initdata_t _object_Kthread_priority_scheduling;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIO_INHERIT))
  object_initdata_t _object_Kthread_prio_inherit;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIO_PROTECT))
  object_initdata_t _object_Kthread_prio_protect;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PROCESS_SHARED))
  object_initdata_t _object_Kthread_process_shared;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_SAFE_FUNCTIONS))
  object_initdata_t _object_Kthread_safe_functions;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_SPORADIC_SERVER))
  object_initdata_t _object_Kthread_sporadic_server;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_STACK_MIN))
  object_initdata_t _object_Kthread_stack_min;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_THREADS_MAX))
  object_initdata_t _object_Kthread_threads_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kthumbnail;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMEOUTS))
  object_initdata_t _object_Ktimeouts;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMERS))
  object_initdata_t _object_Ktimers;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMER_MAX))
  object_initdata_t _object_Ktimer_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Ktitle;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE))
  object_initdata_t _object_Ktrace;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_EVENT_FILTER))
  object_initdata_t _object_Ktrace_event_filter;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_EVENT_NAME_MAX))
  object_initdata_t _object_Ktrace_event_name_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_INHERIT))
  object_initdata_t _object_Ktrace_inherit;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_LOG))
  object_initdata_t _object_Ktrace_log;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_NAME_MAX))
  object_initdata_t _object_Ktrace_name_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_SYS_MAX))
  object_initdata_t _object_Ktrace_sys_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_USER_EVENT_MAX))
  object_initdata_t _object_Ktrace_user_event_max;
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_TRUNC))
  object_initdata_t _object_Ktrunc;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TTY_NAME_MAX))
  object_initdata_t _object_Ktty_name_max;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TYPED_MEMORY_OBJECTS))
  object_initdata_t _object_Ktyped_memory_objects;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TZNAME_MAX))
  object_initdata_t _object_Ktzname_max;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kui1;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kui2;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kui4;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kui8;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  object_initdata_t _object_Kuid;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kuint;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kunknown;
#endif
#if ((defined(HAVE_SYSLOG)) && (defined(LOG_USER))) || (defined(PRIO_USER))
  object_initdata_t _object_Kuser;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kuser_defined;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(USER_PROCESS))
  object_initdata_t _object_Kuser_process;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_UUCP))
  object_initdata_t _object_Kuucp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_ILP32_OFF32))
  object_initdata_t _object_Kv6_ilp32_off32;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_ILP32_OFFBIG))
  object_initdata_t _object_Kv6_ilp32_offbig;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_LP64_OFF64))
  object_initdata_t _object_Kv6_lp64_off64;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_LPBIG_OFFBIG))
  object_initdata_t _object_Kv6_lpbig_offbig;
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_VDISABLE))
  object_initdata_t _object_Kvdisable;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_VERSION))
  object_initdata_t _object_Kversion;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_WARNING))
  object_initdata_t _object_Kwarning;
#endif
#if defined(S_IWGRP)
  object_initdata_t _object_Kwgrp;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kwindows;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kwordcount;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kworking_directory;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_Kworkstation;
#endif
#if defined(S_IWOTH)
  object_initdata_t _object_Kwoth;
#endif
#if defined(HAVE_FCNTL)
  object_initdata_t _object_Kwronly;
#endif
#if defined(S_IWUSR)
  object_initdata_t _object_Kwusr;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_ILP32_OFF32))
  object_initdata_t _object_Kxbs5_ilp32_off32;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_CFLAGS))
  object_initdata_t _object_Kxbs5_ilp32_off32_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LDFLAGS))
  object_initdata_t _object_Kxbs5_ilp32_off32_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LIBS))
  object_initdata_t _object_Kxbs5_ilp32_off32_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LINTFLAGS))
  object_initdata_t _object_Kxbs5_ilp32_off32_lintflags;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_ILP32_OFFBIG))
  object_initdata_t _object_Kxbs5_ilp32_offbig;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_CFLAGS))
  object_initdata_t _object_Kxbs5_ilp32_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LDFLAGS))
  object_initdata_t _object_Kxbs5_ilp32_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LIBS))
  object_initdata_t _object_Kxbs5_ilp32_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LINTFLAGS))
  object_initdata_t _object_Kxbs5_ilp32_offbig_lintflags;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_LP64_OFF64))
  object_initdata_t _object_Kxbs5_lp64_off64;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_CFLAGS))
  object_initdata_t _object_Kxbs5_lp64_off64_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LDFLAGS))
  object_initdata_t _object_Kxbs5_lp64_off64_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LIBS))
  object_initdata_t _object_Kxbs5_lp64_off64_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LINTFLAGS))
  object_initdata_t _object_Kxbs5_lp64_off64_lintflags;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_LPBIG_OFFBIG))
  object_initdata_t _object_Kxbs5_lpbig_offbig;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_CFLAGS))
  object_initdata_t _object_Kxbs5_lpbig_offbig_cflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LDFLAGS))
  object_initdata_t _object_Kxbs5_lpbig_offbig_ldflags;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LIBS))
  object_initdata_t _object_Kxbs5_lpbig_offbig_libs;
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LINTFLAGS))
  object_initdata_t _object_Kxbs5_lpbig_offbig_lintflags;
#endif
#if defined(S_IXGRP)
  object_initdata_t _object_Kxgrp;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_CRYPT))
  object_initdata_t _object_Kxopen_crypt;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_ENH_I18N))
  object_initdata_t _object_Kxopen_enh_i18n;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_LEGACY))
  object_initdata_t _object_Kxopen_legacy;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_REALTIME))
  object_initdata_t _object_Kxopen_realtime;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_REALTIME_THREADS))
  object_initdata_t _object_Kxopen_realtime_threads;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_SHM))
  object_initdata_t _object_Kxopen_shm;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_STREAMS))
  object_initdata_t _object_Kxopen_streams;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_UNIX))
  object_initdata_t _object_Kxopen_unix;
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_VERSION))
  object_initdata_t _object_Kxopen_version;
#endif
#if defined(S_IXOTH)
  object_initdata_t _object_Kxoth;
#endif
#if defined(S_IXUSR)
  object_initdata_t _object_Kxusr;
#endif
  object_initdata_t _object_posix__copy_file;
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_posix__make_file_info;
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  object_initdata_t _object_posix__make_file_stat;
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  object_initdata_t _object_posix__make_group_info;
#endif
  object_initdata_t _object_posix__make_hostent;
#if defined(HAVE_GETRLIMIT)
  object_initdata_t _object_posix__make_rlimit;
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  object_initdata_t _object_posix__make_service;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_posix__make_shortcut_info;
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  object_initdata_t _object_posix__make_stat_vfs;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_posix__make_system_info;
#endif
#if defined(HAVE_UNAME)
  object_initdata_t _object_posix__make_uname;
#endif
#if defined(HAVE_GETRUSAGE)
  object_initdata_t _object_posix__make_usage;
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  object_initdata_t _object_posix__make_user_info;
#endif
#if defined(HAVE_UTMPX_H)
  object_initdata_t _object_posix__make_utmpx;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_posix__make_version;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object_posix__mkmemstat;
#endif
  object_initdata_t _object_posix__resolve_host_ipaddr;
#if defined(HAVE_SETRLIMIT)
  object_initdata_t _object_posix__rlimit;
#endif
#if defined(HAVE_UTMPX_H)
  object_initdata_t _object_posix__utmpx;
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!(defined(WIN32_NATIVE))) && (!(defined(HAVE_CHMOD)))
  object_initdata_t _object__22chmod_28_29_22;
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!(defined(HAVE_CHOWN)))
  object_initdata_t _object__22chown_28_29_22;
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!defined(WIN32_NATIVE)) && (!(defined(HAVE_UTIME)))
  object_initdata_t _object__22utime_28_29_22;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object__28member_20_alt_20_control_20_ext_20_shift_29;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object__28member_20_normal_20_max_20_min_29;
#endif
  object_initdata_t _object__28or_20null_20integer_20_28member_20_copy_20_symlink_20_hardlink_20_rename_29_29;
#if defined(HAVE_ENCRYPT) || defined(HAVE_SETKEY)
  object_initdata_t _object__28vector_20_28unsigned_byte_208_29_208_29;
#endif
#if defined(HAVE_UTMPX_H)
  object_initdata_t _object___28or_20integer_20_28member_defined_28empty_29_Kempty_defined_28run_lvl_29_Krun_lvl_defined_28boot_time_29_Kboot_time_defined_28old_time_29_Kold_time_defined_28new_time_29_Knew_time_defined_28user_process_29_Kuser_process_defined_28init_process_29_Kinit_process_defined_28login_process_29_Klogin_process_defined_28dead_process_29_Kdead_process_defined_28accounting_29_Kaccounting__29_29;
#endif
#if defined(HAVE_FCNTL)
  object_initdata_t _object___28or_20integer_20_28member_defined_28fd_cloexec_29_Kcloexec__29_29;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  object_initdata_t _object___28or_20integer_20_28member_defined_28file_attribute_archive_29_Karchive_defined_28file_attribute_compressed_29_Kcompressed_defined_28file_attribute_device_29_Kdevice_defined_28file_attribute_directory_29_Kdirectory_defined_28file_attribute_encrypted_29_Kencrypted_defined_28file_attribute_hidden_29_Khidden_defined_28file_attribute_normal_29_Knormal_defined_28file_attribute_not_content_indexed_29_Knot_content_indexed_defined_28file_attribute_offline_29_Koffline_defined_28file_attribute_readonly_29_Kreadonly_defined_28file_attribute_reparse_point_29_Kreparse_point_defined_28file_attribute_sparse_file_29_Ksparse_file_defined_28file_attribute_system_29_Ksystem_defined_28file_attribute_temporary_29_Ktemporary__29_29;
#endif
#if defined(HAVE_FCNTL)
  object_initdata_t _object___28or_20integer_20_28member_defined_28f_getfd_29_Kfd_defined_28f_getfl_29_Kfl__29_29;
#endif
#if defined(HAVE_SYSLOG)
  object_initdata_t _object___28or_20integer_20_28member_defined_28log_emerg_29_Kemerg_defined_28log_alert_29_Kalert_defined_28log_crit_29_Kcrit_defined_28log_err_29_Kerr_defined_28log_warning_29_Kwarning_defined_28log_notice_29_Knotice_defined_28log_info_29_Kinfo_defined_28log_debug_29_Kdebug__29_29;
#endif
#if defined(HAVE_FCNTL)
  object_initdata_t _object___28or_20integer_20_28member_defined_28o_rdonly_29_Krdonly_defined_28o_wronly_29_Kwronly_defined_28o_rdwr_29_Krdwr_defined_28o_append_29_Kappend_defined_28o_creat_29_Kcreat_defined_28o_trunc_29_Ktrunc_defined_28o_excl_29_Kexcl_defined_28o_noctty_29_Knoctty_defined_28o_sync_29_Ksync_defined_28o_nonblock_29_Knonblock_defined_28o_binary_29_Kbinary_defined_28o_text_29_Ktext_defined_28o_noinherit_29_Knoinherit_defined_28o_direct_29_Kdirect_defined_28o_largefile_29_Klargefile_defined_28o_directory_29_Kdirectory_defined_28o_nofollow_29_Knofollow__29_29;
#endif
#if defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)
  object_initdata_t _object___28or_20integer_20_28member_defined_28rlimit_cpu_29_Kcpu_defined_28rlimit_fsize_29_Kfsize_defined_28rlimit_data_29_Kdata_defined_28rlimit_stack_29_Kstack_defined_28rlimit_core_29_Kcore_defined_28rlimit_rss_29_Krss_defined_28rlimit_nofile_29_Knofile_defined_28rlimit_as_29_Kas_defined_28rlimit_nproc_29_Knproc_defined_28rlimit_memlock_29_Kmemlock_defined_28rlimit_locks_29_Klocks__29_29;
#endif
  object_initdata_t _object___28or_20integer_20_28member_defined_28sigabrt_29_Ksigabrt_defined_28sigalrm_29_Ksigalrm_defined_28sigbus_29_Ksigbus_defined_28sigchld_29_Ksigchld_defined_28sigcont_29_Ksigcont_defined_28sigfpe_29_Ksigfpe_defined_28sighup_29_Ksighup_defined_28sigill_29_Ksigill_defined_28sigint_29_Ksigint_defined_28sigkill_29_Ksigkill_defined_28sigpipe_29_Ksigpipe_defined_28sigquit_29_Ksigquit_defined_28sigsegv_29_Ksigsegv_defined_28sigstop_29_Ksigstop_defined_28sigterm_29_Ksigterm_defined_28sigtstp_29_Ksigtstp_defined_28sigttin_29_Ksigttin_defined_28sigttou_29_Ksigttou_defined_28sigusr1_29_Ksigusr1_defined_28sigusr2_29_Ksigusr2_defined_28sigpoll_29_Ksigpoll_defined_28sigprof_29_Ksigprof_defined_28sigsys_29_Ksigsys_defined_28sigtrap_29_Ksigtrap_defined_28sigurg_29_Ksigurg_defined_28sigvtalrm_29_Ksigvtalrm_defined_28sigxcpu_29_Ksigxcpu_defined_28sigxfsz_29_Ksigxfsz__29_29;
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  object_initdata_t _object___28or_20integer_20_28member_defined_28st_rdonly_29_Kst_rdonly_defined_28st_nosuid_29_Kst_nosuid_defined_28st_notrunc_29_Kst_notrunc_defined_28st_nodev_29_Kst_nodev_defined_28st_noexec_29_Kst_noexec_defined_28st_synchronous_29_Kst_synchronous_defined_28st_mandlock_29_Kst_mandlock_defined_28st_write_29_Kst_write_defined_28st_append_29_Kst_append_defined_28st_immutable_29_Kst_immutable_defined_28st_noatime_29_Kst_noatime_defined_28st_nodiratime_29_Kst_nodiratime_defined_28file_named_streams_29_Kfile_named_streams_defined_28file_read_only_volume_29_Kfile_read_only_volume_defined_28file_supports_object_ids_29_Kfile_supports_object_ids_defined_28file_supports_reparse_points_29_Kfile_supports_reparse_points_defined_28file_supports_sparse_files_29_Kfile_supports_sparse_files_defined_28file_volume_quotas_29_Kfile_volume_quotas_defined_28file_supports_encryption_29_Kfile_supports_encryption_defined_28fs_case_is_preserved_29_Kfs_case_is_preserved_defined_28fs_case_sensitive_29_Kfs_case_sensitive_defined_28fs_file_compression_29_Kfs_file_compression_defined_28fs_file_encryption_29_Kfs_file_encryption_defined_28fs_persistent_acls_29_Kfs_persistent_acls_defined_28fs_unicode_stored_on_disk_29_Kfs_unicode_stored_on_disk_defined_28fs_vol_is_compressed_29_Kfs_vol_is_compressed__29_29;
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  object_initdata_t _object___28or_20integer_20_28member_defined_28s_ififo_29_Kfifo_defined_28s_ifsock_29_Kfsock_defined_28s_ifchr_29_Kfchr_defined_28s_ifdir_29_Kfdir_defined_28s_ifblk_29_Kfblk_defined_28s_ifreg_29_Kfreg__29_29;
#endif
  object_initdata_t _object___28or_20integer_20_28member_defined_28s_isuid_29_Ksuid_defined_28s_isgid_29_Ksgid_defined_28s_isvtx_29_Ksvtx_defined_28s_irwxu_29_Krwxu_defined_28s_irusr_29_Krusr_defined_28s_iwusr_29_Kwusr_defined_28s_ixusr_29_Kxusr_defined_28s_irwxg_29_Krwxg_defined_28s_irgrp_29_Krgrp_defined_28s_iwgrp_29_Kwgrp_defined_28s_ixgrp_29_Kxgrp_defined_28s_irwxo_29_Krwxo_defined_28s_iroth_29_Kroth_defined_28s_iwoth_29_Kwoth_defined_28s_ixoth_29_Kxoth__29_29;
#if defined(HAVE_CONFSTR)
  object_initdata_t _object___28or_20integer_20_28member_defined_28_cs_path_29_Kpath_defined_28_cs_posix_v6_ilp32_off32_cflags_29_Kposix_v6_ilp32_off32_cflags_defined_28_cs_posix_v6_ilp32_off32_ldflags_29_Kposix_v6_ilp32_off32_ldflags_defined_28_cs_posix_v6_ilp32_off32_libs_29_Kposix_v6_ilp32_off32_libs_defined_28_cs_posix_v6_ilp32_offbig_cflags_29_Kposix_v6_ilp32_offbig_cflags_defined_28_cs_posix_v6_ilp32_offbig_ldflags_29_Kposix_v6_ilp32_offbig_ldflags_defined_28_cs_posix_v6_ilp32_offbig_libs_29_Kposix_v6_ilp32_offbig_libs_defined_28_cs_posix_v6_lp64_off64_cflags_29_Kposix_v6_lp64_off64_cflags_defined_28_cs_posix_v6_lp64_off64_ldflags_29_Kposix_v6_lp64_off64_ldflags_defined_28_cs_posix_v6_lp64_off64_libs_29_Kposix_v6_lp64_off64_libs_defined_28_cs_posix_v6_lpbig_offbig_cflags_29_Kposix_v6_lpbig_offbig_cflags_defined_28_cs_posix_v6_lpbig_offbig_ldflags_29_Kposix_v6_lpbig_offbig_ldflags_defined_28_cs_posix_v6_lpbig_offbig_libs_29_Kposix_v6_lpbig_offbig_libs_defined_28_cs_posix_v6_width_restricted_envs_29_Kposix_v6_width_restricted_envs_defined_28_cs_xbs5_ilp32_off32_cflags_29_Kxbs5_ilp32_off32_cflags_defined_28_cs_xbs5_ilp32_off32_ldflags_29_Kxbs5_ilp32_off32_ldflags_defined_28_cs_xbs5_ilp32_off32_libs_29_Kxbs5_ilp32_off32_libs_defined_28_cs_xbs5_ilp32_off32_lintflags_29_Kxbs5_ilp32_off32_lintflags_defined_28_cs_xbs5_ilp32_offbig_cflags_29_Kxbs5_ilp32_offbig_cflags_defined_28_cs_xbs5_ilp32_offbig_ldflags_29_Kxbs5_ilp32_offbig_ldflags_defined_28_cs_xbs5_ilp32_offbig_libs_29_Kxbs5_ilp32_offbig_libs_defined_28_cs_xbs5_ilp32_offbig_lintflags_29_Kxbs5_ilp32_offbig_lintflags_defined_28_cs_xbs5_lp64_off64_cflags_29_Kxbs5_lp64_off64_cflags_defined_28_cs_xbs5_lp64_off64_ldflags_29_Kxbs5_lp64_off64_ldflags_defined_28_cs_xbs5_lp64_off64_libs_29_Kxbs5_lp64_off64_libs_defined_28_cs_xbs5_lp64_off64_lintflags_29_Kxbs5_lp64_off64_lintflags_defined_28_cs_xbs5_lpbig_offbig_cflags_29_Kxbs5_lpbig_offbig_cflags_defined_28_cs_xbs5_lpbig_offbig_ldflags_29_Kxbs5_lpbig_offbig_ldflags_defined_28_cs_xbs5_lpbig_off;
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  object_initdata_t _object___28or_20integer_20_28member_defined_28_pc_filesizebits_29_Kfilesizebits_defined_28_pc_link_max_29_Klink_max_defined_28_pc_max_canon_29_Kmax_canon_defined_28_pc_max_input_29_Kmax_input_defined_28_pc_name_max_29_Kname_max_defined_28_pc_path_max_29_Kpath_max_defined_28_pc_pipe_buf_29_Kpipe_buf_defined_28_pc_2_symlinks_29_K2_symlinks_defined_28_pc_alloc_size_min_29_Kalloc_size_min_defined_28_pc_rec_incr_xfer_size_29_Krec_incr_xfer_size_defined_28_pc_rec_max_xfer_size_29_Krec_max_xfer_size_defined_28_pc_rec_min_xfer_size_29_Krec_min_xfer_size_defined_28_pc_rec_xfer_align_29_Krec_xfer_align_defined_28_pc_symlink_max_29_Ksymlink_max_defined_28_pc_chown_restricted_29_Kchown_restricted_defined_28_pc_no_trunc_29_Kno_trunc_defined_28_pc_vdisable_29_Kvdisable_defined_28_pc_async_io_29_Kasync_io_defined_28_pc_prio_io_29_Kprio_io_defined_28_pc_sync_io_29_Ksync_io_defined_28_pc_sock_maxbuf_29_Ksock_maxbuf__29_29;
#endif
#if defined(HAVE_SYSCONF)
  object_initdata_t _object___28or_20integer_20_28member_defined_28_sc_aio_listio_max_29_Kaio_listio_max_defined_28_sc_aio_max_29_Kaio_max_defined_28_sc_aio_prio_delta_max_29_Kaio_prio_delta_max_defined_28_sc_arg_max_29_Karg_max_defined_28_sc_atexit_max_29_Katexit_max_defined_28_sc_bc_base_max_29_Kbc_base_max_defined_28_sc_bc_dim_max_29_Kbc_dim_max_defined_28_sc_bc_scale_max_29_Kbc_scale_max_defined_28_sc_bc_string_max_29_Kbc_string_max_defined_28_sc_child_max_29_Kchild_max_defined_28_sc_clk_tck_29_Kclk_tck_defined_28_sc_coll_weights_max_29_Kcoll_weights_max_defined_28_sc_delaytimer_max_29_Kdelaytimer_max_defined_28_sc_expr_nest_max_29_Kexpr_nest_max_defined_28_sc_host_name_max_29_Khost_name_max_defined_28_sc_iov_max_29_Kiov_max_defined_28_sc_line_max_29_Kline_max_defined_28_sc_login_name_max_29_Klogin_name_max_defined_28_sc_ngroups_max_29_Kngroups_max_defined_28_sc_getgr_r_size_max_29_Kgetgr_r_size_max_defined_28_sc_getpw_r_size_max_29_Kgetpw_r_size_max_defined_28_sc_mq_open_max_29_Kmq_open_max_defined_28_sc_mq_prio_max_29_Kmq_prio_max_defined_28_sc_open_max_29_Kopen_max_defined_28_sc_advisory_info_29_Kadvisory_info_defined_28_sc_barriers_29_Kbarriers_defined_28_sc_asynchronous_io_29_Kasynchronous_io_defined_28_sc_clock_selection_29_Kclock_selection_defined_28_sc_cputime_29_Kcputime_defined_28_sc_fsync_29_Kfsync_defined_28_sc_ipv6_29_Kipv6_defined_28_sc_job_control_29_Kjob_control_defined_28_sc_mapped_files_29_Kmapped_files_defined_28_sc_memlock_29_Kmemlock_defined_28_sc_memlock_range_29_Kmemlock_range_defined_28_sc_memory_protection_29_Kmemory_protection_defined_28_sc_message_passing_29_Kmessage_passing_defined_28_sc_monotonic_clock_29_Kmonotonic_clock_defined_28_sc_prioritized_io_29_Kprioritized_io_defined_28_sc_priority_scheduling_29_Kpriority_scheduling_defined_28_sc_raw_sockets_29_Kraw_sockets_defined_28_sc_reader_writer_locks_29_Kreader_writer_locks_defined_28_sc_realtime_signals_29_Krealtime_signals_defined_28_sc_regexp_29_Kregexp_defined_28_sc_saved_ids_29_Ksaved_ids_defined_28;
#endif
#if defined(HAVE_SYSLOG)
  object_initdata_t _object___28or_20null_20integer_20_28member_defined_28log_kern_29_Kkern_defined_28log_user_29_Kuser_defined_28log_mail_29_Kmail_defined_28log_news_29_Knews_defined_28log_uucp_29_Kuucp_defined_28log_daemon_29_Kdaemon_defined_28log_auth_29_Kauth_defined_28log_cron_29_Kcron_defined_28log_lpr_29_Klpr_defined_28log_syslog_29_Ksyslog_defined_28log_authpriv_29_Kauthpriv_defined_28log_ftp_29_Kftp_defined_28log_local0_29_Klocal0_defined_28log_local1_29_Klocal1_defined_28log_local2_29_Klocal2_defined_28log_local3_29_Klocal3_defined_28log_local4_29_Klocal4_defined_28log_local5_29_Klocal5_defined_28log_local6_29_Klocal6_defined_28log_local7_29_Klocal7__29_29;
#endif
  object_initdata_t _object___28or_20null_20integer_20_28member_defined_28prio_process_29_Kprocess_defined_28prio_pgrp_29_Kpgrp_defined_28prio_user_29_Kuser__29_29;
#if defined(WIN32_NATIVE)
  object_initdata_t _object___28or_20null_20integer_20_28member_defined_28realtime_priority_class_29_Krealtime_defined_28high_priority_class_29_Khigh_defined_28above_normal_priority_class_29_Kabove_normal_defined_28normal_priority_class_29_Knormal_defined_28below_normal_priority_class_29_Kbelow_normal_defined_28low_priority_class_29_Klow_defined_28idle_priority_class_29_Kidle__29_29;
#endif
#if !(defined(WIN32_NATIVE))
  object_initdata_t _object___28or_20null_20integer_20_28member_Krealtime_Khigh_Kabove_normal_Knormal_Kbelow_normal_Klow_Kidle__29_29;
#endif
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__syscalls__object_tab_initdata = {
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  { "CL:DECODE-UNIVERSAL-TIME" },
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD)))
  { "CL:MEMBER" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_CHAR_TERM))
  { ":2-CHAR-TERM" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_C_BIND))
  { ":2-C-BIND" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_C_DEV))
  { ":2-C-DEV" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_FORT_DEV))
  { ":2-FORT-DEV" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_FORT_RUN))
  { ":2-FORT-RUN" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_LOCALEDEF))
  { ":2-LOCALEDEF" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS))
  { ":2-PBS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_ACCOUNTING))
  { ":2-PBS-ACCOUNTING" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_CHECKPOINT))
  { ":2-PBS-CHECKPOINT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_LOCATE))
  { ":2-PBS-LOCATE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_MESSAGE))
  { ":2-PBS-MESSAGE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_PBS_TRACK))
  { ":2-PBS-TRACK" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_SW_DEV))
  { ":2-SW-DEV" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_2_SYMLINKS))
  { ":2-SYMLINKS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_UPE))
  { ":2-UPE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_2_VERSION))
  { ":2-VERSION" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(ABOVE_NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  { ":ABOVE-NORMAL" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(ACCOUNTING))
  { ":ACCOUNTING" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ADVISORY_INFO))
  { ":ADVISORY-INFO" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_LISTIO_MAX))
  { ":AIO-LISTIO-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_MAX))
  { ":AIO-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AIO_PRIO_DELTA_MAX))
  { ":AIO-PRIO-DELTA-MAX" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_ALERT))
  { ":ALERT" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_ALLOC_SIZE_MIN))
  { ":ALLOC-SIZE-MIN" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":ALPHA" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":ALT" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_APPEND))
  { ":APPEND" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":APPNAME" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_ARCHIVE))
  { ":ARCHIVE" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":ARGUMENTS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ARG_MAX))
  { ":ARG-MAX" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_AS))
  { ":AS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ASYNCHRONOUS_IO))
  { ":ASYNCHRONOUS-IO" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_ASYNC_IO))
  { ":ASYNC-IO" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_ATEXIT_MAX))
  { ":ATEXIT-MAX" },
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  { ":ATIME" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_AUTH))
  { ":AUTH" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":AUTHOR" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_AUTHPRIV))
  { ":AUTHPRIV" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_AVPHYS_PAGES))
  { ":AVPHYS-PAGES" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":BACKOFFICE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BARRIERS))
  { ":BARRIERS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_BASE_MAX))
  { ":BC-BASE-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_DIM_MAX))
  { ":BC-DIM-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_SCALE_MAX))
  { ":BC-SCALE-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_BC_STRING_MAX))
  { ":BC-STRING-MAX" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(BELOW_NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  { ":BELOW-NORMAL" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_BINARY))
  { ":BINARY" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":BLOB" },
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  { ":BLOCK" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":BOOL" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(BOOT_TIME))
  { ":BOOT-TIME" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":BSTR" },
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  { ":BUFFERED" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":BUILT-IN" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":BYREF" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":CHARCOUNT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CHILD_MAX))
  { ":CHILD-MAX" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_CHOWN_RESTRICTED))
  { ":CHOWN-RESTRICTED" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":CLIPBOARD-FORMAT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CLK_TCK))
  { ":CLK-TCK" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CLOCK_SELECTION))
  { ":CLOCK-SELECTION" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(FD_CLOEXEC))
  { ":CLOEXEC" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":CODEPAGE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_COLL_WEIGHTS_MAX))
  { ":COLL-WEIGHTS-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":COMMENTS" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_COMPRESSED))
  { ":COMPRESSED" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { ":CONS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":CONTROL" },
#endif
  { ":COPY" },
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_CORE))
  { ":CORE" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_CPU))
  { ":CPU" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_CPUTIME))
  { ":CPUTIME" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_CREAT))
  { ":CREAT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":CREATE-DTM" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_CRIT))
  { ":CRIT" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_CRON))
  { ":CRON" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":CY" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_DAEMON))
  { ":DAEMON" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_DATA))
  { ":DATA" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":DATACENTER" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":DATE" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(DEAD_PROCESS))
  { ":DEAD-PROCESS" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_DEBUG))
  { ":DEBUG" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_DELAYTIMER_MAX))
  { ":DELAYTIMER-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":DESCRIPTION" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_DEVICE))
  { ":DEVICE" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_DIRECT))
  { ":DIRECT" },
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  { ":DIRECTION" },
#endif
#if ((defined(HAVE_FCNTL)) && (defined(O_DIRECTORY))) || ((defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_DIRECTORY)))
  { ":DIRECTORY" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":DOC-SECURITY" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":DOMAIN-CONTROLLER" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":EDITTIME" },
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  { ":ELEMENT-TYPE" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_EMERG))
  { ":EMERG" },
#endif
#if ((defined(HAVE_UTMPX_H)) && (defined(EMPTY))) || (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32))
  { ":EMPTY" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_ENCRYPTED))
  { ":ENCRYPTED" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":ENTERPRISE" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_ERR))
  { ":ERR" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":ERROR" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_EXCL))
  { ":EXCL" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_EXPR_NEST_MAX))
  { ":EXPR-NEST-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":EXT" },
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  { ":EXTERNAL-FORMAT" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { ":FACILITY" },
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFBLK))
  { ":FBLK" },
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFCHR))
  { ":FCHR" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(F_GETFD))
  { ":FD" },
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFDIR))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_MKDIR))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_CREAT)))
  { ":FDIR" },
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFIFO))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_MKFIFO)))
  { ":FIFO" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_FILESIZEBITS))
  { ":FILESIZEBITS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":FILETIME" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_NAMED_STREAMS))
  { ":FILE_NAMED_STREAMS" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_READ_ONLY_VOLUME))
  { ":FILE_READ_ONLY_VOLUME" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_ENCRYPTION))
  { ":FILE_SUPPORTS_ENCRYPTION" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_OBJECT_IDS))
  { ":FILE_SUPPORTS_OBJECT_IDS" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_REPARSE_POINTS))
  { ":FILE_SUPPORTS_REPARSE_POINTS" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_SUPPORTS_SPARSE_FILES))
  { ":FILE_SUPPORTS_SPARSE_FILES" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FILE_VOLUME_QUOTAS))
  { ":FILE_VOLUME_QUOTAS" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(F_GETFL))
  { ":FL" },
#endif
#if ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFREG))) || ((defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (!(defined(HAVE_MKNOD))) && (defined(HAVE_CREAT)))
  { ":FREG" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_FSIZE))
  { ":FSIZE" },
#endif
#if (defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)) && (defined(S_IFSOCK))
  { ":FSOCK" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_FSYNC))
  { ":FSYNC" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_CASE_IS_PRESERVED))
  { ":FS_CASE_IS_PRESERVED" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_CASE_SENSITIVE))
  { ":FS_CASE_SENSITIVE" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_FILE_COMPRESSION))
  { ":FS_FILE_COMPRESSION" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_FILE_ENCRYPTION))
  { ":FS_FILE_ENCRYPTION" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_PERSISTENT_ACLS))
  { ":FS_PERSISTENT_ACLS" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_UNICODE_STORED_ON_DISK))
  { ":FS_UNICODE_STORED_ON_DISK" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(FS_VOL_IS_COMPRESSED))
  { ":FS_VOL_IS_COMPRESSED" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_FTP))
  { ":FTP" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_GETGR_R_SIZE_MAX))
  { ":GETGR-R-SIZE-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_GETPW_R_SIZE_MAX))
  { ":GETPW-R-SIZE-MAX" },
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  { ":GID" },
#endif
  { ":HARDLINK" },
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_HIDDEN))
  { ":HIDDEN" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(HIGH_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  { ":HIGH" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_HOST_NAME_MAX))
  { ":HOST-NAME-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":HOT-KEY" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":I1" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":I2" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":I4" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":I8" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":IA64" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":ICON" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(IDLE_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  { ":IDLE" },
#endif
  { ":IF-DOES-NOT-EXIST" },
  { ":IF-EXISTS" },
#if (defined(HAVE_SYSLOG)) && (defined(LOG_INFO))
  { ":INFO" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":INITID" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(INIT_PROCESS))
  { ":INIT-PROCESS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":INT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":INTEL" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_IOV_MAX))
  { ":IOV-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_IPV6))
  { ":IPV6" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_JOB_CONTROL))
  { ":JOB-CONTROL" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_KERN))
  { ":KERN" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":KEYWORDS" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_LARGEFILE))
  { ":LARGEFILE" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":LASTAUTHOR" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":LASTPRINTED" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":LASTSAVE-DTM" },
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  { ":LENGTH" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_LINE_MAX))
  { ":LINE-MAX" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_LINK_MAX))
  { ":LINK-MAX" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL0))
  { ":LOCAL0" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL1))
  { ":LOCAL1" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL2))
  { ":LOCAL2" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL3))
  { ":LOCAL3" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL4))
  { ":LOCAL4" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL5))
  { ":LOCAL5" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL6))
  { ":LOCAL6" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LOCAL7))
  { ":LOCAL7" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":LOCALE" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_LOCKS))
  { ":LOCKS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_LOGIN_NAME_MAX))
  { ":LOGIN-NAME-MAX" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(LOGIN_PROCESS))
  { ":LOGIN-PROCESS" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(LOW_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  { ":LOW" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_LPR))
  { ":LPR" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":LPSTR" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":LPWSTR" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_MAIL))
  { ":MAIL" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MAPPED_FILES))
  { ":MAPPED-FILES" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":MAX" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_MAX_CANON))
  { ":MAX-CANON" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_MAX_INPUT))
  { ":MAX-INPUT" },
#endif
#if ((defined(HAVE_SYSCONF)) && (defined(_SC_MEMLOCK))) || ((defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_MEMLOCK)))
  { ":MEMLOCK" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MEMLOCK_RANGE))
  { ":MEMLOCK-RANGE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MEMORY_PROTECTION))
  { ":MEMORY-PROTECTION" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MESSAGE_PASSING))
  { ":MESSAGE-PASSING" },
#endif
  { ":METHOD" },
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":MIN" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":MIPS" },
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  { ":MODE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MONOTONIC_CLOCK))
  { ":MONOTONIC-CLOCK" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MQ_OPEN_MAX))
  { ":MQ-OPEN-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_MQ_PRIO_MAX))
  { ":MQ-PRIO-MAX" },
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  { ":MTIME" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_NAME_MAX))
  { ":NAME-MAX" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { ":NDELAY" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_NEWS))
  { ":NEWS" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(NEW_TIME))
  { ":NEW-TIME" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NGROUPS_MAX))
  { ":NGROUPS-MAX" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOCTTY))
  { ":NOCTTY" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_NOFILE))
  { ":NOFILE" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOFOLLOW))
  { ":NOFOLLOW" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NOINHERIT))
  { ":NOINHERIT" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_NONBLOCK))
  { ":NONBLOCK" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(NORMAL_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE))) || ((defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_NORMAL)))
  { ":NORMAL" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_NOTICE))
  { ":NOTICE" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":NOTIMPLEMENTED" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED))
  { ":NOT-CONTENT-INDEXED" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { ":NOWAIT" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_NO_TRUNC))
  { ":NO-TRUNC" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_NPROC))
  { ":NPROC" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NPROCESSORS_CONF))
  { ":NPROCESSORS-CONF" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_NPROCESSORS_ONLN))
  { ":NPROCESSORS-ONLN" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":NT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":NULL" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { ":ODELAY" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_OFFLINE))
  { ":OFFLINE" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(OLD_TIME))
  { ":OLD-TIME" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_OPEN_MAX))
  { ":OPEN-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":PAGECOUNT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PAGESIZE))
  { ":PAGESIZE" },
#endif
#if ((defined(HAVE_CONFSTR)) && (defined(_CS_PATH))) || (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32))
  { ":PATH" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PATH_MAX))
  { ":PATH-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":PERSONAL" },
#endif
#if defined(PRIO_PGRP)
  { ":PGRP" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PHYS_PAGES))
  { ":PHYS-PAGES" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { ":PID" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PIPE_BUF))
  { ":PIPE-BUF" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_CFLAGS))
  { ":POSIX-V6-ILP32-OFF32-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_LDFLAGS))
  { ":POSIX-V6-ILP32-OFF32-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFF32_LIBS))
  { ":POSIX-V6-ILP32-OFF32-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS))
  { ":POSIX-V6-ILP32-OFFBIG-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS))
  { ":POSIX-V6-ILP32-OFFBIG-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_ILP32_OFFBIG_LIBS))
  { ":POSIX-V6-ILP32-OFFBIG-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_CFLAGS))
  { ":POSIX-V6-LP64-OFF64-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_LDFLAGS))
  { ":POSIX-V6-LP64-OFF64-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LP64_OFF64_LIBS))
  { ":POSIX-V6-LP64-OFF64-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS))
  { ":POSIX-V6-LPBIG-OFFBIG-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS))
  { ":POSIX-V6-LPBIG-OFFBIG-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_LPBIG_OFFBIG_LIBS))
  { ":POSIX-V6-LPBIG-OFFBIG-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS))
  { ":POSIX-V6-WIDTH-RESTRICTED-ENVS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":PPC" },
#endif
  { ":PRESERVE" },
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PRIORITIZED_IO))
  { ":PRIORITIZED-IO" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_PRIORITY_SCHEDULING))
  { ":PRIORITY-SCHEDULING" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_PRIO_IO))
  { ":PRIO-IO" },
#endif
#if defined(PRIO_PROCESS)
  { ":PROCESS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":R4" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":R8" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RAW_SOCKETS))
  { ":RAW-SOCKETS" },
#endif
#if defined(HAVE_FCNTL)
  { ":RDONLY" },
#endif
#if defined(HAVE_FCNTL)
  { ":RDWR" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_READER_WRITER_LOCKS))
  { ":READER-WRITER-LOCKS" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_READONLY))
  { ":READONLY" },
#endif
#if ((defined(WIN32_NATIVE)) && (defined(REALTIME_PRIORITY_CLASS))) || (!(defined(WIN32_NATIVE)))
  { ":REALTIME" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_REALTIME_SIGNALS))
  { ":REALTIME-SIGNALS" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_INCR_XFER_SIZE))
  { ":REC-INCR-XFER-SIZE" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_MAX_XFER_SIZE))
  { ":REC-MAX-XFER-SIZE" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_MIN_XFER_SIZE))
  { ":REC-MIN-XFER-SIZE" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_REC_XFER_ALIGN))
  { ":REC-XFER-ALIGN" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_REGEXP))
  { ":REGEXP" },
#endif
  { ":RENAME" },
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_REPARSE_POINT))
  { ":REPARSE-POINT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":REVNUMBER" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RE_DUP_MAX))
  { ":RE-DUP-MAX" },
#endif
#if defined(S_IRGRP)
  { ":RGRP" },
#endif
#if defined(S_IROTH)
  { ":ROTH" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_RSS))
  { ":RSS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_RTSIG_MAX))
  { ":RTSIG-MAX" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(RUN_LVL))
  { ":RUN-LVL" },
#endif
#if defined(S_IRUSR)
  { ":RUSR" },
#endif
#if defined(S_IRWXG)
  { ":RWXG" },
#endif
#if defined(S_IRWXO)
  { ":RWXO" },
#endif
#if defined(S_IRWXU)
  { ":RWXU" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":S" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SAVED_IDS))
  { ":SAVED-IDS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEMAPHORES))
  { ":SEMAPHORES" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEM_NSEMS_MAX))
  { ":SEM-NSEMS-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SEM_VALUE_MAX))
  { ":SEM-VALUE-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":SERVER" },
#endif
#if defined(S_ISGID)
  { ":SGID" },
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  { ":SHARED" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SHARED_MEMORY_OBJECTS))
  { ":SHARED-MEMORY-OBJECTS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SHELL))
  { ":SHELL" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":SHIFT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":SHOW-COMMAND" },
#endif
#if defined(SIGABRT)
  { ":SIGABRT" },
#endif
#if defined(SIGALRM)
  { ":SIGALRM" },
#endif
#if defined(SIGBUS)
  { ":SIGBUS" },
#endif
#if defined(SIGCHLD)
  { ":SIGCHLD" },
#endif
#if defined(SIGCONT)
  { ":SIGCONT" },
#endif
#if defined(SIGFPE)
  { ":SIGFPE" },
#endif
#if defined(SIGHUP)
  { ":SIGHUP" },
#endif
#if defined(SIGILL)
  { ":SIGILL" },
#endif
#if defined(SIGINT)
  { ":SIGINT" },
#endif
#if defined(SIGKILL)
  { ":SIGKILL" },
#endif
#if defined(SIGPIPE)
  { ":SIGPIPE" },
#endif
#if defined(SIGPOLL)
  { ":SIGPOLL" },
#endif
#if defined(SIGPROF)
  { ":SIGPROF" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SIGQUEUE_MAX))
  { ":SIGQUEUE-MAX" },
#endif
#if defined(SIGQUIT)
  { ":SIGQUIT" },
#endif
#if defined(SIGSEGV)
  { ":SIGSEGV" },
#endif
#if defined(SIGSTOP)
  { ":SIGSTOP" },
#endif
#if defined(SIGSYS)
  { ":SIGSYS" },
#endif
#if defined(SIGTERM)
  { ":SIGTERM" },
#endif
#if defined(SIGTRAP)
  { ":SIGTRAP" },
#endif
#if defined(SIGTSTP)
  { ":SIGTSTP" },
#endif
#if defined(SIGTTIN)
  { ":SIGTTIN" },
#endif
#if defined(SIGTTOU)
  { ":SIGTTOU" },
#endif
#if defined(SIGURG)
  { ":SIGURG" },
#endif
#if defined(SIGUSR1)
  { ":SIGUSR1" },
#endif
#if defined(SIGUSR2)
  { ":SIGUSR2" },
#endif
#if defined(SIGVTALRM)
  { ":SIGVTALRM" },
#endif
#if defined(SIGXCPU)
  { ":SIGXCPU" },
#endif
#if defined(SIGXFSZ)
  { ":SIGXFSZ" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":SMALLBUSINESS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":SMALLBUSINESS-RESTRICTED" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SOCK_MAXBUF))
  { ":SOCK-MAXBUF" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_SPARSE_FILE))
  { ":SPARSE-FILE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPAWN))
  { ":SPAWN" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPIN_LOCKS))
  { ":SPIN-LOCKS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SPORADIC_SERVER))
  { ":SPORADIC-SERVER" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SS_REPL_MAX))
  { ":SS-REPL-MAX" },
#endif
#if (defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)) && (defined(RLIMIT_STACK))
  { ":STACK" },
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  { ":START" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_STREAM_MAX))
  { ":STREAM-MAX" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_APPEND))
  { ":ST_APPEND" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_IMMUTABLE))
  { ":ST_IMMUTABLE" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_MANDLOCK))
  { ":ST_MANDLOCK" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOATIME))
  { ":ST_NOATIME" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NODEV))
  { ":ST_NODEV" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NODIRATIME))
  { ":ST_NODIRATIME" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOEXEC))
  { ":ST_NOEXEC" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOSUID))
  { ":ST_NOSUID" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_NOTRUNC))
  { ":ST_NOTRUNC" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_RDONLY))
  { ":ST_RDONLY" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_SYNCHRONOUS))
  { ":ST_SYNCHRONOUS" },
#endif
#if (defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))) && (defined(ST_WRITE))
  { ":ST_WRITE" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":SUBJECT" },
#endif
#if defined(S_ISUID)
  { ":SUID" },
#endif
#if defined(S_ISVTX)
  { ":SVTX" },
#endif
  { ":SYMLINK" },
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SYMLINK_MAX))
  { ":SYMLINK-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SYMLOOP_MAX))
  { ":SYMLOOP-MAX" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_SYNC))
  { ":SYNC" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_SYNCHRONIZED_IO))
  { ":SYNCHRONIZED-IO" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_SYNC_IO))
  { ":SYNC-IO" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_SYSLOG))
  { ":SYSLOG" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_SYSTEM))
  { ":SYSTEM" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":TEMPLATE" },
#endif
#if (defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)) && (defined(FILE_ATTRIBUTE_TEMPORARY))
  { ":TEMPORARY" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":TERMINAL" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_TEXT))
  { ":TEXT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREADS))
  { ":THREADS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_ATTR_STACKADDR))
  { ":THREAD-ATTR-STACKADDR" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_ATTR_STACKSIZE))
  { ":THREAD-ATTR-STACKSIZE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_CPUTIME))
  { ":THREAD-CPUTIME" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_DESTRUCTOR_ITERATIONS))
  { ":THREAD-DESTRUCTOR-ITERATIONS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_KEYS_MAX))
  { ":THREAD-KEYS-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIORITY_SCHEDULING))
  { ":THREAD-PRIORITY-SCHEDULING" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIO_INHERIT))
  { ":THREAD-PRIO-INHERIT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PRIO_PROTECT))
  { ":THREAD-PRIO-PROTECT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_PROCESS_SHARED))
  { ":THREAD-PROCESS-SHARED" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_SAFE_FUNCTIONS))
  { ":THREAD-SAFE-FUNCTIONS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_SPORADIC_SERVER))
  { ":THREAD-SPORADIC-SERVER" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_STACK_MIN))
  { ":THREAD-STACK-MIN" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_THREAD_THREADS_MAX))
  { ":THREAD-THREADS-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":THUMBNAIL" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMEOUTS))
  { ":TIMEOUTS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMERS))
  { ":TIMERS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TIMER_MAX))
  { ":TIMER-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":TITLE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE))
  { ":TRACE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_EVENT_FILTER))
  { ":TRACE-EVENT-FILTER" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_EVENT_NAME_MAX))
  { ":TRACE-EVENT-NAME-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_INHERIT))
  { ":TRACE-INHERIT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_LOG))
  { ":TRACE-LOG" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_NAME_MAX))
  { ":TRACE-NAME-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_SYS_MAX))
  { ":TRACE-SYS-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TRACE_USER_EVENT_MAX))
  { ":TRACE-USER-EVENT-MAX" },
#endif
#if (defined(HAVE_FCNTL)) && (defined(O_TRUNC))
  { ":TRUNC" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TTY_NAME_MAX))
  { ":TTY-NAME-MAX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TYPED_MEMORY_OBJECTS))
  { ":TYPED-MEMORY-OBJECTS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_TZNAME_MAX))
  { ":TZNAME-MAX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":UI1" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":UI2" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":UI4" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":UI8" },
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  { ":UID" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":UINT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":UNKNOWN" },
#endif
#if ((defined(HAVE_SYSLOG)) && (defined(LOG_USER))) || (defined(PRIO_USER))
  { ":USER" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":USER-DEFINED" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(USER_PROCESS))
  { ":USER-PROCESS" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_UUCP))
  { ":UUCP" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_ILP32_OFF32))
  { ":V6-ILP32-OFF32" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_ILP32_OFFBIG))
  { ":V6-ILP32-OFFBIG" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_LP64_OFF64))
  { ":V6-LP64-OFF64" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_V6_LPBIG_OFFBIG))
  { ":V6-LPBIG-OFFBIG" },
#endif
#if (defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)) && (defined(_PC_VDISABLE))
  { ":VDISABLE" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_VERSION))
  { ":VERSION" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(LOG_WARNING))
  { ":WARNING" },
#endif
#if defined(S_IWGRP)
  { ":WGRP" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":WINDOWS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":WORDCOUNT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":WORKING-DIRECTORY" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { ":WORKSTATION" },
#endif
#if defined(S_IWOTH)
  { ":WOTH" },
#endif
#if defined(HAVE_FCNTL)
  { ":WRONLY" },
#endif
#if defined(S_IWUSR)
  { ":WUSR" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_ILP32_OFF32))
  { ":XBS5-ILP32-OFF32" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_CFLAGS))
  { ":XBS5-ILP32-OFF32-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LDFLAGS))
  { ":XBS5-ILP32-OFF32-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LIBS))
  { ":XBS5-ILP32-OFF32-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFF32_LINTFLAGS))
  { ":XBS5-ILP32-OFF32-LINTFLAGS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_ILP32_OFFBIG))
  { ":XBS5-ILP32-OFFBIG" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_CFLAGS))
  { ":XBS5-ILP32-OFFBIG-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LDFLAGS))
  { ":XBS5-ILP32-OFFBIG-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LIBS))
  { ":XBS5-ILP32-OFFBIG-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_ILP32_OFFBIG_LINTFLAGS))
  { ":XBS5-ILP32-OFFBIG-LINTFLAGS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_LP64_OFF64))
  { ":XBS5-LP64-OFF64" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_CFLAGS))
  { ":XBS5-LP64-OFF64-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LDFLAGS))
  { ":XBS5-LP64-OFF64-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LIBS))
  { ":XBS5-LP64-OFF64-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LP64_OFF64_LINTFLAGS))
  { ":XBS5-LP64-OFF64-LINTFLAGS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XBS5_LPBIG_OFFBIG))
  { ":XBS5-LPBIG-OFFBIG" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_CFLAGS))
  { ":XBS5-LPBIG-OFFBIG-CFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LDFLAGS))
  { ":XBS5-LPBIG-OFFBIG-LDFLAGS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LIBS))
  { ":XBS5-LPBIG-OFFBIG-LIBS" },
#endif
#if (defined(HAVE_CONFSTR)) && (defined(_CS_XBS5_LPBIG_OFFBIG_LINTFLAGS))
  { ":XBS5-LPBIG-OFFBIG-LINTFLAGS" },
#endif
#if defined(S_IXGRP)
  { ":XGRP" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_CRYPT))
  { ":XOPEN-CRYPT" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_ENH_I18N))
  { ":XOPEN-ENH-I18N" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_LEGACY))
  { ":XOPEN-LEGACY" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_REALTIME))
  { ":XOPEN-REALTIME" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_REALTIME_THREADS))
  { ":XOPEN-REALTIME-THREADS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_SHM))
  { ":XOPEN-SHM" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_STREAMS))
  { ":XOPEN-STREAMS" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_UNIX))
  { ":XOPEN-UNIX" },
#endif
#if (defined(HAVE_SYSCONF)) && (defined(_SC_XOPEN_VERSION))
  { ":XOPEN-VERSION" },
#endif
#if defined(S_IXOTH)
  { ":XOTH" },
#endif
#if defined(S_IXUSR)
  { ":XUSR" },
#endif
  { "POSIX::COPY-FILE" },
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX::MAKE-FILE-INFO" },
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  { "POSIX::MAKE-FILE-STAT" },
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  { "POSIX::MAKE-GROUP-INFO" },
#endif
  { "POSIX::MAKE-HOSTENT" },
#if defined(HAVE_GETRLIMIT)
  { "POSIX::MAKE-RLIMIT" },
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  { "POSIX::MAKE-SERVICE" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX::MAKE-SHORTCUT-INFO" },
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  { "POSIX::MAKE-STAT-VFS" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX::MAKE-SYSTEM-INFO" },
#endif
#if defined(HAVE_UNAME)
  { "POSIX::MAKE-UNAME" },
#endif
#if defined(HAVE_GETRUSAGE)
  { "POSIX::MAKE-USAGE" },
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  { "POSIX::MAKE-USER-INFO" },
#endif
#if defined(HAVE_UTMPX_H)
  { "POSIX::MAKE-UTMPX" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX::MAKE-VERSION" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX::MKMEMSTAT" },
#endif
  { "POSIX::RESOLVE-HOST-IPADDR" },
#if defined(HAVE_SETRLIMIT)
  { "POSIX::RLIMIT" },
#endif
#if defined(HAVE_UTMPX_H)
  { "POSIX::UTMPX" },
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!(defined(WIN32_NATIVE))) && (!(defined(HAVE_CHMOD)))
  { "\"chmod()\"" },
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!(defined(HAVE_CHOWN)))
  { "\"chown()\"" },
#endif
#if (defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))) && (!defined(WIN32_NATIVE)) && (!(defined(HAVE_UTIME)))
  { "\"utime()\"" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "(MEMBER :ALT :CONTROL :EXT :SHIFT)" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "(MEMBER :NORMAL :MAX :MIN)" },
#endif
  { "(OR NULL INTEGER (MEMBER :COPY :SYMLINK :HARDLINK :RENAME))" },
#if defined(HAVE_ENCRYPT) || defined(HAVE_SETKEY)
  { "(VECTOR (UNSIGNED-BYTE 8) 8)" },
#endif
#if defined(HAVE_UTMPX_H)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(EMPTY)
    " " ":EMPTY"
#  endif
#  if defined(RUN_LVL)
    " " ":RUN-LVL"
#  endif
#  if defined(BOOT_TIME)
    " " ":BOOT-TIME"
#  endif
#  if defined(OLD_TIME)
    " " ":OLD-TIME"
#  endif
#  if defined(NEW_TIME)
    " " ":NEW-TIME"
#  endif
#  if defined(USER_PROCESS)
    " " ":USER-PROCESS"
#  endif
#  if defined(INIT_PROCESS)
    " " ":INIT-PROCESS"
#  endif
#  if defined(LOGIN_PROCESS)
    " " ":LOGIN-PROCESS"
#  endif
#  if defined(DEAD_PROCESS)
    " " ":DEAD-PROCESS"
#  endif
#  if defined(ACCOUNTING)
    " " ":ACCOUNTING"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_FCNTL)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(FD_CLOEXEC)
    " " ":CLOEXEC"
#  endif
    " " "))"
  },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(FILE_ATTRIBUTE_ARCHIVE)
    " " ":ARCHIVE"
#  endif
#  if defined(FILE_ATTRIBUTE_COMPRESSED)
    " " ":COMPRESSED"
#  endif
#  if defined(FILE_ATTRIBUTE_DEVICE)
    " " ":DEVICE"
#  endif
#  if defined(FILE_ATTRIBUTE_DIRECTORY)
    " " ":DIRECTORY"
#  endif
#  if defined(FILE_ATTRIBUTE_ENCRYPTED)
    " " ":ENCRYPTED"
#  endif
#  if defined(FILE_ATTRIBUTE_HIDDEN)
    " " ":HIDDEN"
#  endif
#  if defined(FILE_ATTRIBUTE_NORMAL)
    " " ":NORMAL"
#  endif
#  if defined(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED)
    " " ":NOT-CONTENT-INDEXED"
#  endif
#  if defined(FILE_ATTRIBUTE_OFFLINE)
    " " ":OFFLINE"
#  endif
#  if defined(FILE_ATTRIBUTE_READONLY)
    " " ":READONLY"
#  endif
#  if defined(FILE_ATTRIBUTE_REPARSE_POINT)
    " " ":REPARSE-POINT"
#  endif
#  if defined(FILE_ATTRIBUTE_SPARSE_FILE)
    " " ":SPARSE-FILE"
#  endif
#  if defined(FILE_ATTRIBUTE_SYSTEM)
    " " ":SYSTEM"
#  endif
#  if defined(FILE_ATTRIBUTE_TEMPORARY)
    " " ":TEMPORARY"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_FCNTL)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(F_GETFD)
    " " ":FD"
#  endif
#  if defined(F_GETFL)
    " " ":FL"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_SYSLOG)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(LOG_EMERG)
    " " ":EMERG"
#  endif
#  if defined(LOG_ALERT)
    " " ":ALERT"
#  endif
#  if defined(LOG_CRIT)
    " " ":CRIT"
#  endif
#  if defined(LOG_ERR)
    " " ":ERR"
#  endif
#  if defined(LOG_WARNING)
    " " ":WARNING"
#  endif
#  if defined(LOG_NOTICE)
    " " ":NOTICE"
#  endif
#  if defined(LOG_INFO)
    " " ":INFO"
#  endif
#  if defined(LOG_DEBUG)
    " " ":DEBUG"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_FCNTL)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(O_RDONLY)
    " " ":RDONLY"
#  endif
#  if defined(O_WRONLY)
    " " ":WRONLY"
#  endif
#  if defined(O_RDWR)
    " " ":RDWR"
#  endif
#  if defined(O_APPEND)
    " " ":APPEND"
#  endif
#  if defined(O_CREAT)
    " " ":CREAT"
#  endif
#  if defined(O_TRUNC)
    " " ":TRUNC"
#  endif
#  if defined(O_EXCL)
    " " ":EXCL"
#  endif
#  if defined(O_NOCTTY)
    " " ":NOCTTY"
#  endif
#  if defined(O_SYNC)
    " " ":SYNC"
#  endif
#  if defined(O_NONBLOCK)
    " " ":NONBLOCK"
#  endif
#  if defined(O_BINARY)
    " " ":BINARY"
#  endif
#  if defined(O_TEXT)
    " " ":TEXT"
#  endif
#  if defined(O_NOINHERIT)
    " " ":NOINHERIT"
#  endif
#  if defined(O_DIRECT)
    " " ":DIRECT"
#  endif
#  if defined(O_LARGEFILE)
    " " ":LARGEFILE"
#  endif
#  if defined(O_DIRECTORY)
    " " ":DIRECTORY"
#  endif
#  if defined(O_NOFOLLOW)
    " " ":NOFOLLOW"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(RLIMIT_CPU)
    " " ":CPU"
#  endif
#  if defined(RLIMIT_FSIZE)
    " " ":FSIZE"
#  endif
#  if defined(RLIMIT_DATA)
    " " ":DATA"
#  endif
#  if defined(RLIMIT_STACK)
    " " ":STACK"
#  endif
#  if defined(RLIMIT_CORE)
    " " ":CORE"
#  endif
#  if defined(RLIMIT_RSS)
    " " ":RSS"
#  endif
#  if defined(RLIMIT_NOFILE)
    " " ":NOFILE"
#  endif
#  if defined(RLIMIT_AS)
    " " ":AS"
#  endif
#  if defined(RLIMIT_NPROC)
    " " ":NPROC"
#  endif
#  if defined(RLIMIT_MEMLOCK)
    " " ":MEMLOCK"
#  endif
#  if defined(RLIMIT_LOCKS)
    " " ":LOCKS"
#  endif
    " " "))"
  },
#endif
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(SIGABRT)
    " " ":SIGABRT"
#  endif
#  if defined(SIGALRM)
    " " ":SIGALRM"
#  endif
#  if defined(SIGBUS)
    " " ":SIGBUS"
#  endif
#  if defined(SIGCHLD)
    " " ":SIGCHLD"
#  endif
#  if defined(SIGCONT)
    " " ":SIGCONT"
#  endif
#  if defined(SIGFPE)
    " " ":SIGFPE"
#  endif
#  if defined(SIGHUP)
    " " ":SIGHUP"
#  endif
#  if defined(SIGILL)
    " " ":SIGILL"
#  endif
#  if defined(SIGINT)
    " " ":SIGINT"
#  endif
#  if defined(SIGKILL)
    " " ":SIGKILL"
#  endif
#  if defined(SIGPIPE)
    " " ":SIGPIPE"
#  endif
#  if defined(SIGQUIT)
    " " ":SIGQUIT"
#  endif
#  if defined(SIGSEGV)
    " " ":SIGSEGV"
#  endif
#  if defined(SIGSTOP)
    " " ":SIGSTOP"
#  endif
#  if defined(SIGTERM)
    " " ":SIGTERM"
#  endif
#  if defined(SIGTSTP)
    " " ":SIGTSTP"
#  endif
#  if defined(SIGTTIN)
    " " ":SIGTTIN"
#  endif
#  if defined(SIGTTOU)
    " " ":SIGTTOU"
#  endif
#  if defined(SIGUSR1)
    " " ":SIGUSR1"
#  endif
#  if defined(SIGUSR2)
    " " ":SIGUSR2"
#  endif
#  if defined(SIGPOLL)
    " " ":SIGPOLL"
#  endif
#  if defined(SIGPROF)
    " " ":SIGPROF"
#  endif
#  if defined(SIGSYS)
    " " ":SIGSYS"
#  endif
#  if defined(SIGTRAP)
    " " ":SIGTRAP"
#  endif
#  if defined(SIGURG)
    " " ":SIGURG"
#  endif
#  if defined(SIGVTALRM)
    " " ":SIGVTALRM"
#  endif
#  if defined(SIGXCPU)
    " " ":SIGXCPU"
#  endif
#  if defined(SIGXFSZ)
    " " ":SIGXFSZ"
#  endif
    " " "))"
  },
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(ST_RDONLY)
    " " ":ST_RDONLY"
#  endif
#  if defined(ST_NOSUID)
    " " ":ST_NOSUID"
#  endif
#  if defined(ST_NOTRUNC)
    " " ":ST_NOTRUNC"
#  endif
#  if defined(ST_NODEV)
    " " ":ST_NODEV"
#  endif
#  if defined(ST_NOEXEC)
    " " ":ST_NOEXEC"
#  endif
#  if defined(ST_SYNCHRONOUS)
    " " ":ST_SYNCHRONOUS"
#  endif
#  if defined(ST_MANDLOCK)
    " " ":ST_MANDLOCK"
#  endif
#  if defined(ST_WRITE)
    " " ":ST_WRITE"
#  endif
#  if defined(ST_APPEND)
    " " ":ST_APPEND"
#  endif
#  if defined(ST_IMMUTABLE)
    " " ":ST_IMMUTABLE"
#  endif
#  if defined(ST_NOATIME)
    " " ":ST_NOATIME"
#  endif
#  if defined(ST_NODIRATIME)
    " " ":ST_NODIRATIME"
#  endif
#  if defined(FILE_NAMED_STREAMS)
    " " ":FILE_NAMED_STREAMS"
#  endif
#  if defined(FILE_READ_ONLY_VOLUME)
    " " ":FILE_READ_ONLY_VOLUME"
#  endif
#  if defined(FILE_SUPPORTS_OBJECT_IDS)
    " " ":FILE_SUPPORTS_OBJECT_IDS"
#  endif
#  if defined(FILE_SUPPORTS_REPARSE_POINTS)
    " " ":FILE_SUPPORTS_REPARSE_POINTS"
#  endif
#  if defined(FILE_SUPPORTS_SPARSE_FILES)
    " " ":FILE_SUPPORTS_SPARSE_FILES"
#  endif
#  if defined(FILE_VOLUME_QUOTAS)
    " " ":FILE_VOLUME_QUOTAS"
#  endif
#  if defined(FILE_SUPPORTS_ENCRYPTION)
    " " ":FILE_SUPPORTS_ENCRYPTION"
#  endif
#  if defined(FS_CASE_IS_PRESERVED)
    " " ":FS_CASE_IS_PRESERVED"
#  endif
#  if defined(FS_CASE_SENSITIVE)
    " " ":FS_CASE_SENSITIVE"
#  endif
#  if defined(FS_FILE_COMPRESSION)
    " " ":FS_FILE_COMPRESSION"
#  endif
#  if defined(FS_FILE_ENCRYPTION)
    " " ":FS_FILE_ENCRYPTION"
#  endif
#  if defined(FS_PERSISTENT_ACLS)
    " " ":FS_PERSISTENT_ACLS"
#  endif
#  if defined(FS_UNICODE_STORED_ON_DISK)
    " " ":FS_UNICODE_STORED_ON_DISK"
#  endif
#  if defined(FS_VOL_IS_COMPRESSED)
    " " ":FS_VOL_IS_COMPRESSED"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(S_IFIFO)
    " " ":FIFO"
#  endif
#  if defined(S_IFSOCK)
    " " ":FSOCK"
#  endif
#  if defined(S_IFCHR)
    " " ":FCHR"
#  endif
#  if defined(S_IFDIR)
    " " ":FDIR"
#  endif
#  if defined(S_IFBLK)
    " " ":FBLK"
#  endif
#  if defined(S_IFREG)
    " " ":FREG"
#  endif
    " " "))"
  },
#endif
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(S_ISUID)
    " " ":SUID"
#  endif
#  if defined(S_ISGID)
    " " ":SGID"
#  endif
#  if defined(S_ISVTX)
    " " ":SVTX"
#  endif
#  if defined(S_IRWXU)
    " " ":RWXU"
#  endif
#  if defined(S_IRUSR)
    " " ":RUSR"
#  endif
#  if defined(S_IWUSR)
    " " ":WUSR"
#  endif
#  if defined(S_IXUSR)
    " " ":XUSR"
#  endif
#  if defined(S_IRWXG)
    " " ":RWXG"
#  endif
#  if defined(S_IRGRP)
    " " ":RGRP"
#  endif
#  if defined(S_IWGRP)
    " " ":WGRP"
#  endif
#  if defined(S_IXGRP)
    " " ":XGRP"
#  endif
#  if defined(S_IRWXO)
    " " ":RWXO"
#  endif
#  if defined(S_IROTH)
    " " ":ROTH"
#  endif
#  if defined(S_IWOTH)
    " " ":WOTH"
#  endif
#  if defined(S_IXOTH)
    " " ":XOTH"
#  endif
    " " "))"
  },
#if defined(HAVE_CONFSTR)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(_CS_PATH)
    " " ":PATH"
#  endif
#  if defined(_CS_POSIX_V6_ILP32_OFF32_CFLAGS)
    " " ":POSIX-V6-ILP32-OFF32-CFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_ILP32_OFF32_LDFLAGS)
    " " ":POSIX-V6-ILP32-OFF32-LDFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_ILP32_OFF32_LIBS)
    " " ":POSIX-V6-ILP32-OFF32-LIBS"
#  endif
#  if defined(_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS)
    " " ":POSIX-V6-ILP32-OFFBIG-CFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS)
    " " ":POSIX-V6-ILP32-OFFBIG-LDFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_ILP32_OFFBIG_LIBS)
    " " ":POSIX-V6-ILP32-OFFBIG-LIBS"
#  endif
#  if defined(_CS_POSIX_V6_LP64_OFF64_CFLAGS)
    " " ":POSIX-V6-LP64-OFF64-CFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_LP64_OFF64_LDFLAGS)
    " " ":POSIX-V6-LP64-OFF64-LDFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_LP64_OFF64_LIBS)
    " " ":POSIX-V6-LP64-OFF64-LIBS"
#  endif
#  if defined(_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS)
    " " ":POSIX-V6-LPBIG-OFFBIG-CFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS)
    " " ":POSIX-V6-LPBIG-OFFBIG-LDFLAGS"
#  endif
#  if defined(_CS_POSIX_V6_LPBIG_OFFBIG_LIBS)
    " " ":POSIX-V6-LPBIG-OFFBIG-LIBS"
#  endif
#  if defined(_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS)
    " " ":POSIX-V6-WIDTH-RESTRICTED-ENVS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFF32_CFLAGS)
    " " ":XBS5-ILP32-OFF32-CFLAGS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFF32_LDFLAGS)
    " " ":XBS5-ILP32-OFF32-LDFLAGS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFF32_LIBS)
    " " ":XBS5-ILP32-OFF32-LIBS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFF32_LINTFLAGS)
    " " ":XBS5-ILP32-OFF32-LINTFLAGS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFFBIG_CFLAGS)
    " " ":XBS5-ILP32-OFFBIG-CFLAGS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFFBIG_LDFLAGS)
    " " ":XBS5-ILP32-OFFBIG-LDFLAGS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFFBIG_LIBS)
    " " ":XBS5-ILP32-OFFBIG-LIBS"
#  endif
#  if defined(_CS_XBS5_ILP32_OFFBIG_LINTFLAGS)
    " " ":XBS5-ILP32-OFFBIG-LINTFLAGS"
#  endif
#  if defined(_CS_XBS5_LP64_OFF64_CFLAGS)
    " " ":XBS5-LP64-OFF64-CFLAGS"
#  endif
#  if defined(_CS_XBS5_LP64_OFF64_LDFLAGS)
    " " ":XBS5-LP64-OFF64-LDFLAGS"
#  endif
#  if defined(_CS_XBS5_LP64_OFF64_LIBS)
    " " ":XBS5-LP64-OFF64-LIBS"
#  endif
#  if defined(_CS_XBS5_LP64_OFF64_LINTFLAGS)
    " " ":XBS5-LP64-OFF64-LINTFLAGS"
#  endif
#  if defined(_CS_XBS5_LPBIG_OFFBIG_CFLAGS)
    " " ":XBS5-LPBIG-OFFBIG-CFLAGS"
#  endif
#  if defined(_CS_XBS5_LPBIG_OFFBIG_LDFLAGS)
    " " ":XBS5-LPBIG-OFFBIG-LDFLAGS"
#  endif
#  if defined(_CS_XBS5_LPBIG_OFFBIG_LIBS)
    " " ":XBS5-LPBIG-OFFBIG-LIBS"
#  endif
#  if defined(_CS_XBS5_LPBIG_OFFBIG_LINTFLAGS)
    " " ":XBS5-LPBIG-OFFBIG-LINTFLAGS"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(_PC_FILESIZEBITS)
    " " ":FILESIZEBITS"
#  endif
#  if defined(_PC_LINK_MAX)
    " " ":LINK-MAX"
#  endif
#  if defined(_PC_MAX_CANON)
    " " ":MAX-CANON"
#  endif
#  if defined(_PC_MAX_INPUT)
    " " ":MAX-INPUT"
#  endif
#  if defined(_PC_NAME_MAX)
    " " ":NAME-MAX"
#  endif
#  if defined(_PC_PATH_MAX)
    " " ":PATH-MAX"
#  endif
#  if defined(_PC_PIPE_BUF)
    " " ":PIPE-BUF"
#  endif
#  if defined(_PC_2_SYMLINKS)
    " " ":2-SYMLINKS"
#  endif
#  if defined(_PC_ALLOC_SIZE_MIN)
    " " ":ALLOC-SIZE-MIN"
#  endif
#  if defined(_PC_REC_INCR_XFER_SIZE)
    " " ":REC-INCR-XFER-SIZE"
#  endif
#  if defined(_PC_REC_MAX_XFER_SIZE)
    " " ":REC-MAX-XFER-SIZE"
#  endif
#  if defined(_PC_REC_MIN_XFER_SIZE)
    " " ":REC-MIN-XFER-SIZE"
#  endif
#  if defined(_PC_REC_XFER_ALIGN)
    " " ":REC-XFER-ALIGN"
#  endif
#  if defined(_PC_SYMLINK_MAX)
    " " ":SYMLINK-MAX"
#  endif
#  if defined(_PC_CHOWN_RESTRICTED)
    " " ":CHOWN-RESTRICTED"
#  endif
#  if defined(_PC_NO_TRUNC)
    " " ":NO-TRUNC"
#  endif
#  if defined(_PC_VDISABLE)
    " " ":VDISABLE"
#  endif
#  if defined(_PC_ASYNC_IO)
    " " ":ASYNC-IO"
#  endif
#  if defined(_PC_PRIO_IO)
    " " ":PRIO-IO"
#  endif
#  if defined(_PC_SYNC_IO)
    " " ":SYNC-IO"
#  endif
#  if defined(_PC_SOCK_MAXBUF)
    " " ":SOCK-MAXBUF"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_SYSCONF)
  {
    " " "(OR INTEGER (MEMBER"
#  if defined(_SC_AIO_LISTIO_MAX)
    " " ":AIO-LISTIO-MAX"
#  endif
#  if defined(_SC_AIO_MAX)
    " " ":AIO-MAX"
#  endif
#  if defined(_SC_AIO_PRIO_DELTA_MAX)
    " " ":AIO-PRIO-DELTA-MAX"
#  endif
#  if defined(_SC_ARG_MAX)
    " " ":ARG-MAX"
#  endif
#  if defined(_SC_ATEXIT_MAX)
    " " ":ATEXIT-MAX"
#  endif
#  if defined(_SC_BC_BASE_MAX)
    " " ":BC-BASE-MAX"
#  endif
#  if defined(_SC_BC_DIM_MAX)
    " " ":BC-DIM-MAX"
#  endif
#  if defined(_SC_BC_SCALE_MAX)
    " " ":BC-SCALE-MAX"
#  endif
#  if defined(_SC_BC_STRING_MAX)
    " " ":BC-STRING-MAX"
#  endif
#  if defined(_SC_CHILD_MAX)
    " " ":CHILD-MAX"
#  endif
#  if defined(_SC_CLK_TCK)
    " " ":CLK-TCK"
#  endif
#  if defined(_SC_COLL_WEIGHTS_MAX)
    " " ":COLL-WEIGHTS-MAX"
#  endif
#  if defined(_SC_DELAYTIMER_MAX)
    " " ":DELAYTIMER-MAX"
#  endif
#  if defined(_SC_EXPR_NEST_MAX)
    " " ":EXPR-NEST-MAX"
#  endif
#  if defined(_SC_HOST_NAME_MAX)
    " " ":HOST-NAME-MAX"
#  endif
#  if defined(_SC_IOV_MAX)
    " " ":IOV-MAX"
#  endif
#  if defined(_SC_LINE_MAX)
    " " ":LINE-MAX"
#  endif
#  if defined(_SC_LOGIN_NAME_MAX)
    " " ":LOGIN-NAME-MAX"
#  endif
#  if defined(_SC_NGROUPS_MAX)
    " " ":NGROUPS-MAX"
#  endif
#  if defined(_SC_GETGR_R_SIZE_MAX)
    " " ":GETGR-R-SIZE-MAX"
#  endif
#  if defined(_SC_GETPW_R_SIZE_MAX)
    " " ":GETPW-R-SIZE-MAX"
#  endif
#  if defined(_SC_MQ_OPEN_MAX)
    " " ":MQ-OPEN-MAX"
#  endif
#  if defined(_SC_MQ_PRIO_MAX)
    " " ":MQ-PRIO-MAX"
#  endif
#  if defined(_SC_OPEN_MAX)
    " " ":OPEN-MAX"
#  endif
#  if defined(_SC_ADVISORY_INFO)
    " " ":ADVISORY-INFO"
#  endif
#  if defined(_SC_BARRIERS)
    " " ":BARRIERS"
#  endif
#  if defined(_SC_ASYNCHRONOUS_IO)
    " " ":ASYNCHRONOUS-IO"
#  endif
#  if defined(_SC_CLOCK_SELECTION)
    " " ":CLOCK-SELECTION"
#  endif
#  if defined(_SC_CPUTIME)
    " " ":CPUTIME"
#  endif
#  if defined(_SC_FSYNC)
    " " ":FSYNC"
#  endif
#  if defined(_SC_IPV6)
    " " ":IPV6"
#  endif
#  if defined(_SC_JOB_CONTROL)
    " " ":JOB-CONTROL"
#  endif
#  if defined(_SC_MAPPED_FILES)
    " " ":MAPPED-FILES"
#  endif
#  if defined(_SC_MEMLOCK)
    " " ":MEMLOCK"
#  endif
#  if defined(_SC_MEMLOCK_RANGE)
    " " ":MEMLOCK-RANGE"
#  endif
#  if defined(_SC_MEMORY_PROTECTION)
    " " ":MEMORY-PROTECTION"
#  endif
#  if defined(_SC_MESSAGE_PASSING)
    " " ":MESSAGE-PASSING"
#  endif
#  if defined(_SC_MONOTONIC_CLOCK)
    " " ":MONOTONIC-CLOCK"
#  endif
#  if defined(_SC_PRIORITIZED_IO)
    " " ":PRIORITIZED-IO"
#  endif
#  if defined(_SC_PRIORITY_SCHEDULING)
    " " ":PRIORITY-SCHEDULING"
#  endif
#  if defined(_SC_RAW_SOCKETS)
    " " ":RAW-SOCKETS"
#  endif
#  if defined(_SC_READER_WRITER_LOCKS)
    " " ":READER-WRITER-LOCKS"
#  endif
#  if defined(_SC_REALTIME_SIGNALS)
    " " ":REALTIME-SIGNALS"
#  endif
#  if defined(_SC_REGEXP)
    " " ":REGEXP"
#  endif
#  if defined(_SC_SAVED_IDS)
    " " ":SAVED-IDS"
#  endif
#  if defined(_SC_SEMAPHORES)
    " " ":SEMAPHORES"
#  endif
#  if defined(_SC_SHARED_MEMORY_OBJECTS)
    " " ":SHARED-MEMORY-OBJECTS"
#  endif
#  if defined(_SC_SHELL)
    " " ":SHELL"
#  endif
#  if defined(_SC_SPAWN)
    " " ":SPAWN"
#  endif
#  if defined(_SC_SPIN_LOCKS)
    " " ":SPIN-LOCKS"
#  endif
#  if defined(_SC_SPORADIC_SERVER)
    " " ":SPORADIC-SERVER"
#  endif
#  if defined(_SC_SS_REPL_MAX)
    " " ":SS-REPL-MAX"
#  endif
#  if defined(_SC_SYNCHRONIZED_IO)
    " " ":SYNCHRONIZED-IO"
#  endif
#  if defined(_SC_THREAD_ATTR_STACKADDR)
    " " ":THREAD-ATTR-STACKADDR"
#  endif
#  if defined(_SC_THREAD_ATTR_STACKSIZE)
    " " ":THREAD-ATTR-STACKSIZE"
#  endif
#  if defined(_SC_THREAD_CPUTIME)
    " " ":THREAD-CPUTIME"
#  endif
#  if defined(_SC_THREAD_PRIO_INHERIT)
    " " ":THREAD-PRIO-INHERIT"
#  endif
#  if defined(_SC_THREAD_PRIO_PROTECT)
    " " ":THREAD-PRIO-PROTECT"
#  endif
#  if defined(_SC_THREAD_PRIORITY_SCHEDULING)
    " " ":THREAD-PRIORITY-SCHEDULING"
#  endif
#  if defined(_SC_THREAD_PROCESS_SHARED)
    " " ":THREAD-PROCESS-SHARED"
#  endif
#  if defined(_SC_THREAD_SAFE_FUNCTIONS)
    " " ":THREAD-SAFE-FUNCTIONS"
#  endif
#  if defined(_SC_THREAD_SPORADIC_SERVER)
    " " ":THREAD-SPORADIC-SERVER"
#  endif
#  if defined(_SC_THREADS)
    " " ":THREADS"
#  endif
#  if defined(_SC_TIMEOUTS)
    " " ":TIMEOUTS"
#  endif
#  if defined(_SC_TIMERS)
    " " ":TIMERS"
#  endif
#  if defined(_SC_TRACE)
    " " ":TRACE"
#  endif
#  if defined(_SC_TRACE_EVENT_FILTER)
    " " ":TRACE-EVENT-FILTER"
#  endif
#  if defined(_SC_TRACE_EVENT_NAME_MAX)
    " " ":TRACE-EVENT-NAME-MAX"
#  endif
#  if defined(_SC_TRACE_INHERIT)
    " " ":TRACE-INHERIT"
#  endif
#  if defined(_SC_TRACE_LOG)
    " " ":TRACE-LOG"
#  endif
#  if defined(_SC_TRACE_NAME_MAX)
    " " ":TRACE-NAME-MAX"
#  endif
#  if defined(_SC_TRACE_SYS_MAX)
    " " ":TRACE-SYS-MAX"
#  endif
#  if defined(_SC_TRACE_USER_EVENT_MAX)
    " " ":TRACE-USER-EVENT-MAX"
#  endif
#  if defined(_SC_TYPED_MEMORY_OBJECTS)
    " " ":TYPED-MEMORY-OBJECTS"
#  endif
#  if defined(_SC_VERSION)
    " " ":VERSION"
#  endif
#  if defined(_SC_V6_ILP32_OFF32)
    " " ":V6-ILP32-OFF32"
#  endif
#  if defined(_SC_V6_ILP32_OFFBIG)
    " " ":V6-ILP32-OFFBIG"
#  endif
#  if defined(_SC_V6_LP64_OFF64)
    " " ":V6-LP64-OFF64"
#  endif
#  if defined(_SC_V6_LPBIG_OFFBIG)
    " " ":V6-LPBIG-OFFBIG"
#  endif
#  if defined(_SC_2_C_BIND)
    " " ":2-C-BIND"
#  endif
#  if defined(_SC_2_C_DEV)
    " " ":2-C-DEV"
#  endif
#  if defined(_SC_2_CHAR_TERM)
    " " ":2-CHAR-TERM"
#  endif
#  if defined(_SC_2_FORT_DEV)
    " " ":2-FORT-DEV"
#  endif
#  if defined(_SC_2_FORT_RUN)
    " " ":2-FORT-RUN"
#  endif
#  if defined(_SC_2_LOCALEDEF)
    " " ":2-LOCALEDEF"
#  endif
#  if defined(_SC_2_PBS)
    " " ":2-PBS"
#  endif
#  if defined(_SC_2_PBS_ACCOUNTING)
    " " ":2-PBS-ACCOUNTING"
#  endif
#  if defined(_SC_2_PBS_CHECKPOINT)
    " " ":2-PBS-CHECKPOINT"
#  endif
#  if defined(_SC_2_PBS_LOCATE)
    " " ":2-PBS-LOCATE"
#  endif
#  if defined(_SC_2_PBS_MESSAGE)
    " " ":2-PBS-MESSAGE"
#  endif
#  if defined(_SC_2_PBS_TRACK)
    " " ":2-PBS-TRACK"
#  endif
#  if defined(_SC_2_SW_DEV)
    " " ":2-SW-DEV"
#  endif
#  if defined(_SC_2_UPE)
    " " ":2-UPE"
#  endif
#  if defined(_SC_2_VERSION)
    " " ":2-VERSION"
#  endif
#  if defined(_SC_PAGESIZE)
    " " ":PAGESIZE"
#  endif
#  if defined(_SC_PHYS_PAGES)
    " " ":PHYS-PAGES"
#  endif
#  if defined(_SC_AVPHYS_PAGES)
    " " ":AVPHYS-PAGES"
#  endif
#  if defined(_SC_THREAD_DESTRUCTOR_ITERATIONS)
    " " ":THREAD-DESTRUCTOR-ITERATIONS"
#  endif
#  if defined(_SC_THREAD_KEYS_MAX)
    " " ":THREAD-KEYS-MAX"
#  endif
#  if defined(_SC_THREAD_STACK_MIN)
    " " ":THREAD-STACK-MIN"
#  endif
#  if defined(_SC_THREAD_THREADS_MAX)
    " " ":THREAD-THREADS-MAX"
#  endif
#  if defined(_SC_RE_DUP_MAX)
    " " ":RE-DUP-MAX"
#  endif
#  if defined(_SC_RTSIG_MAX)
    " " ":RTSIG-MAX"
#  endif
#  if defined(_SC_SEM_NSEMS_MAX)
    " " ":SEM-NSEMS-MAX"
#  endif
#  if defined(_SC_SEM_VALUE_MAX)
    " " ":SEM-VALUE-MAX"
#  endif
#  if defined(_SC_SIGQUEUE_MAX)
    " " ":SIGQUEUE-MAX"
#  endif
#  if defined(_SC_STREAM_MAX)
    " " ":STREAM-MAX"
#  endif
#  if defined(_SC_SYMLOOP_MAX)
    " " ":SYMLOOP-MAX"
#  endif
#  if defined(_SC_TIMER_MAX)
    " " ":TIMER-MAX"
#  endif
#  if defined(_SC_TTY_NAME_MAX)
    " " ":TTY-NAME-MAX"
#  endif
#  if defined(_SC_TZNAME_MAX)
    " " ":TZNAME-MAX"
#  endif
#  if defined(_SC_XBS5_ILP32_OFF32)
    " " ":XBS5-ILP32-OFF32"
#  endif
#  if defined(_SC_XBS5_ILP32_OFFBIG)
    " " ":XBS5-ILP32-OFFBIG"
#  endif
#  if defined(_SC_XBS5_LP64_OFF64)
    " " ":XBS5-LP64-OFF64"
#  endif
#  if defined(_SC_XBS5_LPBIG_OFFBIG)
    " " ":XBS5-LPBIG-OFFBIG"
#  endif
#  if defined(_SC_XOPEN_CRYPT)
    " " ":XOPEN-CRYPT"
#  endif
#  if defined(_SC_XOPEN_ENH_I18N)
    " " ":XOPEN-ENH-I18N"
#  endif
#  if defined(_SC_XOPEN_LEGACY)
    " " ":XOPEN-LEGACY"
#  endif
#  if defined(_SC_XOPEN_REALTIME)
    " " ":XOPEN-REALTIME"
#  endif
#  if defined(_SC_XOPEN_REALTIME_THREADS)
    " " ":XOPEN-REALTIME-THREADS"
#  endif
#  if defined(_SC_XOPEN_SHM)
    " " ":XOPEN-SHM"
#  endif
#  if defined(_SC_XOPEN_STREAMS)
    " " ":XOPEN-STREAMS"
#  endif
#  if defined(_SC_XOPEN_UNIX)
    " " ":XOPEN-UNIX"
#  endif
#  if defined(_SC_XOPEN_VERSION)
    " " ":XOPEN-VERSION"
#  endif
#  if defined(_SC_NPROCESSORS_CONF)
    " " ":NPROCESSORS-CONF"
#  endif
#  if defined(_SC_NPROCESSORS_ONLN)
    " " ":NPROCESSORS-ONLN"
#  endif
    " " "))"
  },
#endif
#if defined(HAVE_SYSLOG)
  {
    " " "(OR NULL INTEGER (MEMBER"
#  if defined(LOG_KERN)
    " " ":KERN"
#  endif
#  if defined(LOG_USER)
    " " ":USER"
#  endif
#  if defined(LOG_MAIL)
    " " ":MAIL"
#  endif
#  if defined(LOG_NEWS)
    " " ":NEWS"
#  endif
#  if defined(LOG_UUCP)
    " " ":UUCP"
#  endif
#  if defined(LOG_DAEMON)
    " " ":DAEMON"
#  endif
#  if defined(LOG_AUTH)
    " " ":AUTH"
#  endif
#  if defined(LOG_CRON)
    " " ":CRON"
#  endif
#  if defined(LOG_LPR)
    " " ":LPR"
#  endif
#  if defined(LOG_SYSLOG)
    " " ":SYSLOG"
#  endif
#  if defined(LOG_AUTHPRIV)
    " " ":AUTHPRIV"
#  endif
#  if defined(LOG_FTP)
    " " ":FTP"
#  endif
#  if defined(LOG_LOCAL0)
    " " ":LOCAL0"
#  endif
#  if defined(LOG_LOCAL1)
    " " ":LOCAL1"
#  endif
#  if defined(LOG_LOCAL2)
    " " ":LOCAL2"
#  endif
#  if defined(LOG_LOCAL3)
    " " ":LOCAL3"
#  endif
#  if defined(LOG_LOCAL4)
    " " ":LOCAL4"
#  endif
#  if defined(LOG_LOCAL5)
    " " ":LOCAL5"
#  endif
#  if defined(LOG_LOCAL6)
    " " ":LOCAL6"
#  endif
#  if defined(LOG_LOCAL7)
    " " ":LOCAL7"
#  endif
    " " "))"
  },
#endif
  {
    " " "(OR NULL INTEGER (MEMBER"
#  if defined(PRIO_PROCESS)
    " " ":PROCESS"
#  endif
#  if defined(PRIO_PGRP)
    " " ":PGRP"
#  endif
#  if defined(PRIO_USER)
    " " ":USER"
#  endif
    " " "))"
  },
#if defined(WIN32_NATIVE)
  {
    " " "(OR NULL INTEGER (MEMBER"
#  if defined(REALTIME_PRIORITY_CLASS)
    " " ":REALTIME"
#  endif
#  if defined(HIGH_PRIORITY_CLASS)
    " " ":HIGH"
#  endif
#  if defined(ABOVE_NORMAL_PRIORITY_CLASS)
    " " ":ABOVE-NORMAL"
#  endif
#  if defined(NORMAL_PRIORITY_CLASS)
    " " ":NORMAL"
#  endif
#  if defined(BELOW_NORMAL_PRIORITY_CLASS)
    " " ":BELOW-NORMAL"
#  endif
#  if defined(LOW_PRIORITY_CLASS)
    " " ":LOW"
#  endif
#  if defined(IDLE_PRIORITY_CLASS)
    " " ":IDLE"
#  endif
    " " "))"
  },
#endif
#if !(defined(WIN32_NATIVE))
  {
    " " "(OR NULL INTEGER (MEMBER"
    " " ":REALTIME"
    " " ":HIGH"
    " " ":ABOVE-NORMAL"
    " " ":NORMAL"
    " " ":BELOW-NORMAL"
    " " ":LOW"
    " " ":IDLE"
    " " "))"
  },
#endif
  0
};

struct module__syscalls__subr_tab_t {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  subr_t _subr_os_file_owner;
  subr_t _subr_os_priority;
  subr_t _subr_os__25set_priority;
#if defined(HAVE_CLOCK)
  subr_t _subr_posix_bogomips;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_clearerr;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_CLOSELOG))
  subr_t _subr_posix_closelog;
#endif
#if defined(HAVE_CONFSTR)
  subr_t _subr_posix_confstr;
#endif
#if defined(DEBUG_SPVW)
  subr_t _subr_posix_constsym;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_convert_attributes;
#endif
  subr_t _subr_posix_convert_mode;
  subr_t _subr_posix_copy_file;
#if defined(HAVE_CRYPT) && !defined(WIN32_NATIVE)
  subr_t _subr_posix_crypt;
#endif
  subr_t _subr_posix_duplicate_handle;
#if defined(HAVE_ENCRYPT) && !defined(WIN32_NATIVE)
  subr_t _subr_posix_encrypt;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_ENDUTXENT))
  subr_t _subr_posix_endutxent;
#endif
#if defined(HAVE_ERFC)
  subr_t _subr_posix_erf;
#endif
#if defined(HAVE_ERFC)
  subr_t _subr_posix_erfc;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_fclose;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_fdopen;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_feof;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_ferror;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_fflush;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_fileno;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_file_info;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_file_properties;
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  subr_t _subr_posix_file_stat;
#endif
#if defined(HAVE_FFI)
  subr_t _subr_posix_fopen;
#endif
#if defined(HAVE_GETEGID)
  subr_t _subr_posix_getegid;
#endif
#if defined(HAVE_GETEUID)
  subr_t _subr_posix_geteuid;
#endif
#if defined(HAVE_GETGID)
  subr_t _subr_posix_getgid;
#endif
#if defined(HAVE_GETGROUPS)
  subr_t _subr_posix_getgroups;
#endif
#if defined(HAVE_GETPGID)
  subr_t _subr_posix_getpgid;
#endif
#if defined(HAVE_GETPGRP)
  subr_t _subr_posix_getpgrp;
#endif
#if defined(HAVE_GETPPID)
  subr_t _subr_posix_getppid;
#endif
#if defined(HAVE_GETSID)
  subr_t _subr_posix_getsid;
#endif
#if defined(HAVE_GETUID)
  subr_t _subr_posix_getuid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXENT))
  subr_t _subr_posix_getutxent;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXID))
  subr_t _subr_posix_getutxid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXLINE))
  subr_t _subr_posix_getutxline;
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  subr_t _subr_posix_group_info;
#endif
  subr_t _subr_posix_j0;
  subr_t _subr_posix_j1;
  subr_t _subr_posix_jn;
#if defined(HAVE_KILL)
  subr_t _subr_posix_kill;
#endif
#if defined(HAVE_LGAMMA) || HAVE_DECL_LGAMMA_R
  subr_t _subr_posix_lgamma;
#endif
#if defined(HAVE_GETLOADAVG)
  subr_t _subr_posix_loadavg;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_make_shortcut;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_memory_status;
#endif
#if defined(HAVE_MKDTEMP) || defined(WIN32_NATIVE) || (defined(HAVE_MKDIR) && defined(HAVE_TEMPNAM))
  subr_t _subr_posix_mkdtemp;
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  subr_t _subr_posix_mknod;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  subr_t _subr_posix_mkstemp;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  subr_t _subr_posix_openlog;
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  subr_t _subr_posix_pathconf;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_PUTUTXLINE))
  subr_t _subr_posix_pututxline;
#endif
  subr_t _subr_posix_resolve_host_ipaddr;
#if defined(HAVE_GETRLIMIT)
  subr_t _subr_posix_rlimit;
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  subr_t _subr_posix_service;
#endif
#if defined(HAVE_SETKEY) && !defined(WIN32_NATIVE)
  subr_t _subr_posix_setkey;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_SETLOGMASK))
  subr_t _subr_posix_setlogmask;
#endif
#if defined(HAVE_SETPGRP)
  subr_t _subr_posix_setpgrp;
#endif
#if defined(HAVE_SETREGID)
  subr_t _subr_posix_setregid;
#endif
#if defined(HAVE_SETREUID)
  subr_t _subr_posix_setreuid;
#endif
#if defined(HAVE_SETSID)
  subr_t _subr_posix_setsid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_SETUTXENT))
  subr_t _subr_posix_setutxent;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  subr_t _subr_posix_set_file_stat;
#endif
#if defined(HAVE_SETRLIMIT)
  subr_t _subr_posix_set_rlimit;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_shortcut_info;
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  subr_t _subr_posix_stat_vfs;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  subr_t _subr_posix_stream_lock;
#endif
#if defined(HAVE_FCNTL)
  subr_t _subr_posix_stream_options;
#endif
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  subr_t _subr_posix_string_time;
#endif
#if defined(WIN32_NATIVE) || defined(HAVE_SYNC) || defined(HAVE_FSYNC)
  subr_t _subr_posix_sync;
#endif
#if defined(HAVE_SYSCONF)
  subr_t _subr_posix_sysconf;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_system_info;
#endif
  subr_t _subr_posix_tgamma;
#if defined(HAVE_UMASK)
  subr_t _subr_posix_umask;
#endif
#if defined(HAVE_UNAME)
  subr_t _subr_posix_uname;
#endif
#if defined(HAVE_GETRUSAGE)
  subr_t _subr_posix_usage;
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  subr_t _subr_posix_user_info;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_t _subr_posix_version;
#endif
  subr_t _subr_posix_y0;
  subr_t _subr_posix_y1;
  subr_t _subr_posix_yn;
#if defined(HAVE_SETEGID)
  subr_t _subr_posix__25setegid;
#endif
#if defined(HAVE_SETEUID)
  subr_t _subr_posix__25seteuid;
#endif
#if defined(HAVE_SETGID)
  subr_t _subr_posix__25setgid;
#endif
#if defined(HAVE_SETGROUPS)
  subr_t _subr_posix__25setgroups;
#endif
#if defined(HAVE_SETPGID)
  subr_t _subr_posix__25setpgid;
#endif
#if defined(HAVE_SETUID)
  subr_t _subr_posix__25setuid;
#endif
#if defined(HAVE_SYSLOG)
  subr_t _subr_posix__25syslog;
#endif
  int _dummy_to_avoid_trailing_comma_in_initializer;
};
extern struct module__syscalls__subr_tab_t module__syscalls__subr_tab;

#if defined(HAVE_SYSLOG)
static uintL syslog_opt_flags (void) {
  uintL flags = 0
#  ifdef LOG_NOWAIT
    | (missingp(STACK_(0)) ? 0 : LOG_NOWAIT)
#  endif
#  ifdef LOG_ODELAY
    | (missingp(STACK_(1)) ? 0 : LOG_ODELAY)
#  endif
#  ifdef LOG_NDELAY
    | (missingp(STACK_(2)) ? 0 : LOG_NDELAY)
#  endif
#  ifdef LOG_CONS
    | (missingp(STACK_(3)) ? 0 : LOG_CONS)
#  endif
#  ifdef LOG_PID
    | (missingp(STACK_(4)) ? 0 : LOG_PID)
#  endif
   ;
  skipSTACK(5);
  return flags;
}

#endif

#if defined(HAVE_FCNTL)
static const c_lisp_pair_t check_fcntl_cmd_table[] = {
 #ifdef F_GETFD
  { F_GETFD, &(O(object_Kfd)) },
 #endif
 #ifdef F_GETFL
  { F_GETFL, &(O(object_Kfl)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_fcntl_cmd_map = {
  check_fcntl_cmd_table,
  (sizeof(check_fcntl_cmd_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_fcntl_cmd"
};
#define check_fcntl_cmd(a) (int)map_lisp_to_c(a,&check_fcntl_cmd_map)
#define check_fcntl_cmd_reverse(a) map_c_to_lisp(a,&check_fcntl_cmd_map)

#endif
#if defined(HAVE_FCNTL)
static const c_lisp_pair_t check_fl_flags_table[] = {
 #ifdef O_RDONLY
  { O_RDONLY, &(O(object_Krdonly)) },
 #endif
 #ifdef O_WRONLY
  { O_WRONLY, &(O(object_Kwronly)) },
 #endif
 #ifdef O_RDWR
  { O_RDWR, &(O(object_Krdwr)) },
 #endif
 #ifdef O_APPEND
  { O_APPEND, &(O(object_Kappend)) },
 #endif
 #ifdef O_CREAT
  { O_CREAT, &(O(object_Kcreat)) },
 #endif
 #ifdef O_TRUNC
  { O_TRUNC, &(O(object_Ktrunc)) },
 #endif
 #ifdef O_EXCL
  { O_EXCL, &(O(object_Kexcl)) },
 #endif
 #ifdef O_NOCTTY
  { O_NOCTTY, &(O(object_Knoctty)) },
 #endif
 #ifdef O_SYNC
  { O_SYNC, &(O(object_Ksync)) },
 #endif
 #ifdef O_NONBLOCK
  { O_NONBLOCK, &(O(object_Knonblock)) },
 #endif
 #ifdef O_BINARY
  { O_BINARY, &(O(object_Kbinary)) },
 #endif
 #ifdef O_TEXT
  { O_TEXT, &(O(object_Ktext)) },
 #endif
 #ifdef O_NOINHERIT
  { O_NOINHERIT, &(O(object_Knoinherit)) },
 #endif
 #ifdef O_DIRECT
  { O_DIRECT, &(O(object_Kdirect)) },
 #endif
 #ifdef O_LARGEFILE
  { O_LARGEFILE, &(O(object_Klargefile)) },
 #endif
 #ifdef O_DIRECTORY
  { O_DIRECTORY, &(O(object_Kdirectory)) },
 #endif
 #ifdef O_NOFOLLOW
  { O_NOFOLLOW, &(O(object_Knofollow)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_fl_flags_map = {
  check_fl_flags_table,
  (sizeof(check_fl_flags_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_fl_flags"
};
#define check_fl_flags(a) (int)map_lisp_to_c(a,&check_fl_flags_map)
#define check_fl_flags_reverse(a) map_c_to_lisp(a,&check_fl_flags_map)
#define check_fl_flags_to_list(a) map_c_to_list(a,&check_fl_flags_map)
#define check_fl_flags_from_list(a) map_list_to_c(a,&check_fl_flags_map)

#endif
#if defined(HAVE_FCNTL)
static const c_lisp_pair_t check_fd_flags_table[] = {
 #ifdef FD_CLOEXEC
  { FD_CLOEXEC, &(O(object_Kcloexec)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_fd_flags_map = {
  check_fd_flags_table,
  (sizeof(check_fd_flags_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_fd_flags"
};
#define check_fd_flags(a) (int)map_lisp_to_c(a,&check_fd_flags_map)
#define check_fd_flags_reverse(a) map_c_to_lisp(a,&check_fd_flags_map)
#define check_fd_flags_to_list(a) map_c_to_list(a,&check_fd_flags_map)
#define check_fd_flags_from_list(a) map_list_to_c(a,&check_fd_flags_map)

#endif
#if defined(HAVE_SYSLOG)
static const c_lisp_pair_t check_syslog_severity_table[] = {
 #ifdef LOG_EMERG
  { LOG_EMERG, &(O(object_Kemerg)) },
 #endif
 #ifdef LOG_ALERT
  { LOG_ALERT, &(O(object_Kalert)) },
 #endif
 #ifdef LOG_CRIT
  { LOG_CRIT, &(O(object_Kcrit)) },
 #endif
 #ifdef LOG_ERR
  { LOG_ERR, &(O(object_Kerr)) },
 #endif
 #ifdef LOG_WARNING
  { LOG_WARNING, &(O(object_Kwarning)) },
 #endif
 #ifdef LOG_NOTICE
  { LOG_NOTICE, &(O(object_Knotice)) },
 #endif
 #ifdef LOG_INFO
  { LOG_INFO, &(O(object_Kinfo)) },
 #endif
 #ifdef LOG_DEBUG
  { LOG_DEBUG, &(O(object_Kdebug)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_syslog_severity_map = {
  check_syslog_severity_table,
  (sizeof(check_syslog_severity_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_syslog_severity"
};
#define check_syslog_severity(a) (int)map_lisp_to_c(a,&check_syslog_severity_map)
#define check_syslog_severity_reverse(a) map_c_to_lisp(a,&check_syslog_severity_map)

#endif
#if defined(HAVE_SYSLOG)
static const c_lisp_pair_t check_syslog_facility_table[] = {
 #ifdef LOG_KERN
  { LOG_KERN, &(O(object_Kkern)) },
 #endif
 #ifdef LOG_USER
  { LOG_USER, &(O(object_Kuser)) },
 #endif
 #ifdef LOG_MAIL
  { LOG_MAIL, &(O(object_Kmail)) },
 #endif
 #ifdef LOG_NEWS
  { LOG_NEWS, &(O(object_Knews)) },
 #endif
 #ifdef LOG_UUCP
  { LOG_UUCP, &(O(object_Kuucp)) },
 #endif
 #ifdef LOG_DAEMON
  { LOG_DAEMON, &(O(object_Kdaemon)) },
 #endif
 #ifdef LOG_AUTH
  { LOG_AUTH, &(O(object_Kauth)) },
 #endif
 #ifdef LOG_CRON
  { LOG_CRON, &(O(object_Kcron)) },
 #endif
 #ifdef LOG_LPR
  { LOG_LPR, &(O(object_Klpr)) },
 #endif
 #ifdef LOG_SYSLOG
  { LOG_SYSLOG, &(O(object_Ksyslog)) },
 #endif
 #ifdef LOG_AUTHPRIV
  { LOG_AUTHPRIV, &(O(object_Kauthpriv)) },
 #endif
 #ifdef LOG_FTP
  { LOG_FTP, &(O(object_Kftp)) },
 #endif
 #ifdef LOG_LOCAL0
  { LOG_LOCAL0, &(O(object_Klocal0)) },
 #endif
 #ifdef LOG_LOCAL1
  { LOG_LOCAL1, &(O(object_Klocal1)) },
 #endif
 #ifdef LOG_LOCAL2
  { LOG_LOCAL2, &(O(object_Klocal2)) },
 #endif
 #ifdef LOG_LOCAL3
  { LOG_LOCAL3, &(O(object_Klocal3)) },
 #endif
 #ifdef LOG_LOCAL4
  { LOG_LOCAL4, &(O(object_Klocal4)) },
 #endif
 #ifdef LOG_LOCAL5
  { LOG_LOCAL5, &(O(object_Klocal5)) },
 #endif
 #ifdef LOG_LOCAL6
  { LOG_LOCAL6, &(O(object_Klocal6)) },
 #endif
 #ifdef LOG_LOCAL7
  { LOG_LOCAL7, &(O(object_Klocal7)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_syslog_facility_map = {
  check_syslog_facility_table,
  (sizeof(check_syslog_facility_table)/sizeof(c_lisp_pair_t))-1,
# if defined(LOG_USER)
  LOG_USER,true,
# else
  0,false,
# endif
  true,
  "check_syslog_facility"
};
#define check_syslog_facility(a) (int)map_lisp_to_c(a,&check_syslog_facility_map)
#define check_syslog_facility_reverse(a) map_c_to_lisp(a,&check_syslog_facility_map)

#endif
#if defined(HAVE_UTMPX_H)
static const c_lisp_pair_t check_ut_type_table[] = {
 #ifdef EMPTY
  { EMPTY, &(O(object_Kempty)) },
 #endif
 #ifdef RUN_LVL
  { RUN_LVL, &(O(object_Krun_lvl)) },
 #endif
 #ifdef BOOT_TIME
  { BOOT_TIME, &(O(object_Kboot_time)) },
 #endif
 #ifdef OLD_TIME
  { OLD_TIME, &(O(object_Kold_time)) },
 #endif
 #ifdef NEW_TIME
  { NEW_TIME, &(O(object_Knew_time)) },
 #endif
 #ifdef USER_PROCESS
  { USER_PROCESS, &(O(object_Kuser_process)) },
 #endif
 #ifdef INIT_PROCESS
  { INIT_PROCESS, &(O(object_Kinit_process)) },
 #endif
 #ifdef LOGIN_PROCESS
  { LOGIN_PROCESS, &(O(object_Klogin_process)) },
 #endif
 #ifdef DEAD_PROCESS
  { DEAD_PROCESS, &(O(object_Kdead_process)) },
 #endif
 #ifdef ACCOUNTING
  { ACCOUNTING, &(O(object_Kaccounting)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_ut_type_map = {
  check_ut_type_table,
  (sizeof(check_ut_type_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_ut_type"
};
#define check_ut_type(a) (int)map_lisp_to_c(a,&check_ut_type_map)
#define check_ut_type_reverse(a) map_c_to_lisp(a,&check_ut_type_map)

#endif
static const c_lisp_pair_t check_signal_table[] = {
 #ifdef SIGABRT
  { SIGABRT, &(O(object_Ksigabrt)) },
 #endif
 #ifdef SIGALRM
  { SIGALRM, &(O(object_Ksigalrm)) },
 #endif
 #ifdef SIGBUS
  { SIGBUS, &(O(object_Ksigbus)) },
 #endif
 #ifdef SIGCHLD
  { SIGCHLD, &(O(object_Ksigchld)) },
 #endif
 #ifdef SIGCONT
  { SIGCONT, &(O(object_Ksigcont)) },
 #endif
 #ifdef SIGFPE
  { SIGFPE, &(O(object_Ksigfpe)) },
 #endif
 #ifdef SIGHUP
  { SIGHUP, &(O(object_Ksighup)) },
 #endif
 #ifdef SIGILL
  { SIGILL, &(O(object_Ksigill)) },
 #endif
 #ifdef SIGINT
  { SIGINT, &(O(object_Ksigint)) },
 #endif
 #ifdef SIGKILL
  { SIGKILL, &(O(object_Ksigkill)) },
 #endif
 #ifdef SIGPIPE
  { SIGPIPE, &(O(object_Ksigpipe)) },
 #endif
 #ifdef SIGQUIT
  { SIGQUIT, &(O(object_Ksigquit)) },
 #endif
 #ifdef SIGSEGV
  { SIGSEGV, &(O(object_Ksigsegv)) },
 #endif
 #ifdef SIGSTOP
  { SIGSTOP, &(O(object_Ksigstop)) },
 #endif
 #ifdef SIGTERM
  { SIGTERM, &(O(object_Ksigterm)) },
 #endif
 #ifdef SIGTSTP
  { SIGTSTP, &(O(object_Ksigtstp)) },
 #endif
 #ifdef SIGTTIN
  { SIGTTIN, &(O(object_Ksigttin)) },
 #endif
 #ifdef SIGTTOU
  { SIGTTOU, &(O(object_Ksigttou)) },
 #endif
 #ifdef SIGUSR1
  { SIGUSR1, &(O(object_Ksigusr1)) },
 #endif
 #ifdef SIGUSR2
  { SIGUSR2, &(O(object_Ksigusr2)) },
 #endif
 #ifdef SIGPOLL
  { SIGPOLL, &(O(object_Ksigpoll)) },
 #endif
 #ifdef SIGPROF
  { SIGPROF, &(O(object_Ksigprof)) },
 #endif
 #ifdef SIGSYS
  { SIGSYS, &(O(object_Ksigsys)) },
 #endif
 #ifdef SIGTRAP
  { SIGTRAP, &(O(object_Ksigtrap)) },
 #endif
 #ifdef SIGURG
  { SIGURG, &(O(object_Ksigurg)) },
 #endif
 #ifdef SIGVTALRM
  { SIGVTALRM, &(O(object_Ksigvtalrm)) },
 #endif
 #ifdef SIGXCPU
  { SIGXCPU, &(O(object_Ksigxcpu)) },
 #endif
 #ifdef SIGXFSZ
  { SIGXFSZ, &(O(object_Ksigxfsz)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_signal_map = {
  check_signal_table,
  (sizeof(check_signal_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_signal"
};
#define check_signal(a) (int)map_lisp_to_c(a,&check_signal_map)
#define check_signal_reverse(a) map_c_to_lisp(a,&check_signal_map)

#if defined(WIN32_NATIVE)
static const c_lisp_pair_t check_priority_value_table[] = {
 #ifdef REALTIME_PRIORITY_CLASS
  { REALTIME_PRIORITY_CLASS, &(O(object_Krealtime)) },
 #endif
 #ifdef HIGH_PRIORITY_CLASS
  { HIGH_PRIORITY_CLASS, &(O(object_Khigh)) },
 #endif
 #ifdef ABOVE_NORMAL_PRIORITY_CLASS
  { ABOVE_NORMAL_PRIORITY_CLASS, &(O(object_Kabove_normal)) },
 #endif
 #ifdef NORMAL_PRIORITY_CLASS
  { NORMAL_PRIORITY_CLASS, &(O(object_Knormal)) },
 #endif
 #ifdef BELOW_NORMAL_PRIORITY_CLASS
  { BELOW_NORMAL_PRIORITY_CLASS, &(O(object_Kbelow_normal)) },
 #endif
 #ifdef LOW_PRIORITY_CLASS
  { LOW_PRIORITY_CLASS, &(O(object_Klow)) },
 #endif
 #ifdef IDLE_PRIORITY_CLASS
  { IDLE_PRIORITY_CLASS, &(O(object_Kidle)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_priority_value_map = {
  check_priority_value_table,
  (sizeof(check_priority_value_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_priority_value"
};
#define check_priority_value(a) (int)map_lisp_to_c(a,&check_priority_value_map)
#define check_priority_value_reverse(a) map_c_to_lisp(a,&check_priority_value_map)

#endif
#if !(defined(WIN32_NATIVE))
static const c_lisp_pair_t check_priority_value_table[] = {
  { -NZERO, &(O(object_Krealtime)) },
  { (-NZERO/2), &(O(object_Khigh)) },
  { (-NZERO/4), &(O(object_Kabove_normal)) },
  { 0, &(O(object_Knormal)) },
  { (NZERO/4), &(O(object_Kbelow_normal)) },
  { (NZERO/2), &(O(object_Klow)) },
  { NZERO, &(O(object_Kidle)) },
  { 0, NULL }
};
static const c_lisp_map_t check_priority_value_map = {
  check_priority_value_table,
  (sizeof(check_priority_value_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_priority_value"
};
#define check_priority_value(a) (int)map_lisp_to_c(a,&check_priority_value_map)
#define check_priority_value_reverse(a) map_c_to_lisp(a,&check_priority_value_map)

#endif
static const c_lisp_pair_t check_priority_which_table[] = {
 #ifdef PRIO_PROCESS
  { PRIO_PROCESS, &(O(object_Kprocess)) },
 #endif
 #ifdef PRIO_PGRP
  { PRIO_PGRP, &(O(object_Kpgrp)) },
 #endif
 #ifdef PRIO_USER
  { PRIO_USER, &(O(object_Kuser)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_priority_which_map = {
  check_priority_which_table,
  (sizeof(check_priority_which_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_priority_which"
};
#define check_priority_which(a) (int)map_lisp_to_c(a,&check_priority_which_map)
#define check_priority_which_reverse(a) map_c_to_lisp(a,&check_priority_which_map)

#if defined(HAVE_SYSCONF)
static const c_lisp_pair_t sysconf_arg_table[] = {
 #ifdef _SC_AIO_LISTIO_MAX
  { _SC_AIO_LISTIO_MAX, &(O(object_Kaio_listio_max)) },
 #endif
 #ifdef _SC_AIO_MAX
  { _SC_AIO_MAX, &(O(object_Kaio_max)) },
 #endif
 #ifdef _SC_AIO_PRIO_DELTA_MAX
  { _SC_AIO_PRIO_DELTA_MAX, &(O(object_Kaio_prio_delta_max)) },
 #endif
 #ifdef _SC_ARG_MAX
  { _SC_ARG_MAX, &(O(object_Karg_max)) },
 #endif
 #ifdef _SC_ATEXIT_MAX
  { _SC_ATEXIT_MAX, &(O(object_Katexit_max)) },
 #endif
 #ifdef _SC_BC_BASE_MAX
  { _SC_BC_BASE_MAX, &(O(object_Kbc_base_max)) },
 #endif
 #ifdef _SC_BC_DIM_MAX
  { _SC_BC_DIM_MAX, &(O(object_Kbc_dim_max)) },
 #endif
 #ifdef _SC_BC_SCALE_MAX
  { _SC_BC_SCALE_MAX, &(O(object_Kbc_scale_max)) },
 #endif
 #ifdef _SC_BC_STRING_MAX
  { _SC_BC_STRING_MAX, &(O(object_Kbc_string_max)) },
 #endif
 #ifdef _SC_CHILD_MAX
  { _SC_CHILD_MAX, &(O(object_Kchild_max)) },
 #endif
 #ifdef _SC_CLK_TCK
  { _SC_CLK_TCK, &(O(object_Kclk_tck)) },
 #endif
 #ifdef _SC_COLL_WEIGHTS_MAX
  { _SC_COLL_WEIGHTS_MAX, &(O(object_Kcoll_weights_max)) },
 #endif
 #ifdef _SC_DELAYTIMER_MAX
  { _SC_DELAYTIMER_MAX, &(O(object_Kdelaytimer_max)) },
 #endif
 #ifdef _SC_EXPR_NEST_MAX
  { _SC_EXPR_NEST_MAX, &(O(object_Kexpr_nest_max)) },
 #endif
 #ifdef _SC_HOST_NAME_MAX
  { _SC_HOST_NAME_MAX, &(O(object_Khost_name_max)) },
 #endif
 #ifdef _SC_IOV_MAX
  { _SC_IOV_MAX, &(O(object_Kiov_max)) },
 #endif
 #ifdef _SC_LINE_MAX
  { _SC_LINE_MAX, &(O(object_Kline_max)) },
 #endif
 #ifdef _SC_LOGIN_NAME_MAX
  { _SC_LOGIN_NAME_MAX, &(O(object_Klogin_name_max)) },
 #endif
 #ifdef _SC_NGROUPS_MAX
  { _SC_NGROUPS_MAX, &(O(object_Kngroups_max)) },
 #endif
 #ifdef _SC_GETGR_R_SIZE_MAX
  { _SC_GETGR_R_SIZE_MAX, &(O(object_Kgetgr_r_size_max)) },
 #endif
 #ifdef _SC_GETPW_R_SIZE_MAX
  { _SC_GETPW_R_SIZE_MAX, &(O(object_Kgetpw_r_size_max)) },
 #endif
 #ifdef _SC_MQ_OPEN_MAX
  { _SC_MQ_OPEN_MAX, &(O(object_Kmq_open_max)) },
 #endif
 #ifdef _SC_MQ_PRIO_MAX
  { _SC_MQ_PRIO_MAX, &(O(object_Kmq_prio_max)) },
 #endif
 #ifdef _SC_OPEN_MAX
  { _SC_OPEN_MAX, &(O(object_Kopen_max)) },
 #endif
 #ifdef _SC_ADVISORY_INFO
  { _SC_ADVISORY_INFO, &(O(object_Kadvisory_info)) },
 #endif
 #ifdef _SC_BARRIERS
  { _SC_BARRIERS, &(O(object_Kbarriers)) },
 #endif
 #ifdef _SC_ASYNCHRONOUS_IO
  { _SC_ASYNCHRONOUS_IO, &(O(object_Kasynchronous_io)) },
 #endif
 #ifdef _SC_CLOCK_SELECTION
  { _SC_CLOCK_SELECTION, &(O(object_Kclock_selection)) },
 #endif
 #ifdef _SC_CPUTIME
  { _SC_CPUTIME, &(O(object_Kcputime)) },
 #endif
 #ifdef _SC_FSYNC
  { _SC_FSYNC, &(O(object_Kfsync)) },
 #endif
 #ifdef _SC_IPV6
  { _SC_IPV6, &(O(object_Kipv6)) },
 #endif
 #ifdef _SC_JOB_CONTROL
  { _SC_JOB_CONTROL, &(O(object_Kjob_control)) },
 #endif
 #ifdef _SC_MAPPED_FILES
  { _SC_MAPPED_FILES, &(O(object_Kmapped_files)) },
 #endif
 #ifdef _SC_MEMLOCK
  { _SC_MEMLOCK, &(O(object_Kmemlock)) },
 #endif
 #ifdef _SC_MEMLOCK_RANGE
  { _SC_MEMLOCK_RANGE, &(O(object_Kmemlock_range)) },
 #endif
 #ifdef _SC_MEMORY_PROTECTION
  { _SC_MEMORY_PROTECTION, &(O(object_Kmemory_protection)) },
 #endif
 #ifdef _SC_MESSAGE_PASSING
  { _SC_MESSAGE_PASSING, &(O(object_Kmessage_passing)) },
 #endif
 #ifdef _SC_MONOTONIC_CLOCK
  { _SC_MONOTONIC_CLOCK, &(O(object_Kmonotonic_clock)) },
 #endif
 #ifdef _SC_PRIORITIZED_IO
  { _SC_PRIORITIZED_IO, &(O(object_Kprioritized_io)) },
 #endif
 #ifdef _SC_PRIORITY_SCHEDULING
  { _SC_PRIORITY_SCHEDULING, &(O(object_Kpriority_scheduling)) },
 #endif
 #ifdef _SC_RAW_SOCKETS
  { _SC_RAW_SOCKETS, &(O(object_Kraw_sockets)) },
 #endif
 #ifdef _SC_READER_WRITER_LOCKS
  { _SC_READER_WRITER_LOCKS, &(O(object_Kreader_writer_locks)) },
 #endif
 #ifdef _SC_REALTIME_SIGNALS
  { _SC_REALTIME_SIGNALS, &(O(object_Krealtime_signals)) },
 #endif
 #ifdef _SC_REGEXP
  { _SC_REGEXP, &(O(object_Kregexp)) },
 #endif
 #ifdef _SC_SAVED_IDS
  { _SC_SAVED_IDS, &(O(object_Ksaved_ids)) },
 #endif
 #ifdef _SC_SEMAPHORES
  { _SC_SEMAPHORES, &(O(object_Ksemaphores)) },
 #endif
 #ifdef _SC_SHARED_MEMORY_OBJECTS
  { _SC_SHARED_MEMORY_OBJECTS, &(O(object_Kshared_memory_objects)) },
 #endif
 #ifdef _SC_SHELL
  { _SC_SHELL, &(O(object_Kshell)) },
 #endif
 #ifdef _SC_SPAWN
  { _SC_SPAWN, &(O(object_Kspawn)) },
 #endif
 #ifdef _SC_SPIN_LOCKS
  { _SC_SPIN_LOCKS, &(O(object_Kspin_locks)) },
 #endif
 #ifdef _SC_SPORADIC_SERVER
  { _SC_SPORADIC_SERVER, &(O(object_Ksporadic_server)) },
 #endif
 #ifdef _SC_SS_REPL_MAX
  { _SC_SS_REPL_MAX, &(O(object_Kss_repl_max)) },
 #endif
 #ifdef _SC_SYNCHRONIZED_IO
  { _SC_SYNCHRONIZED_IO, &(O(object_Ksynchronized_io)) },
 #endif
 #ifdef _SC_THREAD_ATTR_STACKADDR
  { _SC_THREAD_ATTR_STACKADDR, &(O(object_Kthread_attr_stackaddr)) },
 #endif
 #ifdef _SC_THREAD_ATTR_STACKSIZE
  { _SC_THREAD_ATTR_STACKSIZE, &(O(object_Kthread_attr_stacksize)) },
 #endif
 #ifdef _SC_THREAD_CPUTIME
  { _SC_THREAD_CPUTIME, &(O(object_Kthread_cputime)) },
 #endif
 #ifdef _SC_THREAD_PRIO_INHERIT
  { _SC_THREAD_PRIO_INHERIT, &(O(object_Kthread_prio_inherit)) },
 #endif
 #ifdef _SC_THREAD_PRIO_PROTECT
  { _SC_THREAD_PRIO_PROTECT, &(O(object_Kthread_prio_protect)) },
 #endif
 #ifdef _SC_THREAD_PRIORITY_SCHEDULING
  { _SC_THREAD_PRIORITY_SCHEDULING, &(O(object_Kthread_priority_scheduling)) },
 #endif
 #ifdef _SC_THREAD_PROCESS_SHARED
  { _SC_THREAD_PROCESS_SHARED, &(O(object_Kthread_process_shared)) },
 #endif
 #ifdef _SC_THREAD_SAFE_FUNCTIONS
  { _SC_THREAD_SAFE_FUNCTIONS, &(O(object_Kthread_safe_functions)) },
 #endif
 #ifdef _SC_THREAD_SPORADIC_SERVER
  { _SC_THREAD_SPORADIC_SERVER, &(O(object_Kthread_sporadic_server)) },
 #endif
 #ifdef _SC_THREADS
  { _SC_THREADS, &(O(object_Kthreads)) },
 #endif
 #ifdef _SC_TIMEOUTS
  { _SC_TIMEOUTS, &(O(object_Ktimeouts)) },
 #endif
 #ifdef _SC_TIMERS
  { _SC_TIMERS, &(O(object_Ktimers)) },
 #endif
 #ifdef _SC_TRACE
  { _SC_TRACE, &(O(object_Ktrace)) },
 #endif
 #ifdef _SC_TRACE_EVENT_FILTER
  { _SC_TRACE_EVENT_FILTER, &(O(object_Ktrace_event_filter)) },
 #endif
 #ifdef _SC_TRACE_EVENT_NAME_MAX
  { _SC_TRACE_EVENT_NAME_MAX, &(O(object_Ktrace_event_name_max)) },
 #endif
 #ifdef _SC_TRACE_INHERIT
  { _SC_TRACE_INHERIT, &(O(object_Ktrace_inherit)) },
 #endif
 #ifdef _SC_TRACE_LOG
  { _SC_TRACE_LOG, &(O(object_Ktrace_log)) },
 #endif
 #ifdef _SC_TRACE_NAME_MAX
  { _SC_TRACE_NAME_MAX, &(O(object_Ktrace_name_max)) },
 #endif
 #ifdef _SC_TRACE_SYS_MAX
  { _SC_TRACE_SYS_MAX, &(O(object_Ktrace_sys_max)) },
 #endif
 #ifdef _SC_TRACE_USER_EVENT_MAX
  { _SC_TRACE_USER_EVENT_MAX, &(O(object_Ktrace_user_event_max)) },
 #endif
 #ifdef _SC_TYPED_MEMORY_OBJECTS
  { _SC_TYPED_MEMORY_OBJECTS, &(O(object_Ktyped_memory_objects)) },
 #endif
 #ifdef _SC_VERSION
  { _SC_VERSION, &(O(object_Kversion)) },
 #endif
 #ifdef _SC_V6_ILP32_OFF32
  { _SC_V6_ILP32_OFF32, &(O(object_Kv6_ilp32_off32)) },
 #endif
 #ifdef _SC_V6_ILP32_OFFBIG
  { _SC_V6_ILP32_OFFBIG, &(O(object_Kv6_ilp32_offbig)) },
 #endif
 #ifdef _SC_V6_LP64_OFF64
  { _SC_V6_LP64_OFF64, &(O(object_Kv6_lp64_off64)) },
 #endif
 #ifdef _SC_V6_LPBIG_OFFBIG
  { _SC_V6_LPBIG_OFFBIG, &(O(object_Kv6_lpbig_offbig)) },
 #endif
 #ifdef _SC_2_C_BIND
  { _SC_2_C_BIND, &(O(object_K2_c_bind)) },
 #endif
 #ifdef _SC_2_C_DEV
  { _SC_2_C_DEV, &(O(object_K2_c_dev)) },
 #endif
 #ifdef _SC_2_CHAR_TERM
  { _SC_2_CHAR_TERM, &(O(object_K2_char_term)) },
 #endif
 #ifdef _SC_2_FORT_DEV
  { _SC_2_FORT_DEV, &(O(object_K2_fort_dev)) },
 #endif
 #ifdef _SC_2_FORT_RUN
  { _SC_2_FORT_RUN, &(O(object_K2_fort_run)) },
 #endif
 #ifdef _SC_2_LOCALEDEF
  { _SC_2_LOCALEDEF, &(O(object_K2_localedef)) },
 #endif
 #ifdef _SC_2_PBS
  { _SC_2_PBS, &(O(object_K2_pbs)) },
 #endif
 #ifdef _SC_2_PBS_ACCOUNTING
  { _SC_2_PBS_ACCOUNTING, &(O(object_K2_pbs_accounting)) },
 #endif
 #ifdef _SC_2_PBS_CHECKPOINT
  { _SC_2_PBS_CHECKPOINT, &(O(object_K2_pbs_checkpoint)) },
 #endif
 #ifdef _SC_2_PBS_LOCATE
  { _SC_2_PBS_LOCATE, &(O(object_K2_pbs_locate)) },
 #endif
 #ifdef _SC_2_PBS_MESSAGE
  { _SC_2_PBS_MESSAGE, &(O(object_K2_pbs_message)) },
 #endif
 #ifdef _SC_2_PBS_TRACK
  { _SC_2_PBS_TRACK, &(O(object_K2_pbs_track)) },
 #endif
 #ifdef _SC_2_SW_DEV
  { _SC_2_SW_DEV, &(O(object_K2_sw_dev)) },
 #endif
 #ifdef _SC_2_UPE
  { _SC_2_UPE, &(O(object_K2_upe)) },
 #endif
 #ifdef _SC_2_VERSION
  { _SC_2_VERSION, &(O(object_K2_version)) },
 #endif
 #ifdef _SC_PAGESIZE
  { _SC_PAGESIZE, &(O(object_Kpagesize)) },
 #endif
 #ifdef _SC_PHYS_PAGES
  { _SC_PHYS_PAGES, &(O(object_Kphys_pages)) },
 #endif
 #ifdef _SC_AVPHYS_PAGES
  { _SC_AVPHYS_PAGES, &(O(object_Kavphys_pages)) },
 #endif
 #ifdef _SC_THREAD_DESTRUCTOR_ITERATIONS
  { _SC_THREAD_DESTRUCTOR_ITERATIONS, &(O(object_Kthread_destructor_iterations)) },
 #endif
 #ifdef _SC_THREAD_KEYS_MAX
  { _SC_THREAD_KEYS_MAX, &(O(object_Kthread_keys_max)) },
 #endif
 #ifdef _SC_THREAD_STACK_MIN
  { _SC_THREAD_STACK_MIN, &(O(object_Kthread_stack_min)) },
 #endif
 #ifdef _SC_THREAD_THREADS_MAX
  { _SC_THREAD_THREADS_MAX, &(O(object_Kthread_threads_max)) },
 #endif
 #ifdef _SC_RE_DUP_MAX
  { _SC_RE_DUP_MAX, &(O(object_Kre_dup_max)) },
 #endif
 #ifdef _SC_RTSIG_MAX
  { _SC_RTSIG_MAX, &(O(object_Krtsig_max)) },
 #endif
 #ifdef _SC_SEM_NSEMS_MAX
  { _SC_SEM_NSEMS_MAX, &(O(object_Ksem_nsems_max)) },
 #endif
 #ifdef _SC_SEM_VALUE_MAX
  { _SC_SEM_VALUE_MAX, &(O(object_Ksem_value_max)) },
 #endif
 #ifdef _SC_SIGQUEUE_MAX
  { _SC_SIGQUEUE_MAX, &(O(object_Ksigqueue_max)) },
 #endif
 #ifdef _SC_STREAM_MAX
  { _SC_STREAM_MAX, &(O(object_Kstream_max)) },
 #endif
 #ifdef _SC_SYMLOOP_MAX
  { _SC_SYMLOOP_MAX, &(O(object_Ksymloop_max)) },
 #endif
 #ifdef _SC_TIMER_MAX
  { _SC_TIMER_MAX, &(O(object_Ktimer_max)) },
 #endif
 #ifdef _SC_TTY_NAME_MAX
  { _SC_TTY_NAME_MAX, &(O(object_Ktty_name_max)) },
 #endif
 #ifdef _SC_TZNAME_MAX
  { _SC_TZNAME_MAX, &(O(object_Ktzname_max)) },
 #endif
 #ifdef _SC_XBS5_ILP32_OFF32
  { _SC_XBS5_ILP32_OFF32, &(O(object_Kxbs5_ilp32_off32)) },
 #endif
 #ifdef _SC_XBS5_ILP32_OFFBIG
  { _SC_XBS5_ILP32_OFFBIG, &(O(object_Kxbs5_ilp32_offbig)) },
 #endif
 #ifdef _SC_XBS5_LP64_OFF64
  { _SC_XBS5_LP64_OFF64, &(O(object_Kxbs5_lp64_off64)) },
 #endif
 #ifdef _SC_XBS5_LPBIG_OFFBIG
  { _SC_XBS5_LPBIG_OFFBIG, &(O(object_Kxbs5_lpbig_offbig)) },
 #endif
 #ifdef _SC_XOPEN_CRYPT
  { _SC_XOPEN_CRYPT, &(O(object_Kxopen_crypt)) },
 #endif
 #ifdef _SC_XOPEN_ENH_I18N
  { _SC_XOPEN_ENH_I18N, &(O(object_Kxopen_enh_i18n)) },
 #endif
 #ifdef _SC_XOPEN_LEGACY
  { _SC_XOPEN_LEGACY, &(O(object_Kxopen_legacy)) },
 #endif
 #ifdef _SC_XOPEN_REALTIME
  { _SC_XOPEN_REALTIME, &(O(object_Kxopen_realtime)) },
 #endif
 #ifdef _SC_XOPEN_REALTIME_THREADS
  { _SC_XOPEN_REALTIME_THREADS, &(O(object_Kxopen_realtime_threads)) },
 #endif
 #ifdef _SC_XOPEN_SHM
  { _SC_XOPEN_SHM, &(O(object_Kxopen_shm)) },
 #endif
 #ifdef _SC_XOPEN_STREAMS
  { _SC_XOPEN_STREAMS, &(O(object_Kxopen_streams)) },
 #endif
 #ifdef _SC_XOPEN_UNIX
  { _SC_XOPEN_UNIX, &(O(object_Kxopen_unix)) },
 #endif
 #ifdef _SC_XOPEN_VERSION
  { _SC_XOPEN_VERSION, &(O(object_Kxopen_version)) },
 #endif
 #ifdef _SC_NPROCESSORS_CONF
  { _SC_NPROCESSORS_CONF, &(O(object_Knprocessors_conf)) },
 #endif
 #ifdef _SC_NPROCESSORS_ONLN
  { _SC_NPROCESSORS_ONLN, &(O(object_Knprocessors_onln)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t sysconf_arg_map = {
  sysconf_arg_table,
  (sizeof(sysconf_arg_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "sysconf_arg"
};
#define sysconf_arg(a) (int)map_lisp_to_c(a,&sysconf_arg_map)
#define sysconf_arg_reverse(a) map_c_to_lisp(a,&sysconf_arg_map)

#endif
#if defined(HAVE_CONFSTR)
static const c_lisp_pair_t confstr_arg_table[] = {
 #ifdef _CS_PATH
  { _CS_PATH, &(O(object_Kpath)) },
 #endif
 #ifdef _CS_POSIX_V6_ILP32_OFF32_CFLAGS
  { _CS_POSIX_V6_ILP32_OFF32_CFLAGS, &(O(object_Kposix_v6_ilp32_off32_cflags)) },
 #endif
 #ifdef _CS_POSIX_V6_ILP32_OFF32_LDFLAGS
  { _CS_POSIX_V6_ILP32_OFF32_LDFLAGS, &(O(object_Kposix_v6_ilp32_off32_ldflags)) },
 #endif
 #ifdef _CS_POSIX_V6_ILP32_OFF32_LIBS
  { _CS_POSIX_V6_ILP32_OFF32_LIBS, &(O(object_Kposix_v6_ilp32_off32_libs)) },
 #endif
 #ifdef _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS
  { _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS, &(O(object_Kposix_v6_ilp32_offbig_cflags)) },
 #endif
 #ifdef _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS
  { _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS, &(O(object_Kposix_v6_ilp32_offbig_ldflags)) },
 #endif
 #ifdef _CS_POSIX_V6_ILP32_OFFBIG_LIBS
  { _CS_POSIX_V6_ILP32_OFFBIG_LIBS, &(O(object_Kposix_v6_ilp32_offbig_libs)) },
 #endif
 #ifdef _CS_POSIX_V6_LP64_OFF64_CFLAGS
  { _CS_POSIX_V6_LP64_OFF64_CFLAGS, &(O(object_Kposix_v6_lp64_off64_cflags)) },
 #endif
 #ifdef _CS_POSIX_V6_LP64_OFF64_LDFLAGS
  { _CS_POSIX_V6_LP64_OFF64_LDFLAGS, &(O(object_Kposix_v6_lp64_off64_ldflags)) },
 #endif
 #ifdef _CS_POSIX_V6_LP64_OFF64_LIBS
  { _CS_POSIX_V6_LP64_OFF64_LIBS, &(O(object_Kposix_v6_lp64_off64_libs)) },
 #endif
 #ifdef _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS
  { _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS, &(O(object_Kposix_v6_lpbig_offbig_cflags)) },
 #endif
 #ifdef _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS
  { _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS, &(O(object_Kposix_v6_lpbig_offbig_ldflags)) },
 #endif
 #ifdef _CS_POSIX_V6_LPBIG_OFFBIG_LIBS
  { _CS_POSIX_V6_LPBIG_OFFBIG_LIBS, &(O(object_Kposix_v6_lpbig_offbig_libs)) },
 #endif
 #ifdef _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS
  { _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS, &(O(object_Kposix_v6_width_restricted_envs)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFF32_CFLAGS
  { _CS_XBS5_ILP32_OFF32_CFLAGS, &(O(object_Kxbs5_ilp32_off32_cflags)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFF32_LDFLAGS
  { _CS_XBS5_ILP32_OFF32_LDFLAGS, &(O(object_Kxbs5_ilp32_off32_ldflags)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFF32_LIBS
  { _CS_XBS5_ILP32_OFF32_LIBS, &(O(object_Kxbs5_ilp32_off32_libs)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFF32_LINTFLAGS
  { _CS_XBS5_ILP32_OFF32_LINTFLAGS, &(O(object_Kxbs5_ilp32_off32_lintflags)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFFBIG_CFLAGS
  { _CS_XBS5_ILP32_OFFBIG_CFLAGS, &(O(object_Kxbs5_ilp32_offbig_cflags)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFFBIG_LDFLAGS
  { _CS_XBS5_ILP32_OFFBIG_LDFLAGS, &(O(object_Kxbs5_ilp32_offbig_ldflags)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFFBIG_LIBS
  { _CS_XBS5_ILP32_OFFBIG_LIBS, &(O(object_Kxbs5_ilp32_offbig_libs)) },
 #endif
 #ifdef _CS_XBS5_ILP32_OFFBIG_LINTFLAGS
  { _CS_XBS5_ILP32_OFFBIG_LINTFLAGS, &(O(object_Kxbs5_ilp32_offbig_lintflags)) },
 #endif
 #ifdef _CS_XBS5_LP64_OFF64_CFLAGS
  { _CS_XBS5_LP64_OFF64_CFLAGS, &(O(object_Kxbs5_lp64_off64_cflags)) },
 #endif
 #ifdef _CS_XBS5_LP64_OFF64_LDFLAGS
  { _CS_XBS5_LP64_OFF64_LDFLAGS, &(O(object_Kxbs5_lp64_off64_ldflags)) },
 #endif
 #ifdef _CS_XBS5_LP64_OFF64_LIBS
  { _CS_XBS5_LP64_OFF64_LIBS, &(O(object_Kxbs5_lp64_off64_libs)) },
 #endif
 #ifdef _CS_XBS5_LP64_OFF64_LINTFLAGS
  { _CS_XBS5_LP64_OFF64_LINTFLAGS, &(O(object_Kxbs5_lp64_off64_lintflags)) },
 #endif
 #ifdef _CS_XBS5_LPBIG_OFFBIG_CFLAGS
  { _CS_XBS5_LPBIG_OFFBIG_CFLAGS, &(O(object_Kxbs5_lpbig_offbig_cflags)) },
 #endif
 #ifdef _CS_XBS5_LPBIG_OFFBIG_LDFLAGS
  { _CS_XBS5_LPBIG_OFFBIG_LDFLAGS, &(O(object_Kxbs5_lpbig_offbig_ldflags)) },
 #endif
 #ifdef _CS_XBS5_LPBIG_OFFBIG_LIBS
  { _CS_XBS5_LPBIG_OFFBIG_LIBS, &(O(object_Kxbs5_lpbig_offbig_libs)) },
 #endif
 #ifdef _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS
  { _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS, &(O(object_Kxbs5_lpbig_offbig_lintflags)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t confstr_arg_map = {
  confstr_arg_table,
  (sizeof(confstr_arg_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "confstr_arg"
};
#define confstr_arg(a) (int)map_lisp_to_c(a,&confstr_arg_map)
#define confstr_arg_reverse(a) map_c_to_lisp(a,&confstr_arg_map)

#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
static const c_lisp_pair_t pathconf_arg_table[] = {
 #ifdef _PC_FILESIZEBITS
  { _PC_FILESIZEBITS, &(O(object_Kfilesizebits)) },
 #endif
 #ifdef _PC_LINK_MAX
  { _PC_LINK_MAX, &(O(object_Klink_max)) },
 #endif
 #ifdef _PC_MAX_CANON
  { _PC_MAX_CANON, &(O(object_Kmax_canon)) },
 #endif
 #ifdef _PC_MAX_INPUT
  { _PC_MAX_INPUT, &(O(object_Kmax_input)) },
 #endif
 #ifdef _PC_NAME_MAX
  { _PC_NAME_MAX, &(O(object_Kname_max)) },
 #endif
 #ifdef _PC_PATH_MAX
  { _PC_PATH_MAX, &(O(object_Kpath_max)) },
 #endif
 #ifdef _PC_PIPE_BUF
  { _PC_PIPE_BUF, &(O(object_Kpipe_buf)) },
 #endif
 #ifdef _PC_2_SYMLINKS
  { _PC_2_SYMLINKS, &(O(object_K2_symlinks)) },
 #endif
 #ifdef _PC_ALLOC_SIZE_MIN
  { _PC_ALLOC_SIZE_MIN, &(O(object_Kalloc_size_min)) },
 #endif
 #ifdef _PC_REC_INCR_XFER_SIZE
  { _PC_REC_INCR_XFER_SIZE, &(O(object_Krec_incr_xfer_size)) },
 #endif
 #ifdef _PC_REC_MAX_XFER_SIZE
  { _PC_REC_MAX_XFER_SIZE, &(O(object_Krec_max_xfer_size)) },
 #endif
 #ifdef _PC_REC_MIN_XFER_SIZE
  { _PC_REC_MIN_XFER_SIZE, &(O(object_Krec_min_xfer_size)) },
 #endif
 #ifdef _PC_REC_XFER_ALIGN
  { _PC_REC_XFER_ALIGN, &(O(object_Krec_xfer_align)) },
 #endif
 #ifdef _PC_SYMLINK_MAX
  { _PC_SYMLINK_MAX, &(O(object_Ksymlink_max)) },
 #endif
 #ifdef _PC_CHOWN_RESTRICTED
  { _PC_CHOWN_RESTRICTED, &(O(object_Kchown_restricted)) },
 #endif
 #ifdef _PC_NO_TRUNC
  { _PC_NO_TRUNC, &(O(object_Kno_trunc)) },
 #endif
 #ifdef _PC_VDISABLE
  { _PC_VDISABLE, &(O(object_Kvdisable)) },
 #endif
 #ifdef _PC_ASYNC_IO
  { _PC_ASYNC_IO, &(O(object_Kasync_io)) },
 #endif
 #ifdef _PC_PRIO_IO
  { _PC_PRIO_IO, &(O(object_Kprio_io)) },
 #endif
 #ifdef _PC_SYNC_IO
  { _PC_SYNC_IO, &(O(object_Ksync_io)) },
 #endif
 #ifdef _PC_SOCK_MAXBUF
  { _PC_SOCK_MAXBUF, &(O(object_Ksock_maxbuf)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t pathconf_arg_map = {
  pathconf_arg_table,
  (sizeof(pathconf_arg_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "pathconf_arg"
};
#define pathconf_arg(a) (int)map_lisp_to_c(a,&pathconf_arg_map)
#define pathconf_arg_reverse(a) map_c_to_lisp(a,&pathconf_arg_map)

#endif
#if defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)
static const c_lisp_pair_t getrlimit_arg_table[] = {
 #ifdef RLIMIT_CPU
  { RLIMIT_CPU, &(O(object_Kcpu)) },
 #endif
 #ifdef RLIMIT_FSIZE
  { RLIMIT_FSIZE, &(O(object_Kfsize)) },
 #endif
 #ifdef RLIMIT_DATA
  { RLIMIT_DATA, &(O(object_Kdata)) },
 #endif
 #ifdef RLIMIT_STACK
  { RLIMIT_STACK, &(O(object_Kstack)) },
 #endif
 #ifdef RLIMIT_CORE
  { RLIMIT_CORE, &(O(object_Kcore)) },
 #endif
 #ifdef RLIMIT_RSS
  { RLIMIT_RSS, &(O(object_Krss)) },
 #endif
 #ifdef RLIMIT_NOFILE
  { RLIMIT_NOFILE, &(O(object_Knofile)) },
 #endif
 #ifdef RLIMIT_AS
  { RLIMIT_AS, &(O(object_Kas)) },
 #endif
 #ifdef RLIMIT_NPROC
  { RLIMIT_NPROC, &(O(object_Knproc)) },
 #endif
 #ifdef RLIMIT_MEMLOCK
  { RLIMIT_MEMLOCK, &(O(object_Kmemlock)) },
 #endif
 #ifdef RLIMIT_LOCKS
  { RLIMIT_LOCKS, &(O(object_Klocks)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t getrlimit_arg_map = {
  getrlimit_arg_table,
  (sizeof(getrlimit_arg_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "getrlimit_arg"
};
#define getrlimit_arg(a) (int)map_lisp_to_c(a,&getrlimit_arg_map)
#define getrlimit_arg_reverse(a) map_c_to_lisp(a,&getrlimit_arg_map)

#endif
static const c_lisp_pair_t check_chmod_mode_table[] = {
 #ifdef S_ISUID
  { S_ISUID, &(O(object_Ksuid)) },
 #endif
 #ifdef S_ISGID
  { S_ISGID, &(O(object_Ksgid)) },
 #endif
 #ifdef S_ISVTX
  { S_ISVTX, &(O(object_Ksvtx)) },
 #endif
 #ifdef S_IRWXU
  { S_IRWXU, &(O(object_Krwxu)) },
 #endif
 #ifdef S_IRUSR
  { S_IRUSR, &(O(object_Krusr)) },
 #endif
 #ifdef S_IWUSR
  { S_IWUSR, &(O(object_Kwusr)) },
 #endif
 #ifdef S_IXUSR
  { S_IXUSR, &(O(object_Kxusr)) },
 #endif
 #ifdef S_IRWXG
  { S_IRWXG, &(O(object_Krwxg)) },
 #endif
 #ifdef S_IRGRP
  { S_IRGRP, &(O(object_Krgrp)) },
 #endif
 #ifdef S_IWGRP
  { S_IWGRP, &(O(object_Kwgrp)) },
 #endif
 #ifdef S_IXGRP
  { S_IXGRP, &(O(object_Kxgrp)) },
 #endif
 #ifdef S_IRWXO
  { S_IRWXO, &(O(object_Krwxo)) },
 #endif
 #ifdef S_IROTH
  { S_IROTH, &(O(object_Kroth)) },
 #endif
 #ifdef S_IWOTH
  { S_IWOTH, &(O(object_Kwoth)) },
 #endif
 #ifdef S_IXOTH
  { S_IXOTH, &(O(object_Kxoth)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_chmod_mode_map = {
  check_chmod_mode_table,
  (sizeof(check_chmod_mode_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_chmod_mode"
};
#define check_chmod_mode(a) (mode_t)map_lisp_to_c(a,&check_chmod_mode_map)
#define check_chmod_mode_reverse(a) map_c_to_lisp(a,&check_chmod_mode_map)
#define check_chmod_mode_to_list(a) map_c_to_list(a,&check_chmod_mode_map)
#define check_chmod_mode_from_list(a) map_list_to_c(a,&check_chmod_mode_map)

#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
static const c_lisp_pair_t mknod_type_check_table[] = {
 #ifdef S_IFIFO
  { S_IFIFO, &(O(object_Kfifo)) },
 #endif
 #ifdef S_IFSOCK
  { S_IFSOCK, &(O(object_Kfsock)) },
 #endif
 #ifdef S_IFCHR
  { S_IFCHR, &(O(object_Kfchr)) },
 #endif
 #ifdef S_IFDIR
  { S_IFDIR, &(O(object_Kfdir)) },
 #endif
 #ifdef S_IFBLK
  { S_IFBLK, &(O(object_Kfblk)) },
 #endif
 #ifdef S_IFREG
  { S_IFREG, &(O(object_Kfreg)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t mknod_type_check_map = {
  mknod_type_check_table,
  (sizeof(mknod_type_check_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "mknod_type_check"
};
#define mknod_type_check(a) (int)map_lisp_to_c(a,&mknod_type_check_map)
#define mknod_type_check_reverse(a) map_c_to_lisp(a,&mknod_type_check_map)

#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
static const c_lisp_pair_t vfs_flags_table[] = {
 #ifdef ST_RDONLY
  { ST_RDONLY, &(O(object_Kst_rdonly)) },
 #endif
 #ifdef ST_NOSUID
  { ST_NOSUID, &(O(object_Kst_nosuid)) },
 #endif
 #ifdef ST_NOTRUNC
  { ST_NOTRUNC, &(O(object_Kst_notrunc)) },
 #endif
 #ifdef ST_NODEV
  { ST_NODEV, &(O(object_Kst_nodev)) },
 #endif
 #ifdef ST_NOEXEC
  { ST_NOEXEC, &(O(object_Kst_noexec)) },
 #endif
 #ifdef ST_SYNCHRONOUS
  { ST_SYNCHRONOUS, &(O(object_Kst_synchronous)) },
 #endif
 #ifdef ST_MANDLOCK
  { ST_MANDLOCK, &(O(object_Kst_mandlock)) },
 #endif
 #ifdef ST_WRITE
  { ST_WRITE, &(O(object_Kst_write)) },
 #endif
 #ifdef ST_APPEND
  { ST_APPEND, &(O(object_Kst_append)) },
 #endif
 #ifdef ST_IMMUTABLE
  { ST_IMMUTABLE, &(O(object_Kst_immutable)) },
 #endif
 #ifdef ST_NOATIME
  { ST_NOATIME, &(O(object_Kst_noatime)) },
 #endif
 #ifdef ST_NODIRATIME
  { ST_NODIRATIME, &(O(object_Kst_nodiratime)) },
 #endif
 #ifdef FILE_NAMED_STREAMS
  { FILE_NAMED_STREAMS, &(O(object_Kfile_named_streams)) },
 #endif
 #ifdef FILE_READ_ONLY_VOLUME
  { FILE_READ_ONLY_VOLUME, &(O(object_Kfile_read_only_volume)) },
 #endif
 #ifdef FILE_SUPPORTS_OBJECT_IDS
  { FILE_SUPPORTS_OBJECT_IDS, &(O(object_Kfile_supports_object_ids)) },
 #endif
 #ifdef FILE_SUPPORTS_REPARSE_POINTS
  { FILE_SUPPORTS_REPARSE_POINTS, &(O(object_Kfile_supports_reparse_points)) },
 #endif
 #ifdef FILE_SUPPORTS_SPARSE_FILES
  { FILE_SUPPORTS_SPARSE_FILES, &(O(object_Kfile_supports_sparse_files)) },
 #endif
 #ifdef FILE_VOLUME_QUOTAS
  { FILE_VOLUME_QUOTAS, &(O(object_Kfile_volume_quotas)) },
 #endif
 #ifdef FILE_SUPPORTS_ENCRYPTION
  { FILE_SUPPORTS_ENCRYPTION, &(O(object_Kfile_supports_encryption)) },
 #endif
 #ifdef FS_CASE_IS_PRESERVED
  { FS_CASE_IS_PRESERVED, &(O(object_Kfs_case_is_preserved)) },
 #endif
 #ifdef FS_CASE_SENSITIVE
  { FS_CASE_SENSITIVE, &(O(object_Kfs_case_sensitive)) },
 #endif
 #ifdef FS_FILE_COMPRESSION
  { FS_FILE_COMPRESSION, &(O(object_Kfs_file_compression)) },
 #endif
 #ifdef FS_FILE_ENCRYPTION
  { FS_FILE_ENCRYPTION, &(O(object_Kfs_file_encryption)) },
 #endif
 #ifdef FS_PERSISTENT_ACLS
  { FS_PERSISTENT_ACLS, &(O(object_Kfs_persistent_acls)) },
 #endif
 #ifdef FS_UNICODE_STORED_ON_DISK
  { FS_UNICODE_STORED_ON_DISK, &(O(object_Kfs_unicode_stored_on_disk)) },
 #endif
 #ifdef FS_VOL_IS_COMPRESSED
  { FS_VOL_IS_COMPRESSED, &(O(object_Kfs_vol_is_compressed)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t vfs_flags_map = {
  vfs_flags_table,
  (sizeof(vfs_flags_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "vfs_flags"
};
#define vfs_flags(a) (int)map_lisp_to_c(a,&vfs_flags_map)
#define vfs_flags_reverse(a) map_c_to_lisp(a,&vfs_flags_map)
#define vfs_flags_to_list(a) map_c_to_list(a,&vfs_flags_map)
#define vfs_flags_from_list(a) map_list_to_c(a,&vfs_flags_map)

#endif
static const c_lisp_pair_t check_copy_method_table[] = {
  { COPY_METHOD_COPY, &(O(object_Kcopy)) },
  { COPY_METHOD_SYMLINK, &(O(object_Ksymlink)) },
  { COPY_METHOD_HARDLINK, &(O(object_Khardlink)) },
  { COPY_METHOD_RENAME, &(O(object_Krename)) },
  { 0, NULL }
};
static const c_lisp_map_t check_copy_method_map = {
  check_copy_method_table,
  (sizeof(check_copy_method_table)/sizeof(c_lisp_pair_t))-1,
  COPY_METHOD_COPY,true,
  true,
  "check_copy_method"
};
#define check_copy_method(a) (copy_method_t)map_lisp_to_c(a,&check_copy_method_map)
#define check_copy_method_reverse(a) map_c_to_lisp(a,&check_copy_method_map)

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
static const c_lisp_pair_t check_file_attributes_table[] = {
 #ifdef FILE_ATTRIBUTE_ARCHIVE
  { FILE_ATTRIBUTE_ARCHIVE, &(O(object_Karchive)) },
 #endif
 #ifdef FILE_ATTRIBUTE_COMPRESSED
  { FILE_ATTRIBUTE_COMPRESSED, &(O(object_Kcompressed)) },
 #endif
 #ifdef FILE_ATTRIBUTE_DEVICE
  { FILE_ATTRIBUTE_DEVICE, &(O(object_Kdevice)) },
 #endif
 #ifdef FILE_ATTRIBUTE_DIRECTORY
  { FILE_ATTRIBUTE_DIRECTORY, &(O(object_Kdirectory)) },
 #endif
 #ifdef FILE_ATTRIBUTE_ENCRYPTED
  { FILE_ATTRIBUTE_ENCRYPTED, &(O(object_Kencrypted)) },
 #endif
 #ifdef FILE_ATTRIBUTE_HIDDEN
  { FILE_ATTRIBUTE_HIDDEN, &(O(object_Khidden)) },
 #endif
 #ifdef FILE_ATTRIBUTE_NORMAL
  { FILE_ATTRIBUTE_NORMAL, &(O(object_Knormal)) },
 #endif
 #ifdef FILE_ATTRIBUTE_NOT_CONTENT_INDEXED
  { FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, &(O(object_Knot_content_indexed)) },
 #endif
 #ifdef FILE_ATTRIBUTE_OFFLINE
  { FILE_ATTRIBUTE_OFFLINE, &(O(object_Koffline)) },
 #endif
 #ifdef FILE_ATTRIBUTE_READONLY
  { FILE_ATTRIBUTE_READONLY, &(O(object_Kreadonly)) },
 #endif
 #ifdef FILE_ATTRIBUTE_REPARSE_POINT
  { FILE_ATTRIBUTE_REPARSE_POINT, &(O(object_Kreparse_point)) },
 #endif
 #ifdef FILE_ATTRIBUTE_SPARSE_FILE
  { FILE_ATTRIBUTE_SPARSE_FILE, &(O(object_Ksparse_file)) },
 #endif
 #ifdef FILE_ATTRIBUTE_SYSTEM
  { FILE_ATTRIBUTE_SYSTEM, &(O(object_Ksystem)) },
 #endif
 #ifdef FILE_ATTRIBUTE_TEMPORARY
  { FILE_ATTRIBUTE_TEMPORARY, &(O(object_Ktemporary)) },
 #endif
  { 0, NULL }
};
static const c_lisp_map_t check_file_attributes_map = {
  check_file_attributes_table,
  (sizeof(check_file_attributes_table)/sizeof(c_lisp_pair_t))-1,
  0,true,
  true,
  "check_file_attributes"
};
#define check_file_attributes(a) (DWORD)map_lisp_to_c(a,&check_file_attributes_map)
#define check_file_attributes_reverse(a) map_c_to_lisp(a,&check_file_attributes_map)
#define check_file_attributes_to_list(a) map_c_to_list(a,&check_file_attributes_map)
#define check_file_attributes_from_list(a) map_list_to_c(a,&check_file_attributes_map)

#endif

#line 171

/* ============================== aux ============================== */

/* the input handle from input stream and output handle from output stream
 can trigger GC */
static Handle stream_get_handle (gcv_object_t *stream_) {
  if (uint_p(*stream_)) {
    Handle fd = (Handle)I_to_uint(*stream_);
    *stream_ = nullobj;
    return fd;
  } else {
    pushSTACK(*stream_); funcall(L(input_stream_p),1);
    return stream_lend_handle(stream_,!nullp(value1),NULL);
  }
}

/* signal the appropriate error error */
nonreturning_function(static, error_OS_stream, (object stream)) {
  if (eq(stream,nullobj)) OS_error();
  else OS_filestream_error(stream);
}

/* ============================== locking ============================== */
#if defined(WIN32_NATIVE)
/* LockFileEx does not exist on Windows95/98/ME. */
typedef BOOL (WINAPI * LockFileExFuncType)
  (HANDLE hFile, DWORD dwFlags, DWORD dwReserved,
   DWORD nNumberOfBytesToLockLow, DWORD nNumberOfBytesToLockHigh,
   LPOVERLAPPED lpOverlapped);
static LockFileExFuncType LockFileExFunc = NULL;
static BOOL my_LockFileEx
(HANDLE hFile, DWORD dwFlags, DWORD dwReserved,
 DWORD nNumberOfBytesToLockLow, DWORD nNumberOfBytesToLockHigh,
 LPOVERLAPPED lpOverlapped)
{
  (void)dwFlags; (void)dwReserved;
  return LockFile(hFile,lpOverlapped->Offset,lpOverlapped->OffsetHigh,
                  nNumberOfBytesToLockLow,nNumberOfBytesToLockHigh);
}
typedef BOOL (WINAPI * UnlockFileExFuncType)
  (HANDLE hFile, DWORD dwReserved,
   DWORD nNumberOfBytesToUnlockLow, DWORD nNumberOfBytesToUnlockHigh,
   LPOVERLAPPED lpOverlapped);
static UnlockFileExFuncType UnlockFileExFunc = NULL;
static BOOL my_UnlockFileEx
(HANDLE hFile, DWORD dwReserved,
 DWORD nNumberOfBytesToUnlockLow, DWORD nNumberOfBytesToUnlockHigh,
 LPOVERLAPPED lpOverlapped)
{
  (void)dwReserved;
  return UnlockFile(hFile,lpOverlapped->Offset,lpOverlapped->OffsetHigh,
                    nNumberOfBytesToUnlockLow,nNumberOfBytesToUnlockHigh);
}
#endif

#if defined(SIZEOF_OFF_T) && SIZEOF_OFF_T == 8
# define I_to_offset(x)  I_to_uint64(x)
#else
# define I_to_offset(x)  I_to_uint32(x)
#endif
DEFUN(POSIX::STREAM-LOCK, stream lockp &key :BLOCK SHARED :START :LENGTH,(subr_posix_stream_lock,seclass_default,2,0,norest,key,4,NIL))
{ /* the interface to fcntl(2) */
  Handle fd = (Handle)-1;
  bool lock_p = !nullp(STACK_4), failed_p;
  object stream;
  uintL start = missingp(STACK_1) ? 0 : I_to_UL(STACK_1);
#if defined(WIN32_NATIVE)
  uint64 length;
  DWORD flags = !lock_p ? 0 :
    (missingp(STACK_2) ? LOCKFILE_EXCLUSIVE_LOCK : 0) | /* (SHARED NIL) */
    (nullp(STACK_3) ? LOCKFILE_FAIL_IMMEDIATELY : 0);   /* (BLOCK T) */
  OVERLAPPED ol = {0,0,start,0,NULL};
#else
  off_t length;
  int cmd = nullp(STACK_3) ? F_SETLK : F_SETLKW; /* (BLOCK T) */
  struct flock fl;
  fl.l_type = !lock_p ? F_UNLCK :          /* unlock */
    missingp(STACK_2) ? F_WRLCK : F_RDLCK; /* (SHARED NIL) */
  fl.l_whence = SEEK_SET;
  fl.l_start = start;
#endif
  if (uint_p(STACK_5)) {        /* STREAM */
    fd = (Handle)I_to_uint(STACK_5);
    stream = nullobj;
  } else
    stream = open_file_stream_handle(STACK_5,&fd);
  if (missingp(STACK_0)) {     /* no :LENGTH => use file size */
    /* we use OS to get file size instead of calling FILE-LENGTH because
       on win32 FILE-LENGTH will fail with ERROR_LOCK_VIOLATION when the
       underlying file is locked */
#  if defined(WIN32_NATIVE)
    uint32 size_hi;
    uint32 size_lo;
    begin_system_call();
    size_lo = GetFileSize(fd,(DWORD*)&size_hi);
    /* Value returned can be (LONG) -1 even on success,
       check the last error code */
    failed_p = (size_lo == INVALID_FILE_SIZE) && (GetLastError() != 0);
    end_system_call();
    if (failed_p) goto stream_lock_error;
    length = ((uint64)size_hi << 32) | (uint64)size_lo;
#  elif defined(HAVE_FSTAT)
    struct stat st;
    begin_system_call();
    failed_p = (-1 == fstat(fd,&st));
    end_system_call();
    if (failed_p) goto stream_lock_error;
    length = st.st_size;
#  else
    length = 0;
#  endif
  } else
    length = I_to_offset(STACK_0);
  begin_system_call();
#if defined(WIN32_NATIVE)
  if (lock_p) {
    failed_p = !(*LockFileExFunc)(fd,flags,0,length,0,&ol);
    if (failed_p && nullp(STACK_3) && GetLastError() == ERROR_LOCK_VIOLATION)
      failed_p = lock_p = false; /* failed to lock, :BLOCK NIL */
  } else
    failed_p = !(*UnlockFileExFunc)(fd,0,length,0,&ol);
#else
  fl.l_len = length;
  if ((failed_p = (-1 == fcntl(fd,cmd,&fl)))
      && lock_p && (cmd == F_SETLK) && (errno == EACCES || errno == EAGAIN))
    failed_p = lock_p = false; /* failed to lock, :BLOCK NIL */
#endif
  end_system_call();
  if (failed_p) stream_lock_error:
    error_OS_stream(stream);
  skipSTACK(6);
  VALUES_IF(lock_p);
}
#endif  /* fcntl | WIN32_NATIVE */

/* ============================== fcntl ============================== */
#if defined(HAVE_FCNTL)

/* note that O_ACCMODE is treated specially */
#line 312


DEFUN(POSIX::STREAM-OPTIONS, stream cmd &optional value,(subr_posix_stream_options,seclass_default,2,1,norest,nokey,0,NIL))
{ /* http://www.opengroup.org/onlinepubs/009695399/functions/fcntl.html */
  int cmd = check_fcntl_cmd(STACK_1);
  object stream;                /* for error reporting */
  Handle fd = stream_get_handle(&STACK_2);
  int value;
  if (boundp(STACK_0)) {        /* SET */
    switch (cmd) {
      case F_GETFD: value = check_fd_flags_from_list(STACK_0);
        cmd = F_SETFD; break;
      case F_GETFL: value = check_fl_flags_from_list(STACK_0);
        cmd = F_SETFL; break;
      default: NOTREACHED;
    }
    begin_system_call();
    if (-1 == fcntl(fd,cmd,value)) error_OS_stream(STACK_2);
    end_system_call();
    VALUES0;
  } else {                      /* GET */
    begin_system_call();
    if (-1 == (value = fcntl(fd,cmd))) error_OS_stream(STACK_2);
    end_system_call();
    switch (cmd) {
      case F_GETFD: value1 = check_fd_flags_to_list(value); break;
      case F_GETFL:
        switch (value & O_ACCMODE) {
          case O_RDONLY: STACK_0 = O(object_Krdonly); break;
          case O_WRONLY: STACK_0 = O(object_Kwronly); break;
          case O_RDWR: STACK_0 = O(object_Krdwr); break;
          default: NOTREACHED;
        }
        STACK_1 = check_fl_flags_to_list(value & ~O_ACCMODE);
        value1 = allocate_cons();
        Car(value1) = STACK_0;
        Cdr(value1) = STACK_1;
        break;
      default: NOTREACHED;
    }
    mv_count = 1;
  }
  skipSTACK(3);
}
#endif

/* ============================== syslog ============================== */
#if defined(HAVE_SYSLOG)
#line 361

#line 364


#if defined(HAVE_OPENLOG)
static char* log_ident=NULL;
DEFUN(POSIX:OPENLOG,ident &key PID CONS NDELAY ODELAY NOWAIT FACILITY,(subr_posix_openlog,seclass_default,1,0,norest,key,6,NIL)) {
  int facility = check_syslog_facility(popSTACK());
  int logopt = syslog_opt_flags();
  with_string_0(check_string(popSTACK()),GLO(misc_encoding),ident, {
      begin_system_call();
      if (log_ident) { free(log_ident); }
      log_ident = (char*)my_malloc(strlen(ident)+1);
      strcpy(log_ident,ident);
      openlog(log_ident,logopt,facility);
      end_system_call();
    });
  VALUES0;
}
#endif
#if defined(HAVE_SETLOGMASK)
DEFUN(POSIX:SETLOGMASK, maskpri,(subr_posix_setlogmask,seclass_default,1,0,norest,nokey,0,NIL)) {
  int priority = (missingp(STACK_0) ? (skipSTACK(1),0) /*query*/ :
                  check_syslog_severity(popSTACK()));
  int logmask;
  begin_system_call();
  logmask = setlogmask(LOG_MASK(priority));
  end_system_call();
  VALUES1(check_syslog_severity_reverse(logmask));
}
#endif
DEFUN(POSIX::%SYSLOG, severity facility message,(subr_posix__25syslog,seclass_default,3,0,norest,nokey,0,NIL)) {
  int priority =
    check_syslog_severity(STACK_2) | check_syslog_facility(STACK_1);
  with_string_0(STACK_0 = check_string(STACK_0),GLO(misc_encoding),mesg, {
      begin_system_call();
      /* disable %m but avoid surprises with % special handling
         http://www.opengroup.org/onlinepubs/009695399/functions/syslog.html */
      syslog(priority,"%s",mesg);
      end_system_call();
    });
  VALUES0; skipSTACK(3);
}
#if defined(HAVE_CLOSELOG)
DEFUN(POSIX:CLOSELOG,,(subr_posix_closelog,seclass_default,0,0,norest,nokey,0,NIL)) {
  begin_system_call();
  closelog();
#if defined(HAVE_OPENLOG)
  if(log_ident) { free(log_ident); log_ident=NULL; }
#endif
  end_system_call();
  VALUES0;
}
#endif
#endif  /* HAVE_SYSLOG */

/* ========================== time conversion ========================== */
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
DEFUN(POSIX:STRING-TIME, format &optional datum timezone,(subr_posix_string_time,seclass_default,1,2,norest,nokey,0,NIL))
{ /* http://www.opengroup.org/onlinepubs/009695399/functions/strptime.html
     http://www.opengroup.org/onlinepubs/009695399/functions/strftime.html */
  STACK_2 = check_string(STACK_2); /* format */
  if (missingp(STACK_1)) { /* datum defaults to the current time */
    funcall(L(get_universal_time),0);
    STACK_1 = value1;
  }
  if (stringp(STACK_1)) {          /* parse: strptime */
    struct tm tm;
    unsigned int offset;
    with_string_0(STACK_1,GLO(misc_encoding),buf, {
        with_string_0(STACK_2,GLO(misc_encoding),format, {
            char *ret;
            begin_system_call();
            if ((ret = strptime(buf,format,&tm))) offset = ret - buf;
            else offset = 0;
            end_system_call();
          });
      });
    if (offset == 0) {
      pushSTACK(STACK_(1+1)); pushSTACK(STACK_(2+2));
      pushSTACK(TheSubr(subr_self)->name);
      error(error_condition,GETTEXT("~S: invalid format ~S or datum ~S"));
    }
    pushSTACK(fixnum(tm.tm_sec));
    pushSTACK(fixnum(tm.tm_min));
    pushSTACK(fixnum(tm.tm_hour));
    pushSTACK(fixnum(tm.tm_mday));
    pushSTACK(fixnum(1+tm.tm_mon));
    pushSTACK(fixnum(1900+tm.tm_year));
    pushSTACK(STACK_(0+6));     /* timezone */
    funcall(S(encode_universal_time),7);
    /* value1 from ENCODE-UNIVERSAL-TIME */
    value2 = tm.tm_isdst > 0 ? T : NIL;
    value3 = fixnum(offset);
    mv_count = 3;
    skipSTACK(3);
  } else if (integerp(STACK_1)) { /* format: strftime */
    struct tm tm;
    funcall(O(object_cl_decode_universal_time),2);
    tm.tm_sec = posfixnum_to_V(value1); /* Seconds [0,60]. */
    tm.tm_min = posfixnum_to_V(value2); /* Minutes [0,59]. */
    tm.tm_hour = posfixnum_to_V(value3); /* Hour [0,23]. */
    tm.tm_mday = posfixnum_to_V(value4); /* Day of month [1,31]. */
    tm.tm_mon = posfixnum_to_V(value5) - 1; /* Month of year [0,11]. */
    tm.tm_year = posfixnum_to_V(value6) - 1900; /* Years since 1900. */
    /* Day of week [0,6] (C: Sunday=0 <== CL: Monday=0 */
    tm.tm_wday = (posfixnum_to_V(value7) + 1) % 7;
    tm.tm_isdst = !nullp(value8); /* Daylight Savings flag. */
    /* tm.tm_yday == Day of year [0,365]. -- use mkime() */
    begin_system_call();
    if (mktime(&tm) == (time_t)-1) OS_error();
    end_system_call();
    with_string_0(STACK_0,GLO(misc_encoding),format, {
        /* at least 4 characters per each format char + safety */
        size_t bufsize = 4 * format_bytelen + 64;
        char* buf = (char*)alloca(bufsize);
        size_t retval;
        begin_system_call();
        retval = strftime(buf,bufsize,format,&tm);
        end_system_call();
        VALUES1(n_char_to_string(buf,retval,GLO(misc_encoding)));
      });
    skipSTACK(1);
  } else error_string_integer(STACK_1);
}
#endif  /* strftime strptime mktime */

/* ========================== temporary files ========================== */
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
#if defined(HAVE_TEMPNAM)
static object temp_name (char *dir, char *prefix) {
  char *ret_s; object ret_o;
  begin_system_call(); ret_s = tempnam(dir,prefix); end_system_call();
  if (ret_s == NULL) OS_error();
  ret_o = asciz_to_string(ret_s,GLO(pathname_encoding));
  begin_system_call(); free(ret_s); end_system_call();
  return ret_o;
}
#elif defined(WIN32_NATIVE)
static object temp_name (char *dir, char *prefix) {
  char path[MAX_PATH];
  begin_system_call();
  if (0 == GetTempFileName(dir,prefix,0,path)) OS_error();
  end_system_call();
  return asciz_to_string(path,GLO(pathname_encoding));
}
#endif
#line 510
DEFUN(POSIX:MKSTEMP, template &key :DIRECTION :BUFFERED :EXTERNAL-FORMAT        :ELEMENT-TYPE,(subr_posix_mkstemp,seclass_default,1,0,norest,key,4,NIL)) {
#if defined(HAVE_MKSTEMP)
  /* http://www.opengroup.org/onlinepubs/009695399/functions/mkstemp.html */
  object fname = physical_namestring(STACK_4);
  direction_t dir = (boundp(STACK_3) ? check_direction(STACK_3)
                     : DIRECTION_OUTPUT);
  Handle fd;
  with_string_0(fname,GLO(pathname_encoding),namez,{
      char *c_template;
      begin_system_call();
      if (namez_bytelen > 6
          && namez[namez_bytelen-1]=='X'
          && namez[namez_bytelen-2]=='X'
          && namez[namez_bytelen-3]=='X'
          && namez[namez_bytelen-4]=='X'
          && namez[namez_bytelen-5]=='X'
          && namez[namez_bytelen-6]=='X') {
        c_template = namez;
      } else {
        c_template = (char*)alloca(namez_bytelen+6);
        strcpy(c_template,namez);
        strcat(c_template,"XXXXXX");
      }
      fd = mkstemp(c_template);
      end_system_call();
      fname = asciz_to_string(c_template,GLO(pathname_encoding));
    });
  pushSTACK(fname);  funcall(L(pathname),1); STACK_4=value1; /* filename */
  pushSTACK(value1); funcall(L(truename),1); STACK_3=value1; /* truename */
  pushSTACK(allocate_handle(fd));
  /* stack layout: FD, eltype, extfmt, buff, truename, filename */
  VALUES1(make_file_stream(dir,false,true));
#elif defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  /* http://www.opengroup.org/onlinepubs/009695399/functions/tempnam.html */
  object path;
  pushSTACK(STACK_4); funcall(L(pathname),1); pushSTACK(value1);
  pushSTACK(value1); funcall(L(directory_namestring),1); pushSTACK(value1);
  pushSTACK(STACK_1); funcall(L(file_namestring),1); pushSTACK(value1);
  with_string_0(STACK_0,GLO(pathname_encoding),prefix, {
      with_string_0(STACK_1,GLO(pathname_encoding),dir, {
          /* if no directory ==> use current "." */
          STACK_7 = temp_name(dir[0] ? dir : (char*)".",prefix);
        });
    });
  pushSTACK(STACK_3);           /* ELEMENT-TYPE */
  STACK_1 = S(Kelement_type);
  STACK_2 = STACK_5;            /* EXTERNAL-FORMAT */
  STACK_3 = S(Kexternal_format);
  STACK_4 = STACK_6;            /* BUFFERED */
  STACK_5 = S(Kbuffered);
  STACK_6 = missingp(STACK_7) ? S(Koutput) : STACK_7; /* DIRECTION */
  STACK_7 = S(Kdirection);
  funcall(L(open),9);
#endif
}
#endif /* HAVE_MKSTEMP || HAVE_TEMPNAM || WIN32_NATIVE */

/* ================= user accounting database functions ================= */
#if defined(HAVE_UTMPX_H)
#line 570

static int check_utmpx (gcv_object_t *arg) {
  *arg = check_classname(*arg,O(object_posix__utmpx));
  return check_ut_type(TheStructure(*arg)->recdata[4]);
}
/* convert C struct utmpx to Lisp
 can trigger GC */
static Values utmpx_to_lisp (struct utmpx *utmpx, gcv_object_t *utmpx_o) {
  pushSTACK(check_ut_type_reverse(utmpx->ut_type));
  pushSTACK(safe_to_string(utmpx->ut_user));
  pushSTACK(safe_to_string(utmpx->ut_id));
  pushSTACK(safe_to_string(utmpx->ut_line));
  pushSTACK(L_to_I(utmpx->ut_pid));
#if defined(HAVE_UTMPX_UT_HOST)
  pushSTACK(safe_to_string(utmpx->ut_host));
#else
  pushSTACK(NIL);
#endif
  pushSTACK(sec_usec_number(utmpx->ut_tv.tv_sec,utmpx->ut_tv.tv_usec,1));
  if (utmpx_o) {
    TheStructure(*utmpx_o)->recdata[7] = popSTACK(); /* tv */
    TheStructure(*utmpx_o)->recdata[6] = popSTACK(); /* host */
    TheStructure(*utmpx_o)->recdata[5] = popSTACK(); /* pid */
    TheStructure(*utmpx_o)->recdata[4] = popSTACK(); /* line */
    TheStructure(*utmpx_o)->recdata[3] = popSTACK(); /* id */
    TheStructure(*utmpx_o)->recdata[2] = popSTACK(); /* user */
    TheStructure(*utmpx_o)->recdata[1] = popSTACK(); /* type */
    VALUES1(*utmpx_o);
  } else funcall(O(object_posix__make_utmpx),7);
}
#if defined(HAVE_ENDUTXENT)
DEFUN(POSIX::ENDUTXENT,,(subr_posix_endutxent,seclass_default,0,0,norest,nokey,0,NIL)) {
  begin_system_call(); endutxent(); end_system_call(); VALUES0;
}
#endif
#if defined(HAVE_GETUTXENT)
DEFUN(POSIX::GETUTXENT, &optional utmpx,(subr_posix_getutxent,seclass_default,0,1,norest,nokey,0,NIL)) {
  struct utmpx *utmpx;
  if (!missingp(STACK_0)) STACK_0 = check_classname(STACK_0,O(object_posix__utmpx));
  begin_system_call(); utmpx=getutxent(); end_system_call();
  if (utmpx) utmpx_to_lisp(utmpx,missingp(STACK_0) ? NULL : &STACK_0);
  else VALUES1(NIL);
  skipSTACK(1);
}
#endif
#if defined(HAVE_GETUTXID)
DEFUN(POSIX::GETUTXID, id,(subr_posix_getutxid,seclass_default,1,0,norest,nokey,0,NIL)) {
  struct utmpx utmpx, *utmpx_p;
  utmpx.ut_type = check_utmpx(&STACK_0);
  begin_system_call(); utmpx_p = getutxid(&utmpx); end_system_call();
  if (utmpx_p) utmpx_to_lisp(utmpx_p,&STACK_0);
  else VALUES1(NIL);
  skipSTACK(1);
}
#endif
#if defined(HAVE_GETUTXLINE)
DEFUN(POSIX::GETUTXLINE, line,(subr_posix_getutxline,seclass_default,1,0,norest,nokey,0,NIL)) {
  struct utmpx utmpx, *utmpx_p;
  utmpx.ut_type = check_utmpx(&STACK_0);
  begin_system_call(); utmpx_p = getutxline(&utmpx); end_system_call();
  if (utmpx_p) utmpx_to_lisp(utmpx_p,&STACK_0);
  else VALUES1(NIL);
  skipSTACK(1);
}
#endif
#if defined(HAVE_PUTUTXLINE)
DEFUN(POSIX::PUTUTXLINE, utmpx,(subr_posix_pututxline,seclass_default,1,0,norest,nokey,0,NIL)) {
  struct utmpx utmpx, *utmpx_p;
  utmpx.ut_type = check_utmpx(&STACK_0);
  begin_system_call(); utmpx_p = pututxline(&utmpx); end_system_call();
  if (utmpx_p) utmpx_to_lisp(utmpx_p,&STACK_0);
  else OS_error();
  skipSTACK(1);
}
#endif
#if defined(HAVE_SETUTXENT)
DEFUN(POSIX::SETUTXENT,,(subr_posix_setutxent,seclass_default,0,0,norest,nokey,0,NIL)) {
  begin_system_call(); setutxent(); end_system_call(); VALUES0;
}
#endif
#endif  /* HAVE_UTMPX_H */

/* ========================= processes & signals ========================= */
#if defined(HAVE_GETPPID)
DEFUN(POSIX:GETPPID,,(subr_posix_getppid,seclass_default,0,0,norest,nokey,0,NIL)) { GETTER(pid,getppid); }
#endif
#if defined(HAVE_GETSID)
DEFUN(POSIX:GETSID, pid,(subr_posix_getsid,seclass_default,1,0,norest,nokey,0,NIL)) { GETTER1(pid,getsid); }
#endif
#if defined(HAVE_SETSID)
DEFUN(POSIX:SETSID,,(subr_posix_setsid,seclass_default,0,0,norest,nokey,0,NIL)) { GETTER(pid,setsid); } /* sic! */
#endif
#if defined(HAVE_GETPGRP)
DEFUN(POSIX:GETPGRP,,(subr_posix_getpgrp,seclass_default,0,0,norest,nokey,0,NIL)) { GETTER(pid,getpgrp); }
#endif
#if defined(HAVE_SETPGRP)
DEFUN(POSIX:SETPGRP,,(subr_posix_setpgrp,seclass_default,0,0,norest,nokey,0,NIL)) {
  pid_t ret;
# if defined(HAVE_SETPGRP_POSIX)
  begin_system_call(); ret=setpgrp(); end_system_call();
# else  /* BSD version, identical to setpgid() */
  begin_system_call(); ret=setpgrp(0,0); end_system_call();
# endif
  if (ret==(pid_t)-1) OS_error();
  VALUES1(pid_to_I(ret));
}
#endif
#if defined(HAVE_GETPGID)
DEFUN(POSIX:GETPGID, pid,(subr_posix_getpgid,seclass_default,1,0,norest,nokey,0,NIL)) { GETTER1(pid,getpgid); }
#endif
#if defined(HAVE_SETPGID)
DEFUN(POSIX::%SETPGID, pid pgid,(subr_posix__25setpgid,seclass_default,2,0,norest,nokey,0,NIL)) {
  pid_t pgid = I_to_pid(STACK_0);
  pid_t pid = I_to_pid(STACK_1);
  int ret;
  begin_system_call(); ret=setpgid(pid,pgid); end_system_call();
  if (ret==-1) OS_error();
  VALUES1(STACK_0); skipSTACK(2);
}
#endif
#if defined(HAVE_SETREUID)
DEFUN(POSIX:SETREUID, ruid euid,(subr_posix_setreuid,seclass_default,2,0,norest,nokey,0,NIL)) { SETTER2(uid,setreuid); }
#endif
#if defined(HAVE_SETREGID)
DEFUN(POSIX:SETREGID, rgid egid,(subr_posix_setregid,seclass_default,2,0,norest,nokey,0,NIL)) { SETTER2(gid,setregid); }
#endif
/* http://www.opengroup.org/onlinepubs/009695399/basedefs/signal.h.html */
#line 700

#if defined(HAVE_KILL)
DEFUN(POSIX:KILL, pid sig,(subr_posix_kill,seclass_default,2,0,norest,nokey,0,NIL)) {
  int sig = check_signal(STACK_0);
  pid_t pid = I_to_pid(STACK_1);
  int ret;
  begin_system_call(); ret=kill(pid,sig); end_system_call();
  if (ret==-1) OS_error();
  VALUES0; skipSTACK(2);
}
#endif

/* ============================= file sync ============================= */
#if defined(WIN32_NATIVE) || defined(HAVE_SYNC) || defined(HAVE_FSYNC)
DEFUN(POSIX:SYNC, &optional file,(subr_posix_sync,seclass_default,0,1,norest,nokey,0,NIL)) {
  if (missingp(STACK_0)) {      /* sync() */
#  if defined(HAVE_SYNC)
    begin_system_call(); sync(); end_system_call();
#  endif
  } else {                      /* fsync() */
    Handle fd = stream_get_handle(&STACK_0);
    begin_system_call();
#  if defined(HAVE_FSYNC)
    if (-1 == fsync(fd)) error_OS_stream(STACK_0);
#  elif defined(WIN32_NATIVE)
    if (!FlushFileBuffers(fd)) error_OS_stream(STACK_0);
#  endif
    end_system_call();
  }
  VALUES0; skipSTACK(1);
}
#endif
/* ========================== process priority ========================== */
#if defined(WIN32_NATIVE)
#line 735

#else
#line 739

#endif

DEFUN(OS:PRIORITY, pid &optional which,(subr_os_priority,seclass_default,1,1,norest,nokey,0,NIL)) {
  int which = check_priority_which(popSTACK());
  int pid = I_to_uint32(check_uint32(popSTACK()));
  int res;
#if defined(HAVE_GETPRIORITY)
  errno = 0;
  begin_system_call();
  res = getpriority(which,pid);
  end_system_call();
  if (errno) OS_error();
#elif defined(WIN32_NATIVE)
  HANDLE handle;
  begin_system_call();
  handle = OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pid);
  if (handle == NULL) OS_error();
  res = (int)GetPriorityClass(handle);
  CloseHandle(handle);
  end_system_call();
#else
  NOTREACHED;
#endif
  VALUES1(check_priority_value_reverse(res));
}
DEFUN(OS::%SET-PRIORITY, value pid which,(subr_os__25set_priority,seclass_default,3,0,norest,nokey,0,NIL)) {
  int which = check_priority_which(popSTACK());
  int pid = I_to_uint32(check_uint32(popSTACK()));
  int value = check_priority_value(STACK_0);
  begin_system_call();
#if defined(HAVE_SETPRIORITY)
  if (setpriority(which,pid,value)) OS_error();
#elif defined(WIN32_NATIVE)
  {
    HANDLE handle = OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pid);
    if (handle == NULL) OS_error();
    if (!SetPriorityClass(handle,value)) OS_error();
    CloseHandle(handle);
  }
#else
  NOTREACHED;
#endif
  end_system_call();
  VALUES1(popSTACK());
}

/* posix math functions in <math.h> */
/* Must include <math.h> */
#define decimal_string  solaris_decimal_string  /* needed on Solaris */
#undef floor  /* needed on Linux */
#include <math.h>
#define floor(a,b)  ((a) / (b))
#undef decimal_string

#define D_S           to_double(popSTACK())
#define I_S           to_int(popSTACK())
#line 797
#define N_D(n,v)     { double x=n; v=c_double_to_DF((dfloatjanus*)&x); }
#define VAL_D(func)   double res=func(D_S); N_D(res,value1)
#line 800
#define VAL_ID(func)    double xx=D_S; int nn=I_S; double res=func(nn,xx); N_D(res,value1)

#if defined(HAVE_ERFC)
DEFUNF(POSIX::ERF,x,(subr_posix_erf,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(erf); mv_count=1; }
#endif
#if defined(HAVE_ERFC)
DEFUNF(POSIX::ERFC,x,(subr_posix_erfc,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(erfc); mv_count=1; }
#endif

DEFUNF(POSIX::J0,x,(subr_posix_j0,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(j0); mv_count=1; }
DEFUNF(POSIX::J1,x,(subr_posix_j1,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(j1); mv_count=1; }
DEFUNF(POSIX::JN,i x,(subr_posix_jn,seclass_default,2,0,norest,nokey,0,NIL)) { VAL_ID(jn); mv_count=1; }
DEFUNF(POSIX::Y0,x,(subr_posix_y0,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(y0); mv_count=1; }
DEFUNF(POSIX::Y1,x,(subr_posix_y1,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(y1); mv_count=1; }
DEFUNF(POSIX::YN,i y,(subr_posix_yn,seclass_default,2,0,norest,nokey,0,NIL)) { VAL_ID(yn); mv_count=1; }
DEFUNF(POSIX::TGAMMA,x,(subr_posix_tgamma,seclass_default,1,0,norest,nokey,0,NIL)) { VAL_D(tgamma); mv_count=1; }

#if defined(HAVE_LGAMMA) || HAVE_DECL_LGAMMA_R
DEFUNF(POSIX::LGAMMA,x,(subr_posix_lgamma,seclass_default,1,0,norest,nokey,0,NIL)) {
# if HAVE_DECL_LGAMMA_R
  int sign;
  double res = lgamma_r(D_S,&sign);
  value2 = (sign > 0 ? Fixnum_1 : Fixnum_minus1);
# else
  double res = lgamma(D_S);
# if HAVE_DECL_SIGNGAM
  value2 = (signgam > 0 ? Fixnum_1 : Fixnum_minus1);
# else
  value2 = NIL;
# endif
# endif
  N_D(res,value1); mv_count=2;
}
#endif

#if defined(HAVE_CLOCK)
DEFUN(POSIX:BOGOMIPS,,(subr_posix_bogomips,seclass_default,0,0,norest,nokey,0,NIL))
{
  if (clock() != (clock_t)-1) {
    unsigned long loops = 1;
    while ((loops <<= 1)) {
      unsigned long ticks, ii;
      ticks = clock();
      for (ii = loops; ii > 0; ii--);
      ticks = clock() - ticks;
      if (ticks >= CLOCKS_PER_SEC) {
        double bogo = (1.0 * loops / ticks) * (CLOCKS_PER_SEC / 500000.0);
        N_D(bogo,value1); mv_count=1;
        return;
      }
    }
  }
  N_D(-1.0,value1); mv_count=1;
}
#endif /* HAVE_CLOCK */

#if defined(HAVE_GETLOADAVG)
DEFUN(POSIX:LOADAVG, &optional percentp,(subr_posix_loadavg,seclass_default,0,1,norest,nokey,0,NIL)) {
  double loadavg[3];
  int ret;
  begin_system_call();
  ret = getloadavg(loadavg,3);
  end_system_call();
  if (ret != 3) OS_error();
  mv_count=3;
  if (missingp(STACK_0)) {
    N_D(loadavg[0],value1);
    N_D(loadavg[1],value2);
    N_D(loadavg[2],value3);
  } else { /* return % as ints, to avoid consing */
    value1 = fixnum((int)(loadavg[0]*100));
    value2 = fixnum((int)(loadavg[1]*100));
    value3 = fixnum((int)(loadavg[2]*100));
  }
  skipSTACK(1);
}
#endif  /* HAVE_GETLOADAVG */

#undef D_S
#undef I_S
#undef N_D
#undef VAL_D
#undef VAL_ID

/* "gcc --mno-cygwin -l crypt" links with cygwin lib-crypt,
   so we have to disable this explicitly */
#if defined(HAVE_CRYPT) && !defined(WIN32_NATIVE)
DEFUN(POSIX::CRYPT, key salt,(subr_posix_crypt,seclass_default,2,0,norest,nokey,0,NIL)) {
  char *result;
  STACK_0 = check_string(STACK_0);
  STACK_1 = check_string(STACK_1);
  with_string_0(STACK_0,GLO(misc_encoding),salt, {
      with_string_0(STACK_1,GLO(misc_encoding),key, {
          begin_system_call();
          result = crypt(key,salt);
          end_system_call();
        });
    });
  if (result == NULL) OS_error();
  VALUES1(asciz_to_string(result,GLO(misc_encoding)));
  skipSTACK(2);
}
#endif
#if defined(HAVE_ENCRYPT) || defined(HAVE_SETKEY)
/* move information from a bit vector to the char block
 can trigger GC */
static void get_block (char block[64], object vector) {
  while (!bit_vector_p(Atype_8Bit,vector)
         || vector_length(vector) != 8) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(vector);          /* TYPE-ERROR slot DATUM */
    pushSTACK(O(object__28vector_20_28unsigned_byte_208_29_208_29)); /* EXPECTED-TYPE */
    pushSTACK(STACK_0); pushSTACK(vector);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not of type ~S"));
    vector = value1;
  }
  {
    uintL index=0, ii, jj, kk=0;
    object dv = array_displace_check(vector,8,&index);
    uint8* ptr1 = TheSbvector(dv)->data + index;
    for (ii = 0; ii<8; ii++) {
      uint8 bb = *ptr1++;
      for (jj = 0; jj<8; jj++)
        block[kk++] = ((bb & bit(jj)) != 0);
    }
  }
}
#endif
#if defined(HAVE_ENCRYPT) && !defined(WIN32_NATIVE)
/* the inverse of get_block(): move data from block to vector,
 which is known to be a (VECTOR BIT) */
static void set_block (char block[64], object vector) {
  uintL index=0, ii, jj, kk=0;
  object dv = array_displace_check(vector,8,&index);
  uint8* ptr1 = TheSbvector(dv)->data + index;
  for (ii = 0; ii<8; ii++) {
    uint8 bb = 0;
    for (jj = 0; jj<8; jj++)
      bb |= (block[kk++]!=0) << jj;
    *ptr1++ = bb;
  }
}
DEFUN(POSIX::ENCRYPT, block flag,(subr_posix_encrypt,seclass_default,2,0,norest,nokey,0,NIL)) {
  int flag = nullp(popSTACK());
  char block[64];
  get_block(block,STACK_0);
  begin_system_call();
  errno = 0; encrypt(block,flag);
  if (errno) OS_error();
  end_system_call();
  set_block(block,STACK_0);
  VALUES1(popSTACK());
}
#endif
#if defined(HAVE_SETKEY) && !defined(WIN32_NATIVE)
DEFUN(POSIX::SETKEY, key,(subr_posix_setkey,seclass_default,1,0,norest,nokey,0,NIL)) {
  char block[64];
  get_block(block,popSTACK());
  begin_system_call();
  errno = 0; setkey(block);
  if (errno) OS_error();
  end_system_call();
  VALUES0;
}
#endif

/* ========= SYSTEM INFORMATION ========== */

#if defined(HAVE_SYS_UTSNAME_H)
# include <sys/utsname.h>
#endif

#if defined(HAVE_UNAME)
DEFUN(POSIX::UNAME,,(subr_posix_uname,seclass_default,0,0,norest,nokey,0,NIL))
{ /* Lisp interface to uname(2) */
  struct utsname utsname;
  begin_system_call(); uname(&utsname); end_system_call();
  pushSTACK(safe_to_string(utsname.sysname));
  pushSTACK(safe_to_string(utsname.nodename));
  pushSTACK(safe_to_string(utsname.release));
  pushSTACK(safe_to_string(utsname.version));
  pushSTACK(safe_to_string(utsname.machine));
  funcall(O(object_posix__make_uname),5);
}
#endif /* HAVE_UNAME */

#if defined(HAVE_SYSCONF)
#line 1018

DEFUN(POSIX::SYSCONF, &optional what,(subr_posix_sysconf,seclass_default,0,1,norest,nokey,0,NIL))
{ /* Lisp interface to sysconf(3c) */
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = sysconf_arg(what), res;
    begin_system_call(); res = sysconf(cmd); end_system_call();
    VALUES1(L_to_I(res));
  } else { /* all possible values */
    int pos = 0;
    for (; pos < sysconf_arg_map.size; pos++) {
      int res;
      begin_system_call();
      res = sysconf(sysconf_arg_map.table[pos].c_const);
      end_system_call();
      pushSTACK(*sysconf_arg_map.table[pos].l_const);
      pushSTACK(L_to_I(res));
    }
    VALUES1(listof(2*sysconf_arg_map.size));
  }
}
#endif /* HAVE_SYSCONF */

#if defined(HAVE_CONFSTR)
#line 1056

DEFUN(POSIX::CONFSTR, &optional what,(subr_posix_confstr,seclass_default,0,1,norest,nokey,0,NIL))
{ /* Lisp interface to confstr(3c) */
#line 1075
#define CS_S(cmd)    begin_system_call(); res = confstr(cmd,buf,BUFSIZ); end_system_call();    if (res == 0) pushSTACK(T);                                              else if (res <= BUFSIZ) value1 = asciz_to_string(buf,GLO(misc_encoding));    else {                                                                     /* Here we cannot use alloca(), because alloca() is generally unsafe         for sizes > BUFSIZ. */                                                char *tmp;                                                               begin_system_call();                                                     tmp = (char*)my_malloc(res);                                             confstr(cmd,tmp,res);                                                    end_system_call();                                                       value1 = asciz_to_string(tmp,GLO(misc_encoding));                        begin_system_call();                                                     free(tmp);                                                               end_system_call();                                                     }

  size_t res;
  char buf[BUFSIZ];
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = confstr_arg(what);
    CS_S(cmd); mv_count = 1;
  } else { /* all possible values */
    unsigned int pos = 0;
    for (; pos < confstr_arg_map.size; pos++) {
      CS_S(confstr_arg_map.table[pos].c_const);
      pushSTACK(*confstr_arg_map.table[pos].l_const);
      pushSTACK(value1);
    }
    VALUES1(listof(2*confstr_arg_map.size));
  }
}
#endif /* HAVE_CONFSTR */

#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
#line 1100

DEFUN(POSIX::PATHCONF, pathspec &optional what,(subr_posix_pathconf,seclass_default,1,1,norest,nokey,0,NIL))
{ /* http://www.opengroup.org/onlinepubs/009695399/functions/pathconf.html */
  Handle fd;
  if (builtin_stream_p(STACK_1)) {
    pushSTACK(STACK_1); funcall(L(built_in_stream_open_p),1);
    if (!nullp(value1)) { /* open stream ==> use FD */
      fd = stream_get_handle(&STACK_1);
     pathconf_fd:
      if (missingp(STACK_0)) { /* all possible values */
        unsigned int pos = 0;
        for (; pos < pathconf_arg_map.size; pos++) {
          long res;
          begin_system_call();
          res = fpathconf(fd,pathconf_arg_map.table[pos].c_const);
          end_system_call();
          pushSTACK(*pathconf_arg_map.table[pos].l_const);
          pushSTACK(res == -1 ? S(Kerror) : L_to_I(res));
        }
        VALUES1(listof(2*pathconf_arg_map.size));
      } else {
        long res;
        begin_system_call();
        if ((res = fpathconf(fd,pathconf_arg(STACK_0))) == -1)
          integerp(STACK_1) ? OS_error() : error_OS_stream(STACK_1);
        end_system_call();
        VALUES1(L_to_I(res));
      }
    } else goto pathconf_path; /* not an open stream ==> use truename */
  } else if (integerp(STACK_1)) {
    fd = I_to_L(STACK_1);
    goto pathconf_fd;
  } else pathconf_path:
    with_string_0(STACK_1 = physical_namestring(STACK_1),
                  GLO(pathname_encoding), namez, {
      if (missingp(STACK_0)) { /* all possible values */
        unsigned int pos = 0;
        for (; pos < pathconf_arg_map.size; pos++) {
          long res;
          begin_system_call();
          res = pathconf(namez,pathconf_arg_map.table[pos].c_const);
          end_system_call();
          pushSTACK(*pathconf_arg_map.table[pos].l_const);
          pushSTACK(res == -1 ? S(Kerror) : L_to_I(res));
        }
        VALUES1(listof(2*pathconf_arg_map.size));
      } else {
        long res;
        begin_system_call();
        if ((res = pathconf(namez,pathconf_arg(STACK_0))) == -1) OS_error();
        end_system_call();
        VALUES1(L_to_I(res));
      }
    });
  skipSTACK(2);
}
#endif  /* HAVE_PATHCONF && HAVE_FPATHCONF */


#if defined(HAVE_GETRUSAGE)
static /*maygc*/ Values rusage_to_lisp (int who) {
  struct rusage ru;
  int count = 2;
  begin_system_call(); getrusage(who,&ru); end_system_call();
  pushSTACK(sec_usec_number(ru.ru_utime.tv_sec,ru.ru_utime.tv_usec,0));
  pushSTACK(sec_usec_number(ru.ru_stime.tv_sec,ru.ru_stime.tv_usec,0));
  pushSTACK(L_to_I(ru.ru_maxrss)); count++;
  pushSTACK(L_to_I(ru.ru_ixrss)); count++;
  pushSTACK(L_to_I(ru.ru_idrss)); count++;
  pushSTACK(L_to_I(ru.ru_isrss)); count++;
  pushSTACK(L_to_I(ru.ru_minflt)); count++;
  pushSTACK(L_to_I(ru.ru_majflt)); count++;
  pushSTACK(L_to_I(ru.ru_nswap)); count++;
  pushSTACK(L_to_I(ru.ru_inblock)); count++;
  pushSTACK(L_to_I(ru.ru_oublock)); count++;
  pushSTACK(L_to_I(ru.ru_msgsnd)); count++;
  pushSTACK(L_to_I(ru.ru_msgrcv)); count++;
  pushSTACK(L_to_I(ru.ru_nsignals)); count++;
  pushSTACK(L_to_I(ru.ru_nvcsw)); count++;
  pushSTACK(L_to_I(ru.ru_nivcsw)); count++;
  funcall(O(object_posix__make_usage),count);
}
DEFUN(POSIX::USAGE,,(subr_posix_usage,seclass_default,0,0,norest,nokey,0,NIL)) { /* getrusage(3) */
  rusage_to_lisp(RUSAGE_CHILDREN); pushSTACK(value1);
  rusage_to_lisp(RUSAGE_SELF);
  value2 = popSTACK(); mv_count = 2;
}
#endif /* HAVE_GETRUSAGE */

#if defined(HAVE_GETRLIMIT) || defined(HAVE_SETRLIMIT)
#line 1191

#if SIZEOF_RLIMT_T == 8
# define rlim_to_I_0(lim) uint64_to_I(lim)
# define I_to_rlim_0(lim) I_to_uint64(check_uint64(lim))
#else
# define rlim_to_I_0(lim) uint32_to_I(lim)
# define I_to_rlim_0(lim) I_to_uint32(check_uint32(lim))
#endif
static /* maygc */ inline object rlim_to_I (rlim_t lim)
{ return lim == RLIM_INFINITY ? NIL : rlim_to_I_0(lim); }
static /* maygc */ inline rlim_t I_to_rlim (object lim)
{ return missingp(lim) ? RLIM_INFINITY : I_to_rlim_0(lim); }
#endif /* HAVE_GETRLIMIT || HAVE_SETRLIMIT */
#if defined(HAVE_GETRLIMIT)
DEFUN(POSIX::RLIMIT, &optional what,(subr_posix_rlimit,seclass_default,0,1,norest,nokey,0,NIL))
{ /* getrlimit(3) */
  struct rlimit rl;
  object what = popSTACK();
  if (!missingp(what)) {
    int cmd = getrlimit_arg(what);
    begin_system_call();
    if (getrlimit(cmd,&rl)) OS_error();
    end_system_call();
    pushSTACK(rlim_to_I(rl.rlim_cur)); pushSTACK(rlim_to_I(rl.rlim_max));
    VALUES2(STACK_1,STACK_0); skipSTACK(2);
  } else {
    unsigned int pos;
    for (pos = 0; pos < getrlimit_arg_map.size; pos++) {
      int status;
      pushSTACK(*getrlimit_arg_map.table[pos].l_const);
      begin_system_call();
      status = getrlimit(getrlimit_arg_map.table[pos].c_const,&rl);
      end_system_call();
      if (status) pushSTACK(S(Kerror));
      else {
        pushSTACK(rlim_to_I(rl.rlim_cur)); pushSTACK(rlim_to_I(rl.rlim_max));
        funcall(O(object_posix__make_rlimit),2); pushSTACK(value1);
      }
    }
    VALUES1(listof(2*getrlimit_arg_map.size));
  }
}
#endif /* HAVE_GETRLIMIT */
#if defined(HAVE_SETRLIMIT)
/* parse the RLIMIT structure
   NOTE: arg is intentionally not reset by check_classname
   to avoid argument modification
 can trigger GC */
static void check_rlimit (object arg, struct rlimit *rl) {
  pushSTACK(check_classname(arg,O(object_posix__rlimit)));
  rl->rlim_cur = I_to_rlim(TheStructure(STACK_0)->recdata[1]);
  rl->rlim_max = I_to_rlim(TheStructure(STACK_0)->recdata[2]);
  skipSTACK(1);
}
DEFUN(POSIX::SET-RLIMIT, what cur max,(subr_posix_set_rlimit,seclass_default,3,0,norest,nokey,0,NIL))
{ /* setrlimit(3): 3 ways to call:
   (setf (rlimit what) (values cur max))
   (setf (rlimit what) #S(rlimit :cur cur :max max))
   (setf (rlimit) rlimit-alist-as-returned-by-rlimit-without-arguments) */
  if (nullp(STACK_2)) {         /* 3rd way */
    if (!nullp(STACK_0)) goto rlimit_bad;
    STACK_0 = STACK_1;
    while (!endp(STACK_0)) {
      int what = getrlimit_arg(Car(STACK_0));
      struct rlimit rl;
      STACK_0 = Cdr(STACK_0);
      if (!consp(STACK_0)) { STACK_0 = NIL; goto rlimit_bad; }
      check_rlimit(Car(STACK_0),&rl);
      STACK_0 = Cdr(STACK_0);
      begin_system_call();
      if (setrlimit(what,&rl)) OS_error();
      end_system_call();
    }
  } else {
    int what = getrlimit_arg(STACK_2);
    struct rlimit rl;
    if (nullp(STACK_1) || posfixnump(STACK_1)) { /* 1st way */
      rl.rlim_cur = I_to_rlim(STACK_1);
      rl.rlim_max = I_to_rlim(STACK_0);
    } else {                    /* 2nd way */
      if (!nullp(STACK_0)) goto rlimit_bad;
      check_rlimit(STACK_1,&rl);
    }
    begin_system_call();
    if (setrlimit(what,&rl)) OS_error();
    end_system_call();
  }
  VALUES2(STACK_1,STACK_0); skipSTACK(3); return;
 rlimit_bad:
  pushSTACK(TheSubr(subr_self)->name);
  error(error_condition,GETTEXT("~S: bad arguments: ~S ~S ~S"));
}
#endif /* HAVE_SETRLIMIT */

/* ==== SOCKETS ===== */
#if defined(HAVE_NETDB_H)
# include <netdb.h>
#endif
#if defined(HAVE_NETINET_IN_H)
# include <netinet/in.h>
#endif
#if defined(HAVE_ARPA_INET_H)
# include <arpa/inet.h>
#endif

#line 1302
#define H_ERRMSG                                                                    (h_errno == HOST_NOT_FOUND ? "host not found" :                              (h_errno == TRY_AGAIN ? "try again later" :                                  (h_errno == NO_RECOVERY ? "a non-recoverable error occurred" :               (h_errno == NO_DATA ? "valid name, but no data for this host" :              (h_errno == NO_ADDRESS ? "no IP address for this host" :                     "unknown error")))))

#if 0
void print_he (struct hostent *he) {
 int ii;
 char **pp;
 struct in_addr in;
 printf("h_name: %s; h_length: %d; h_addrtype: %d\n [size in.s_addr: %d]\n",
        he->h_name,he->h_length,he->h_addrtype,sizeof(in.s_addr));
 for (pp = he->h_aliases; *pp != 0; pp++) printf("\t%s", *pp);
 printf("\n IP:");
 for (pp = he->h_addr_list; *pp != 0; pp++) {
   (void) memcpy(&in.s_addr, *pp, sizeof (in.s_addr));
   (void) printf("\t%s", inet_ntoa(in));
 }
 printf("\n");
}
#endif

/* C struct hostent --> Lisp HOSTENT structure
 can trigger GC */
Values hostent_to_lisp (struct hostent *he); /* used by NEW-CLX => not static */
Values hostent_to_lisp (struct hostent *he) {
  pushSTACK(ascii_to_string(he->h_name));
  push_string_array(he->h_aliases);
  { int ii = 0;
    for (; he->h_addr_list[ii]; ii++)
      pushSTACK(addr_to_string(he->h_addrtype,he->h_addr_list[ii]));
    { object tmp = listof(ii); pushSTACK(tmp); }}
  pushSTACK(fixnum(he->h_addrtype));
  funcall(O(object_posix__make_hostent),4);
}

DEFUN(POSIX::RESOLVE-HOST-IPADDR,&optional host,(subr_posix_resolve_host_ipaddr,seclass_default,0,1,norest,nokey,0,NIL))
{ /* Lisp interface to gethostbyname(3) and gethostbyaddr(3) */
  object arg = popSTACK();
  struct hostent *he = NULL;

  if (missingp(arg)) {
#  if !defined(HAVE_GETHOSTENT)
    VALUES1(NIL);
#  else
    int count = 0;
    begin_system_call();
    for (; (he = gethostent()); count++) {
      hostent_to_lisp(he);
      pushSTACK(value1);
    }
    endhostent();
    end_system_call();
    VALUES1(listof(count));
#  endif
    return;
  }

  he = resolve_host(arg);

  if (he == NULL) {
    pushSTACK(arg); pushSTACK(arg);
    STACK_1 = ascii_to_string(H_ERRMSG);
    pushSTACK(O(object_posix__resolve_host_ipaddr));
    error(os_error,"~S (~S): ~S");
  }

  hostent_to_lisp(he);
}

#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
/* Lisp interface to getservbyport(3) and getservbyname(3) */

/* C struct servent --> Lisp SERVICE structure
 can trigger GC */
static Values servent_to_lisp (struct servent * se) {
  pushSTACK(safe_to_string(se->s_name));
  push_string_array(se->s_aliases);
  pushSTACK(L_to_I(ntohs(se->s_port)));
  pushSTACK(safe_to_string(se->s_proto));
  funcall(O(object_posix__make_service),4);
}

DEFUN(POSIX:SERVICE, &optional service-name protocol,(subr_posix_service,seclass_default,0,2,norest,nokey,0,NIL))
{
  object protocol = popSTACK();
  char *proto = NULL;
  char proto_buf[16];
  object serv;
  struct servent * se;
  if (!missingp(protocol)) {    /* check protocol */
    with_string_0(check_string(protocol),Symbol_value(GLO(misc_encoding)),
                  protocolz, {
                    begin_system_call();
                    strncpy(proto_buf,protocolz,15);
                    end_system_call();
                  });
    proto = proto_buf;
    proto_buf[15] = 0;
  }
  serv = popSTACK();
  if (missingp(serv)) {
    uintL count = 0;
#  if defined(HAVE_SETSERVENT) && defined(HAVE_GETSERVENT) && defined(HAVE_ENDSERVENT)
    begin_system_call();
    setservent(1);
    for (; (se = getservent()); count++) {
      end_system_call();
      servent_to_lisp(se); pushSTACK(value1);
      begin_system_call();
    }
    endservent();
    end_system_call();
#  else /* no getservent - emulate */
    uintL port;
    begin_system_call();
    for (port = 0; port < 0x10000; port++) {
      se = getservbyport(port,proto);
      if (se != NULL) {
        end_system_call();
        servent_to_lisp(se); pushSTACK(value1); count++;
        begin_system_call();
      }
    }
    end_system_call();
#  endif
    VALUES1(listof(count));
    return;
  } else if (symbolp(serv)) {
    serv = Symbol_name(serv);
    goto servent_string;
  } else if (stringp(serv)) { servent_string:
    with_string_0(serv,GLO(misc_encoding),servz, {
        begin_system_call();
        se = getservbyname(servz,proto);
        end_system_call();
      });
  } else if (integerp(serv)) {
    uintL port = I_to_UL(serv);
    begin_system_call();
    se = getservbyport(htons(port),proto);
    end_system_call();
  } else
    error_string_integer(serv);
  if (se == NULL) OS_error();
  servent_to_lisp(se);
}

#endif /* getservbyname getservbyport */

#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)

#if defined(HAVE_GRP_H)
# include <grp.h>
#endif

/* C struct group --> Lisp GROUP-INFO structure
 can trigger GC */
static Values grp_to_lisp (struct group *group) {
  pushSTACK(safe_to_string(group->gr_name));
  pushSTACK(UL_to_I(group->gr_gid));
  push_string_array(group->gr_mem);
  funcall(O(object_posix__make_group_info),3);
}

DEFUN(POSIX::GROUP-INFO, &optional group,(subr_posix_group_info,seclass_default,0,1,norest,nokey,0,NIL))
{ /* return the GROUP-INFO for the group or a list thereof if it is NIL. */
  object group = popSTACK();
  struct group *gr = NULL;
 group_info_restart:

# if defined(HAVE_GETGRENT) && defined(HAVE_SETGRENT) && defined(HAVE_ENDGRENT)
  if (missingp(group)) { /* all groups as a list */
    int count = 0;
    begin_system_call();
    setgrent();
    for (; (gr = getgrent()); count++) {
      end_system_call();
      grp_to_lisp(gr); pushSTACK(value1);
      begin_system_call();
    }
    endgrent();
    end_system_call();
    VALUES1(listof(count));
    return;
  }
# endif  /* setgrent getgrent endgrent */

  begin_system_call();
  errno = 0;
  if (uint32_p(group))
    gr = getgrgid(I_to_uint32(group));
  else if (symbolp(group)) {
    group = Symbol_name(group);
    goto group_info_string;
  } else if (stringp(group)) { group_info_string:
    with_string_0(group,GLO(misc_encoding),groupz, { gr = getgrnam(groupz); });
  } else {
    end_system_call(); error_string_integer(group);
  }
  end_system_call();

  if (NULL == gr) {
    if (errno == 0) {
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(group); pushSTACK(TheSubr(subr_self)->name);
      check_value(error_condition,GETTEXT("~S(~S): No such group"));
      group = value1;
      goto group_info_restart;
    } else OS_error();
  }
  grp_to_lisp(gr);
}
#endif  /* getgrgid getgrnam */

#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)

#if defined(HAVE_PWD_H)
# include <pwd.h>
#endif

/* C struct passwd --> Lisp USER-INFO structure
 can trigger GC */
static Values passwd_to_lisp (struct passwd *pwd) {
  pushSTACK(safe_to_string(pwd->pw_name));
  pushSTACK(safe_to_string(pwd->pw_passwd));
  pushSTACK(UL_to_I(pwd->pw_uid));
  pushSTACK(UL_to_I(pwd->pw_gid));
  pushSTACK(safe_to_string(pwd->pw_gecos));
  pushSTACK(safe_to_string(pwd->pw_dir));
  pushSTACK(safe_to_string(pwd->pw_shell));
  funcall(O(object_posix__make_user_info),7);
}

DEFUN(POSIX::USER-INFO, &optional user,(subr_posix_user_info,seclass_default,0,1,norest,nokey,0,NIL))
{ /* return the USER-INFO for the user or a list thereof if user is NIL. */
  object user = popSTACK();
  struct passwd *pwd = NULL;
 user_info_restart:

# if defined(HAVE_GETPWENT) && defined(HAVE_SETPWENT) && defined(HAVE_ENDPWENT)
  if (missingp(user)) { /* all users as a list */
    int count = 0;
    begin_system_call();
    setpwent();
    for (; (pwd = getpwent()); count++) {
      end_system_call();
      passwd_to_lisp(pwd); pushSTACK(value1);
      begin_system_call();
    }
    endpwent();
    end_system_call();
    VALUES1(listof(count));
    return;
  }
# endif  /* setpwent getpwent endpwent */

  begin_system_call();
  errno = 0;
  if (integerp(user))
    pwd = getpwuid(I_to_uid(user));
  else if (eq(user,S(Kdefault))) {
    char *username = getlogin();
    if (username != NULL)
      pwd = getpwnam(username);
  } else if (symbolp(user)) {
    user = Symbol_name(user);
    goto user_info_string;
  } else if (stringp(user)) { user_info_string:
    with_string_0(user,GLO(misc_encoding),userz, { pwd = getpwnam(userz); });
  } else {
    end_system_call(); error_string_integer(user);
  }
  end_system_call();

  if (NULL == pwd) {
    if (errno == 0) {
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(user); pushSTACK(TheSubr(subr_self)->name);
      check_value(error_condition,GETTEXT("~S(~S): No such user"));
      user = value1;
      goto user_info_restart;
    } else OS_error();
  }
  passwd_to_lisp(pwd);
}
#elif defined(WIN32_NATIVE)
/* FIXME: use
 http://msdn.microsoft.com/library/en-us/netmgmt/netmgmt/user_info_1_str.asp
 http://msdn.microsoft.com/library/en-us/netmgmt/netmgmt/netusergetinfo.asp
 http://msdn.microsoft.com/library/en-us/netmgmt/netmgmt/netuserenum.asp */
#endif  /* user-info */

#if defined(HAVE_GETUID)
DEFUN(POSIX:GETUID,,(subr_posix_getuid,seclass_default,0,0,norest,nokey,0,NIL)){ GETTER(uid,getuid); }
#endif
#if defined(HAVE_SETUID)
DEFUN(POSIX::%SETUID, uid,(subr_posix__25setuid,seclass_default,1,0,norest,nokey,0,NIL)) { SETTER(uid,setuid); }
#endif
#if defined(HAVE_GETGID)
DEFUN(POSIX:GETGID,,(subr_posix_getgid,seclass_default,0,0,norest,nokey,0,NIL)){ GETTER(gid,getgid); }
#endif
#if defined(HAVE_SETGID)
DEFUN(POSIX::%SETGID, gid,(subr_posix__25setgid,seclass_default,1,0,norest,nokey,0,NIL)) { SETTER(gid,setgid); }
#endif
#if defined(HAVE_GETEUID)
DEFUN(POSIX:GETEUID,,(subr_posix_geteuid,seclass_default,0,0,norest,nokey,0,NIL)){ GETTER(uid,geteuid); }
#endif
#if defined(HAVE_SETEUID)
DEFUN(POSIX::%SETEUID, euid,(subr_posix__25seteuid,seclass_default,1,0,norest,nokey,0,NIL)) { SETTER(uid,seteuid); }
#endif
#if defined(HAVE_GETEGID)
DEFUN(POSIX:GETEGID,,(subr_posix_getegid,seclass_default,0,0,norest,nokey,0,NIL)){ GETTER(gid,getegid); }
#endif
#if defined(HAVE_SETEGID)
DEFUN(POSIX::%SETEGID, egid,(subr_posix__25setegid,seclass_default,1,0,norest,nokey,0,NIL)) { SETTER(gid,setegid); }
#endif
#if defined(HAVE_GETGROUPS)
DEFUN(POSIX::GETGROUPS,,(subr_posix_getgroups,seclass_default,0,0,norest,nokey,0,NIL)) {
  int group_count, ret;
  gid_t *groups;
  begin_system_call(); group_count = getgroups(0,NULL); end_system_call();
  groups = (gid_t*)alloca(sizeof(gid_t) * group_count);
  begin_system_call(); ret = getgroups(group_count,groups); end_system_call();
  if (ret == -1) OS_error();
  while (ret--) pushSTACK(gid_to_I(*groups++));
  VALUES1(listof(group_count));
}
#endif
#if defined(HAVE_SETGROUPS)
DEFUN(POSIX::%SETGROUPS, groups,(subr_posix__25setgroups,seclass_default,1,0,norest,nokey,0,NIL)) {
  int group_count = llength1(STACK_0,NULL), i = group_count;
  gid_t *groups = (gid_t*)alloca(sizeof(gid_t) * group_count), *pgrp = groups;
  pushSTACK(STACK_0);
  while (i--) {
    *pgrp++ = I_to_gid(Car(STACK_0));
    STACK_0 = Cdr(STACK_0);
  }
  if (!nullp(popSTACK())) NOTREACHED;
  begin_system_call(); i = setgroups(group_count,groups); end_system_call();
  if (i == -1) OS_error();
  VALUES1(popSTACK());
}
#endif

#if defined(HAVE_STAT)
/* call stat() on the pathname
 return the value returned by stat()
 value1 = the actual pathname on which stat was called
 can trigger GC */
static int stat_obj (object path, struct stat *buf) {
  int ret;
  with_string_0(value1=physical_namestring(path),GLO(pathname_encoding),pathz,{
      begin_system_call(); ret = stat(pathz,buf); end_system_call(); });
  return ret;
}
#endif

#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
# if !defined(HAVE_LSTAT)
#  define lstat stat
# endif
DEFUN(POSIX::FILE-STAT, file &optional linkp,(subr_posix_file_stat,seclass_default,1,1,norest,nokey,0,NIL))
{ /* Lisp interface to stat(2), lstat(2) and fstat(2)
 the first arg can be a pathname designator or a file descriptor designator
 the return value is the FILE-STAT structure */
  bool link_p = missingp(STACK_0);
  object file = STACK_1;
  struct stat buf;

  if (builtin_stream_p(file)) {
    Handle fd;
    pushSTACK(file); funcall(L(built_in_stream_open_p),1);
    file = STACK_1;             /* restore */
    if (!nullp(value1)) {       /* open stream ==> use FD */
#    if defined(WIN32_NATIVE)
      /* woe32 does have fstat(), but it does not accept a file handle,
         only an integer of an unknown nature */
      BY_HANDLE_FILE_INFORMATION fi;
      begin_system_call();
      if (!GetFileInformationByHandle(fd=stream_get_handle(&STACK_1),&fi))
        error_OS_stream(STACK_1);
      end_system_call();
      pushSTACK(STACK_1);       /* file */
      pushSTACK(uint32_to_I(fi.dwVolumeSerialNumber)); /* device */
      pushSTACK(UL2_to_I(fi.nFileIndexHigh,fi.nFileIndexLow)); /* "inode" */
      pushSTACK(check_file_attributes_to_list(fi.dwFileAttributes));
      pushSTACK(uint32_to_I(fi.nNumberOfLinks)); /* number of hard links */
      pushSTACK(NIL); pushSTACK(NIL);            /* no GID or UID */
      pushSTACK(NIL);                            /* no rdev */
      pushSTACK(UL2_to_I(fi.nFileSizeHigh,fi.nFileSizeLow)); /* size */
      pushSTACK(NIL); pushSTACK(NIL); /* no blocksize od blocks */
      pushSTACK(convert_time_to_universal(&(fi.ftLastAccessTime)));
      pushSTACK(convert_time_to_universal(&(fi.ftLastWriteTime)));
      pushSTACK(convert_time_to_universal(&(fi.ftCreationTime)));
      goto call_make_file_stat;
#    else
      if (fstat(fd=stream_get_handle(&STACK_1),&buf) < 0)
        error_OS_stream(STACK_1);
      end_system_call();
      file = eq(STACK_1,nullobj) ? fixnum(fd) : (object)STACK_1; /* restore */
#    endif
    } else goto stat_pathname;
  } else if (integerp(file)) {
    begin_system_call();
    if (fstat(I_to_UL(file),&buf) < 0) OS_error();
    end_system_call();
  } else { stat_pathname:
    file = physical_namestring(file);
    with_string_0(file,GLO(pathname_encoding),namez, {
      begin_system_call();
      if ((link_p ? stat(namez,&buf) : lstat(namez,&buf)) < 0)
        OS_error();
      end_system_call();
    });
  }

  pushSTACK(file);                    /* the object stat'ed */
  pushSTACK(L_to_I(buf.st_dev));      /* device */
#if defined(SIZEOF_INO_T) && SIZEOF_INO_T == 8
  pushSTACK(uint64_to_I(buf.st_ino)); /* inode */
#else
  pushSTACK(uint32_to_I(buf.st_ino)); /* inode */
#endif
  pushSTACK(check_chmod_mode_to_list(buf.st_mode)); /* protection */
  pushSTACK(UL_to_I(buf.st_nlink));   /* number of hard links */
  pushSTACK(UL_to_I(buf.st_uid));     /* user ID of owner */
  pushSTACK(UL_to_I(buf.st_gid));     /* group ID of owner */
#if defined(HAVE_STAT_ST_RDEV)
  pushSTACK(L_to_I(buf.st_rdev));     /* device type (if inode device) */
#else
  pushSTACK(NIL);
#endif
  pushSTACK(L_to_I(buf.st_size));     /* total size, in bytes */
#if defined(HAVE_STAT_ST_BLKSIZE)
  pushSTACK(UL_to_I(buf.st_blksize)); /* blocksize for filesystem I/O */
#else
  pushSTACK(NIL);
#endif
#if defined(HAVE_STAT_ST_BLOCKS)
  pushSTACK(UL_to_I(buf.st_blocks));  /* number of blocks allocated */
#else
  pushSTACK(NIL);
#endif
  /* cannot use convert_time_to_universal() because this is used on win32 */
  pushSTACK(UL_to_I(buf.st_atime+UNIX_LISP_TIME_DIFF));/*time of last access*/
  pushSTACK(UL_to_I(buf.st_mtime+UNIX_LISP_TIME_DIFF));/*last modification*/
  pushSTACK(UL_to_I(buf.st_ctime+UNIX_LISP_TIME_DIFF));/*time of last change*/
 call_make_file_stat:
  funcall(O(object_posix__make_file_stat),14);
  skipSTACK(2);                 /* drop linkp & file */
}
#endif  /* fstat lstat fstat */

#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
/* error-signalling replacement for chmod()
   STACK_O is the path - for error reporting
 can trigger GC */
static void my_chmod (char *path, mode_t mode) {
#if defined(WIN32_NATIVE)
  if (!SetFileAttributes(path,mode)) OS_file_error(STACK_0);
#elif defined(HAVE_CHMOD)
  if (chmod(path,mode)) OS_file_error(STACK_0);
#else
  end_system_call();
  pushSTACK(CLSTEXT("~S(~S ~S ~S): this platform lacks ~S"));
  pushSTACK(TheSubr(subr_self)->name); pushSTACK(STACK_2);
  pushSTACK(O(object_Kmode)); pushSTACK(fixnum(mode));
  pushSTACK(O(object__22chmod_28_29_22));
  funcall(S(warn),5);
  begin_system_call();
#endif
}
/* error-signalling replacement for chown()
   STACK_O is the path - for error reporting
 can trigger GC */
static void my_chown (char *path, uid_t uid, gid_t gid) {
#if defined(HAVE_CHOWN)
  if (chown(path,uid,gid)) OS_file_error(STACK_0);
#else
  end_system_call();
  pushSTACK(CLSTEXT("~S(~S ~S ~S ~S ~S): this platform lacks ~S"));
  pushSTACK(TheSubr(subr_self)->name); pushSTACK(STACK_2);
  pushSTACK(O(object_Kuid)); pushSTACK((uid != (uid_t)-1) ? fixnum(uid) : NIL);
  pushSTACK(O(object_Kgid)); pushSTACK((gid != (gid_t)-1) ? fixnum(gid) : NIL);
  pushSTACK(O(object__22chown_28_29_22));
  funcall(S(warn),7);
  begin_system_call();
#endif
}
/* error-signalling replacement for utime()
   STACK_O is the path - for error reporting
 can trigger GC */
#if !defined(WIN32_NATIVE)
static void my_utime (char *path, bool utb_a, bool utb_m, struct utimbuf *utb) {
  if (utb_a && !utb_m) {
    struct stat st;
    if (stat(path,&st) < 0) OS_file_error(STACK_0);
    utb->modtime = st.st_mtime;
  }
  if (utb_m && !utb_a) {
    struct stat st;
    if (stat(path,&st) < 0) OS_file_error(STACK_0);
    utb->actime = st.st_atime;
  }
#if defined(HAVE_UTIME)
  if (utime(path,utb)) OS_file_error(STACK_0);
#else
  end_system_call();
  pushSTACK(CLSTEXT("~S(~S ~S ~S ~S ~S): this platform lacks ~S"));
  pushSTACK(TheSubr(subr_self)->name); pushSTACK(STACK_2);
  pushSTACK(O(object_Katime));
  pushSTACK(utb_a ? convert_time_to_universal(&(utb->actime)) : NIL);
  pushSTACK(O(object_Kmtime));
  pushSTACK(utb_m ? convert_time_to_universal(&(utb->modtime)) : NIL);
  pushSTACK(O(object__22utime_28_29_22));
  funcall(S(warn),7);
  begin_system_call();
#endif
}
#else  /* WIN32_NATIVE */
/* win32 implementation of utime() is severely broken:
   http://www.codeproject.com/datetime/dstbugs.asp */
struct a_m_time { FILETIME actime; FILETIME modtime; };
static void my_utime (char *path, bool utb_a, bool utb_m, struct a_m_time *tm) {
  HANDLE hfile = CreateFile(path, GENERIC_WRITE, 0 , NULL, OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL, NULL);
  BOOL success_p;
  if (hfile == INVALID_HANDLE_VALUE) OS_file_error(STACK_0);
  success_p = SetFileTime(hfile,NULL,utb_a ? &(tm->actime) : NULL,
                          utb_m ? &(tm->modtime) : NULL);
  CloseHandle(hfile);
  if (!success_p) OS_file_error(STACK_0);
}
#endif  /* WIN32_NATIVE */
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
/* get WIN32_FIND_DATA from the PATH
 < sh - search handle (optional)
 < wfd - file information
 < value1 - the actual path used
 can trigger GC */
static void find_first_file (object path, WIN32_FIND_DATA *wfd, HANDLE *sh) {
  HANDLE s_h;
  with_string_0(value1=physical_namestring(path),GLO(pathname_encoding),pathz,{
      begin_system_call(); s_h = FindFirstFile(pathz,wfd); end_system_call();
    });
  if (s_h == INVALID_HANDLE_VALUE) OS_file_error(value1);
  if (sh) *sh = s_h;
  else { begin_system_call(); FindClose(s_h); begin_system_call(); }
}
/* get file times from an object (like stat_obj())
 can trigger GC */
static void get_file_time (object path, FILETIME *atime, FILETIME *mtime) {
  WIN32_FIND_DATA wfd;
  find_first_file(path,&wfd,NULL);
  if (atime) *atime = wfd.ftLastAccessTime;
  if (mtime) *mtime = wfd.ftLastWriteTime;
}
#endif  /* WIN32_NATIVE | UNIX_CYGWIN32*/
DEFUN(POSIX::SET-FILE-STAT, file &key ATIME MTIME MODE UID GID,(subr_posix_set_file_stat,seclass_default,1,0,norest,key,5,NIL))
{ /* interface to chmod(2), chown(2), utime(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/utime.html
     http://www.opengroup.org/onlinepubs/009695399/functions/chown.html
     http://www.opengroup.org/onlinepubs/009695399/functions/chmod.html */
  gid_t gid = (missingp(STACK_0) ? skipSTACK(1), (gid_t)-1
               : I_to_uint32(check_uint32(popSTACK())));
  uid_t uid = (missingp(STACK_0) ? skipSTACK(1), (uid_t)-1
               : I_to_uint32(check_uint32(popSTACK())));
  mode_t mode = (missingp(STACK_0) ? skipSTACK(1), (mode_t)-1
#               if defined(WIN32_NATIVE)
                 : (mode_t)check_file_attributes_from_list(popSTACK())
#               else
                 : check_chmod_mode_from_list(popSTACK())
#               endif
                 );
# if defined(WIN32_NATIVE)
  struct a_m_time utb;
# else
  struct utimbuf utb;
# endif
  bool utb_a = false, utb_m = false;
  if (!missingp(STACK_0)) {     /* mtime */
    if (integerp(STACK_0))
      convert_time_from_universal(STACK_0,&(utb.modtime));
    else if (eq(STACK_0,T)) {
      funcall(L(get_universal_time),0);
      convert_time_from_universal(value1,&(utb.modtime));
    } else {                    /* set from another file */
#    if defined(WIN32_NATIVE)
      get_file_time(STACK_0,NULL,&(utb.modtime));
#    else
      struct stat st;
      if (stat_obj(STACK_0,&st)) OS_file_error(value1);
      utb.modtime = st.st_mtime;
#    endif
    }
    utb_m = true;
  }
  if (!missingp(STACK_1)) {     /* atime */
    if (integerp(STACK_1))
      convert_time_from_universal(STACK_1,&(utb.actime));
    else if (eq(STACK_1,T)) {
      funcall(L(get_universal_time),0);
      convert_time_from_universal(value1,&(utb.actime));
    } else {                    /* set from another file */
#    if defined(WIN32_NATIVE)
      get_file_time(STACK_0,&(utb.actime),NULL);
#    else
      struct stat st;
      if (stat_obj(STACK_1,&st)) OS_file_error(value1);
      utb.actime = st.st_atime;
#    endif
   }
    utb_a = true;
  }
  skipSTACK(2);                 /* drop atime & mtime */
  STACK_0 = physical_namestring(STACK_0);
  with_string_0(STACK_0,GLO(pathname_encoding),path, {
      begin_system_call();
      if (mode != (mode_t)-1) my_chmod(path,mode);
      if ((uid != (uid_t)-1) || (gid != (gid_t)-1)) my_chown(path,uid,gid);
      if (utb_a || utb_m) my_utime(path,utb_a,utb_m,&utb);
      end_system_call();
    });
  VALUES0; skipSTACK(1);
}
#endif  /* chmod chown utime */

/* <http://www.opengroup.org/onlinepubs/009695399/basedefs/sys/stat.h.html> */
#line 1931

DEFUN(POSIX::CONVERT-MODE, mode,(subr_posix_convert_mode,seclass_default,1,0,norest,nokey,0,NIL))
{ /* convert between symbolic and numeric permissions */
  VALUES1(integerp(STACK_0)
          ? check_chmod_mode_to_list(I_to_uint32(check_uint32(popSTACK())))
          : uint32_to_I(check_chmod_mode_from_list(popSTACK())));
}

#if defined(HAVE_UMASK)
DEFUN(POSIX::UMASK, cmask,(subr_posix_umask,seclass_default,1,0,norest,nokey,0,NIL))
{ /* lisp interface to umask(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/umask.html */
  mode_t cmask = check_chmod_mode_from_list(popSTACK());
  begin_system_call();
  cmask = umask(cmask);
  end_system_call();
  VALUES1(fixnum(cmask));
}
#endif  /* umask */

#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
#if defined(HAVE_CREAT) && !defined(HAVE_MKNOD)
static int creat1 (const char *path, mode_t mode)
{ /* creat() and close() immediately  */
  int fd = creat(path,mode);
  if (fd == -1) return -1;
  return close(fd);
}
#endif
#if defined(WIN32_NATIVE)
static int mkdir1 (const char *path, mode_t mode)
{ (void)mode; return mkdir(path); }
#else
# define mkdir1 mkdir
#endif
#line 1967

DEFUN(POSIX::MKNOD, path type mode,(subr_posix_mknod,seclass_default,3,0,norest,nokey,0,NIL))
{ /* lisp interface to mknod(2)
     http://www.opengroup.org/onlinepubs/009695399/functions/mknod.html */
  mode_t mode = check_chmod_mode_from_list(popSTACK());
#if defined(HAVE_MKNOD)
  mode |= mknod_type_check(popSTACK());
#else
  /* emulate mknod using mkfifo(), mkdir() and creat() */
#define mknod(p,m,d) mknod1(p,m)
  int (*mknod1)(const char *path, mode_t mode);
 mknod_restart:
# if defined(HAVE_MKFIFO)
  if (eq(O(object_Kfifo),STACK_0)) {
    mknod1 = mkfifo; skipSTACK(1);
    goto mknod_do_it;
  }
# endif  /* mkfifo */
# if defined(HAVE_MKDIR)
  if (eq(O(object_Kfdir),STACK_0)) {
    mknod1 = mkdir1; skipSTACK(1);
    goto mknod_do_it;
  }
# endif  /* mkfifo */
# if defined(HAVE_CREAT)
  if (eq(O(object_Kfdir),STACK_0)) {
    mknod1 = creat1; skipSTACK(1);
    goto mknod_do_it;
  }
# endif  /* mkfifo */
  /* invalid type */
  pushSTACK(NIL);               /* no PLACE */
  pushSTACK(STACK_1);           /* TYPE-ERROR slot DATUM */
  { int count = 1;
    pushSTACK(O(object_cl_member));
#  if defined(HAVE_MKFIFO)
    pushSTACK(O(object_Kfifo)); count++;
#  endif
#  if defined(HAVE_MKDIR)
    pushSTACK(O(object_Kfdir)); count++;
#  endif
#  if defined(HAVE_CREAT)
    pushSTACK(O(object_Kfreg)); count++;
#  endif
    value1 = listof(count);
  } pushSTACK(value1);          /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(STACK_0); pushSTACK(STACK_2);
  pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,GETTEXT("~S: ~S is not of type ~S"));
  STACK_0 = value1;
  goto mknod_restart;
 mknod_do_it:
#endif                          /* no mknod() */
  funcall(L(namestring),1);     /* drop path from STACK */
  with_string_0(value1,GLO(pathname_encoding),path, {
      begin_system_call();
      if (mknod(path,mode,0)) OS_file_error(value1);
      end_system_call();
    });
  VALUES0;
}
#endif  /* mknod | mkfifo | mkdir | creat */

#if defined(HAVE_MKDTEMP) || defined(WIN32_NATIVE) || (defined(HAVE_MKDIR) && defined(HAVE_TEMPNAM))
DEFUN(POSIX:MKDTEMP, template,(subr_posix_mkdtemp,seclass_default,1,0,norest,nokey,0,NIL)) {
#if defined(HAVE_MKDTEMP)
  object fname = physical_namestring(popSTACK());
  with_string_0(fname,GLO(pathname_encoding),namez,{
      char *c_template;
      begin_system_call();
      if (namez_bytelen > 6
          && namez[namez_bytelen-1]=='X'
          && namez[namez_bytelen-2]=='X'
          && namez[namez_bytelen-3]=='X'
          && namez[namez_bytelen-4]=='X'
          && namez[namez_bytelen-5]=='X'
          && namez[namez_bytelen-6]=='X') {
        c_template = namez;
      } else {
        c_template = (char*)alloca(namez_bytelen+6);
        strcpy(c_template,namez);
        strcat(c_template,"XXXXXX");
      }
      if (NULL == mkdtemp(c_template)) OS_error();
      end_system_call();
      fname = asciz_to_string(c_template,GLO(pathname_encoding));
    });
  pushSTACK(fname);
#else  /* WIN32_NATIVE || (MKDIR && TEMPNAM) */
  /* http://www.opengroup.org/onlinepubs/009695399/functions/tempnam.html */
  pushSTACK(STACK_0); funcall(L(pathname),1); pushSTACK(value1);
  pushSTACK(value1); funcall(L(directory_namestring),1); pushSTACK(value1);
  pushSTACK(STACK_1); funcall(L(file_namestring),1); pushSTACK(value1);
  /* stack layout: template arg, template pathname, dir, file */
  with_string_0(STACK_0,GLO(pathname_encoding),prefix, {
      with_string_0(STACK_1,GLO(pathname_encoding),dir, {
          /* if no directory ==> use current "." */
          STACK_3 = temp_name(dir[0] ? dir : (char*)".",prefix);
          with_string_0(STACK_3,GLO(pathname_encoding),newdir, {
              begin_system_call();
              if (mkdir1(newdir,0700)) OS_file_error(STACK_2);
              end_system_call();
            });
        });
    });
  skipSTACK(3);
#endif
  /* stack layout: the name of the new directory - without the trailing slash */
#if defined(WIN32_NATIVE)
  pushSTACK(GLO(backslash_string));
#else
  pushSTACK(GLO(slash_string));
#endif
  VALUES1(string_concat(2));
}
#endif

#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
#if defined(WIN32_NATIVE)
/* winsup/src/winsup/cygwin/syscalls.cc */
typedef unsigned long fsblkcnt_t;
typedef unsigned long fsfilcnt_t;
struct statvfs {
  unsigned long f_bsize;        /* file system block size */
  unsigned long f_frsize;       /* fragment size */
  fsblkcnt_t f_blocks;          /* size of fs in f_frsize units */
  fsblkcnt_t f_bfree;           /* free blocks in fs */
  fsblkcnt_t f_bavail;          /* free blocks avail to non-superuser */
  fsfilcnt_t f_files;           /* total file nodes in file system */
  fsfilcnt_t f_ffree;           /* free file nodes in fs */
  fsfilcnt_t f_favail;          /* avail file nodes in fs */
  unsigned long f_fsid;         /* file system id */
  unsigned long f_flag;         /* mount flags */
  unsigned long f_namemax;      /* maximum length of filenames */
  char f_volname[MAX_PATH];     /* volume name */
  char f_fstype[MAX_PATH];      /* file system type */
};
#define HAVE_STATVFS_F_VOLNAME
#define HAVE_STATVFS_F_FSTYPE
static int statvfs (const char *fname, struct statvfs *sfs) {
  /* GetDiskFreeSpaceEx must be called before GetDiskFreeSpace on
     WinME, to avoid the MS KB 314417 bug */
  ULARGE_INTEGER availb, freeb, totalb;
  DWORD spc, bps, availc, freec, totalc, vsn, maxlen, flags, bpc;
  char root[MAX_PATH], *rootp = root;
  if (fname[1] == ':') {        /* c:\ */
    *rootp++ = *fname++;
    *rootp++ = *fname++;
  } else if (fname[0] == '\\' && fname[1] == '\\') { /* \\host\dir\ */
    const char *cp = strchr(fname + 2,'\\');
    unsigned int len;
    if (cp) cp = strchr(cp+1,'\\'); /* just host, no dir => error later */
    memcpy(root,fname,(len = cp - fname));
    rootp = root + len;
  } else {
    SetLastError(ERROR_DIRECTORY);
    return -1;
  }
  *rootp++ = '\\';
  *rootp = 0;

  if (!GetDiskFreeSpace(root,&spc,&bps,&freec,&totalc))
    return -1;                  /* bytes per sector */
  bpc = spc*bps;
  if (GetDiskFreeSpaceEx(root,&availb,&totalb,&freeb)) {
    availc = availb.QuadPart / bpc;
    totalc = totalb.QuadPart / bpc;
    freec = freeb.QuadPart / bpc;
  } else
    availc = freec;
  if (!GetVolumeInformation(root,sfs->f_volname,MAX_PATH,&vsn,&maxlen,&flags,
                            sfs->f_fstype,MAX_PATH))
    return -1;
  sfs->f_bsize = bpc;
  sfs->f_frsize = bpc;
  sfs->f_blocks = totalc;
  sfs->f_bfree = freec;
  sfs->f_bavail = availc;
  sfs->f_files = (fsfilcnt_t)-1;
  sfs->f_ffree = (fsfilcnt_t)-1;
  sfs->f_favail = (fsfilcnt_t)-1;
  sfs->f_fsid = vsn;
  sfs->f_flag = flags;
  sfs->f_namemax = maxlen;
  return 0;
}
#endif
#line 2162

/* there is also a legacy interface (f)statfs()
   which is not POSIX and is not supported */
DEFUN(POSIX::STAT-VFS, file,(subr_posix_stat_vfs,seclass_default,1,0,norest,nokey,0,NIL))
{ /* Lisp interface to statvfs(2), fstatvfs(2)
 the first arg can be a pathname designator or a file descriptor designator
 the return value is the STAT-VFS structure */
  object file = popSTACK();
  struct statvfs buf;

#if defined(HAVE_FSTATVFS)
  if (builtin_stream_p(file)) {
    pushSTACK(file);            /* save */
    pushSTACK(file); funcall(L(built_in_stream_open_p),1);
    file = popSTACK();          /* restore */
    if (!nullp(value1)) { /* open stream ==> use FD */
      Handle fd;
      pushSTACK(file);          /* save */
      begin_system_call();
      if (fstatvfs(fd=stream_get_handle(&STACK_0),&buf) < 0)
        error_OS_stream(STACK_0);
      end_system_call();
      file = eq(STACK_0,nullobj) ? fixnum(fd) : (object)STACK_0; /* restore */
      skipSTACK(1);
    } else goto stat_pathname;
  } else if (integerp(file)) {
    begin_system_call();
    if (fstatvfs(I_to_L(file),&buf) < 0) OS_error();
    end_system_call();
  } else stat_pathname:
#endif
    with_string_0(file = physical_namestring(file),GLO(pathname_encoding),
                  namez, {
      begin_system_call();
      if (statvfs(namez,&buf) < 0) OS_error();
      end_system_call();
    });

  pushSTACK(file);                  /* the object statvfs'ed */
#define pushSLOT(s) pushSTACK(s==(unsigned long)-1 ? NIL : ulong_to_I(s))
  pushSLOT(buf.f_bsize);  /* file system block size */
  pushSLOT(buf.f_frsize); /* fundamental file system block size */
#if defined(SIZEOF_FSBLKCNT_T) && SIZEOF_FSBLKCNT_T == 8
# define pushBSLOT(s) pushSTACK(s==(fsblkcnt_t)-1 ? NIL : uint64_to_I(s))
#else
# define pushBSLOT(s) pushSTACK(s==(fsblkcnt_t)-1 ? NIL : uint32_to_I(s))
#endif
  pushBSLOT(buf.f_blocks); /* total # of blocks on file system */
  pushBSLOT(buf.f_bfree);  /* total number of free blocks */
  pushBSLOT(buf.f_bavail); /* # of free blocks available to
                              non-privileged processes */
#undef pushBSLOT
#if defined(SIZEOF_FSBLKCNT_T) && SIZEOF_FSBLKCNT_T == 8
# define pushFSLOT(s) pushSTACK(s==(fsfilcnt_t)-1 ? NIL : uint64_to_I(s))
#else
# define pushFSLOT(s) pushSTACK(s==(fsfilcnt_t)-1 ? NIL : uint32_to_I(s))
#endif
  pushFSLOT(buf.f_files);  /* total # of file serial numbers */
  pushFSLOT(buf.f_ffree);  /* total # of free file serial numbers */
  pushFSLOT(buf.f_favail); /* # of file serial numbers available to
                              non-privileged processes */
#undef pushFSLOT
#if HAVE_SCALAR_FSID
  pushSLOT(buf.f_fsid);   /* file system ID */
#else
  /* On Linux, f_fsid of 'struct statfs' is a struct consisting of two ints.
     With glibc <= 2.1, f_fsid of 'struct statvfs' is the same. We are
     prepared to return one number only, so we just return the first int.
     This matches the behaviour of glibc >= 2.2 on 32-bit platforms. */
  pushSLOT((*(uintL*)&buf.f_fsid));   /* file system ID */
#endif
  pushSTACK(vfs_flags_to_list(buf.f_flag)); /* Bit mask of f_flag values. */
  pushSLOT(buf.f_namemax);      /* maximum filename length */
#if defined(HAVE_STATVFS_F_VOLNAME)
  pushSTACK(asciz_to_string(buf.f_volname,GLO(pathname_encoding)));
#else
  pushSTACK(NIL);
#endif
#if defined(HAVE_STATVFS_F_FSTYPE)
  pushSTACK(asciz_to_string(buf.f_fstype,GLO(pathname_encoding)));
#else
  pushSTACK(NIL);
#endif
  funcall(O(object_posix__make_stat_vfs),14);
#undef pushSLOT
}

#endif  /* fstatvfs statvfs */


/* FILE-OWNER */

#if defined(UNIX)

static const char *
get_owner (const char *filename)
{
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) {
    struct passwd *pwd = getpwuid(statbuf.st_uid);
    if (pwd)
      return pwd->pw_name;
  }
  return "";
}

#endif

#if defined(WIN32_NATIVE)

#include <windows.h>
#include <aclapi.h>

/* Some functions missing in Windows95/98/ME. */

/* Added in Windows NT 4.0 */
static DWORD WINAPI (*GetSecurityInfoFunc) (HANDLE handle, SE_OBJECT_TYPE ObjectType, SECURITY_INFORMATION SecurityInfo, PSID* ppsidOwner, PSID* ppsidGroup, PACL* ppDacl, PACL* ppSacl, PSECURITY_DESCRIPTOR* ppSecurityDescriptor);
#undef GetSecurityInfo
#define GetSecurityInfo (*GetSecurityInfoFunc)

/* Added in Windows NT Workstation */
static BOOL WINAPI (*LookupAccountSidFunc) (LPCTSTR lpSystemName, PSID lpSid, LPTSTR lpName, LPDWORD cchName, LPTSTR lpReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse);
#undef LookupAccountSid
#define LookupAccountSid (*LookupAccountSidFunc)

/* Added in Windows NT Workstation */
static DWORD WINAPI (*GetLengthSidFunc) (PSID pSid);
#undef GetLengthSid
#define GetLengthSid (*GetLengthSidFunc)

/* Added in Windows NT Workstation */
static BOOL WINAPI (*CopySidFunc) (DWORD nDestinationSidLength, PSID pDestinationSid, PSID pSourceSid);
#undef CopySid
#define CopySid (*CopySidFunc)

/* Added in Windows NT Workstation */
static BOOL WINAPI (*EqualSidFunc) (PSID pSid1, PSID pSid2);
#undef EqualSid
#define EqualSid (*EqualSidFunc)

/* Added in Windows 2000 Professional */
static BOOL WINAPI (*ConvertSidToStringSidFunc) (IN PSID Sid, OUT LPTSTR *StringSid);
#undef ConvertSidToStringSid
#define ConvertSidToStringSid (*ConvertSidToStringSidFunc)

static BOOL initialized_sid_apis = FALSE;

static void
initialize_sid_apis ()
{
  HMODULE advapi32 = LoadLibrary("advapi32.dll");
  if (advapi32 != NULL) {
    GetSecurityInfoFunc =
      (DWORD WINAPI (*) (HANDLE, SE_OBJECT_TYPE, SECURITY_INFORMATION, PSID*, PSID*, PACL*, PACL*, PSECURITY_DESCRIPTOR*))
      GetProcAddress(advapi32, "GetSecurityInfo");
    LookupAccountSidFunc =
      (BOOL WINAPI (*) (LPCTSTR, PSID, LPTSTR, LPDWORD, LPTSTR, LPDWORD, PSID_NAME_USE))
      GetProcAddress(advapi32, "LookupAccountSidA");
    GetLengthSidFunc =
      (DWORD WINAPI (*) (PSID)) GetProcAddress(advapi32, "GetLengthSid");
    CopySidFunc =
      (BOOL WINAPI (*) (DWORD, PSID, PSID)) GetProcAddress(advapi32, "CopySid");
    EqualSidFunc =
      (BOOL WINAPI (*) (PSID, PSID)) GetProcAddress(advapi32, "EqualSid");
    ConvertSidToStringSidFunc =
      (BOOL WINAPI (*) (PSID, LPTSTR*))
      GetProcAddress(advapi32, "ConvertSidToStringSidA");
  }
  initialized_sid_apis = TRUE;
}

/* A cache mapping SID -> owner. */
struct sid_cache_entry {
  PSID psid;
  char *name;
};
static struct sid_cache_entry *sid_cache = NULL;
static size_t sid_cache_count = 0;
static size_t sid_cache_allocated = 0;

static const char *
sid_cache_get (PSID psid)
{
  size_t i;
  for (i = 0; i < sid_cache_count; i++)
    if (EqualSid(psid, sid_cache[i].psid))
      return sid_cache[i].name;
  return NULL;
}

static void
sid_cache_put (PSID psid, const char *name)
{
  if (sid_cache_count == sid_cache_allocated) {
    size_t new_allocated = 2 * sid_cache_allocated + 5;
    sid_cache = (struct sid_cache_entry*)
      (sid_cache != NULL
       ? realloc(sid_cache, new_allocated * sizeof(struct sid_cache_entry))
       : malloc(new_allocated * sizeof(struct sid_cache_entry)));
    sid_cache_allocated = (sid_cache == NULL)?0:new_allocated;
  }
  if (sid_cache != NULL) {
    DWORD psid_len = GetLengthSid(psid);
    size_t name_len = strlen(name) + 1;
    char *memory = (char *)malloc(psid_len+name_len);
    if (memory == NULL)
      return;
    if (!CopySid(psid_len, memory, psid)) return;
    memcpy(memory+psid_len, name, name_len);
    sid_cache[sid_cache_count].psid = memory;
    sid_cache[sid_cache_count].name = memory + psid_len;
    sid_cache_count++;
  }
}

static const char *
get_owner (const char *filename)
{
  const char *owner;

  if (!initialized_sid_apis)
    initialize_sid_apis();
  owner = "";
  if (GetSecurityInfoFunc != NULL
      && LookupAccountSidFunc != NULL
      && GetLengthSidFunc != NULL
      && CopySidFunc != NULL
      && EqualSidFunc != NULL) {
    /* On Windows, directories don't have an owner. */
    WIN32_FIND_DATA entry;
    HANDLE searchhandle = FindFirstFile(filename, &entry);
    if (searchhandle != INVALID_HANDLE_VALUE) {
      if (!(entry.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
        /* It's a file. */
        HANDLE filehandle =
         CreateFile(filename, GENERIC_READ,
                    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
        if (filehandle != INVALID_HANDLE_VALUE) {
          /* Get the owner. */
          PSID psid;
          PSECURITY_DESCRIPTOR psd;
          DWORD err =
            GetSecurityInfo(filehandle, SE_FILE_OBJECT,
                            OWNER_SECURITY_INFORMATION,
                            &psid, NULL, NULL, NULL, &psd);
          if (err == 0) {
            owner = sid_cache_get(psid);
            if (owner == NULL) {
              static char buf1[4000];
              DWORD buf1size = sizeof(buf1);
              static char buf2[4000];
              DWORD buf2size = sizeof(buf2);
              SID_NAME_USE role;
              if (!LookupAccountSid(NULL, psid, buf1, &buf1size, buf2, &buf2size, &role)) {
                if (ConvertSidToStringSidFunc != NULL) {
                  /* Fallback: Use S-R-I-S-S... notation.  */
                  char *s;
                  if (!ConvertSidToStringSid(psid, &s)) owner = "";
                  else {
                    strcpy(buf1, s);
                    LocalFree(s);
                    owner = buf1;
                  }
                } else {
                  strcpy(buf1, "");
                  owner = buf1;
                }
              } else { /* DOMAIN\Account */
                int len = strlen(buf2);
                buf2[len] = '\\';
                strcpy(buf2+len+1,buf1);
                owner = buf2;
              }
              sid_cache_put(psid, owner);
            }
            LocalFree(psd);
          }
          CloseHandle(filehandle);
        }
      }
      FindClose(searchhandle);
    }
  }
  return owner;
}

#endif /* WIN32_NATIVE */

DEFUN(OS::FILE-OWNER, file,(subr_os_file_owner,seclass_default,1,0,norest,nokey,0,NIL))
{
  object file;
  const char *result;
  file = physical_namestring(STACK_0);
  with_string_0(file,GLO(misc_encoding),filename, {
    begin_system_call();
    result = get_owner(filename);
    end_system_call();
  });
  VALUES1(safe_to_string(result));
  skipSTACK(1);
}

/* end of FILE-OWNER */

#if defined(WIN32_NATIVE)

/* Pointers to functions unavailable on windows 95, 98, ME */

typedef BOOL (WINAPI * CreateHardLinkFuncType)
  (LPCTSTR lpFileName, LPCTSTR lpExistingFileName,
   LPSECURITY_ATTRIBUTES lpSecurityAttributes);
static CreateHardLinkFuncType CreateHardLinkFunc = NULL;

typedef BOOL (WINAPI * BackupWriteFuncType)
  (HANDLE hFile, LPBYTE lpBuffer, DWORD nNumberOfBytesToWrite,
   LPDWORD lpNumberOfBytesWritten, BOOL bAbort, BOOL bProcessSecurity,
   LPVOID *lpContext);
static BackupWriteFuncType BackupWriteFunc = NULL;
#endif

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
typedef HRESULT (WINAPI * StgOpenStorageExFuncType) (const WCHAR* pwcsName,
            DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, void * reserved1,
            void * reserved2, REFIID riid, void ** ppObjectOpen);
static StgOpenStorageExFuncType StgOpenStorageExFunc = NULL;
#endif

void module__syscalls__init_function_2 (module_t* module);
void module__syscalls__init_function_2 (module_t* module) {
#if defined(WIN32_NATIVE)
  HMODULE kernel32 = LoadLibrary ("kernel32.dll");
  if (kernel32 != NULL) {
    CreateHardLinkFunc = (CreateHardLinkFuncType)
      GetProcAddress (kernel32, "CreateHardLinkA");
    BackupWriteFunc = (BackupWriteFuncType)
      GetProcAddress (kernel32, "BackupWrite");
    LockFileExFunc = (LockFileExFuncType)
      GetProcAddress (kernel32, "LockFileEx");
    if (LockFileExFunc == NULL)
      LockFileExFunc = (LockFileExFuncType) &my_LockFileEx;
    UnlockFileExFunc = (UnlockFileExFuncType)
      GetProcAddress (kernel32, "UnlockFileEx");
    if (UnlockFileExFunc == NULL)
      UnlockFileExFunc = (UnlockFileExFuncType) &my_UnlockFileEx;
  }
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { HMODULE ole32 = LoadLibrary ("ole32.dll");
    if (ole32 != NULL)
      StgOpenStorageExFunc = (StgOpenStorageExFuncType)
        GetProcAddress (ole32, "StgOpenStorageEx");
  }
#endif
}

/* COPY-FILE related functions. */

#if defined(WIN32_NATIVE)
/* Checks if it's safe to call OldHardLink */
static BOOL OldHardLinkGuard () {
  OSVERSIONINFO vi;
  if (BackupWriteFunc == NULL) return FALSE;
  vi.dwOSVersionInfoSize = sizeof(vi);
  if (!GetVersionEx(&vi)) return FALSE;
  return vi.dwPlatformId == VER_PLATFORM_WIN32_NT;
}

/* From knowledge base article Q234727
   This approach works on NT >= 3.51. */
static BOOL OldHardLink( LPCTSTR source, LPCTSTR dest ) {

   WCHAR  wsource[ MAX_PATH + 1 ];
   WCHAR  wdest[ MAX_PATH + 1 ];
   WCHAR  wdestfull[ MAX_PATH + 1 ];
   LPWSTR wdestfullfile;

   HANDLE hFileSource;

   WIN32_STREAM_ID StreamId;
   DWORD dwBytesWritten;
   LPVOID lpContext;
   DWORD cbPathLen;
   DWORD StreamHeaderSize;

   BOOL bSuccess;

   /* convert from ANSI to UNICODE */
   if (MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS /*error on invalid chars*/,
     dest, -1/*null terminated*/, wdest, MAX_PATH + 1) == 0) return FALSE;

   /* open existing file that we link to */
   hFileSource = CreateFile(source, FILE_WRITE_ATTRIBUTES,
     FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
     NULL, /* sa */ OPEN_EXISTING, 0, NULL );

   if (hFileSource == INVALID_HANDLE_VALUE) return FALSE;

   /* validate and sanitize supplied link path and use the result
      the full path MUST be Unicode for BackupWrite */
   cbPathLen = GetFullPathNameW( wdest , MAX_PATH, wdestfull, &wdestfullfile);

   if (cbPathLen == 0) return FALSE;

   cbPathLen = (cbPathLen + 1) * sizeof(WCHAR); // adjust for byte count

   /* prepare and write the WIN32_STREAM_ID out */
   lpContext = NULL;

   StreamId.dwStreamId = BACKUP_LINK;
   StreamId.dwStreamAttributes = 0;
   StreamId.dwStreamNameSize = 0;
   StreamId.Size.HighPart = 0;
   StreamId.Size.LowPart = cbPathLen;

   /* compute length of variable size WIN32_STREAM_ID */
   StreamHeaderSize = (LPBYTE)&StreamId.cStreamName - (LPBYTE)&StreamId
                      + StreamId.dwStreamNameSize ;

   bSuccess = BackupWriteFunc(hFileSource,
                         (LPBYTE)&StreamId,  /* buffer to write */
                         StreamHeaderSize,   /* number of bytes to write */
                         &dwBytesWritten,
                         FALSE,              /* don't abort yet */
                         FALSE,              /* don't process security */
                         &lpContext);
   bSuccess &= BackupWriteFunc(hFileSource,(LPBYTE)wdestfull, cbPathLen,
        &dwBytesWritten, FALSE, FALSE, &lpContext);
   /* free context */
   bSuccess &= BackupWriteFunc(hFileSource,NULL,0,&dwBytesWritten,TRUE, FALSE,
        &lpContext);
   CloseHandle( hFileSource );
   return bSuccess;
}

static inline int MkHardLink (char* old_pathstring, char* new_pathstring) {
  if (CreateHardLinkFunc != NULL)
    return CreateHardLinkFunc(new_pathstring,old_pathstring,NULL);
  if (OldHardLinkGuard())
    return OldHardLink(old_pathstring,new_pathstring);
  SetLastError(ERROR_INVALID_FUNCTION); /* or what ? */
  return 0;
}
#endif

/* Hard/Soft Link a file
 > old_pathstring: old file name, ASCIZ-String
 > new_pathstring: new file name, ASCIZ-String
 > STACK_3: old pathname
 > STACK_1: new pathname */
#if defined(WIN32_NATIVE)
# define HAVE_LINK
#elif !defined(LINK_FOLLOWS_SYMLINKS) && defined(HAVE_REALPATH)
static inline int my_link (const char* source, const char* destination) {
# ifndef MAXPATHLEN             /* see unix.d */
#  define MAXPATHLEN  1024      /* <sys/param.h> */
# endif
  char path_buffer[MAXPATHLEN];
  if (NULL == realpath(source,path_buffer)) OS_file_error(STACK_3);
  return link(path_buffer,destination);
}
#else
# define my_link link
#endif
#if defined(HAVE_LINK)
static void hardlink_file (char* old_pathstring, char* new_pathstring) {
  begin_system_call();
# if defined(WIN32_NATIVE)
  if (MkHardLink(old_pathstring,new_pathstring) == FALSE)
    if (GetLastError() == ERROR_FILE_NOT_FOUND)
# else
  if (my_link(old_pathstring,new_pathstring) < 0)
    if (errno==ENOENT)
# endif
      OS_file_error(STACK_3);
    else OS_file_error(STACK_1);
  end_system_call();
}
#endif
#if defined(HAVE_SYMLINK)
static inline void symlink_file (char* old_pathstring, char* new_pathstring) {
  begin_system_call();
  if (symlink(old_pathstring,new_pathstring) < 0) { /* symlink file */
    if (errno==ENOENT) OS_file_error(STACK_3);
    else OS_file_error(STACK_1);
  }
  end_system_call();
}
#endif

/* Copy attributes from stream STACK_1 to stream STACK_0 and close them
   can trigger GC */
static void copy_attributes_and_close () {
  Handle source_fd = stream_lend_handle(&STACK_1,true,NULL);
  Handle dest_fd = stream_lend_handle(&STACK_0,false,NULL);
  struct stat source_sb;
  struct stat dest_sb;

# if defined(HAVE_FSTAT) && !defined(WIN32_NATIVE)
  begin_system_call();
  if (fstat(source_fd, &source_sb) == -1) {
    end_system_call();
    pushSTACK(file_stream_truename(STACK_1));
    goto close_and_err;
  }
  if (fstat(dest_fd, &dest_sb) == -1) {
    end_system_call();
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
  end_system_call();
# elif defined(HAVE_STAT)
  if (stat_obj(STACK_1, &source_sb) == -1) {
    pushSTACK(file_stream_truename(STACK_1));
    goto close_and_err;
  }
  if (stat_obj(STACK_0, &dest_sb) == -1) {
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
# else
  goto close_success;
# endif

# if defined(WIN32_NATIVE) /*** file mode ***/
  { BOOL ret;
    BY_HANDLE_FILE_INFORMATION fi;
    begin_system_call();
    ret = GetFileInformationByHandle(source_fd,&fi);
    end_system_call();
    if (!ret) {
      pushSTACK(file_stream_truename(STACK_1));
      goto close_and_err;
    }
    with_string_0(physical_namestring(STACK_0),GLO(pathname_encoding),destz,{
        begin_system_call();
        ret = SetFileAttributes(destz,fi.dwFileAttributes);
        end_system_call();
      });
    if (!ret) {
      pushSTACK(file_stream_truename(STACK_0));
      goto close_and_err;
    }
  }
# elif defined(HAVE_FCHMOD)
  begin_system_call();
  if (((source_sb.st_mode & 0777) != (dest_sb.st_mode & 0777))
      && (fchmod(dest_fd, source_sb.st_mode & 0777) == -1)) {
    end_system_call();
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
  end_system_call();
# elif defined(HAVE_CHMOD)
  if ((source_sb.st_mode & 0777) != (dest_sb.st_mode & 0777)) {
    int ret;
    with_string_0(physical_namestring(STACK_0),GLO(pathname_encoding),destz,{
        begin_system_call();
        ret = chmod(destz, source_sb.st_mode & 0777);
        end_system_call();
      });
    if (ret == -1) {
      pushSTACK(file_stream_truename(STACK_0));
      goto close_and_err;
    }
  }
# endif

# if defined(HAVE_FCHOWN) /*** owner/group ***/
  begin_system_call();
  if (fchown(dest_fd, source_sb.st_uid, source_sb.st_gid) == -1) {
    end_system_call();
    pushSTACK(file_stream_truename(STACK_0));
    goto close_and_err;
  }
  end_system_call();
# elif defined(HAVE_CHOWN)
  { int ret;
    with_string_0(physical_namestring(STACK_0),GLO(pathname_encoding),destz,{
        begin_system_call();
        ret = chown(destz, source_sb.st_uid, source_sb.st_gid);
        end_system_call();
      });
    if (ret == -1) {
      pushSTACK(file_stream_truename(STACK_0));
      goto close_and_err;
    }
  }
# endif

# if defined(HAVE_UTIME)
  /* we must close the streams now - before utime() -
     because close() modifies write and access times */
  builtin_stream_close(&STACK_0,0);
  builtin_stream_close(&STACK_1,0);
  { /*** access/mod times ***/
    struct utimbuf utb;
    int utime_ret;
    /* first element of the array is access time, second is mod time. set
       both tv_usec to zero since the file system can't gurantee that
       kind of precision anyway. */
    utb.actime  = source_sb.st_atime;
    utb.modtime = source_sb.st_mtime;
    with_string_0(physical_namestring(STACK_0), GLO(pathname_encoding), destz,{
        begin_system_call();
        utime_ret = utime(destz, &utb);
        end_system_call();
      });
    if (utime_ret == -1) {
      pushSTACK(file_stream_truename(STACK_0));
      goto close_and_err;
    }
  }
  return;
# endif
 close_success:
  builtin_stream_close(&STACK_0,0);
  builtin_stream_close(&STACK_1,0);
  return;
 close_and_err:
  builtin_stream_close(&STACK_1,0);
  builtin_stream_close(&STACK_2,0);
  OS_file_error(STACK_0);
}

/* on success, push (source dest byte-count) on retval (an address in STACK)
 can trigger GC */
static void copy_file_low (object source, object dest,
                           bool preserve_p, if_exists_t if_exists,
                           if_does_not_exist_t if_not_exists,
                           gcv_object_t* retval) {
/* (let ((buffer (make-array buffer-size :element-type 'unsigned-byte)))
    (with-open-file (source-stream source :direction :input
                                   :element-type 'unsigned-byte)
     (with-open-file (dest-stream dest :direction (if append-p :append :output)
                                  :element-type 'unsigned-byte)
       (loop for bytes-read = (read-byte-sequence buffer source-stream)
             until (= 0 bytes-read)
             do (write-byte-sequence buffer dest-stream :end bytes-read)))))
*/
  uintL total_count = 0; /* return value: total byte count */
  /* create the two streams */
  pushSTACK(dest);
  /* input: */
  pushSTACK(source);            /* filename */
  pushSTACK(S(Kdirection)); pushSTACK(S(Kinput));
  pushSTACK(S(Kelement_type)); pushSTACK(S(unsigned_byte));
  pushSTACK(S(Kif_does_not_exist));
  pushSTACK(if_does_not_exist_symbol(if_not_exists));
  funcall(L(open),7); source = value1;
  if (nullp(source)) {
    skipSTACK(1); /* drop dest */
    return;
  }
  pushSTACK(STACK_0); STACK_1 = source;
  /* stack layout: 1: source stream; 0: dest path */
  /* output: */
  pushSTACK(S(Kdirection)); pushSTACK(S(Koutput));
  pushSTACK(S(Kelement_type)); pushSTACK(S(unsigned_byte));
  pushSTACK(S(Kif_exists)); pushSTACK(if_exists_symbol(if_exists));
  funcall(L(open),7); dest = value1;
  if (nullp(dest)) {
    builtin_stream_close(&STACK_0,0);
    skipSTACK(1); /* drop source */
    return;
  }
  pushSTACK(dest);
  /* stack layout: 0=output stream; 1=input stream */
  { /* make the bit buffer and copy data */
    uintL bytes_read;
    char buffer[strm_buffered_bufflen];
    /* stack layout: 0 - dest-stream; 1 - source-stream */
    Handle fd_in = stream_lend_handle(&STACK_1,true,NULL);
    Handle fd_ou = stream_lend_handle(&STACK_0,false,NULL);
    while ((bytes_read = fd_read(fd_in,buffer,strm_buffered_bufflen,
                                 persev_full))) {
      total_count += bytes_read;
      fd_write(fd_ou,buffer,bytes_read,persev_full);
    }
  }
  if (!preserve_p) {
    builtin_stream_close(&STACK_0,0);
    builtin_stream_close(&STACK_1,0);
  } else
    copy_attributes_and_close();
  /* clean up the stack */
  pushSTACK(allocate_cons());
  Cdr(STACK_0) = *retval;
  *retval = STACK_0;
  STACK_2 = file_stream_truename(STACK_2); /* source */
  STACK_1 = file_stream_truename(STACK_1); /* dest */
  STACK_0 = UL_to_I(total_count);
  Car(*retval) = listof(3);
}

#line 2858

/* copy just one file: source --> dest (both STRINGs, NIL or PATHNAME)
   can trigger GC */
static void copy_one_file (object source, object src_path,
                           object dest, object dest_path,
                           copy_method_t method, bool preserve_p,
                           if_exists_t if_exists,
                           if_does_not_exist_t if_not_exists,
                           gcv_object_t* retval) {
  pushSTACK(source); pushSTACK(src_path);
  pushSTACK(dest); pushSTACK(dest_path);
  XOUT(source,"copy_one_file");
  XOUT(src_path,"copy_one_file");
  XOUT(dest,"copy_one_file");
  XOUT(dest_path,"copy_one_file");
  /* merge source into dest: "cp foo bar/" --> "cp foo bar/foo" */
  pushSTACK(STACK_2); /* src_path */
  funcall(L(merge_pathnames),2); pushSTACK(value1); /* dest_path */

  if (method == COPY_METHOD_COPY) {
    copy_file_low(STACK_2,STACK_0,preserve_p,if_exists,if_not_exists,retval);
    skipSTACK(4);
    return;
  }

  pushSTACK(STACK_0); funcall(L(probe_file),1);
  if (!nullp(value1)) { /* destination exists; value1 == truename */
    pushSTACK(value1); STACK_2 = dest = value1;
    /* STACK: 0=dest_true; 1=dest_path; 2=dest; 3=src_path; 4=src */
    switch (if_exists) {
      case IF_EXISTS_NIL: skipSTACK(5); return;
      case IF_EXISTS_APPEND:
        /* we know that method != COPY_METHOD_COPY - handled above! */
        pushSTACK(S(Kappend));
        pushSTACK(check_copy_method_reverse(method));
        pushSTACK(O(object_posix__copy_file));
        error(error_condition,GETTEXT("~S: ~S forbids ~S"));
      case IF_EXISTS_OVERWRITE:
      case IF_EXISTS_SUPERSEDE:
      case IF_EXISTS_RENAME_AND_DELETE:
        /* these are the same since (sym)link/rename are atomic */
        break;
      case IF_EXISTS_UNBOUND: case IF_EXISTS_ERROR:
      case IF_EXISTS_RENAME:    /* delegate to OPEN */
        pushSTACK(value1);      /* destination */
        pushSTACK(S(Kif_exists)); pushSTACK(if_exists_symbol(if_exists));
        pushSTACK(S(Kdirection)); pushSTACK(S(Koutput));
        funcall(L(open),5);
        pushSTACK(value1); builtin_stream_close(&STACK_0,0);
        funcall(L(delete_file),1);
        break;
      default: NOTREACHED;
    }
  } else pushSTACK(STACK_0); /* destination does not exist, use dest_path */

  pushSTACK(STACK_3); funcall(L(probe_file),1);
  if (nullp(value1)) { /* source does not exist */
    if (method == COPY_METHOD_RENAME || method == COPY_METHOD_HARDLINK) {
      if (if_not_exists == IF_DOES_NOT_EXIST_NIL) {
        skipSTACK(6); return;
      } else { /* delegate error to OPEN */
        pushSTACK(STACK_3);     /* source */
        pushSTACK(S(Kif_does_not_exist));
        pushSTACK(if_does_not_exist_symbol(if_not_exists));
        pushSTACK(S(Kdirection)); pushSTACK(S(Kinput));
        funcall(L(open),5);
        NOTREACHED;
      }
    }
  } else {
    pushSTACK(value1); funcall(L(truename),1); pushSTACK(value1);
  }

  /* stack layout: 0=src_true; 1=dest_true ... */
  switch (method) {
    case COPY_METHOD_RENAME:
      pushSTACK(STACK_0); pushSTACK(STACK_2); funcall(L(rename_file),2);
      source = STACK_4; dest = STACK_1;
      break;
    case COPY_METHOD_SYMLINK:
#    if defined(HAVE_SYMLINK)
      dest = physical_namestring(STACK_1);
      /* use the original argument, not the truename here,
         so that the user can create relative symlinks */
      source = (stringp(STACK_5) ? (object)STACK_5
                : physical_namestring(STACK_4));
      with_string_0(source, GLO(pathname_encoding), source_asciz, {
        with_string_0(dest, GLO(pathname_encoding), dest_asciz,
                      { symlink_file(source_asciz,dest_asciz); });
      });
      break;
#    endif
      /* FALLTHROUGH if no symlinks */
    case COPY_METHOD_HARDLINK:
#    if defined(HAVE_LINK)
      dest = physical_namestring(STACK_1);
      source = physical_namestring(STACK_0);
      with_string_0(source, GLO(pathname_encoding), source_asciz, {
        with_string_0(dest, GLO(pathname_encoding), dest_asciz,
                      { hardlink_file(source_asciz,dest_asciz); });
      });
      break;
#    endif
      /* FALLTHROUGH if no hardlinks */
    default:
      copy_file_low(STACK_0,STACK_1,preserve_p,if_exists,if_not_exists,retval);
      skipSTACK(6);
      return;
  }
  /* update retval */
  STACK_0 = dest;
  STACK_1 = source;
  STACK_2 = allocate_cons();
  Cdr(STACK_2) = *retval;
  *retval = STACK_2;
  Car(*retval) = listof(2);
  skipSTACK(4);
}

/* (COPY-FILE source target &key method preserve (if-exists :supersede)
              (if-does-not-exist :error))
 source and target are pathname designators (whether or not they
 can be streams is up for debate). if target is missing a name or
 type designator it is taken from source.
 keywords:
 method := :hardlink      ; make a hard link
         | :symlink       ; make a symbolic link
         | :rename        ; move
         | :copy (or nil) ; make a copy
  if the underlying file system does not support a given operation
  a copy is made

 preserve := t ;; preserve as much of source-file's attributes as
               ;; possible
           | nil ;; don't try to preserve source-file's attributes
                 ;; when creating target-file
 for target:
 if-exists := :supersede ;; the existing file is superseded. that is
                         ;; a new file with the same name (and
                         ;; attributes if possible) is created.
            | :error ;; an error of type file-error is signaled.
            | :new-version ;; a new file is created with a larger
                           ;; version number
            | :rename ;; the existing file is renamed to "orig.bak"
            | :append ;; the contents of source-file are appended to
                      ;; the end of target-file
 for source:
 if-does-not-exist := nil ;; do nothing and return nil
                    | :error ;; (default) signal an error
 */
#line 3009
DEFUN(POSIX::COPY-FILE, source target &key METHOD :PRESERVE            :IF-EXISTS :IF-DOES-NOT-EXIST,(subr_posix_copy_file,seclass_default,2,0,norest,key,4,NIL))
{
  if_does_not_exist_t if_not_exists = check_if_does_not_exist(STACK_0);
  if_exists_t if_exists = check_if_exists(STACK_1);
  bool preserve_p = (!nullp(STACK_2) && boundp(STACK_2));
  bool wild_source_p, wild_dest_p;
  copy_method_t method = check_copy_method(STACK_3);
  STACK_1 = NIL; /* return value */
  /* stack: 5 - source; 4 - dest */
  pushSTACK(STACK_5); funcall(L(pathname),1); STACK_3 = value1;
  pushSTACK(STACK_3); funcall(L(wild_pathname_p),1);
  wild_source_p = !nullp(value1);
  pushSTACK(STACK_4); funcall(L(pathname),1); STACK_2 = value1;
  pushSTACK(STACK_2); funcall(L(wild_pathname_p),1);
  wild_dest_p = !nullp(value1);
  XOUT(STACK_3,"POSIX::COPY-FILE -- source");
  XOUT(STACK_2,"POSIX::COPY-FILE -- dest");
  if (wild_source_p) {
    pushSTACK(STACK_3);         /* source pathname */
    pushSTACK(S(Kif_does_not_exist)); pushSTACK(S(Kdiscard));
    funcall(L(directory),3);
    STACK_0 = value1;
    XOUT(STACK_0,"POSIX::COPY-FILE: source list");
    if (wild_dest_p) {
      while (!nullp(STACK_0)) {
        pushSTACK(Car(STACK_0)); /* truename */
        pushSTACK(STACK_(3+1)); /* source */
        pushSTACK(STACK_(2+2)); /* dest */
        funcall(L(translate_pathname),3);
        copy_one_file(NIL,Car(STACK_0),NIL,value1,method,
                      preserve_p,if_exists,if_not_exists,&STACK_1);
        STACK_0 = Cdr(STACK_0);
      }
    } else { /* non-wild dest, must be a directory */
      pushSTACK(STACK_2); funcall(L(probe_directory),1);
      if (nullp(value1)) {      /* dest is a non-exitent dir */
        pushSTACK(STACK_2); funcall(L(make_directory),1);
      }
      while (!nullp(STACK_0)) {
        copy_one_file(NIL,Car(STACK_0),STACK_4,STACK_2,method,
                      preserve_p,if_exists,if_not_exists,&STACK_1);
        STACK_0 = Cdr(STACK_0);
      }
    }
  } else /* non-wild source */
    copy_one_file(STACK_5,STACK_3,STACK_4,STACK_2,method,preserve_p,
                  if_exists,if_not_exists,&STACK_1);
  VALUES1(STACK_1);
  skipSTACK(6);
}

DEFUN(POSIX::DUPLICATE-HANDLE, old &optional new,(subr_posix_duplicate_handle,seclass_default,1,1,norest,nokey,0,NIL))
{ /* Lisp interface to dup(2)/dup2(2). */
  Handle new_handle = (Handle)check_uint_defaulted(popSTACK(),(uintL)-1);
  Handle old_handle = (Handle)I_to_uint(check_uint(popSTACK()));
  begin_system_call();
  if (new_handle == (Handle)(uintL)-1)
    new_handle = handle_dup(old_handle);
  else
    new_handle = handle_dup2(old_handle,new_handle);
  end_system_call();
  VALUES1(fixnum(new_handle));
}

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
#include <shlobj.h>
#line 3079

DEFUN(POSIX::CONVERT-ATTRIBUTES, attributes,(subr_posix_convert_attributes,seclass_default,1,0,norest,nokey,0,NIL))
{ /* convert between symbolic and numeric file attributes */
  if (posfixnump(STACK_0))
    VALUES1(check_file_attributes_to_list
            (I_to_uint32(check_uint32(popSTACK()))));
  else if (listp(STACK_0))
    VALUES1(fixnum(check_file_attributes_from_list(popSTACK())));
  else VALUES1(fixnum(check_file_attributes(popSTACK())));
}
/* convert the 8 members of WIN32_FIND_DATA to the FILE-INFO struct
 can trigger GC */
static Values wfd_to_file_info (WIN32_FIND_DATA *wfd) {
  pushSTACK(check_file_attributes_to_list(wfd->dwFileAttributes));
  pushSTACK(convert_time_to_universal_w32(&(wfd->ftCreationTime)));
  pushSTACK(convert_time_to_universal_w32(&(wfd->ftLastAccessTime)));
  pushSTACK(convert_time_to_universal_w32(&(wfd->ftLastWriteTime)));
  pushSTACK(UL2_to_I(wfd->nFileSizeHigh,wfd->nFileSizeLow));
  pushSTACK(asciz_to_string(wfd->cFileName,GLO(pathname_encoding)));
  pushSTACK(asciz_to_string(wfd->cAlternateFileName,GLO(pathname_encoding)));
  funcall(O(object_posix__make_file_info),7);
}

DEFUN(POSIX::FILE-INFO, file &optional all,(subr_posix_file_info,seclass_default,1,1,norest,nokey,0,NIL))
{
  WIN32_FIND_DATA wfd;
  if (missingp(STACK_0)) {
    find_first_file(STACK_1,&wfd,NULL);
    wfd_to_file_info(&wfd);
  } else {
    HANDLE sh;
    gcv_object_t *phys = &STACK_0;
    unsigned int count = 1;
    find_first_file(STACK_1,&wfd,&sh); *phys = value1; /* physical name */
    wfd_to_file_info(&wfd); pushSTACK(value1);
    while (1) {
      begin_system_call();
      if (!FindNextFile(sh,&wfd)) {
        if (GetLastError() == ERROR_NO_MORE_FILES) break;
        end_system_call();
        OS_file_error(*phys);
      }
      end_system_call();
      wfd_to_file_info(&wfd); pushSTACK(value1); count++;
    }
    begin_system_call(); FindClose(sh); end_system_call();
    VALUES1(listof(count));
  }
  skipSTACK(2);                 /* drop arguments */
}

#line 3131
DEFUN(POSIX::MAKE-SHORTCUT, file &key WORKING-DIRECTORY ARGUMENTS        SHOW-COMMAND ICON DESCRIPTION HOT-KEY PATH,(subr_posix_make_shortcut,seclass_default,1,0,norest,key,7,NIL))
{
  HRESULT hres;
  IShellLink* psl;
  IPersistFile* ppf;
  gcv_object_t *file = &STACK_7;

  /* Get a pointer to the IShellLink interface. */
  begin_system_call();
  hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                          &IID_IShellLink, (LPVOID*)&psl);
  if (!SUCCEEDED(hres)) goto fail_none;
  end_system_call();
  if (!missingp(STACK_0)) {     /* PATH */
    object path = check_string(STACK_0);
    with_string_0(path,GLO(pathname_encoding),pathz, {
      begin_system_call();
      hres = psl->lpVtbl->SetPath(psl,pathz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop PATH */
  if (!missingp(STACK_0)) {     /* HOT-KEY */
    WORD hot_key = 0;
    object hk = STACK_0;
    BYTE *pb = (BYTE*)&hot_key;
   restart_hot_key:
    if (charp(hk)) hot_key = char_int(hk);
    else while (consp(hk)) {
      if (eq(Car(hk),O(object_Kcontrol))) pb[1] |= HOTKEYF_CONTROL;
      else if (eq(Car(hk),O(object_Kalt))) pb[1] |= HOTKEYF_ALT;
      else if (eq(Car(hk),O(object_Kext))) pb[1] |= HOTKEYF_EXT;
      else if (eq(Car(hk),O(object_Kshift))) pb[1] |= HOTKEYF_SHIFT;
      else if (charp(Car(hk))) {
        pb[0] = char_int(hk);
        break;
      } else {
        pushSTACK(NIL);         /* no PLACE */
        pushSTACK(hk);          /* TYPE-ERROR slot DATUM */
        pushSTACK(O(object__28member_20_alt_20_control_20_ext_20_shift_29)); /* EXPECTED-TYPE */
        pushSTACK(STACK_0); pushSTACK(hk); pushSTACK(TheSubr(subr_self)->name);
        check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
        hk = value1;
        goto restart_hot_key;
      }
      hk = Cdr(hk);
    }
    if (pb[0] == 0) {           /* STACK_0 is the HOT-KEY arg */
      pushSTACK(TheSubr(subr_self)->name);
      error(error_condition,GETTEXT("~S: invalid hotkey spec ~S"));
    }
    begin_system_call();
    hres = psl->lpVtbl->SetHotkey(psl,hot_key);
    if (!SUCCEEDED(hres)) goto fail_psl;
    end_system_call();
  }
  skipSTACK(1);                 /* drop HOT-KEY */
  if (!missingp(STACK_0)) {     /* DESCRIPTION */
    object desc = check_string(STACK_0);
    with_string_0(desc,GLO(pathname_encoding),descz, {
      begin_system_call();
      hres = psl->lpVtbl->SetDescription(psl,descz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop DESCRIPTION */
  if (!missingp(STACK_0)) {     /* ICON */
    object icon_name;
    int icon_idx = 0;
    if (consp(STACK_0)) {       /* (file . index) or (file index) */
      icon_name = check_string(Car(STACK_0));
      icon_idx = I_to_uint32(check_uint32(consp(Cdr(STACK_0))
                                          ? Car(Cdr(STACK_0))
                                          : Cdr(STACK_0)));
    } else icon_name = check_string(STACK_0);
    with_string_0(icon_name,GLO(pathname_encoding),iconz, {
      begin_system_call();
      hres = psl->lpVtbl->SetIconLocation(psl,iconz,icon_idx);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop ICON */
  if (!missingp(STACK_0)) {     /* SHOW-COMMAND */
    object sc = STACK_0;
    int sci;
   restart_show_command:
    if (eq(sc,S(Knormal))) sci = SW_SHOWNORMAL;
    else if (eq(sc,O(object_Kmax))) sci = SW_SHOWMAXIMIZED;
    else if (eq(sc,O(object_Kmin))) sci = SW_SHOWMINIMIZED;
    else {
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(sc);            /* TYPE-ERROR slot DATUM */
      pushSTACK(O(object__28member_20_normal_20_max_20_min_29)); /* EXPECTED-TYPE */
      pushSTACK(STACK_0); pushSTACK(sc); pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
      sc = value1;
      goto restart_show_command;
    }
    begin_system_call();
    hres = psl->lpVtbl->SetShowCmd(psl,sci);
    if (!SUCCEEDED(hres)) goto fail_psl;
    end_system_call();
  }
  skipSTACK(1);                 /* drop SHOW-COMMAND */
  if (!missingp(STACK_0)) {     /* ARGUMENTS */
    object args = check_string(STACK_0);
    with_string_0(args,GLO(pathname_encoding),argz, {
      begin_system_call();
      hres = psl->lpVtbl->SetArguments(psl,argz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop ARGUMENTS */
  if (!missingp(STACK_0)) {     /* WORKING-DIRECTORY */
    object wd = check_string(STACK_0);
    with_string_0(wd,GLO(pathname_encoding),wdz, {
      begin_system_call();
      hres = psl->lpVtbl->SetWorkingDirectory(psl,wdz);
      if (!SUCCEEDED(hres)) goto fail_psl;
      end_system_call();
    });
  }
  skipSTACK(1);                 /* drop WORKING-DIRECTORY */
  STACK_0 = physical_namestring(STACK_0); /* pathname */

  begin_system_call();
  hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile,(LPVOID*)&ppf);
  if (!SUCCEEDED(hres)) goto fail_psl;
  { /* Ensure that the string is Unicode & Save the shortcut. */
    WCHAR wsz[MAX_PATH];
    with_string_0(*file, GLO(pathname_encoding), pathz, {
      MultiByteToWideChar(CP_ACP, 0, pathz, -1, wsz, MAX_PATH);
      hres = ppf->lpVtbl->Save(ppf, wsz, TRUE);
      if (!SUCCEEDED(hres)) goto fail_ppf;
    });
  }
  ppf->lpVtbl->Release(ppf);
  psl->lpVtbl->Release(psl);
  end_system_call();
  VALUES1(popSTACK()); return;
 fail_ppf: ppf->lpVtbl->Release(ppf);
 fail_psl: psl->lpVtbl->Release(psl);
 fail_none: end_system_call(); OS_file_error(*file);
}

DEFUN(POSIX::SHORTCUT-INFO, file,(subr_posix_shortcut_info,seclass_default,1,0,norest,nokey,0,NIL))
{
  HRESULT hres;
  IShellLink* psl;
  char path[MAX_PATH], wd[MAX_PATH], args[MAX_PATH],
    icon[MAX_PATH], desc[MAX_PATH];
  WIN32_FIND_DATA wfd;
  IPersistFile* ppf;
  gcv_object_t *file = &STACK_0;
  int icon_idx, show_cmd;
  WORD hot_key;

  STACK_0 = physical_namestring(STACK_0);

  /* Get a pointer to the IShellLink interface. */
  begin_system_call();
  hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                          &IID_IShellLink, (LPVOID*)&psl);
  if (!SUCCEEDED(hres)) goto fail_none;
  /* Get a pointer to the IPersistFile interface. */
  hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile,(LPVOID*)&ppf);
  if (!SUCCEEDED(hres)) goto fail_psl;
  { /* Ensure that the string is Unicode & Load the shortcut. */
    WCHAR wsz[MAX_PATH];
    with_string_0(STACK_0, GLO(pathname_encoding), pathz, {
      MultiByteToWideChar(CP_ACP, 0, pathz, -1, wsz, MAX_PATH);
      hres = ppf->lpVtbl->Load(ppf, wsz, STGM_READ);
      if (!SUCCEEDED(hres)) goto fail_ppf;
    });
  }
  /* Resolve the link. */
  hres = psl->lpVtbl->Resolve(psl,NULL,0);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 1 path, 2 file info */
  hres = psl->lpVtbl->GetPath(psl,path, MAX_PATH, &wfd, 4/*SLGP_RAWPATH*/);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 3 working directory */
  hres = psl->lpVtbl->GetWorkingDirectory(psl,wd, MAX_PATH);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 4 arguments */
  hres = psl->lpVtbl->GetArguments(psl,args, MAX_PATH);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 5 show command */
  hres = psl->lpVtbl->GetShowCmd(psl,&show_cmd);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 6 icon */
  hres = psl->lpVtbl->GetIconLocation(psl,icon, MAX_PATH, &icon_idx);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 7 description */
  hres = psl->lpVtbl->GetDescription(psl,desc, MAX_PATH);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  /* 8 hot key */
  hres = psl->lpVtbl->GetHotkey(psl,&hot_key);
  if (!SUCCEEDED(hres)) goto fail_ppf;
  ppf->lpVtbl->Release(ppf);
  psl->lpVtbl->Release(psl);
  end_system_call();
  pushSTACK(asciz_to_string(path,GLO(pathname_encoding))); /* 1 */
  wfd_to_file_info(&wfd); pushSTACK(value1);               /* 2 */
  pushSTACK(asciz_to_string(wd,GLO(pathname_encoding)));   /* 3 */
  pushSTACK(asciz_to_string(args,GLO(pathname_encoding))); /* 4 */
  switch (show_cmd) {                                   /* 5 */
    case SW_SHOWNORMAL: pushSTACK(S(Knormal)); break;
    case SW_SHOWMAXIMIZED: pushSTACK(O(object_Kmax)); break;
    case SW_SHOWMINIMIZED: pushSTACK(O(object_Kmin)); break;
    default: NOTREACHED;
  }
  pushSTACK(asciz_to_string(icon,GLO(pathname_encoding)));
  pushSTACK(fixnum(icon_idx));
  { object tmp = listof(2); pushSTACK(tmp); }           /* 6 */
  pushSTACK(asciz_to_string(desc,GLO(pathname_encoding))); /* 7 */
  { int count=0;                                        /* 8 */
    BYTE *pb = (BYTE*)&hot_key;
    if (pb[1] & HOTKEYF_ALT) { pushSTACK(O(object_Kalt)); count++; }
    if (pb[1] & HOTKEYF_CONTROL) { pushSTACK(O(object_Kcontrol)); count++; }
    if (pb[1] & HOTKEYF_EXT) { pushSTACK(O(object_Kext)); count++; }
    if (pb[1] & HOTKEYF_SHIFT) { pushSTACK(O(object_Kshift)); count++; }
    pushSTACK(int_char(pb[0]));
    if (count) { object tmp = listof(count+1); pushSTACK(tmp); }
  }
  funcall(O(object_posix__make_shortcut_info),9);
  return;
 fail_ppf: ppf->lpVtbl->Release(ppf);
 fail_psl: psl->lpVtbl->Release(psl);
 fail_none: end_system_call(); OS_file_error(*file);
}

DEFUN(POSIX::SYSTEM-INFO,,(subr_posix_system_info,seclass_default,0,0,norest,nokey,0,NIL))
{ /* interface to GetSystemInfo() */
  SYSTEM_INFO si;
  begin_system_call();
  GetSystemInfo(&si);
  end_system_call();
  switch (si.wProcessorArchitecture) {
    case PROCESSOR_ARCHITECTURE_UNKNOWN: pushSTACK(O(object_Kunknown)); break;
    case PROCESSOR_ARCHITECTURE_INTEL:   pushSTACK(O(object_Kintel)); break;
    case PROCESSOR_ARCHITECTURE_MIPS:    pushSTACK(O(object_Kmips)); break;
    case PROCESSOR_ARCHITECTURE_ALPHA:   pushSTACK(O(object_Kalpha)); break;
    case PROCESSOR_ARCHITECTURE_PPC:     pushSTACK(O(object_Kppc)); break;
    case PROCESSOR_ARCHITECTURE_IA64 :   pushSTACK(O(object_Kia64)); break;
    default: pushSTACK(UL_to_I(si.wProcessorArchitecture));
  }
  pushSTACK(UL_to_I(si.dwPageSize));
  pushSTACK(UL_to_I((DWORD)si.lpMinimumApplicationAddress));
  pushSTACK(UL_to_I((DWORD)si.lpMaximumApplicationAddress));
  pushSTACK(UL_to_I(si.dwActiveProcessorMask));
  pushSTACK(UL_to_I(si.dwNumberOfProcessors));
  pushSTACK(UL_to_I(si.dwAllocationGranularity));
  pushSTACK(fixnum(si.wProcessorLevel));
  pushSTACK(fixnum(si.wProcessorRevision));
  funcall(O(object_posix__make_system_info),9);
}

DEFUN(POSIX::VERSION,,(subr_posix_version,seclass_default,0,0,norest,nokey,0,NIL))
{ /* interface to GetVersionEx() */
  OSVERSIONINFOEX vi;
  vi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  begin_system_call();
  if (!GetVersionEx((OSVERSIONINFO*)&vi)) OS_error();
  end_system_call();

  pushSTACK(UL_to_I(vi.dwMajorVersion));
  pushSTACK(UL_to_I(vi.dwMinorVersion));
  pushSTACK(UL_to_I(vi.dwBuildNumber));
  switch (vi.dwPlatformId) {
    case VER_PLATFORM_WIN32s:        pushSTACK(O(object_Ks)); break;
    case VER_PLATFORM_WIN32_WINDOWS: pushSTACK(O(object_Kwindows)); break;
    case VER_PLATFORM_WIN32_NT:      pushSTACK(O(object_Knt)); break;
    default: pushSTACK(UL_to_I(vi.dwPlatformId));
  }
  pushSTACK(safe_to_string(vi.szCSDVersion));
  pushSTACK(UL_to_I(vi.wServicePackMajor));
  pushSTACK(UL_to_I(vi.wServicePackMinor));
  { /* wSuiteMask */
    object suites = NIL;
    unsigned int count = 0;
    if (vi.wSuiteMask & VER_SUITE_BACKOFFICE)
      { pushSTACK(O(object_Kbackoffice)); count++; }
    if (vi.wSuiteMask & VER_SUITE_DATACENTER)
      { pushSTACK(O(object_Kdatacenter)); count++; }
    if (vi.wSuiteMask & VER_SUITE_ENTERPRISE)
      { pushSTACK(O(object_Kenterprise)); count++; }
    if (vi.wSuiteMask & VER_SUITE_SMALLBUSINESS)
      { pushSTACK(O(object_Ksmallbusiness)); count++; }
    if (vi.wSuiteMask & VER_SUITE_SMALLBUSINESS_RESTRICTED)
      { pushSTACK(O(object_Ksmallbusiness_restricted)); count++; }
    if (vi.wSuiteMask & VER_SUITE_TERMINAL)
      { pushSTACK(O(object_Kterminal)); count++; }
    if (vi.wSuiteMask & VER_SUITE_PERSONAL)
      { pushSTACK(O(object_Kpersonal)); count++; }
    if (count) suites = listof(count);
    pushSTACK(suites);
  }
  switch (vi.wProductType) {
    case VER_NT_WORKSTATION:       pushSTACK(O(object_Kworkstation)); break;
    case VER_NT_DOMAIN_CONTROLLER: pushSTACK(O(object_Kdomain_controller)); break;
    case VER_NT_SERVER:            pushSTACK(O(object_Kserver)); break;
    default: pushSTACK(UL_to_I(vi.wProductType));
  }
  funcall(O(object_posix__make_version),9);
}

DEFUN(POSIX::MEMORY-STATUS,,(subr_posix_memory_status,seclass_default,0,0,norest,nokey,0,NIL))
{ /* interface to GlobalMemoryStatus() */
#ifdef HAVE_GLOBALMEMORYSTATUSEX
  MEMORYSTATUSEX ms;
  ms.dwLength = sizeof(MEMORYSTATUSEX);
  begin_system_call();
  if (!GlobalMemoryStatusEx(&ms)) OS_error();
  end_system_call();
  pushSTACK(UQ_to_I(ms.ullTotalPhys));
  pushSTACK(UQ_to_I(ms.ullAvailPhys));
  pushSTACK(UQ_to_I(ms.ullTotalPageFile));
  pushSTACK(UQ_to_I(ms.ullAvailPageFile));
  pushSTACK(UQ_to_I(ms.ullTotalVirtual));
  pushSTACK(UQ_to_I(ms.ullAvailVirtual));
#else
  MEMORYSTATUS ms;
  ms.dwLength = sizeof(MEMORYSTATUS);
  begin_system_call(); GlobalMemoryStatus(&ms); end_system_call();
  pushSTACK(UL_to_I(ms.dwTotalPhys));
  pushSTACK(UL_to_I(ms.dwAvailPhys));
  pushSTACK(UL_to_I(ms.dwTotalPageFile));
  pushSTACK(UL_to_I(ms.dwAvailPageFile));
  pushSTACK(UL_to_I(ms.dwTotalVirtual));
  pushSTACK(UL_to_I(ms.dwAvailVirtual));
#endif
  funcall(O(object_posix__mkmemstat),6);
}

/* FILE-PROPERTIES */

#ifndef PIDSI_TITLE
#define PIDSI_TITLE               0x00000002L
#define PIDSI_SUBJECT             0x00000003L
#define PIDSI_AUTHOR              0x00000004L
#define PIDSI_KEYWORDS            0x00000005L
#define PIDSI_COMMENTS            0x00000006L
#define PIDSI_TEMPLATE            0x00000007L
#define PIDSI_LASTAUTHOR          0x00000008L
#define PIDSI_REVNUMBER           0x00000009L
#define PIDSI_EDITTIME            0x0000000aL
#define PIDSI_LASTPRINTED         0x0000000bL
#define PIDSI_CREATE_DTM          0x0000000cL
#define PIDSI_LASTSAVE_DTM        0x0000000dL
#define PIDSI_PAGECOUNT           0x0000000eL
#define PIDSI_WORDCOUNT           0x0000000fL
#define PIDSI_CHARCOUNT           0x00000010L
#define PIDSI_THUMBNAIL           0x00000011L
#define PIDSI_APPNAME             0x00000012L
#define PIDSI_DOC_SECURITY        0x00000013L
#define PRSPEC_LPWSTR	( 0 )
#define PRSPEC_PROPID	( 1 )
#define STG_E_PROPSETMISMATCHED   0x800300F0L
#endif

/* Pushes corresponding value to STACK */
static int PropVariantToLisp (PROPVARIANT *pvar) {
  if(pvar->vt & VT_ARRAY) {
    pushSTACK(S(Karray));
    return 1;
  }
  if(pvar->vt & VT_BYREF) {
    pushSTACK(O(object_Kbyref));
    return 1;
  }
  switch(pvar->vt) {
    case VT_EMPTY: pushSTACK(O(object_Kempty)); break;
    case VT_NULL:  pushSTACK(O(object_Knull));  break;
    case VT_BLOB:  pushSTACK(O(object_Kblob));  break;
    case VT_BOOL:  pushSTACK(pvar->boolVal ? T : NIL); break;
    case VT_I1:    pushSTACK(sfixnum(pvar->cVal)); break;
    case VT_UI1:   pushSTACK(fixnum(pvar->bVal)); break;
    case VT_I2:    pushSTACK(sfixnum(pvar->iVal)); break;
    case VT_UI2:   pushSTACK(fixnum(pvar->uiVal)); break;
    case VT_I4:
    case VT_INT:   pushSTACK(L_to_I(pvar->lVal)); break;
    case VT_UI4:
    case VT_UINT:  pushSTACK(UL_to_I(pvar->ulVal)); break;
    case VT_ERROR: pushSTACK(UL_to_I(pvar->scode)); break;
    case VT_I8:    pushSTACK(sint64_to_I(*((sint64 *)&pvar->hVal))); break;
    case VT_CY: {
      double dbl = (*((sint64 *)&pvar->cyVal))/10000.0;
      pushSTACK(c_double_to_DF((dfloatjanus *)&dbl));
    } break;
    case VT_UI8: pushSTACK(uint64_to_I(*((uint64 *)&pvar->uhVal)));  break;
    case VT_R4:  pushSTACK(c_float_to_FF((ffloatjanus *)&pvar->fltVal)); break;
    case VT_R8:  pushSTACK(c_double_to_DF((dfloatjanus *)&pvar->dblVal));break;
    case VT_DATE:pushSTACK(c_double_to_DF((dfloatjanus *)&pvar->date)); break;
    case VT_BSTR:
      pushSTACK(n_char_to_string((const char *)pvar->bstrVal,
                                 *((DWORD *)(((const char *)pvar->bstrVal)-4)),
                                 Symbol_value(S(unicode_16_little_endian))));
      break;
    case VT_LPSTR:
      pushSTACK(safe_to_string(pvar->pszVal));
      break;
    case VT_LPWSTR:
      pushSTACK(n_char_to_string((const char *)pvar->pwszVal,
                                 wcslen(pvar->pwszVal)*2,
                                 Symbol_value(S(unicode_16_little_endian))));
      break;
    case VT_FILETIME:
      pushSTACK(convert_time_to_universal_w32(&(pvar->filetime))); break;
    case VT_CF: pushSTACK(O(object_Kclipboard_format)); break;
    default:    pushSTACK(O(object_Knotimplemented)); break;
  }
  return 1;
}
/* popSTACK -> pvar  */
static int LispToPropVariant (PROPVARIANT * pvar) {
  int rv = 0;int sfp = 0;
  VARTYPE typehint = VT_EMPTY;
  if (consp(STACK_0)) {
    /* (KW VALUE) OR (KW NIL) ? */
    if (!nullp(Cdr(STACK_0)) && !nullp(Car(STACK_0))
        && consp(Cdr(STACK_0)) && nullp(Cdr(Cdr(STACK_0)))
        && symbolp(Car(STACK_0))) {
           if (eq(Car(STACK_0),O(object_Ki1))) typehint = VT_I1;
      else if (eq(Car(STACK_0),O(object_Kui1))) typehint = VT_UI1;
      else if (eq(Car(STACK_0),O(object_Ki2))) typehint = VT_I2;
      else if (eq(Car(STACK_0),O(object_Kui2))) typehint = VT_UI2;
      else if (eq(Car(STACK_0),O(object_Ki4))) typehint = VT_I4;
      else if (eq(Car(STACK_0),O(object_Kint))) typehint = VT_INT;
      else if (eq(Car(STACK_0),O(object_Kui4))) typehint = VT_UI4;
      else if (eq(Car(STACK_0),O(object_Kuint))) typehint = VT_UINT;
      else if (eq(Car(STACK_0),O(object_Ki8))) typehint = VT_I8;
      else if (eq(Car(STACK_0),O(object_Kui8))) typehint = VT_UI8;
      else if (eq(Car(STACK_0),O(object_Kr4))) typehint = VT_R4;
      else if (eq(Car(STACK_0),O(object_Kr8))) typehint = VT_R8;
      else if (eq(Car(STACK_0),O(object_Kcy))) typehint = VT_CY;
      else if (eq(Car(STACK_0),O(object_Kdate))) typehint = VT_DATE;
      else if (eq(Car(STACK_0),O(object_Kbstr))) typehint = VT_BSTR;
      else if (eq(Car(STACK_0),O(object_Kbool))) typehint = VT_BOOL;
      else if (eq(Car(STACK_0),O(object_Kerror))) typehint = VT_ERROR;
      else if (eq(Car(STACK_0),O(object_Kfiletime))) typehint = VT_FILETIME;
      else if (eq(Car(STACK_0),O(object_Klpstr))) typehint = VT_LPSTR;
      else if (eq(Car(STACK_0),O(object_Klpwstr))) typehint = VT_LPWSTR;
      else { skipSTACK(1); return 0; }
      STACK_0 = Car(Cdr(STACK_0)); /* VALUE */
    } else { skipSTACK(1); return 0; }
  }
  if (stringp(STACK_0)
      && (typehint == VT_EMPTY || typehint == VT_BSTR
          || typehint == VT_LPSTR || typehint == VT_LPWSTR)) {
    if (typehint == VT_EMPTY) {
#    define STG_STRINGS_NONUNICODE
#    ifdef STG_STRINGS_UNICODE
      typehint = VT_LPWSTR;
#    else
      typehint = VT_LPSTR;
#    endif
    }
    do {
      uintL str_len;
      uintL str_offset;
      object str_string = unpack_string_ro(STACK_0,&str_len,&str_offset);
      const chart* ptr1;
      unpack_sstring_alloca(str_string,str_len,str_offset, ptr1=);
      if (typehint == VT_LPWSTR || typehint == VT_BSTR) {
        uintL str_bytelen =
          cslen(Symbol_value(S(unicode_16_little_endian)),ptr1,str_len);
        LPWSTR str = SysAllocStringByteLen(NULL,str_bytelen+4);
        if (typehint == VT_BSTR) {
          /* it's ok, SysAllocStringByteLen returns pointer after DWORD */
          *(((DWORD *)str)-1) = (DWORD)str_bytelen;
        }
        cstombs(Symbol_value(S(unicode_16_little_endian)),ptr1,str_len,
                (uintB *)str,str_bytelen);
        ((uintB *)str)[str_bytelen] = '\0';
        ((uintB *)str)[str_bytelen+1] = '\0';
        pvar->pwszVal = str;
        pvar->vt = typehint;
      } else { /* Win XP explorer seems to create ANSI strings. So do we. */
        uintL str_bytelen = cslen(GLO(misc_encoding),ptr1,str_len);
        char * str = (char *) SysAllocStringByteLen(NULL, str_bytelen+2);
        cstombs(GLO(misc_encoding),ptr1,str_len,(uintB *)str,str_bytelen);
        str[str_bytelen] = '\0';
        pvar->pszVal = str;
        pvar->vt = VT_LPSTR;
      }
      rv = 1;
    } while(0);
  } else if (integerp(STACK_0)) {
    if (typehint == VT_EMPTY) typehint = VT_FILETIME; /* assume FILETIME */
    if (typehint == VT_FILETIME) {
      pvar->vt = VT_FILETIME; rv = 1;
      convert_time_from_universal_w32(STACK_0,&(pvar->filetime));
    } else if (typehint == VT_I1) {
      pvar->vt = typehint; pvar->cVal = I_to_sint8(STACK_0); rv = 1;
    } else if (typehint == VT_UI1) {
      pvar->vt = typehint; pvar->bVal = I_to_uint8(STACK_0); rv = 1;
    } else if (typehint == VT_I2) {
      pvar->vt = typehint; pvar->iVal = I_to_sint16(STACK_0); rv = 1;
    } else if (typehint == VT_UI2) {
      pvar->vt = typehint; pvar->uiVal = I_to_uint16(STACK_0); rv = 1;
    } else if (typehint == VT_I4 || typehint == VT_INT) { /* VT_I4 != VT_INT */
      pvar->vt = typehint; pvar->lVal = I_to_sint32(STACK_0); rv = 1;
    } else if (typehint == VT_UI4 || typehint == VT_UINT) {
      pvar->vt = typehint; pvar->ulVal = I_to_uint32(STACK_0); rv = 1;
    } else if (typehint == VT_ERROR) {
      pvar->vt = typehint; pvar->scode = I_to_uint32(STACK_0); rv = 1;
    } else if (typehint == VT_I8) {
      pvar->vt = typehint;
      *((sint64 *)&pvar->hVal) = I_to_sint64(STACK_0);rv = 1;
    } else if (typehint == VT_UI8) {
      pvar->vt = typehint;
      *((uint64 *)&pvar->uhVal) = I_to_uint64(STACK_0);rv = 1;
    } else if (typehint == VT_CY) {
      sint64 i64 = I_to_uint64(STACK_0);
      pvar->vt = typehint;
      *((uint64 *)&pvar->cyVal) = i64*10000;rv = 1;
    }
  } else if ((sfp = single_float_p(STACK_0)) || double_float_p(STACK_0)) {
    if (typehint == VT_EMPTY) typehint = (sfp?VT_R4:VT_R8);
    if (typehint == VT_R4) {
      if (sfp) {
        pvar->vt = VT_R4;
        pvar->fltVal = 0;
        FF_to_c_float(STACK_0,(ffloatjanus *)&pvar->fltVal);
        rv = 1;
      }
    } else if (typehint == VT_R8) {
      pvar->vt = VT_R8;
      if (sfp) {
        float v = 0;
        FF_to_c_float(STACK_0,(ffloatjanus *)&v);
        pvar->dblVal = v;
      } else {
        pvar->dblVal = 0; /* DF_to_c_double takes only clean doubles */
        DF_to_c_double(STACK_0,(dfloatjanus *)&pvar->dblVal);
      }
      rv = 1;
    } else if (typehint == VT_DATE && double_float_p(STACK_0)) {
      /* A 64-bit floating point number representing the number of days
         (not seconds) since December 31, 1899. For example, January 1, 1900,
         is 2.0, January 2, 1900, is 3.0, and so on). This is stored in the
         same representation as VT_R8. */
      pvar->vt = VT_DATE;
      pvar->date = 0;
      DF_to_c_double(STACK_0,(dfloatjanus *)&pvar->date);
      rv = 1;
    } else if (typehint == VT_CY) {
      double dbl = 0; float v = 0;
      pvar->vt = typehint;
      if (sfp) {
        FF_to_c_float(STACK_0,(ffloatjanus *)&v);
        dbl = v;
      } else {
        DF_to_c_double(STACK_0,(dfloatjanus *)&dbl);
      }
      *((uint64 *)&pvar->cyVal) = (uint64) (dbl*10000 + 0.5);rv = 1;
    }
  } else if (symbolp(STACK_0)) {
    if (typehint == VT_EMPTY && eq(STACK_0,O(object_Kempty))) {
      pvar->vt = VT_EMPTY; rv = 1; } else
    if (typehint == VT_EMPTY && eq(STACK_0,O(object_Knull))) {
      pvar->vt = VT_NULL;  rv = 1; } else
    if (typehint == VT_BOOL && eq(STACK_0,NIL)) {
      pvar->vt = VT_BOOL; pvar->boolVal = FALSE;  rv = 1; } else
    if (typehint == VT_BOOL && eq(STACK_0,T)) {
      pvar->vt = VT_BOOL; pvar->boolVal = TRUE;  rv = 1; }
  }
  skipSTACK(1);
  return rv;
}

WINOLEAPI PropVariantClear(PROPVARIANT* pvar);

static PROPID kwtopropid (object kw) {
  if (eq(kw,O(object_Kcodepage))) return 1 /* PID_CODEPAGE */;
  if (eq(kw,O(object_Klocale))) return 0x80000000 /* PID_LOCALE */;
  if (eq(kw,O(object_Ktitle))) return PIDSI_TITLE;
  if (eq(kw,O(object_Ksubject))) return PIDSI_SUBJECT;
  if (eq(kw,O(object_Kauthor))) return PIDSI_AUTHOR;
  if (eq(kw,O(object_Kkeywords))) return PIDSI_KEYWORDS;
  if (eq(kw,O(object_Kcomments))) return PIDSI_COMMENTS;
  if (eq(kw,O(object_Ktemplate))) return PIDSI_TEMPLATE;
  if (eq(kw,O(object_Klastauthor))) return PIDSI_LASTAUTHOR;
  if (eq(kw,O(object_Krevnumber))) return PIDSI_REVNUMBER;
  if (eq(kw,O(object_Kedittime))) return PIDSI_EDITTIME;
  if (eq(kw,O(object_Klastprinted))) return PIDSI_LASTPRINTED;
  if (eq(kw,O(object_Kcreate_dtm))) return PIDSI_CREATE_DTM;
  if (eq(kw,O(object_Klastsave_dtm))) return PIDSI_LASTSAVE_DTM;
  if (eq(kw,O(object_Kpagecount))) return PIDSI_PAGECOUNT;
  if (eq(kw,O(object_Kwordcount))) return PIDSI_WORDCOUNT;
  if (eq(kw,O(object_Kcharcount))) return PIDSI_CHARCOUNT;
  if (eq(kw,O(object_Kthumbnail))) return PIDSI_THUMBNAIL;
  if (eq(kw,O(object_Kappname))) return PIDSI_APPNAME;
  if (eq(kw,O(object_Kdoc_security))) return PIDSI_DOC_SECURITY;
  return (PROPID)-1;
}

/* string -> PROPSPEC */
static void PropSpecSetStr (object str, PROPSPEC * pspec) {
  pspec->ulKind = PRSPEC_LPWSTR;
  { uintL str_len;
    uintL str_offset;
    object str_string = unpack_string_ro(str,&str_len,&str_offset);
    const chart* ptr1;
    unpack_sstring_alloca(str_string,str_len,str_offset, ptr1=);
    { uintL str_bytelen =
        cslen(Symbol_value(S(unicode_16_little_endian)),ptr1,str_len);
      pspec->lpwstr = (LPOLESTR)my_malloc(str_bytelen+2);
      begin_system_call();
      cstombs(Symbol_value(S(unicode_16_little_endian)),ptr1,str_len,
              (uintB *)pspec->lpwstr,str_bytelen);
      end_system_call();
      ((uintB *)pspec->lpwstr)[str_bytelen] = '\0';
      ((uintB *)pspec->lpwstr)[str_bytelen+1] = '\0';
    }
  }
}

/* list (ID STRING) -> PROPSPEC(ID), PROPSPEC(STR)
   STACK may don't match the pattern (then function returns false)
   any of pspec1, pspec2 can be NULL */
static int propspeclistp (object arg, PROPSPEC * pspec1,PROPSPEC * pspec2) {
  /* check if it is (INT STRING) */
  if (consp(arg) && !nullp(Cdr(arg)) && !nullp(Car(arg))
      && consp(Cdr(arg)) && nullp(Cdr(Cdr(arg)))
      && !nullp(Car(Cdr(arg)))
      && (integerp(Car(arg)) || symbolp(Car(arg)))
      && stringp(Car(Cdr(arg)))) {
    /* set pspec1 to ID and pspec2 to STRING */
    if (pspec1) {
      pspec1->ulKind = PRSPEC_PROPID;
      if (integerp(Car(arg)))
        pspec1->propid = I_to_UL(Car(arg));
      else {
        pspec1->propid = kwtopropid(Car(arg));
        if (pspec1->propid == (PROPID) -1)
          return 0;
      }
    }
    if (pspec2)
      PropSpecSetStr(Car(Cdr(arg)),pspec2);
    return 1;
  }
  return 0;
}

/* (keyword, int, list (ID STRING) or string) -> PROPSPEC
   uses malloc to allocate memory for string specifiers
     (when ulKind == PRSPEC_LPWSTR)
   pspec2 can be NULL */
static int PropSpecSet (object arg, PROPSPEC * pspec1, PROPSPEC * pspec2) {
  ZeroMemory(pspec1, sizeof(PROPSPEC));
  if (pspec2) ZeroMemory(pspec2, sizeof(PROPSPEC));
  if (symbolp(arg)) {
    pspec1->ulKind = PRSPEC_PROPID;
    pspec1->propid = kwtopropid(arg);
    if (pspec1->propid == (PROPID) -1) return 0;
    return 1;
  } else if (stringp(arg)) {
    PropSpecSetStr(arg,pspec1);
    return 1;
  } else if (integerp(arg)) {
    pspec1->ulKind = PRSPEC_PROPID;
    pspec1->propid = I_to_UL(arg);
    return 1;
  } else if (propspeclistp(arg,pspec1,pspec2)) return 2;
  return 0;
}

static const char * DecodeHRESULT (HRESULT hres) {
  static char buf[128];
#define msgcase(x) case x: return #x; break;
  switch (hres) {
  msgcase(E_UNEXPECTED)
  msgcase(STG_E_FILENOTFOUND)
  msgcase(STG_E_ACCESSDENIED)
  msgcase(STG_E_INSUFFICIENTMEMORY)
  msgcase(STG_E_INVALIDFUNCTION)
  msgcase(STG_E_REVERTED)
  msgcase(STG_E_INVALIDPARAMETER)
  msgcase(STG_E_INVALIDNAME)
  msgcase(S_FALSE)
  msgcase(STG_E_INVALIDPOINTER)
  msgcase(HRESULT_FROM_WIN32(ERROR_NO_UNICODE_TRANSLATION))
  msgcase(HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED))
  msgcase(STG_E_WRITEFAULT)
  msgcase(STG_E_MEDIUMFULL)
  msgcase(STG_E_PROPSETMISMATCHED)
  }
#undef msgcase
  sprintf(buf,"0x%x",hres);
  return buf;
}

#line 3847
#define with_string_0w(string,wcvar,statement)    do { uintL wcvar##_len;                       uintL wcvar##_offset;                       object wcvar##_string = unpack_string_ro(string,&wcvar##_len,&wcvar##_offset);      const chart* ptr1;                          unpack_sstring_alloca(wcvar##_string,wcvar##_len,wcvar##_offset, ptr1=);     {uintL wcvar##_bytelen =                      cslen(Symbol_value(S(unicode_16_little_endian)),ptr1,wcvar##_len);      DYNAMIC_ARRAY(wcvar##_data,uintB,wcvar##_bytelen+2);      cstombs(Symbol_value(S(unicode_16_little_endian)),ptr1,wcvar##_len,             &wcvar##_data[0],wcvar##_bytelen);      wcvar##_data[wcvar##_bytelen] = '\0';                 wcvar##_data[wcvar##_bytelen+1] = '\0';              {WCHAR* wcvar = (WCHAR*) &wcvar##_data[0];        statement                                            }                                                     FREE_DYNAMIC_ARRAY(wcvar##_data);                   }} while(0)

/* there's no PropVariantInit in my cygwin headers */
#line 3851
#define MyPropVariantInit(ppv)     begin_system_call();    ZeroMemory(ppv,sizeof(PROPVARIANT));end_system_call()

/* (OS::FILE-PROPERTIES filename set [specifier value|:INITID init-id]*)
     Wrapper for Win32 IPropertyStorage functionality
     filename - a compound file name or (on NTFS) name of any file
     set      - :BUILT-IN or :USER-DEFINED property set
     specifier - property specifier: integer, keyword, string or
       list of integer or keyword and string.
       Integer specifier - a property identifier
       Keyword:  :CODEPAGE, :LOCALE,   :TITLE, :SUBJECT, :AUTHOR,
                 :KEYWORDS, :COMMENTS, :TEMPLATE, :LASTAUTHOR,
                 :REVNUMBER, :EDITTIME, :LASTPRINTED,:CREATE-DTM,
                 :LASTSAVE-DTM, :PAGECOUNT, :WORDCOUNT, :CHARCOUNT,
                 :THUMBNAIL, :APPNAME, :DOC-SECURITY - predefined IDs.
       String: string property specifier. If no match is found, first
         ID >= init-id (which defaults to 2) is associated with the
         string and it's value is replaced with new value.
       (int|keyword string) - first element is used as specifier,
         string is associated with this ID.
     value - new value of the property, suitable lisp object, nil or list of
       keyword and value itself. If value is NIL, no assignment is done.
       :EMPTY and :NULL correspond VT_EMPTY and VT_NULL datatypes.
       Keyword in the list specifies the desired type of property being set.
       Supported types are :I1, :UI1, :I2, :UI2, :I4, :UI4, :UINT, :I8,
         :UI8, :R4, :R8, :DATE, :BSTR, :BOOL, :ERROR, :FILETIME,
         :LPSTR, :LPWSTR. FILETIMEs are converted to/from universal time format,
         while DATEs are not.

     returns multiple values - property contents before assignment. */
DEFUN(POSIX::FILE-PROPERTIES, file set &rest pairs,(subr_posix_file_properties,seclass_default,2,0,rest,nokey,0,NIL))
{
  /* TODO: close interfaces even on errors;
           support more datatypes
           use IPropertySetStorage::Create when it doesn't exist */
  IPropertyStorage * ppropstg = NULL;
  IPropertySetStorage * ppropsetstg = NULL;
  HRESULT hres;
  FMTID*  fmtid = NULL;
  PROPSPEC * pspecrd = NULL;
  PROPSPEC * pspecwr = NULL;
  PROPVARIANT * pvarrd = NULL;
  PROPVARIANT * pvarwr = NULL;
  PROPID * propidwpnvec = NULL; /* args for WritePropertyNames */
  LPWSTR * lpwstrwpnvec = NULL;
  int ifile = argcount + 1;
  int iset = argcount;
  int i;
  unsigned int initid = 2;
  int use_wpn = 0; /* should WritePropertyNames be used ? */
  int nproprd = 0, npropwr = 0; /* npropd >= npropwr */
  int cproprd = 0, cpropwr = 0;
  int cwpnpar = 0;
  /* argcount is (length pairs), not the total arg count */
  /* no &rest ? no sense. */
  if (argcount == 0) {
    skipSTACK(2);
    VALUES0;
    return;
  }
  /* count the number of r/rw props, checking arglist sanity */
  if (argcount % 2)
    error_key_odd(argcount,TheSubr(subr_self)->name);
  for(i=argcount-1;i>=0;i--) {
    if (i % 2) { /* specifier */
      if (!symbolp(STACK_(i)) && !stringp(STACK_(i))
          && !posfixnump(STACK_(i))) {
        if (!propspeclistp(STACK_(i),NULL,NULL)) {
          pushSTACK(TheSubr(subr_self)->name);
          error(program_error,
            GETTEXT("~S: bad property specifier - it must be string, "
                    "positive number, list or keyword"));
        } else { use_wpn++; nproprd++; }
      } else if (symbolp(STACK_(i)) && eq(STACK_(i),O(object_Kinitid))) initid = 0;
      else nproprd++;
    } else { /* value */
      if (!initid) {
        if (integerp(STACK_(i))) initid = I_to_UL(STACK_(i));
        else {
          pushSTACK(STACK_(i));
          pushSTACK(TheSubr(subr_self)->name);
          error(program_error,GETTEXT("~S: bad INITID specifier: ~S"));
        }
      } else if (!nullp(STACK_(i))) npropwr++;
    }
  }
  if (!StgOpenStorageExFunc) {
    begin_system_call();
    SetLastError(ERROR_INVALID_FUNCTION);
    end_system_call();
    OS_error();
  }
  STACK_(ifile) = physical_namestring(STACK_(ifile));
  with_string_0w(STACK_(ifile), filename, {
      begin_system_call();
      hres = StgOpenStorageExFunc(filename,
                                  ((npropwr||use_wpn)?STGM_READWRITE:STGM_READ)
                                  | STGM_SHARE_EXCLUSIVE,
                                  4 /* STGFMT_ANY */, 0, NULL /*&stgOp*/, 0,
                                  &IID_IPropertySetStorage,
                                  (void **)&ppropsetstg);
      end_system_call();
  });
  if (FAILED(hres)) {
    pushSTACK(STACK_(ifile));
    pushSTACK(TheSubr(subr_self)->name);
    switch(hres) {
      case STG_E_FILENOTFOUND:
        error(file_error,GETTEXT("~S: file ~S does not exist"));
      case STG_E_FILEALREADYEXISTS:
        error(file_error,GETTEXT("~S: file ~S is not a compound file nor it is on the NTFS file system"));
      default:
        error(file_error,GETTEXT("~S: StgOpenStorageEx() failed on file ~S"));
    }
  }
  if (eq(STACK_(iset),O(object_Kuser_defined)))
    fmtid = (REFFMTID)&FMTID_UserDefinedProperties;
  else if (eq(STACK_(iset),O(object_Kbuilt_in)))
    fmtid = (REFFMTID)&FMTID_SummaryInformation;
  else {
    pushSTACK(STACK_(iset));
    pushSTACK(TheSubr(subr_self)->name);
    error(file_error,GETTEXT("~S: invalid property set specifier ~S"));
  }
  begin_system_call();
  hres = ppropsetstg->lpVtbl->Open(ppropsetstg, fmtid,
                                   ((npropwr||use_wpn)?STGM_READWRITE:STGM_READ)
                                   | STGM_SHARE_EXCLUSIVE, &ppropstg);
  end_system_call();
  if (FAILED(hres)) {
    pushSTACK(safe_to_string(DecodeHRESULT(hres)));
    pushSTACK(STACK_(ifile+1));
    pushSTACK(STACK_(iset+2));
    pushSTACK(TheSubr(subr_self)->name);
    error(file_error,GETTEXT("~S: unable to open ~S IPropertySetStorage of ~S: error ~S"));
  }
  /* fill the specifiers, init the variables */
  pspecrd =   (PROPSPEC *)my_malloc(sizeof(PROPSPEC)    * nproprd);
  pvarrd = (PROPVARIANT *)my_malloc(sizeof(PROPVARIANT) * nproprd);
  pspecwr =   (PROPSPEC *)my_malloc(sizeof(PROPSPEC)    * npropwr);
  pvarwr = (PROPVARIANT *)my_malloc(sizeof(PROPVARIANT) * npropwr);
  if (use_wpn) {
    propidwpnvec = (PROPID *)my_malloc(sizeof(PROPID)*use_wpn);
    lpwstrwpnvec = (LPWSTR *)my_malloc(sizeof(LPWSTR)*use_wpn);
  }
  for(i=0;i<argcount;i+=2) {
    /* i+1 - specifier, i - value */
    PROPSPEC second;
    int pssresult;
    if (symbolp(STACK_(i+1)) && eq(STACK_(i+1),O(object_Kinitid))) continue;
    pssresult = PropSpecSet(STACK_(i+1),pspecrd+nproprd-cproprd-1,&second);
    MyPropVariantInit(pvarrd+nproprd-cproprd-1);
    if (!nullp(STACK_(i))) {
      PropSpecSet(STACK_(i+1),pspecwr+npropwr-cpropwr-1,NULL);
      MyPropVariantInit(pvarwr+npropwr-cpropwr-1);
      pushSTACK(STACK_(i));
      if (!LispToPropVariant(pvarwr+npropwr-cpropwr-1)) {
        pushSTACK(STACK_(i));
        pushSTACK(TheSubr(subr_self)->name);
        error(error_condition,GETTEXT("~S: cannot convert ~S to PROPVARIANT"));
      }
      cpropwr++;
    }
    if (use_wpn && pssresult == 2) {
      propidwpnvec[cwpnpar] = pspecrd[nproprd-cproprd-1].propid;
      lpwstrwpnvec[cwpnpar] = second.lpwstr;
      cwpnpar++;
    }
    cproprd++;
  }
  hres = ppropstg->lpVtbl->ReadMultiple(ppropstg,nproprd, pspecrd, pvarrd);
  if(FAILED(hres)) {
    pushSTACK(safe_to_string(DecodeHRESULT(hres)));
    pushSTACK(TheSubr(subr_self)->name);
    error(error_condition,GETTEXT("~S: ReadMultiple error: ~S"));
  }
  if (npropwr > 0) {
    begin_system_call();
    hres = ppropstg->lpVtbl->WriteMultiple(ppropstg,npropwr,pspecwr,pvarwr,
                                           initid);
    end_system_call();
    if(FAILED(hres)) {
      pushSTACK(safe_to_string(DecodeHRESULT(hres)));
      pushSTACK(TheSubr(subr_self)->name);
      error(error_condition,GETTEXT("~S: WriteMultiple error: ~S"));
    }
  }
  for (i=0;i<nproprd;i++)
    if (!PropVariantToLisp(pvarrd+i)) {
      pushSTACK(fixnum(i));
      pushSTACK(TheSubr(subr_self)->name);
      error(error_condition,GETTEXT("~S: cannot convert value ~S to Lisp datatype"));
    }
  if (use_wpn) {
    hres = ppropstg->lpVtbl->WritePropertyNames(ppropstg,use_wpn,propidwpnvec,lpwstrwpnvec);
    if (FAILED(hres)) {
      pushSTACK(safe_to_string(DecodeHRESULT(hres)));
      pushSTACK(TheSubr(subr_self)->name);
      error(error_condition,GETTEXT("~S: WritePropertyNames: ~S"));
    }
  }
  if (sizeof(mv_space)/sizeof(mv_space[0]) < nproprd) {
    pushSTACK(TheSubr(subr_self)->name);
    error(program_error,GETTEXT("~S: multiple value count limit reached"));
  }
  mv_count = nproprd;
  for (i=0;i<nproprd;i++) mv_space[nproprd-i-1] = popSTACK();
  skipSTACK(argcount+2); /* two first args */
  begin_system_call();
  for (i=0;i<nproprd;i++) {
    PropVariantClear(pvarrd+i);
    if (pspecrd[i].ulKind == PRSPEC_LPWSTR) free(pspecrd[i].lpwstr);
  }
  for (i=0;i<npropwr;i++) {
    if (pvarwr[i].vt == VT_LPWSTR || pvarwr[i].vt == VT_BSTR)
      SysFreeString(pvarwr[i].pwszVal);
    if (pvarwr[i].vt == VT_LPSTR)
      SysFreeString((BSTR)pvarwr[i].pszVal);
    if (pspecwr[i].ulKind == PRSPEC_LPWSTR) free(pspecwr[i].lpwstr);
  }
  for (i=0;i<use_wpn;i++) free(lpwstrwpnvec[i]);
  free(pspecrd); free(pvarrd); free(pspecwr); free(pvarwr);
  free(propidwpnvec); free(lpwstrwpnvec);
  ppropstg->lpVtbl->Release(ppropstg);
  ppropsetstg->lpVtbl->Release(ppropsetstg);
  end_system_call();
}
#endif  /* WIN32_NATIVE || UNIX_CYGWIN32 */

#if defined(HAVE_FFI)
/* STDIO inteface for postgresql et al */
DEFUN(POSIX::FOPEN, file mode,(subr_posix_fopen,seclass_default,2,0,norest,nokey,0,NIL)) {
  STACK_0 = check_string(STACK_0);
  STACK_1 = check_string(STACK_1);
  with_string_0(STACK_1, GLO(pathname_encoding), pathz, {
      with_string_0(STACK_0, GLO(misc_encoding), modez, {
          FILE *fp;
          begin_system_call();
          fp = fopen(pathz,modez);
          end_system_call();
          if (fp) STACK_0 = allocate_fpointer((FOREIGN)fp);
          else OS_error();
        });
    });
  VALUES1(STACK_0); skipSTACK(2);
}
DEFUN(POSIX::FDOPEN, fd mode,(subr_posix_fdopen,seclass_default,2,0,norest,nokey,0,NIL)) {
  STACK_0 = check_string(STACK_0);
  STACK_1 = check_sint(STACK_1);
  with_string_0(STACK_0, GLO(misc_encoding), modez, {
      FILE *fp;
      begin_system_call();
      fp = fdopen(I_to_sint(STACK_1),modez);
      end_system_call();
      if (fp) STACK_0 = allocate_fpointer((FOREIGN)fp);
      else OS_error();
    });
  VALUES1(STACK_0); skipSTACK(2);
}
#line 4115
#define FILE_FUNCTION(fun,finish)                            int ret;                                                 STACK_0 = check_fpointer(STACK_0,1);                     begin_system_call();                                     ret = fun((FILE*)TheFpointer(STACK_0)->fp_pointer);      end_system_call();                                       finish; skipSTACK(1)
DEFUN(POSIX::FILENO, fp,(subr_posix_fileno,seclass_default,1,0,norest,nokey,0,NIL))
{ FILE_FUNCTION(fileno,{ if(ret==-1)OS_error(); VALUES1(sint_to_I(ret)); }); }
DEFUN(POSIX::FEOF, fp,(subr_posix_feof,seclass_default,1,0,norest,nokey,0,NIL)) { FILE_FUNCTION(feof,VALUES_IF(ret)); }
DEFUN(POSIX::FERROR, fp,(subr_posix_ferror,seclass_default,1,0,norest,nokey,0,NIL)) { FILE_FUNCTION(ferror,VALUES_IF(ret)); }
DEFUN(POSIX::FCLOSE, fp,(subr_posix_fclose,seclass_default,1,0,norest,nokey,0,NIL))
{ FILE_FUNCTION(fclose,{ if (ret == EOF) OS_error(); VALUES0; }); }
DEFUN(POSIX::FFLUSH, fp,(subr_posix_fflush,seclass_default,1,0,norest,nokey,0,NIL))
{ FILE_FUNCTION(fflush,{ if (ret == EOF) OS_error(); VALUES0; }); }
/* no fputs & fgets because they will mess with encodings &c */
DEFUN(POSIX::CLEARERR, fp,(subr_posix_clearerr,seclass_default,1,0,norest,nokey,0,NIL)) {
  STACK_0 = check_fpointer(STACK_0,1);
  begin_system_call();
  clearerr((FILE*)TheFpointer(STACK_0)->fp_pointer);
  end_system_call();
  VALUES0; skipSTACK(1);
}
/* fgetc returns -1 on EOF instead of signaling an error. or signal?!
 * DEFUN(POSIX::FGETC, fp) FILE_FUNCTION(fgetc,VALUES1(sint_to_I(ret)))
#line 4142
 * #define INT_FILE_TO_INT(fun) {                                         *     int ret;                                                           *     STACK_0 = check_fpointer(STACK_0,1);                               *     STACK_1 = check_sint(STACK_1);                                     *     begin_system_call();                                               *     ret = fun(I_to_sint(STACK_1),TheFpointer(STACK_0)->fp_pointer);    *     end_system_call();                                                 *     VALUES1(sint_to_I(ret)); skipSTACK(2);                             *   }
 * DEFUN(POSIX::FPUTC, c fp) INT_FILE_TO_INT(fputc)
 * DEFUN(POSIX::UNGETC, c fp) INT_FILE_TO_INT(ungetc) */
#endif  /* HAVE_FFI */

#if defined(DEBUG_SPVW)
/* internal playground - see spvd.d & spvw_debug.d */
extern unsigned int get_constsym_count (void);
extern object get_constsym (unsigned int);
DEFUN(CONSTSYM, &optional pos,(subr_posix_constsym,seclass_default,0,1,norest,nokey,0,NIL)) {
  VALUES1(missingp(STACK_0) ? fixnum(get_constsym_count())
          : get_constsym(I_to_uint(check_uint(STACK_0))));
  skipSTACK(1);
}
#endif


struct module__syscalls__subr_tab_t module__syscalls__subr_tab
  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(__GNUC__)
    __attribute__ ((aligned (varobject_alignment)))
  #endif
  = {
  #if varobjects_misaligned
  { 0 },
  #endif
  LISPFUN_F(subr_os_file_owner,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_os_priority,seclass_default,1,1,norest,nokey,0,NIL)
  LISPFUN_F(subr_os__25set_priority,seclass_default,3,0,norest,nokey,0,NIL)
#if defined(HAVE_CLOCK)
  LISPFUN_F(subr_posix_bogomips,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_clearerr,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_CLOSELOG))
  LISPFUN_F(subr_posix_closelog,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_CONFSTR)
  LISPFUN_F(subr_posix_confstr,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(DEBUG_SPVW)
  LISPFUN_F(subr_posix_constsym,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_convert_attributes,seclass_default,1,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_posix_convert_mode,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_posix_copy_file,seclass_default,2,0,norest,key,4,NIL)
#if defined(HAVE_CRYPT) && !defined(WIN32_NATIVE)
  LISPFUN_F(subr_posix_crypt,seclass_default,2,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_posix_duplicate_handle,seclass_default,1,1,norest,nokey,0,NIL)
#if defined(HAVE_ENCRYPT) && !defined(WIN32_NATIVE)
  LISPFUN_F(subr_posix_encrypt,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_ENDUTXENT))
  LISPFUN_F(subr_posix_endutxent,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_ERFC)
  LISPFUN_F(subr_posix_erf,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_ERFC)
  LISPFUN_F(subr_posix_erfc,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_fclose,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_fdopen,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_feof,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_ferror,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_fflush,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_fileno,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_file_info,seclass_default,1,1,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_file_properties,seclass_default,2,0,rest,nokey,0,NIL)
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  LISPFUN_F(subr_posix_file_stat,seclass_default,1,1,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FFI)
  LISPFUN_F(subr_posix_fopen,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETEGID)
  LISPFUN_F(subr_posix_getegid,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETEUID)
  LISPFUN_F(subr_posix_geteuid,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETGID)
  LISPFUN_F(subr_posix_getgid,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETGROUPS)
  LISPFUN_F(subr_posix_getgroups,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETPGID)
  LISPFUN_F(subr_posix_getpgid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETPGRP)
  LISPFUN_F(subr_posix_getpgrp,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETPPID)
  LISPFUN_F(subr_posix_getppid,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETSID)
  LISPFUN_F(subr_posix_getsid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETUID)
  LISPFUN_F(subr_posix_getuid,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXENT))
  LISPFUN_F(subr_posix_getutxent,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXID))
  LISPFUN_F(subr_posix_getutxid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXLINE))
  LISPFUN_F(subr_posix_getutxline,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  LISPFUN_F(subr_posix_group_info,seclass_default,0,1,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_posix_j0,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_posix_j1,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_posix_jn,seclass_default,2,0,norest,nokey,0,NIL)
#if defined(HAVE_KILL)
  LISPFUN_F(subr_posix_kill,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_LGAMMA) || HAVE_DECL_LGAMMA_R
  LISPFUN_F(subr_posix_lgamma,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETLOADAVG)
  LISPFUN_F(subr_posix_loadavg,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_make_shortcut,seclass_default,1,0,norest,key,7,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_memory_status,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_MKDTEMP) || defined(WIN32_NATIVE) || (defined(HAVE_MKDIR) && defined(HAVE_TEMPNAM))
  LISPFUN_F(subr_posix_mkdtemp,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  LISPFUN_F(subr_posix_mknod,seclass_default,3,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  LISPFUN_F(subr_posix_mkstemp,seclass_default,1,0,norest,key,4,NIL)
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  LISPFUN_F(subr_posix_openlog,seclass_default,1,0,norest,key,6,NIL)
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  LISPFUN_F(subr_posix_pathconf,seclass_default,1,1,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_PUTUTXLINE))
  LISPFUN_F(subr_posix_pututxline,seclass_default,1,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_posix_resolve_host_ipaddr,seclass_default,0,1,norest,nokey,0,NIL)
#if defined(HAVE_GETRLIMIT)
  LISPFUN_F(subr_posix_rlimit,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  LISPFUN_F(subr_posix_service,seclass_default,0,2,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETKEY) && !defined(WIN32_NATIVE)
  LISPFUN_F(subr_posix_setkey,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_SETLOGMASK))
  LISPFUN_F(subr_posix_setlogmask,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETPGRP)
  LISPFUN_F(subr_posix_setpgrp,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETREGID)
  LISPFUN_F(subr_posix_setregid,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETREUID)
  LISPFUN_F(subr_posix_setreuid,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETSID)
  LISPFUN_F(subr_posix_setsid,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_SETUTXENT))
  LISPFUN_F(subr_posix_setutxent,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  LISPFUN_F(subr_posix_set_file_stat,seclass_default,1,0,norest,key,5,NIL)
#endif
#if defined(HAVE_SETRLIMIT)
  LISPFUN_F(subr_posix_set_rlimit,seclass_default,3,0,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_shortcut_info,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  LISPFUN_F(subr_posix_stat_vfs,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  LISPFUN_F(subr_posix_stream_lock,seclass_default,2,0,norest,key,4,NIL)
#endif
#if defined(HAVE_FCNTL)
  LISPFUN_F(subr_posix_stream_options,seclass_default,2,1,norest,nokey,0,NIL)
#endif
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  LISPFUN_F(subr_posix_string_time,seclass_default,1,2,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(HAVE_SYNC) || defined(HAVE_FSYNC)
  LISPFUN_F(subr_posix_sync,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SYSCONF)
  LISPFUN_F(subr_posix_sysconf,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_system_info,seclass_default,0,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_posix_tgamma,seclass_default,1,0,norest,nokey,0,NIL)
#if defined(HAVE_UMASK)
  LISPFUN_F(subr_posix_umask,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_UNAME)
  LISPFUN_F(subr_posix_uname,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETRUSAGE)
  LISPFUN_F(subr_posix_usage,seclass_default,0,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  LISPFUN_F(subr_posix_user_info,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  LISPFUN_F(subr_posix_version,seclass_default,0,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_posix_y0,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_posix_y1,seclass_default,1,0,norest,nokey,0,NIL)
  LISPFUN_F(subr_posix_yn,seclass_default,2,0,norest,nokey,0,NIL)
#if defined(HAVE_SETEGID)
  LISPFUN_F(subr_posix__25setegid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETEUID)
  LISPFUN_F(subr_posix__25seteuid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETGID)
  LISPFUN_F(subr_posix__25setgid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETGROUPS)
  LISPFUN_F(subr_posix__25setgroups,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETPGID)
  LISPFUN_F(subr_posix__25setpgid,seclass_default,2,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SETUID)
  LISPFUN_F(subr_posix__25setuid,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if defined(HAVE_SYSLOG)
  LISPFUN_F(subr_posix__25syslog,seclass_default,3,0,norest,nokey,0,NIL)
#endif
  0
};
uintC module__syscalls__subr_tab_size = (sizeof(struct module__syscalls__subr_tab_t)-varobjects_misaligned-sizeof(int))/sizeof(subr_t);

struct module__syscalls__subr_tab_initdata_t {
  subr_initdata_t _subr_os_file_owner;
  subr_initdata_t _subr_os_priority;
  subr_initdata_t _subr_os__25set_priority;
#if defined(HAVE_CLOCK)
  subr_initdata_t _subr_posix_bogomips;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_clearerr;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_CLOSELOG))
  subr_initdata_t _subr_posix_closelog;
#endif
#if defined(HAVE_CONFSTR)
  subr_initdata_t _subr_posix_confstr;
#endif
#if defined(DEBUG_SPVW)
  subr_initdata_t _subr_posix_constsym;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_convert_attributes;
#endif
  subr_initdata_t _subr_posix_convert_mode;
  subr_initdata_t _subr_posix_copy_file;
#if defined(HAVE_CRYPT) && !defined(WIN32_NATIVE)
  subr_initdata_t _subr_posix_crypt;
#endif
  subr_initdata_t _subr_posix_duplicate_handle;
#if defined(HAVE_ENCRYPT) && !defined(WIN32_NATIVE)
  subr_initdata_t _subr_posix_encrypt;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_ENDUTXENT))
  subr_initdata_t _subr_posix_endutxent;
#endif
#if defined(HAVE_ERFC)
  subr_initdata_t _subr_posix_erf;
#endif
#if defined(HAVE_ERFC)
  subr_initdata_t _subr_posix_erfc;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_fclose;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_fdopen;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_feof;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_ferror;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_fflush;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_fileno;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_file_info;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_file_properties;
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  subr_initdata_t _subr_posix_file_stat;
#endif
#if defined(HAVE_FFI)
  subr_initdata_t _subr_posix_fopen;
#endif
#if defined(HAVE_GETEGID)
  subr_initdata_t _subr_posix_getegid;
#endif
#if defined(HAVE_GETEUID)
  subr_initdata_t _subr_posix_geteuid;
#endif
#if defined(HAVE_GETGID)
  subr_initdata_t _subr_posix_getgid;
#endif
#if defined(HAVE_GETGROUPS)
  subr_initdata_t _subr_posix_getgroups;
#endif
#if defined(HAVE_GETPGID)
  subr_initdata_t _subr_posix_getpgid;
#endif
#if defined(HAVE_GETPGRP)
  subr_initdata_t _subr_posix_getpgrp;
#endif
#if defined(HAVE_GETPPID)
  subr_initdata_t _subr_posix_getppid;
#endif
#if defined(HAVE_GETSID)
  subr_initdata_t _subr_posix_getsid;
#endif
#if defined(HAVE_GETUID)
  subr_initdata_t _subr_posix_getuid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXENT))
  subr_initdata_t _subr_posix_getutxent;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXID))
  subr_initdata_t _subr_posix_getutxid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXLINE))
  subr_initdata_t _subr_posix_getutxline;
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  subr_initdata_t _subr_posix_group_info;
#endif
  subr_initdata_t _subr_posix_j0;
  subr_initdata_t _subr_posix_j1;
  subr_initdata_t _subr_posix_jn;
#if defined(HAVE_KILL)
  subr_initdata_t _subr_posix_kill;
#endif
#if defined(HAVE_LGAMMA) || HAVE_DECL_LGAMMA_R
  subr_initdata_t _subr_posix_lgamma;
#endif
#if defined(HAVE_GETLOADAVG)
  subr_initdata_t _subr_posix_loadavg;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_make_shortcut;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_memory_status;
#endif
#if defined(HAVE_MKDTEMP) || defined(WIN32_NATIVE) || (defined(HAVE_MKDIR) && defined(HAVE_TEMPNAM))
  subr_initdata_t _subr_posix_mkdtemp;
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  subr_initdata_t _subr_posix_mknod;
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  subr_initdata_t _subr_posix_mkstemp;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  subr_initdata_t _subr_posix_openlog;
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  subr_initdata_t _subr_posix_pathconf;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_PUTUTXLINE))
  subr_initdata_t _subr_posix_pututxline;
#endif
  subr_initdata_t _subr_posix_resolve_host_ipaddr;
#if defined(HAVE_GETRLIMIT)
  subr_initdata_t _subr_posix_rlimit;
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  subr_initdata_t _subr_posix_service;
#endif
#if defined(HAVE_SETKEY) && !defined(WIN32_NATIVE)
  subr_initdata_t _subr_posix_setkey;
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_SETLOGMASK))
  subr_initdata_t _subr_posix_setlogmask;
#endif
#if defined(HAVE_SETPGRP)
  subr_initdata_t _subr_posix_setpgrp;
#endif
#if defined(HAVE_SETREGID)
  subr_initdata_t _subr_posix_setregid;
#endif
#if defined(HAVE_SETREUID)
  subr_initdata_t _subr_posix_setreuid;
#endif
#if defined(HAVE_SETSID)
  subr_initdata_t _subr_posix_setsid;
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_SETUTXENT))
  subr_initdata_t _subr_posix_setutxent;
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  subr_initdata_t _subr_posix_set_file_stat;
#endif
#if defined(HAVE_SETRLIMIT)
  subr_initdata_t _subr_posix_set_rlimit;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_shortcut_info;
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  subr_initdata_t _subr_posix_stat_vfs;
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  subr_initdata_t _subr_posix_stream_lock;
#endif
#if defined(HAVE_FCNTL)
  subr_initdata_t _subr_posix_stream_options;
#endif
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  subr_initdata_t _subr_posix_string_time;
#endif
#if defined(WIN32_NATIVE) || defined(HAVE_SYNC) || defined(HAVE_FSYNC)
  subr_initdata_t _subr_posix_sync;
#endif
#if defined(HAVE_SYSCONF)
  subr_initdata_t _subr_posix_sysconf;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_system_info;
#endif
  subr_initdata_t _subr_posix_tgamma;
#if defined(HAVE_UMASK)
  subr_initdata_t _subr_posix_umask;
#endif
#if defined(HAVE_UNAME)
  subr_initdata_t _subr_posix_uname;
#endif
#if defined(HAVE_GETRUSAGE)
  subr_initdata_t _subr_posix_usage;
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  subr_initdata_t _subr_posix_user_info;
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  subr_initdata_t _subr_posix_version;
#endif
  subr_initdata_t _subr_posix_y0;
  subr_initdata_t _subr_posix_y1;
  subr_initdata_t _subr_posix_yn;
#if defined(HAVE_SETEGID)
  subr_initdata_t _subr_posix__25setegid;
#endif
#if defined(HAVE_SETEUID)
  subr_initdata_t _subr_posix__25seteuid;
#endif
#if defined(HAVE_SETGID)
  subr_initdata_t _subr_posix__25setgid;
#endif
#if defined(HAVE_SETGROUPS)
  subr_initdata_t _subr_posix__25setgroups;
#endif
#if defined(HAVE_SETPGID)
  subr_initdata_t _subr_posix__25setpgid;
#endif
#if defined(HAVE_SETUID)
  subr_initdata_t _subr_posix__25setuid;
#endif
#if defined(HAVE_SYSLOG)
  subr_initdata_t _subr_posix__25syslog;
#endif
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__syscalls__subr_tab_initdata = {
  { "OS", "FILE-OWNER" },
  { "OS", "PRIORITY" },
  { "OS", "%SET-PRIORITY" },
#if defined(HAVE_CLOCK)
  { "POSIX", "BOGOMIPS" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "CLEARERR" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_CLOSELOG))
  { "POSIX", "CLOSELOG" },
#endif
#if defined(HAVE_CONFSTR)
  { "POSIX", "CONFSTR" },
#endif
#if defined(DEBUG_SPVW)
  { "POSIX", "CONSTSYM" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "CONVERT-ATTRIBUTES" },
#endif
  { "POSIX", "CONVERT-MODE" },
  { "POSIX", "COPY-FILE" },
#if defined(HAVE_CRYPT) && !defined(WIN32_NATIVE)
  { "POSIX", "CRYPT" },
#endif
  { "POSIX", "DUPLICATE-HANDLE" },
#if defined(HAVE_ENCRYPT) && !defined(WIN32_NATIVE)
  { "POSIX", "ENCRYPT" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_ENDUTXENT))
  { "POSIX", "ENDUTXENT" },
#endif
#if defined(HAVE_ERFC)
  { "POSIX", "ERF" },
#endif
#if defined(HAVE_ERFC)
  { "POSIX", "ERFC" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FCLOSE" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FDOPEN" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FEOF" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FERROR" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FFLUSH" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FILENO" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "FILE-INFO" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "FILE-PROPERTIES" },
#endif
#if defined(HAVE_FSTAT) && defined(HAVE_STAT)
  { "POSIX", "FILE-STAT" },
#endif
#if defined(HAVE_FFI)
  { "POSIX", "FOPEN" },
#endif
#if defined(HAVE_GETEGID)
  { "POSIX", "GETEGID" },
#endif
#if defined(HAVE_GETEUID)
  { "POSIX", "GETEUID" },
#endif
#if defined(HAVE_GETGID)
  { "POSIX", "GETGID" },
#endif
#if defined(HAVE_GETGROUPS)
  { "POSIX", "GETGROUPS" },
#endif
#if defined(HAVE_GETPGID)
  { "POSIX", "GETPGID" },
#endif
#if defined(HAVE_GETPGRP)
  { "POSIX", "GETPGRP" },
#endif
#if defined(HAVE_GETPPID)
  { "POSIX", "GETPPID" },
#endif
#if defined(HAVE_GETSID)
  { "POSIX", "GETSID" },
#endif
#if defined(HAVE_GETUID)
  { "POSIX", "GETUID" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXENT))
  { "POSIX", "GETUTXENT" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXID))
  { "POSIX", "GETUTXID" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_GETUTXLINE))
  { "POSIX", "GETUTXLINE" },
#endif
#if defined(HAVE_GETGRGID) && defined(HAVE_GETGRNAM)
  { "POSIX", "GROUP-INFO" },
#endif
  { "POSIX", "J0" },
  { "POSIX", "J1" },
  { "POSIX", "JN" },
#if defined(HAVE_KILL)
  { "POSIX", "KILL" },
#endif
#if defined(HAVE_LGAMMA) || HAVE_DECL_LGAMMA_R
  { "POSIX", "LGAMMA" },
#endif
#if defined(HAVE_GETLOADAVG)
  { "POSIX", "LOADAVG" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "MAKE-SHORTCUT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "MEMORY-STATUS" },
#endif
#if defined(HAVE_MKDTEMP) || defined(WIN32_NATIVE) || (defined(HAVE_MKDIR) && defined(HAVE_TEMPNAM))
  { "POSIX", "MKDTEMP" },
#endif
#if defined(HAVE_MKNOD) || defined(HAVE_MKFIFO) || defined(HAVE_MKDIR) || defined(HAVE_CREAT)
  { "POSIX", "MKNOD" },
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  { "POSIX", "MKSTEMP" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  { "POSIX", "OPENLOG" },
#endif
#if defined(HAVE_PATHCONF) && defined(HAVE_FPATHCONF)
  { "POSIX", "PATHCONF" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_PUTUTXLINE))
  { "POSIX", "PUTUTXLINE" },
#endif
  { "POSIX", "RESOLVE-HOST-IPADDR" },
#if defined(HAVE_GETRLIMIT)
  { "POSIX", "RLIMIT" },
#endif
#if (defined(HAVE_GETSERVBYPORT) && defined(HAVE_GETSERVBYNAME)) || defined(WIN32_NATIVE)
  { "POSIX", "SERVICE" },
#endif
#if defined(HAVE_SETKEY) && !defined(WIN32_NATIVE)
  { "POSIX", "SETKEY" },
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_SETLOGMASK))
  { "POSIX", "SETLOGMASK" },
#endif
#if defined(HAVE_SETPGRP)
  { "POSIX", "SETPGRP" },
#endif
#if defined(HAVE_SETREGID)
  { "POSIX", "SETREGID" },
#endif
#if defined(HAVE_SETREUID)
  { "POSIX", "SETREUID" },
#endif
#if defined(HAVE_SETSID)
  { "POSIX", "SETSID" },
#endif
#if (defined(HAVE_UTMPX_H)) && (defined(HAVE_SETUTXENT))
  { "POSIX", "SETUTXENT" },
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  { "POSIX", "SET-FILE-STAT" },
#endif
#if defined(HAVE_SETRLIMIT)
  { "POSIX", "SET-RLIMIT" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "SHORTCUT-INFO" },
#endif
#if defined(WIN32_NATIVE) || (defined(HAVE_STATVFS) && defined(HAVE_SYS_STATVFS_H))
  { "POSIX", "STAT-VFS" },
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  { "POSIX", "STREAM-LOCK" },
#endif
#if defined(HAVE_FCNTL)
  { "POSIX", "STREAM-OPTIONS" },
#endif
#if defined(HAVE_STRFTIME) && defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
  { "POSIX", "STRING-TIME" },
#endif
#if defined(WIN32_NATIVE) || defined(HAVE_SYNC) || defined(HAVE_FSYNC)
  { "POSIX", "SYNC" },
#endif
#if defined(HAVE_SYSCONF)
  { "POSIX", "SYSCONF" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "SYSTEM-INFO" },
#endif
  { "POSIX", "TGAMMA" },
#if defined(HAVE_UMASK)
  { "POSIX", "UMASK" },
#endif
#if defined(HAVE_UNAME)
  { "POSIX", "UNAME" },
#endif
#if defined(HAVE_GETRUSAGE)
  { "POSIX", "USAGE" },
#endif
#if defined(HAVE_GETLOGIN) && defined(HAVE_GETPWNAM) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
  { "POSIX", "USER-INFO" },
#endif
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  { "POSIX", "VERSION" },
#endif
  { "POSIX", "Y0" },
  { "POSIX", "Y1" },
  { "POSIX", "YN" },
#if defined(HAVE_SETEGID)
  { "POSIX", "%SETEGID" },
#endif
#if defined(HAVE_SETEUID)
  { "POSIX", "%SETEUID" },
#endif
#if defined(HAVE_SETGID)
  { "POSIX", "%SETGID" },
#endif
#if defined(HAVE_SETGROUPS)
  { "POSIX", "%SETGROUPS" },
#endif
#if defined(HAVE_SETPGID)
  { "POSIX", "%SETPGID" },
#endif
#if defined(HAVE_SETUID)
  { "POSIX", "%SETUID" },
#endif
#if defined(HAVE_SYSLOG)
  { "POSIX", "%SYSLOG" },
#endif
  0
};

void module__syscalls__init_function_1 (module_t* module);
void module__syscalls__init_function_1 (module_t* module)
{
  pushSTACK(O(object_Kmethod));
  pushSTACK(O(object_Kpreserve));
  pushSTACK(O(object_Kif_exists));
  pushSTACK(O(object_Kif_does_not_exist));
  module__syscalls__subr_tab._subr_posix_copy_file.keywords = vectorof(4);
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
  pushSTACK(O(object_Kworking_directory));
  pushSTACK(O(object_Karguments));
  pushSTACK(O(object_Kshow_command));
  pushSTACK(O(object_Kicon));
  pushSTACK(O(object_Kdescription));
  pushSTACK(O(object_Khot_key));
  pushSTACK(O(object_Kpath));
  module__syscalls__subr_tab._subr_posix_make_shortcut.keywords = vectorof(7);
#endif
#if defined(HAVE_MKSTEMP) || defined(HAVE_TEMPNAM) || defined(WIN32_NATIVE)
  pushSTACK(O(object_Kdirection));
  pushSTACK(O(object_Kbuffered));
  pushSTACK(O(object_Kexternal_format));
  pushSTACK(O(object_Kelement_type));
  module__syscalls__subr_tab._subr_posix_mkstemp.keywords = vectorof(4);
#endif
#if (defined(HAVE_SYSLOG)) && (defined(HAVE_OPENLOG))
  pushSTACK(O(object_Kpid));
  pushSTACK(O(object_Kcons));
  pushSTACK(O(object_Kndelay));
  pushSTACK(O(object_Kodelay));
  pushSTACK(O(object_Knowait));
  pushSTACK(O(object_Kfacility));
  module__syscalls__subr_tab._subr_posix_openlog.keywords = vectorof(6);
#endif
#if defined(HAVE_STAT) && (defined(HAVE_CHMOD) || defined(HAVE_CHOWN) || defined(HAVE_UTIME))
  pushSTACK(O(object_Katime));
  pushSTACK(O(object_Kmtime));
  pushSTACK(O(object_Kmode));
  pushSTACK(O(object_Kuid));
  pushSTACK(O(object_Kgid));
  module__syscalls__subr_tab._subr_posix_set_file_stat.keywords = vectorof(5);
#endif
#if defined(HAVE_FCNTL) || defined(WIN32_NATIVE)
  pushSTACK(O(object_Kblock));
  pushSTACK(O(object_Kshared));
  pushSTACK(O(object_Kstart));
  pushSTACK(O(object_Klength));
  module__syscalls__subr_tab._subr_posix_stream_lock.keywords = vectorof(4);
#endif
}

void module__syscalls__fini_function (module_t* module);
void module__syscalls__fini_function (module_t* module)
{
}
