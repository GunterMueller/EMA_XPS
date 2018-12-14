/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */

/* is abort() __volatile__? */
#define ABORT_VOLATILE 

/* is <sys/file.h> needed for R_OK/W_OK/X_OK? */
/* #undef ACCESS_NEEDS_SYS_FILE_H */

/* symbols are prefixed by an underscore in assembly language */
/* #undef ASM_UNDERSCORE */

/* Define to the number of bits in type 'ptrdiff_t'. */
/* #undef BITSIZEOF_PTRDIFF_T */

/* Define to the number of bits in type 'sig_atomic_t'. */
/* #undef BITSIZEOF_SIG_ATOMIC_T */

/* Define to the number of bits in type 'size_t'. */
/* #undef BITSIZEOF_SIZE_T */

/* Define to the number of bits in type 'wchar_t'. */
/* #undef BITSIZEOF_WCHAR_T */

/* Define to the number of bits in type 'wint_t'. */
/* #undef BITSIZEOF_WINT_T */

/* what is your caddr_t type? */
#define CADDR_T caddr_t

/* socklen_t (if defined in <sys/socket.h>) or int otherwise */
#define CLISP_SOCKLEN_T socklen_t

/* address range of program code (text+data+bss) */
#define CODE_ADDRESS_RANGE 0x0000000000000000UL

/* type of `addrlen' in connect() declaration */
#define CONNECT_ADDRLEN_T unsigned int

/* does declaration of connect() need const? */
#define CONNECT_CONST const

/* type of `name' in connect() declaration */
#define CONNECT_NAME_T struct sockaddr *

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* have <dirent.h>? */
#define DIRENT /**/

/* Define to 1 if 'double' division by zero raises an exception. */
/* #undef DOUBLE_DIV0_EXCEPTION */

/* Define to 1 if a 'double' inexact operation raises an exception. */
/* #undef DOUBLE_INEXACT_EXCEPTION */

/* Define to 1 if 'double' overflow raises an exception. */
/* #undef DOUBLE_OVERFLOW_EXCEPTION */

/* Define to 1 if 'double' underflow raises an exception. */
/* #undef DOUBLE_UNDERFLOW_EXCEPTION */

/* the real value of ELOOP even if it is hidden in <errno.h> */
#define ELOOP_VALUE ELOOP

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#define ENABLE_NLS 1

/* Define to 1 if 'float' division by zero raises an exception. */
/* #undef FLOAT_DIV0_EXCEPTION */

/* Define to 1 if a 'float' inexact operation raises an exception. */
/* #undef FLOAT_INEXACT_EXCEPTION */

/* Define to 1 if 'float' overflow raises an exception. */
/* #undef FLOAT_OVERFLOW_EXCEPTION */

/* Define to 1 if 'float' underflow raises an exception. */
/* #undef FLOAT_UNDERFLOW_EXCEPTION */

/* declaration of gettimeofday() needs dots */
/* #undef GETTIMEOFDAY_DOTS */

/* type of `tzp' in gettimeofday() declaration */
#define GETTIMEOFDAY_TZP_T struct timezone *

/* Define to indicate the 'malloc' module. */
#define GNULIB_MALLOC_GNU 1

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define HAVE_ARPA_INET_H 1

/* Define to 1 if you have the <avcall.h> header file. */
/* #undef HAVE_AVCALL_H */

/* Define to 1 if you have the `btowc' function. */
#define HAVE_BTOWC 1

/* __builtin_strlen() is compiled inline (not a call to strlen()) */
/* #undef HAVE_BUILTIN_STRLEN */

/* Define to 1 if you have the <callback.h> header file. */
/* #undef HAVE_CALLBACK_H */

/* Define to 1 if you have the MacOS X function CFLocaleCopyCurrent in the
   CoreFoundation framework. */
/* #undef HAVE_CFLOCALECOPYCURRENT */

/* Define to 1 if you have the MacOS X function CFPreferencesCopyAppValue in
   the CoreFoundation framework. */
/* #undef HAVE_CFPREFERENCESCOPYAPPVALUE */

/* Define to 1 if you have the `connect' function. */
#define HAVE_CONNECT 1

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
#define HAVE_DCGETTEXT 1

/* Define to 1 if you have the declaration of `environ', and to 0 if you
   don't. */
#define HAVE_DECL_ENVIRON 1

/* Define to 1 if you have the declaration of `getc_unlocked', and to 0 if you
   don't. */
#define HAVE_DECL_GETC_UNLOCKED 1

/* Define to 1 if you have the declaration of `isblank', and to 0 if you
   don't. */
#define HAVE_DECL_ISBLANK 1

/* Define to 1 if you have the declaration of `rl_already_prompted', and to 0
   if you don't. */
/* #undef HAVE_DECL_RL_ALREADY_PROMPTED */

/* Define to 1 if you have the declaration of `rl_gnu_readline_p', and to 0 if
   you don't. */
/* #undef HAVE_DECL_RL_GNU_READLINE_P */

/* Define to 1 if you have the declaration of `rl_readline_name', and to 0 if
   you don't. */
/* #undef HAVE_DECL_RL_READLINE_NAME */

/* Define to 1 if you have the `dladdr' function. */
#define HAVE_DLADDR 1

/* Define to 1 if you have the `dlclose' function. */
#define HAVE_DLCLOSE 1

/* Define to 1 if you have the `dlerror' function. */
#define HAVE_DLERROR 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the `dlopen' function. */
#define HAVE_DLOPEN 1

/* Define to 1 if you have the `dlsym' function. */
#define HAVE_DLSYM 1

/* Define to 1 if you have the `dlvsym' function. */
#define HAVE_DLVSYM 1

/* Define to 1 if you have the `fchmod' function. */
#define HAVE_FCHMOD 1

/* have the FIONREAD ioctl() */
#define HAVE_FIONREAD /**/

/* Define to 1 if you have the `flock' function. */
#define HAVE_FLOCK 1

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* have <fpu_control.h> and it defines the fpu_control_t type */
#define HAVE_FPU_CONTROL_T /**/

/* Define to 1 if you have the `fsync' function. */
#define HAVE_FSYNC 1

/* Define to 1 if you have the `ftime' function. */
#define HAVE_FTIME 1

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the `getdtablesize' function. */
#define HAVE_GETDTABLESIZE 1

/* have gethostbyname() */
#define HAVE_GETHOSTBYNAME /**/

/* Define to 1 if you have the `gethostent' function. */
#define HAVE_GETHOSTENT 1

/* Define to 1 if you have the `gethostname' function. */
#define HAVE_GETHOSTNAME 1

/* have getpagesize() */
#define HAVE_GETPAGESIZE /**/

/* Define to 1 if you have the `getrlimit' function. */
#define HAVE_GETRLIMIT 1

/* have <sys/time.h>, the getrusage() function, the struct rusage type, and
   <sys/resource.h> defines RUSAGE_SELF */
#define HAVE_GETRUSAGE /**/

/* Define to 1 if you have the `getsockopt' function. */
#define HAVE_GETSOCKOPT 1

/* Define if the GNU gettext() function is already present or preinstalled. */
#define HAVE_GETTEXT 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have the iconv() function and it works. */
#define HAVE_ICONV 1

/* Define if your compiler supports the #include_next directive. */
#define HAVE_INCLUDE_NEXT 1

/* Define to 1 if you have the `inet_addr' function. */
#define HAVE_INET_ADDR 1

/* Define to 1 if you have the `inet_ntop' function. */
#define HAVE_INET_NTOP 1

/* Define to 1 if you have the `inet_pton' function. */
#define HAVE_INET_PTON 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `ioctl' function. */
#define HAVE_IOCTL 1

/* <sys/socket.h> defines AF_INET */
#define HAVE_IPV4 /**/

/* <sys/socket.h> defines AF_INET6 */
#define HAVE_IPV6 /**/

/* Define to 1 if you have the `isblank' function. */
#define HAVE_ISBLANK 1

/* Define to 1 if you have the `iswcntrl' function. */
#define HAVE_ISWCNTRL 1

/* Define to 1 if you have the `iswctype' function. */
#define HAVE_ISWCTYPE 1

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
#define HAVE_LANGINFO_CODESET 1

/* Define if your <locale.h> file defines LC_MESSAGES. */
#define HAVE_LC_MESSAGES 1

/* Define to 1 if you have the `sun' library (-lsun). */
/* #undef HAVE_LIBSUN */

/* Define to 1 if you have the <lightning.h> header file. */
/* #undef HAVE_LIGHTNING_H */

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define if you have the 'long double' type. */
#define HAVE_LONG_DOUBLE 1

/* Define to 1 if the system has the type `long long int'. */
#define HAVE_LONG_LONG_INT 1

/* have lstat()? */
#define HAVE_LSTAT /**/

/* have vm_allocate() and task_self() functions */
/* #undef HAVE_MACH_VM */

/* Define to 1 if your system has a GNU libc compatible `malloc' function, and
   to 0 otherwise. */
#define HAVE_MALLOC 1

/* Define if the 'malloc' function is POSIX compliant. */
#define HAVE_MALLOC_POSIX 1

/* Define to 1 if you have the `mbrtowc' function. */
#define HAVE_MBRTOWC 1

/* Define to 1 if you have the `mbsrtowcs' function. */
#define HAVE_MBSRTOWCS 1

/* Define to 1 if <wchar.h> declares mbstate_t. */
#define HAVE_MBSTATE_T 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mempcpy' function. */
#define HAVE_MEMPCPY 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* have <sys/mmap.h> and the mmap() function */
#define HAVE_MMAP /**/

/* <sys/mman.h> defines MAP_ANON and mmaping with MAP_ANON works */
#define HAVE_MMAP_ANON /**/

/* <sys/mman.h> defines MAP_ANONYMOUS and mmaping with MAP_ANONYMOUS works */
#define HAVE_MMAP_ANONYMOUS /**/

/* mmaping of the special device /dev/zero works */
#define HAVE_MMAP_DEVZERO /**/

/* mmaping of the special device /dev/zero works, but only on addresses < 2^29
   */
/* #undef HAVE_MMAP_DEVZERO_SUN4_29 */

/* Define to 1 if you have the `mprotect' function. */
#define HAVE_MPROTECT 1

/* Define to 1 if you have the `msync' function. */
#define HAVE_MSYNC 1

/* Define to 1 if you have the `munmap' function. */
#define HAVE_MUNMAP 1

/* Define to 1 if you have the <netdb.h> header file. */
#define HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#define HAVE_NETINET_IN_H 1

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#define HAVE_NETINET_TCP_H 1

/* Define to 1 if you have the `nice' function. */
#define HAVE_NICE 1

/* <stddef.h> defines the offsetof macro */
#define HAVE_OFFSETOF /**/

/* <errno.h> or <stdio.h> contains a declaration for perror() */
#define HAVE_PERROR_DECL /**/

/* Define to 1 if you have the 'poll' function and it works. */
#define HAVE_POLL 1

/* Define if you have the putenv() function. */
#define HAVE_PUTENV 1

/* have raise() function */
#define HAVE_RAISE /**/

/* have a working modern GNU readline */
/* #undef HAVE_READLINE */

/* Define to 1 if you have the <readline/readline.h> header file. */
/* #undef HAVE_READLINE_READLINE_H */

/* Define to 1 if you have the `readlink' function. */
#define HAVE_READLINK 1

/* Define to 1 if you have the `realpath' function. */
#define HAVE_REALPATH 1

/* have the FIONREAD ioctl() and it works reliably on files */
#define HAVE_RELIABLE_FIONREAD /**/

/* have poll() and it works reliably on files */
#define HAVE_RELIABLE_POLL /**/

/* have select() and it works reliably on files */
#define HAVE_RELIABLE_SELECT /**/

/* Define to 1 if you have the `rl_filename_completion_function' function. */
/* #undef HAVE_RL_FILENAME_COMPLETION_FUNCTION */

/* Define to 1 if the f_fsid member of 'struct statvfs' has a integral type.
   */
#define HAVE_SCALAR_FSID 1

/* Define if you have the select() function. */
#define HAVE_SELECT 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* have <fpu_control.h> and it declares the __setfpucw() function */
#define HAVE_SETFPUCW /**/

/* Define to 1 if you have the `setitimer' function. */
#define HAVE_SETITIMER 1

/* Define to 1 if you have the `setpgid' function. */
#define HAVE_SETPGID 1

/* Define to 1 if you have the `setrlimit' function. */
#define HAVE_SETRLIMIT 1

/* Define to 1 if you have the `setsid' function. */
#define HAVE_SETSID 1

/* Define to 1 if you have the `setsockopt' function. */
#define HAVE_SETSOCKOPT 1

/* Define to 1 if you have the <sgtty.h> header file. */
#define HAVE_SGTTY_H 1

/* have <sys/shm.h> and <sys/ipc.h> and shared memory works */
#define HAVE_SHM /**/

/* Define to 1 if you have the `shutdown' function. */
#define HAVE_SHUTDOWN 1

/* Define to 1 if you have the `sigaction' function. */
#define HAVE_SIGACTION 1

/* Define to 1 if you have the `siginterrupt' function. */
#define HAVE_SIGINTERRUPT 1

/* Define to 1 if 'sig_atomic_t' is a signed integer type. */
/* #undef HAVE_SIGNED_SIG_ATOMIC_T */

/* Define to 1 if 'wchar_t' is a signed integer type. */
/* #undef HAVE_SIGNED_WCHAR_T */

/* Define to 1 if 'wint_t' is a signed integer type. */
/* #undef HAVE_SIGNED_WINT_T */

/* Define if you have the libsigsegv library. */
#define HAVE_SIGSEGV 1

/* Define to 1 if you have the `sigvec' function. */
/* #undef HAVE_SIGVEC */

/* `struct sockaddr_un' from <sys/un.h> has a `sun_len' field */
/* #undef HAVE_SOCKADDR_UN_LEN */

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if `d_namlen' is member of `struct dirent'. */
/* #undef HAVE_STRUCT_DIRENT_D_NAMLEN */

/* Define to 1 if `st_blksize' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BLKSIZE 1

/* Define to 1 if `st_blocks' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BLOCKS 1

/* Define to 1 if `st_rdev' is member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_RDEV 1

/* Define to 1 if you have the <sun/netdb.h> header file. */
/* #undef HAVE_SUN_NETDB_H */

/* Define to 1 if you have the `sysconf' function. */
#define HAVE_SYSCONF 1

/* Define to 1 if you have the <sys/bitypes.h> header file. */
/* #undef HAVE_SYS_BITYPES_H */

/* Define to 1 if you have the <sys/file.h> header file. */
#define HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/inttypes.h> header file. */
/* #undef HAVE_SYS_INTTYPES_H */

/* Define to 1 if you have the <sys/ipc.h> header file. */
#define HAVE_SYS_IPC_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
/* #undef HAVE_SYS_PARAM_H */

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* have <sys/select.h>? */
#define HAVE_SYS_SELECT_H /**/

/* Define to 1 if you have the <sys/shm.h> header file. */
#define HAVE_SYS_SHM_H 1

/* Define to 1 if you have the <sys/statfs.h> header file. */
#define HAVE_SYS_STATFS_H 1

/* Define to 1 if you have the <sys/statvfs.h> header file. */
#define HAVE_SYS_STATVFS_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/sysmacros.h> header file. */
#define HAVE_SYS_SYSMACROS_H 1

/* Define to 1 if you have the <sys/termio.h> header file. */
/* #undef HAVE_SYS_TERMIO_H */

/* Define to 1 if you have the <sys/times.h> header file. */
#define HAVE_SYS_TIMES_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/un.h> header file. */
#define HAVE_SYS_UN_H 1

/* have <sys/utsname.h> and it defines struct utsname */
#define HAVE_SYS_UTSNAME_H /**/

/* have tcgetattr(), either as a function or as a macro defined by <termios.h>
   */
#define HAVE_TCGETATTR /**/

/* <termios.h> defines TCSAFLUSH */
#define HAVE_TCSAFLUSH /**/

/* Define to 1 if you have the <termios.h> header file. */
#define HAVE_TERMIOS_H 1

/* Define to 1 if you have the <termio.h> header file. */
#define HAVE_TERMIO_H 1

/* have the times() function and it returns the real time, but do not have the
   gettimeofday() or ftime() function */
/* #undef HAVE_TIMES_CLOCK */

/* Define to 1 if you have the `ualarm' function. */
#define HAVE_UALARM 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `unsetenv' function. */
#define HAVE_UNSETENV 1

/* Define to 1 if the system has the type `unsigned long long int'. */
#define HAVE_UNSIGNED_LONG_LONG_INT 1

/* Define to 1 if you have the `usleep' function. */
#define HAVE_USLEEP 1

/* have the vadvise() system call */
/* #undef HAVE_VADVISE */

/* Define to 1 if you have the `vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if you have the `wcrtomb' function. */
#define HAVE_WCRTOMB 1

/* Define to 1 if you have the `wcscoll' function. */
#define HAVE_WCSCOLL 1

/* Define to 1 if you have the <wctype.h> header file. */
#define HAVE_WCTYPE_H 1

/* Define if you have the 'wint_t' type. */
#define HAVE_WINT_T 1

/* Define to 1 if you have the `wmemchr' function. */
#define HAVE_WMEMCHR 1

/* Define to 1 if you have the `wmemcpy' function. */
#define HAVE_WMEMCPY 1

/* Define to 1 if you have the `wmempcpy' function. */
#define HAVE_WMEMPCPY 1

/* Define to 1 if `fork' works. */
#define HAVE_WORKING_FORK 1

/* have a working mprotect() function */
#define HAVE_WORKING_MPROTECT /**/

/* Define to 1 if `vfork' works. */
#define HAVE_WORKING_VFORK 1

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* have _setjmp() and _longjmp() */
#define HAVE__JMP /**/

/* Define as const if the declaration of iconv() needs const. */
#define ICONV_CONST 

/* declaration of inet_addr() needs const */
#define INET_ADDR_CONST const

/* Define as .s_addr if the return type of inet_addr() is a struct type, as
   empty if it is a scalar type */
#define INET_ADDR_SUFFIX /**/

/* type of `argument' in ioctl() declaration, if not superseded by dots */
/* #undef IOCTL_ARGUMENT_T */

/* declaration of ioctl() needs dots */
#define IOCTL_DOTS /**/

/* type of `request' in ioctl() declaration */
#define IOCTL_REQUEST_T unsigned long

/* need <linux/in6.h> for the in6_addr and sockaddr_in6 types */
/* #undef IPV6_NEED_LINUX_IN6_H */

/* Define if `link(2)' dereferences symbolic links. */
/* #undef LINK_FOLLOWS_SYMLINKS */

/* longjmp() may return */
/* #undef LONGJMP_RETURNS */

/* address range of malloc() memory */
#define MALLOC_ADDRESS_RANGE 0x0000000001000000UL

/* no <dirent.h>, use <ndir.h> */
/* #undef NDIR */

/* need <sys/filio.h> for using ioctl() FIONREAD */
/* #undef NEED_SYS_FILIO_H */

/* need <sys/ioctl.h> for using ioctl() FIONREAD */
#define NEED_SYS_IOCTL_H /**/

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* need <sys/file.h> for using open() flags like O_RDWR */
/* #undef OPEN_NEEDS_SYS_FILE_H */

/* Name of package */
#define PACKAGE "clisp"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "http://clisp.cons.org/"

/* Define to the full name of this package. */
#define PACKAGE_NAME "GNU CLISP"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "GNU CLISP 2.45 (2008-05-15)"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "clisp"

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.45 (2008-05-15)"

/* type of `pid' in waitpid() declaration */
#define PID_T pid_t

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'ptrdiff_t'. */
/* #undef PTRDIFF_T_SUFFIX */

/* declaration of filename_completion_function() needs const in the first
   argument */
/* #undef READLINE_CONST */

/* The readline built-in filename completion function, either
   rl_filename_completion_function() or filename_completion_function() */
/* #undef READLINE_FILE_COMPLETE */

/* abort() return type */
#define RETABORTTYPE void

/* closedir() return type */
#define RETCLOSEDIRTYPE int

/* return type of getpagesize() */
#define RETGETPAGESIZETYPE int

/* return type of signal handlers (int or void) */
#define RETSIGTYPE void

/* return type of inet_addr() */
#define RET_INET_ADDR_TYPE unsigned int

/* type of `resource' in setrlimit() declaration */
#define RLIMIT_RESOURCE_T enum __rlimit_resource

/* type of `who' in getrusage() declaration */
#define RUSAGE_WHO_T int

/* declaration of select() needs const in the fifth argument */
#define SELECT_CONST 

/* type of `* readfds', `* writefds', `* exceptfds' in select() declaration */
#define SELECT_SET_T fd_set

/* type of `width' in select() declaration */
#define SELECT_WIDTH_T int

/* declaration of setrlimit() needs const */
#define SETRLIMIT_CONST const

/* type of `optval' in setsockopt() declaration */
#define SETSOCKOPT_ARG_T void*

/* declaration of setsockopt() needs const */
#define SETSOCKOPT_CONST const

/* type of `optlen' in setsockopt() declaration */
#define SETSOCKOPT_OPTLEN_T unsigned int

/* address range of shared library code */
#define SHLIB_ADDRESS_RANGE 0x00007EFF46000000UL

/* attaching removed (but alive!) shared memory segments works */
/* #undef SHM_RMID_VALID */

/* signal handlers installed via sigaction() need to be reinstalled when they
   are activated */
/* #undef SIGACTION_NEED_REINSTALL */

/* signals need to be unblocked when signal handlers installed via sigaction()
   are left */
/* #undef SIGACTION_NEED_UNBLOCK */

/* how to block and unblock signals */
#define SIGNALBLOCK_BSD /**/

/* how to block and unblock signals */
#define SIGNALBLOCK_POSIX /**/

/* how to block and unblock signals */
#define SIGNALBLOCK_SYSV /**/

/* signal handlers need to be reinstalled when they are activated */
/* #undef SIGNAL_NEED_REINSTALL */

/* SIGNALBLOCK_BSD is defined above and signals need to be unblocked when
   signal handlers are left */
#define SIGNAL_NEED_UNBLOCK /**/

/* SIGNALBLOCK_BSD is defined above and other signals need to be unblocked
   when signal handlers are left */
/* #undef SIGNAL_NEED_UNBLOCK_OTHERS */

/* declaration of the signal handler function type needs dots */
/* #undef SIGTYPE_DOTS */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'sig_atomic_t'. */
/* #undef SIG_ATOMIC_T_SUFFIX */

/* The size of `dev_t', as computed by sizeof. */
#define SIZEOF_DEV_T 8

/* The size of `fsblkcnt_t', as computed by sizeof. */
#define SIZEOF_FSBLKCNT_T 8

/* The size of `fsfilcnt_t', as computed by sizeof. */
#define SIZEOF_FSFILCNT_T 8

/* The size of `ino_t', as computed by sizeof. */
#define SIZEOF_INO_T 8

/* The size of `off_t', as computed by sizeof. */
#define SIZEOF_OFF_T 8

/* The size of `rlim_t', as computed by sizeof. */
#define SIZEOF_RLIM_T 8

/* The size of `struct timeval', as computed by sizeof. */
#define SIZEOF_STRUCT_TIMEVAL 16

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'size_t'. */
/* #undef SIZE_T_SUFFIX */

/* address range of the C stack */
#define STACK_ADDRESS_RANGE 0x00007FFD50000000UL

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if the `S_IS*' macros in <sys/stat.h> do not work properly. */
/* #undef STAT_MACROS_BROKEN */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* no <dirent.h>, use <sys/dir.h> */
/* #undef SYSDIR */

/* no <dirent.h>, use <sys/ndir.h> */
/* #undef SYSNDIR */

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* define to 1 if the return type of unsetenv is int */
#define UNSETENV_POSIX 0

/* expression in ch which is true if ch is a valid character in filenames */
#define VALID_FILENAME_CHAR ((ch >= 1) && (ch != 47))

/* Version number of package */
#define VERSION "2.45 (2008-05-15)"

/* closedir() return value is void or unusable */
/* #undef VOID_CLOSEDIR */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wchar_t'. */
/* #undef WCHAR_T_SUFFIX */

/* have <termios.h> but need <sys/ioctl.h> for `struct winsize' */
#define WINSIZE_NEED_SYS_IOCTL_H /**/

/* have <termios.h> but need <sys/ptem.h> for `struct winsize' */
/* #undef WINSIZE_NEED_SYS_PTEM_H */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wint_t'. */
/* #undef WINT_T_SUFFIX */

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define to 1 if the X Window System is missing or not being used. */
/* #undef X_DISPLAY_MISSING */

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define if you want regoff_t to be at least as wide POSIX requires. */
#define _REGEX_LARGE_OFFSETS 1

/* Define to 1 if type `char' is unsigned and you are not using gcc.  */
#ifndef __CHAR_UNSIGNED__
/* # undef __CHAR_UNSIGNED__ */
#endif

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Define to a replacement function name for fnmatch(). */
/* #undef fnmatch */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to rpl_malloc if the replacement function should be used. */
/* #undef malloc */

/* Define to a type if <wchar.h> does not define. */
/* #undef mbstate_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to rpl_re_comp if the replacement should be used. */
#define re_comp rpl_re_comp

/* Define to rpl_re_compile_fastmap if the replacement should be used. */
#define re_compile_fastmap rpl_re_compile_fastmap

/* Define to rpl_re_compile_pattern if the replacement should be used. */
#define re_compile_pattern rpl_re_compile_pattern

/* Define to rpl_re_exec if the replacement should be used. */
#define re_exec rpl_re_exec

/* Define to rpl_re_match if the replacement should be used. */
#define re_match rpl_re_match

/* Define to rpl_re_match_2 if the replacement should be used. */
#define re_match_2 rpl_re_match_2

/* Define to rpl_re_search if the replacement should be used. */
#define re_search rpl_re_search

/* Define to rpl_re_search_2 if the replacement should be used. */
#define re_search_2 rpl_re_search_2

/* Define to rpl_re_set_registers if the replacement should be used. */
#define re_set_registers rpl_re_set_registers

/* Define to rpl_re_set_syntax if the replacement should be used. */
#define re_set_syntax rpl_re_set_syntax

/* Define to rpl_re_syntax_options if the replacement should be used. */
#define re_syntax_options rpl_re_syntax_options

/* Define to rpl_regcomp if the replacement should be used. */
#define regcomp rpl_regcomp

/* Define to rpl_regerror if the replacement should be used. */
#define regerror rpl_regerror

/* Define to rpl_regexec if the replacement should be used. */
#define regexec rpl_regexec

/* Define to rpl_regfree if the replacement should be used. */
#define regfree rpl_regfree

/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#define restrict __restrict
/* Work around a bug in Sun C++: it does not support _Restrict, even
   though the corresponding Sun C compiler does, which causes
   "#define restrict _Restrict" in the previous line.  Perhaps some future
   version of Sun C++ will work with _Restrict; if so, it'll probably
   define __RESTRICT, just as Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
#endif

/* Define as `return' if your compiler allows returning expressions of type
   `void' from functions */
#define return_void return

/* Define as a signed type of the same size as size_t. */
/* #undef ssize_t */

/* Define as `fork' if `vfork' does not work. */
/* #undef vfork */

/* Define to, say, char, if your system does not support void type */
/* #undef void */
