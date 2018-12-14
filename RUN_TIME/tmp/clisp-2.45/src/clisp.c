/*
 * Driver program for CLISP.
 *
 * Needed so that we can write #!/usr/local/bin/clisp as the first line
 * of a Lisp script. Normally, only real executables and no shell scripts
 * can be mentioned after #!.
 *
 * Since we are at it, this driver program also implements the "-K" option.
 * All other options are passed to the main program.
 *
 * Bruno Haible 31.3.1997
 * Sam Steingold 1998-2008
 */

/*
 * Macros passed during compilation:
 * LISPLIBDIR  string containing the directory with lisp.run and lispinit.mem
 * LOCALEDIR   string containing the locale directory
 */


/* Declare strlen(), strcpy(), strcat(). */
# include <string.h>
/* Declare stderr. */
# include <stdio.h>


# include <stdlib.h>            /* getenv, abort */
/* needed for execname.c to work */
# include <sys/types.h>         /* stat */
# include <sys/stat.h>          /* stat */
# include <unistd.h>            /* access */
# include <errno.h>             /* ENOMEM, ENOENT, errno */
# include <sys/param.h>         /* MAXPATHLEN */
int find_executable (const char * program_name);
/*
 * Finding the full path of the executable.
 * Bruno Haible 20.12.1994
 * Sam Steingold 2004-2006, 2008
 */

/* This assumes that the executable is not removed or renamed while
   running. */

/* file name of the executable */
static char* executable_name = NULL;
#define default_executable_name  "lisp.run"

/* file descriptor of the executable
 (Only used to verify that we find the correct executable.) */
static int executable_fd = -1;

/* maybe_executable(pathname)
 checks whether a given pathname may belong to the executable. */
static int maybe_executable (const char * filename) {
  struct stat statexe;
  struct stat statfile;
  if (access(filename,R_OK|X_OK) < 0)
    return 0/*false*/;
  if (executable_fd < 0)
    return 1/*true*/;
  /* If we already have an executable_fd, check that filename points to
   the same inode. */
  if (fstat(executable_fd,&statexe) < 0)
    return 1/*true*/;
  if (stat(filename,&statfile) < 0)
    return 0/*false*/;
  if (statfile.st_dev
      && statfile.st_dev == statexe.st_dev
      && statfile.st_ino == statexe.st_ino)
    return 1/*true*/;
  return 0/*false*/;
}

/* return the executable name */
char *get_executable_name (void);
char *get_executable_name (void) { return executable_name; }

/* find_executable(program_name)
 is to be called immediately after the program starts,
 with program_name = argv[0],
 before any chdir() operation and before any setenv("PATH",...).
 It determines the full program path and opens a file descriptor to
 the executable, for later use.
 Return value is 0 if successful, -1 and errno set if not. */
int find_executable (const char * program_name) {
  /* Do not need to execute this more than once. */
  if (executable_name != NULL) return 0;
 #if defined(UNIX_LINUX) || defined(UNIX_CYGWIN32)
  { /* The executable is accessible as /proc/<pid>/exe. We try this first
   because it is safer: no race condition w.r.t. the file system. It may
   fail, however, if the user has not compiled /proc support into his
   kernel. */
    int fd = open("/proc/self/exe",O_RDONLY,my_open_mask);
    if (fd >= 0)
      executable_fd = fd;
  }
 #endif
  { /* Now we guess the executable's full path. We assume the executable
   has been called via execlp() or execvp() with properly set up argv[0].
   The login(1) convention to add a '-' prefix to argv[0] is not supported. */
    const char * p;
    for (p = program_name; *p; p++)
      if (*p == '/')
        goto has_slash;
  }
  { /* exec searches paths without slashes in the directory list given
       by $PATH. */
    const char * path = getenv("PATH");
    if (!(path==NULL)) {
      const char * p;
      const char * p_next;
      for (p = path; *p; p = p_next) {
        const char * q;
        unsigned long p_len;
        for (q = p; *q; q++) { if (*q == ':') break; }
        p_len = q-p; p_next = (*q=='\0' ? q : q+1);
        { /* We have a path item at p, of length p_len.
             Now concatenate the path item and program_name. */
          char * concat_name =
            (char*) malloc(p_len + strlen(program_name) + 2);
          if (concat_name == NULL) { errno = ENOMEM; goto notfound; }
          if (p_len == 0) {
            /* empty PATH element designates the current directory */
            strcpy(concat_name,program_name);
          } else {
            memcpy(concat_name, p, p_len);
            concat_name[p_len] = '/';
            strcpy(concat_name+p_len+1, program_name);
          }
          if (maybe_executable(concat_name)) {
            /* Assume we have found the executable */
            program_name = concat_name; goto resolve;
          }
          free(concat_name);
        }
      }
    }
    /* Not found in the PATH, assume the current directory. */
  }
 has_slash:
  /* exec treats paths containing slashes as relative to the current
     directory */
  if (maybe_executable(program_name)) {
   resolve:
    /* resolve program_name: */
#  if !defined(MAXPATHLEN)
#   define MAXPATHLEN 1024      /* see unix.d */
#  endif
    executable_name = (char*) malloc(MAXPATHLEN);
    if (executable_name == NULL) { errno = ENOMEM; goto notfound; }
    if (realpath(program_name,executable_name) == NULL) {
      free(executable_name); goto notfound;
    }
    return 0;
  }
  errno = ENOENT;
 notfound:
  executable_name = (char*)default_executable_name; return -1;
}



int main (int argc, char* argv[])
{
  char* lisplibdir;
  char* localedir;
  char* argv_lisplibdir = NULL;
  char* argv_linkingset = "base";
  char* argv_memfile = NULL;
  char* argv_localedir = NULL;
  char* program_name;
  /*
   * To determine whether -K was given, we go through the options.
   * Because when "clisp foo.lisp -K" is invoked, the "-K" is an argument
   * for foo.lisp, not for clisp. Therefore we have to stop when we encounter
   * the first non-option, non-option-argument. Unfortunately, we have to
   * know about all of clisp's options.
   */
  program_name = argv[0];
  /*
   * The program_name may be absolute or relative if "clisp" is called
   * directly. (For example, "sh /usr/local/bin/clisp ..." will make it
   * absolute, "time clisp ..." will make it relative.)
   * If "clisp" is used as a script interpreter, program_name will be
   * - the full absolute pathname, on SunOS 4, Solaris, HP-UX, IRIX,
   * - only the basename, on Linux, AIX, OSF/1.
   * It follows that we cannot tell whether we have been called as
   * script interpreter or directly.
   */
# if ENABLE_RELOCATABLE
  /* Put this executable's absolute path into executable_name. */
  if (find_executable(program_name) < 0) {
    fprintf(stderr,"%s: cannot figure out the absolute executable path",
            program_name);
    return 1;
  }
  /* Figure out lisplibdir and localedir. */
  {
    unsigned int libdir_len;
    const char *p;
    char *mem;
    /* The libdir is determined as `dirname $executable_name`. */
    for (p = executable_name + strlen(executable_name);; p--) {
      if (p == executable_name) abort();
      if (*p == '/') break;
    }
    libdir_len = p - executable_name;
    mem = (char*)malloc((libdir_len+1)+(libdir_len+7+1));
    if (mem == NULL) goto oom;
    lisplibdir = mem;
    localedir = mem + (libdir_len+1);
    /* Compute lisplibdir from it. */
    memcpy(lisplibdir,executable_name,libdir_len);
    lisplibdir[libdir_len] = '\0';
    /* Compute localedir from it. */
    memcpy(localedir,executable_name,libdir_len);
    localedir[libdir_len] = *p; /* directory separator */
    memcpy(localedir+libdir_len+1,"locale",6);
    localedir[libdir_len+7] = '\0';
  }
# else
  lisplibdir = LISPLIBDIR;
  localedir = LOCALEDIR;
# endif
  /*
   * Script execution on Unix is implemented like this:
   * - The basename/fullname of the interpreter is put into argv[0].
   * - (Optional - only if at least one interpreter-arg is present.) Next
   *   comes the tail of the "#!..." line. On SunOS 4, Linux, IRIX, AIX,
   *   OSF/1: with leading whitespace stripped, but whitespace inside it
   *   untouched (!). On Solaris, HP-UX: with leading whitespace stripped,
   *   and cut off at the next whitespace character (!!).
   * - Next comes the filename of the script.
   * - Then all the arguments of the script.
   * We therefore need to split argv[1] into pieces. We shouldn't split
   * "-x" arguments into pieces. However, fortunately, the "-x" argument
   * cannot appear as argv[1] (because "-x" must precede it), and it
   * cannot appear within the "#!..." line (because "#!" and "-x" are
   * mutually exclusive).
   * As a workaround against the Solaris/HP-UX problem, we split not
   * only at normal spaces, but also at hard spaces.
   * See <impnotes.html#quickstart>.
   */
  if (argc > 1) {
    int wordcount = 0; /* number of pieces in argv[1] */
    { char* arg = argv[1];
      int inword = 0;
      while (*arg != '\0') {
        int spacep = (*arg == '\t' || *arg == ' ' || *arg == (char)0xA0);
        if (!inword && !spacep) wordcount++;
        inword = !spacep;
        arg++;
      }
    }
    {int old_argc = argc;
     char** old_argv = argv;
     int new_argc = argc + wordcount - 1;
     char** new_argv = (char**) malloc((new_argc+1)*sizeof(char*));
     if (!new_argv) goto oom;
     argc = new_argc;
     argv = new_argv;
     /* Copy argv[0] unmodified. */
     *new_argv++ = *old_argv++;
     { /* Split argv[1] into pieces. */
       char* arg = *old_argv++;
       int inword = 0;
       while (*arg != '\0') {
         int spacep = (*arg == '\t' || *arg == ' ' || *arg == (char)0xA0);
         if (!inword) {
           if (!spacep) { *new_argv++ = arg; }
         } else {
           if (spacep) { *arg = '\0'; }
         }
         inword = !spacep;
         arg++;
       }
     }
     { /* Copy argv[2..argc-1] unmodified. */
       int i;
       for (i = old_argc-2; i > 0; i--) { *new_argv++ = *old_argv++; }
     }
     *new_argv = NULL;
    }
  }
  /*
   * Done with script interpreter argument handling.
   */
  { char** argptr = &argv[1];
    char** argptr_limit = &argv[argc];
    enum { for_exec, for_init, for_compile } argv_for = for_exec;
    while (argptr < argptr_limit) {
      char* arg = *argptr++;
      if ((arg[0] == '-') && !(arg[1] == '\0')) {
        switch (arg[1]) {
#        define OPTION_ARG  \
          if (arg[2] == '\0') \
            { if (argptr < argptr_limit) arg = *argptr++; else goto usage; } \
          else { arg = &arg[2]; }
          /* Options to which we have to pay attention. */
          case 'B':
            OPTION_ARG;
            lisplibdir = argv_lisplibdir = arg;
            break;
          case 'K':
            OPTION_ARG;
            argv_linkingset = arg;
            break;
          case 'M':
            OPTION_ARG;
            argv_memfile = arg;
            break;
          case 'N':
            OPTION_ARG;
            argv_localedir = arg;
            break;
          /* Skippable options without arguments. */
          case 'h':
          case 'd':
          case 'q':
          case 'I':
          case 'C':
          case 'l':
          case 'a':
          case 'w':
          case 'n': /* -norc */
          case 'r': /* -repl */
          case 'v':
            break;
          case '-':
            if (arg[2] == '\0') goto done_options; /* --: end of arguments */
            else break;   /* GNU-style long options --help, --version */
          /* Skippable options with arguments. */
          case 'm':
          case 't':
          case 'L':
          case 'o':
          case 'p':
          case 'x':
            OPTION_ARG;
            break;
          case 'E':
            if (argptr < argptr_limit) argptr++; else goto usage;
            break;
          case 'i':
            if (arg[2] == '\0') argv_for = for_init;
            break;
          case 'c':
            argv_for = for_compile;
            break;
          default:
            goto usage;
        }
      } else {
        switch (argv_for) {
          case for_init:
          case for_compile:
            break;
          case for_exec:
            /* All the remaining options are for the Lisp program. */
            goto done_options;
        }
      }
    }
   done_options: ;
  }
  { char* linkingsetdir;
    char* executable;
    char** new_argv;
    /* Compute linking set. */
    if (argv_linkingset == NULL || strlen(argv_linkingset)==0) {
      linkingsetdir = lisplibdir;
    } else if (argv_linkingset[0]=='/') {
      linkingsetdir = argv_linkingset;
    } else {
      linkingsetdir =
        (char*)malloc(strlen(lisplibdir)+1+strlen(argv_linkingset)+1);
      if (!linkingsetdir) goto oom;
      strcpy(linkingsetdir, lisplibdir);
      strcat(linkingsetdir, "/");
      strcat(linkingsetdir, argv_linkingset);
    }
    { /* Compute executable's name. */
      char* execname = "lisp.run";
      executable = (char*)malloc(strlen(linkingsetdir)+1+strlen(execname)+1);
      if (!executable) goto oom;
      strcpy(executable, linkingsetdir);
      strcat(executable, "/");
      strcat(executable, execname);
    }
    /* Compute new arguments. */
    new_argv = (char**)malloc((argc+6+1)*sizeof(char*));
    if (!new_argv) goto oom;
    new_argv[0] = executable;
    { char** argptr = &argv[1];
      char** argptr_limit = &argv[argc];
      char** new_argptr = &new_argv[1];
      if (!argv_lisplibdir) {
        *new_argptr++ = "-B";
        *new_argptr++ = lisplibdir;
      }
      if (!argv_memfile) {
        char* filename = "lispinit.mem";
        argv_memfile =
          (char*)malloc(strlen(linkingsetdir)+1+strlen(filename)+1);
        if (!argv_memfile) goto oom;
        strcpy(argv_memfile, linkingsetdir);
        strcat(argv_memfile, "/");
        strcat(argv_memfile, filename);
        *new_argptr++ = "-M";
        *new_argptr++ = argv_memfile;
      }
      if (!argv_localedir) {
        *new_argptr++ = "-N";
        *new_argptr++ = localedir;
      }
      while (argptr < argptr_limit) { *new_argptr++ = *argptr++; }
      *new_argptr = NULL;
    }
    /* Launch the executable. */
    execv(executable,new_argv);
    { /* execv() returns only if there was an error. */
      int saved_errno = errno;
      fprintf(stderr,"%s: ",program_name);
      errno = saved_errno; perror(executable);
    }
    return 1;
  }
  usage: {
    fprintf(stderr,"%s: invalid command-line option, try `%s --help'\n",
            program_name,program_name);
    return 1;
  }
  oom: {
    fprintf(stderr,"%s: out of memory\n",program_name);
    return 1;
  }
}
