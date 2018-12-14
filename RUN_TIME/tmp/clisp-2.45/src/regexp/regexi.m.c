#line 1 "regexi.c"
/*
 * CLISP interface to GNU regex
 * originally by Bruno Haible 14.4.1995
 * rewritten by Sam Steingold 2003-08-06
 */

#include "clisp.h"
#include <sys/types.h>          /* regex.h needs this */
#include <stdlib.h>             /* declare malloc(), free() */
#include <stdio.h>              /* BUFSIZ */
#include "regex.h"

#ifndef FOREIGN
#error FOREIGN is not defined.
#error REGEXP needs a CLISP built with the foreign pointer datatype support.
#error Go into the main CLISP makefile and add a -DFOREIGN=void*
#error to CFLAGS make variable and rebuild CLISP before coming back here.
#endif
#ifndef HAVE_ALLOCA
/* clisp.h probably defines alloca... */
#endif

DEFMODULE(regexp,"REGEXP")

#define O(varname) module__regexp__object_tab._##varname
#define F(varname) subr_tab_ptr_as_object(&(module__regexp__subr_tab._##varname))

struct module__regexp__object_tab_t {
  gcv_object_t _object_Kboolean;
  gcv_object_t _object_Kend;
  gcv_object_t _object_Kextended;
  gcv_object_t _object_Kignore_case;
  gcv_object_t _object_Knewline;
  gcv_object_t _object_Knosub;
  gcv_object_t _object_Knotbol;
  gcv_object_t _object_Knoteol;
  gcv_object_t _object_Kstart;
  gcv_object_t _object_regexp__make_match_boa;
} module__regexp__object_tab;
uintC module__regexp__object_tab_size = sizeof(module__regexp__object_tab)/sizeof(gcv_object_t);

struct module__regexp__object_tab_initdata_t {
  object_initdata_t _object_Kboolean;
  object_initdata_t _object_Kend;
  object_initdata_t _object_Kextended;
  object_initdata_t _object_Kignore_case;
  object_initdata_t _object_Knewline;
  object_initdata_t _object_Knosub;
  object_initdata_t _object_Knotbol;
  object_initdata_t _object_Knoteol;
  object_initdata_t _object_Kstart;
  object_initdata_t _object_regexp__make_match_boa;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__regexp__object_tab_initdata = {
  { ":BOOLEAN" },
  { ":END" },
  { ":EXTENDED" },
  { ":IGNORE-CASE" },
  { ":NEWLINE" },
  { ":NOSUB" },
  { ":NOTBOL" },
  { ":NOTEOL" },
  { ":START" },
  { "REGEXP::MAKE-MATCH-BOA" },
  0
};

struct module__regexp__subr_tab_t {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  subr_t _subr_regexp_regexp_compile;
  subr_t _subr_regexp_regexp_exec;
  subr_t _subr_regexp_regexp_free;
  int _dummy_to_avoid_trailing_comma_in_initializer;
};
extern struct module__regexp__subr_tab_t module__regexp__subr_tab;

static uintL regexp_compile_flags (void) {
  uintL flags = 0
#  ifdef REG_NOSUB
    | (missingp(STACK_(0)) ? 0 : REG_NOSUB)
#  endif
#  ifdef REG_NEWLINE
    | (missingp(STACK_(1)) ? 0 : REG_NEWLINE)
#  endif
#  ifdef REG_ICASE
    | (missingp(STACK_(2)) ? 0 : REG_ICASE)
#  endif
#  ifdef REG_EXTENDED
    | (missingp(STACK_(3)) ? 0 : REG_EXTENDED)
#  endif
   ;
  skipSTACK(4);
  return flags;
}

static uintL regexp_exec_flags (void) {
  uintL flags = 0
#  ifdef REG_NOTEOL
    | (missingp(STACK_(0)) ? 0 : REG_NOTEOL)
#  endif
#  ifdef REG_NOTBOL
    | (missingp(STACK_(1)) ? 0 : REG_NOTBOL)
#  endif
   ;
  skipSTACK(2);
  return flags;
}



#line 24


DEFUN(REGEXP::REGEXP-COMPILE, pattern &key EXTENDED IGNORE-CASE NEWLINE NOSUB,(subr_regexp_regexp_compile,seclass_default,1,0,norest,key,4,NIL))
{ /* compile the pattern into a regular expression */
  int cflags = regexp_compile_flags();
  object pattern = check_string(popSTACK());
  int status;
  regex_t* re;
 restart_regcomp:
  re = (regex_t*)my_malloc(sizeof(regex_t));
  with_string_0(pattern,GLO(misc_encoding),patternz, {
    begin_system_call();
    status = regcomp(re,patternz,cflags);
    end_system_call();
  });
  if (status) {
    char buf[BUFSIZ];
    begin_system_call();
    regerror(status,re,buf,BUFSIZ);
    free(re);
    end_system_call();
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(NIL); pushSTACK(pattern);
    STACK_1 = asciz_to_string(buf,GLO(misc_encoding));
    pushSTACK(TheSubr(subr_self)->name);
    check_value(error_condition,"~S (~S): ~S");
    pattern = value1;
    goto restart_regcomp;
  }
  pushSTACK(allocate_fpointer((FOREIGN)re));
  pushSTACK(STACK_0);pushSTACK(F(subr_regexp_regexp_free));funcall(L(finalize),2);
  VALUES1(popSTACK());          /* foreign pointer */
}

DEFUN(REGEXP::REGEXP-FREE, compiled,(subr_regexp_regexp_free,seclass_default,1,0,norest,nokey,0,NIL))
{ /* release the contents and the data of the compiled pattern */
  object fp = popSTACK();
  if (fpointerp(fp) && fp_validp(TheFpointer(fp))) {
    regex_t *re = (regex_t*)TheFpointer(fp)->fp_pointer;
    if (re) {
      regfree(re); free(re);
      TheFpointer(fp)->fp_pointer = NULL;
      mark_fp_invalid(TheFpointer(fp));
      VALUES1(T);
    } else VALUES1(NIL);
  } else VALUES1(NIL);
}


DEFUN(REGEXP::REGEXP-EXEC,pattern string &key BOOLEAN :START :END NOTBOL NOTEOL,(subr_regexp_regexp_exec,seclass_default,2,0,norest,key,5,NIL))
{ /* match the compiled pattern against the string */
  int eflags = regexp_exec_flags();
  object string = (STACK_3 = check_string(STACK_3));
  unsigned int length = vector_length(string);
  unsigned int start = check_uint_defaulted(STACK_1,0);
  unsigned int end = check_uint_defaulted(STACK_0,length);
  int status;
  bool bool_p = !missingp(STACK_2);
  regex_t *re;
  regmatch_t *ret;
  skipSTACK(3);                 /* drop all options */
  for (;;) {
    STACK_1 = check_fpointer(STACK_1,true);
    re = (regex_t*)TheFpointer(STACK_1)->fp_pointer;
    if (re != NULL) break;
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(STACK_(1+1)); pushSTACK(TheSubr(subr_self)->name);
    check_value(error_condition,GETTEXT("~S: NULL pattern ~S"));
    STACK_1 = value1;
  }
  string = STACK_0;
  if (end != length || start != 0) {
    pushSTACK(sfixnum((int)(end-start)));
    pushSTACK(S(Kelement_type)); pushSTACK(S(character));
    pushSTACK(S(Kdisplaced_to)); pushSTACK(string);
    pushSTACK(S(Kdisplaced_index_offset)); pushSTACK(posfixnum(start));
    funcall(L(make_array),7);
    string = value1;
  }
  begin_system_call();
  ret = (regmatch_t*)alloca((re->re_nsub+1)*sizeof(regmatch_t));
  end_system_call();
  if (ret == NULL) OS_error();
  with_string_0(string,GLO(misc_encoding),stringz, {
    begin_system_call();
    status = regexec(re,stringz,re->re_nsub+1,ret,eflags);
    end_system_call();
  });
  if (status) {
    VALUES0;
  } else if (bool_p) {
    VALUES1(T);                 /* success indicator */
  } else {
    int count;
    for (count = 0; count <= re->re_nsub; count++)
      if (ret[count].rm_so >= 0 && ret[count].rm_eo >= 0) {
        pushSTACK(posfixnum(start+ret[count].rm_so));
        pushSTACK(posfixnum(start+ret[count].rm_eo));
        funcall(O(object_regexp__make_match_boa),2); pushSTACK(value1);
      } else pushSTACK(NIL);
    funcall(L(values),re->re_nsub+1);
  }
  skipSTACK(2);                 /* drop pattern & string */
}


struct module__regexp__subr_tab_t module__regexp__subr_tab
  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(__GNUC__)
    __attribute__ ((aligned (varobject_alignment)))
  #endif
  = {
  #if varobjects_misaligned
  { 0 },
  #endif
  LISPFUN_F(subr_regexp_regexp_compile,seclass_default,1,0,norest,key,4,NIL)
  LISPFUN_F(subr_regexp_regexp_exec,seclass_default,2,0,norest,key,5,NIL)
  LISPFUN_F(subr_regexp_regexp_free,seclass_default,1,0,norest,nokey,0,NIL)
  0
};
uintC module__regexp__subr_tab_size = (sizeof(struct module__regexp__subr_tab_t)-varobjects_misaligned-sizeof(int))/sizeof(subr_t);

struct module__regexp__subr_tab_initdata_t {
  subr_initdata_t _subr_regexp_regexp_compile;
  subr_initdata_t _subr_regexp_regexp_exec;
  subr_initdata_t _subr_regexp_regexp_free;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__regexp__subr_tab_initdata = {
  { "REGEXP", "REGEXP-COMPILE" },
  { "REGEXP", "REGEXP-EXEC" },
  { "REGEXP", "REGEXP-FREE" },
  0
};

void module__regexp__init_function_1 (module_t* module);
void module__regexp__init_function_1 (module_t* module)
{
  pushSTACK(O(object_Kextended));
  pushSTACK(O(object_Kignore_case));
  pushSTACK(O(object_Knewline));
  pushSTACK(O(object_Knosub));
  module__regexp__subr_tab._subr_regexp_regexp_compile.keywords = vectorof(4);
  pushSTACK(O(object_Kboolean));
  pushSTACK(O(object_Kstart));
  pushSTACK(O(object_Kend));
  pushSTACK(O(object_Knotbol));
  pushSTACK(O(object_Knoteol));
  module__regexp__subr_tab._subr_regexp_regexp_exec.keywords = vectorof(5);
}

void module__regexp__init_function_2 (module_t* module);
void module__regexp__init_function_2 (module_t* module)
{
}

void module__regexp__fini_function (module_t* module);
void module__regexp__fini_function (module_t* module)
{
}
