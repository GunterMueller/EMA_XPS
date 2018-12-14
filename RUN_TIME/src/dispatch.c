/****************************************************
*   dispatch.c    Hans Groschwitz        06.10.94   *
*                                                   *
*   Dispatcher for User Defined Message Handlers    *
#   -DRUNTIME for rema-xps version                  *
*                                                   *
*   Channels 0-9 are reserved for special purposes! *
*   Channels 10-999 may be used by the programmer!  *
*                                                   *
*   BUGS:                                           *
*     -                                             *
****************************************************/

#include <stdio.h>
#include <Xm/Xm.h>         /* Boolean, Widget, ... */

#include "dispatch.h"
#include "uid.h"
#include "ui.h"
#include "explain.h"
#include "sysd.h"
#include "expd.h"

#ifndef RUNTIME
#include "trace.h"
#include "rule.h"
#include "prolog.h"
#include "constraint.h"
#include "csnet.h"
#include "restriction.h"
#include "frame.h"
#include "instance.h"
#include "behavior.h"
#include "task.h"
#include "misc.h"
#endif /* RUNTIME */

/*** Globals ***************************************/

/*
 * User Channels:
 * NULL = no handler registered for this message
 *
 * Use MSG_read(); to cleanly retrieve the message!
 */
FPTR dispatch[990] = {
   /*--------------------UIDs-------------*/
   readUidInformation,		   /* #10 */
   readUidAcceptCancel,   	   /* #11 */
   readUidYesNoCancel,    	   /* #12 */
   readUidPrompt,         	   /* #13 */
   readUidOneOfMany,      	   /* #14 */
   readUidSomeOfMany,     	   /* #15 */
   NULL,                           /* #16 */
   NULL,                           /* #17 */
   NULL,                           /* #18 */
   NULL,                           /* #19 */
   /*------------------UImisc-------------*/
   UiDestroyWidget,                /* #20 */
   UiRaiseToTop,                   /* #21 */
   UiLowerToBottom,                /* #22 */
   UiCreateSimpleSessionScreen,    /* #23 */
   UiStopSession,                  /* #24 */
   UiSave,          		   /* #25 */
   NULL,                           /* #26 */
   NULL,                           /* #27 */
   NULL,                           /* #28 */
   NULL,                           /* #29 */
   /*----------------UIcreate-------------*/
   UiCreatePushbutton,    	   /* #30 */
   UiCreateLabel,         	   /* #31 */
   UiCreateBitmapButton,  	   /* #32 */
   UiCreateBitmapLabel,   	   /* #33 */
   UiCreateInputText,     	   /* #34 */
   UiCreateOutputText,    	   /* #35 */
   UiCreatePulldownMenu,  	   /* #36 */
   UiCreateMenuEntry,     	   /* #37 */
   UiCreateSeparator,     	   /* #38 */
   NULL,                           /* #39 */
   /*-------------------------------------*/
   NULL,                           /* #40 */
   NULL,                           /* #41 */
   NULL,                           /* #42 */
   NULL,                           /* #43 */
   NULL,                           /* #44 */
   NULL,                           /* #45 */
   NULL,                           /* #46 */
   NULL,                           /* #47 */
   NULL,                           /* #48 */
   NULL,                           /* #49 */
   /*-------------------UIset-------------*/
   UiSetSessionBg,                 /* #50 */
   UiSetMenuBg,                    /* #51 */
   UiSetMenuFg,                    /* #52 */
   UiSetMenuFont,         	   /* #53 */
   UiSetVisibility,       	   /* #54 */
   UiSetSensitivity,      	   /* #55 */
   UiSetForeground,       	   /* #56 */
   UiSetBackground,       	   /* #57 */
   UiSetFont,                      /* #58 */
   UiSetBitmap,                    /* #59 */
   /*-------------------------------------*/
   UiSetGeometry,                  /* #60 */
   UiSetAlignment,                 /* #61 */
   UiSetText,                      /* #62 */
   UiAddText,                      /* #63 */
   UiSetCallback,                  /* #64 */
   UiSetMnemonic,                  /* #65 */
   NULL,                           /* #66 */
   NULL,                           /* #67 */
   NULL,                           /* #68 */
   NULL,                           /* #69 */
   /*-------------------UIget-------------*/
   UiGetSessionBg,                 /* #70 */
   UiGetMenuBg,                    /* #71 */
   UiGetMenuFg,                    /* #72 */
   UiGetMenuFont,                  /* #73 */
   UiGetVisibility,                /* #74 */
   UiGetSensitivity,               /* #75 */
   UiGetForeground,                /* #76 */
   UiGetBackground,                /* #77 */
   UiGetFont,                      /* #78 */
   UiGetBitmap,           	   /* #79 */
   /*-------------------------------------*/
   UiGetGeometry,           	   /* #80 */
   UiGetAlignment,           	   /* #81 */
   UiGetText,           	   /* #82 */
   UiGetCallback,           	   /* #83 */
   UiGetMnemonic,          	   /* #84 */
   NULL,                           /* #85 */
   NULL,                           /* #86 */
   NULL,                           /* #87 */
   NULL,                           /* #88 */
   NULL,                           /* #89 */
   /*--------------------SYSd-------------*/
   readSysdWarning,                /* #90 */
   readSysdInformation,            /* #91 */
   readSysdAcceptCancel,           /* #92 */
   readSysdPrompt,                 /* #93 */
   NULL,                           /* #94 */
   NULL,                           /* #95 */
   NULL,                           /* #96 */
   NULL,                           /* #97 */
   NULL,                           /* #98 */
   NULL,                           /* #99 */
   /*-------------------------------------*/
   NULL,                          /* #100 */
   NULL,                          /* #101 */
   NULL,                          /* #102 */
   NULL,                          /* #103 */
   NULL,                          /* #104 */
   NULL,                          /* #105 */
   NULL,                          /* #106 */
   NULL,                          /* #107 */
   NULL,                          /* #108 */
   NULL,                          /* #109 */
   /*-----------------Explain-------------*/
   ExplainMessage,                /* #110 */
   readExpdOneOfMany,             /* #111 */
   readExpdOneOfMany2,            /* #112 */
   readExpdSomeOfMany,            /* #113 */
   ExpdMessage,                   /* #114 */
   NULL,                          /* #115 */
   NULL,                          /* #116 */
   NULL,                          /* #117 */
   NULL,                          /* #118 */
   NULL,                          /* #119 */
#ifndef RUNTIME
   /*-------------------Trace-------------*/
   TraceMessage,                  /* #120 */
   TraceTaskToggle,               /* #121 */
   TraceSystemToggle,             /* #122 */
   TraceRuleToggle,               /* #123 */
   TracePrologToggle,             /* #124 */
   TraceConstraintToggle,         /* #125 */
   NULL,                          /* #126 */
   NULL,                          /* #127 */
   NULL,                          /* #128 */
   NULL,                          /* #129 */
   /*-------------------Frame-------------*/
   emaxps_frame_definition,       /* #130 */
   emaxps_frame_check_on,         /* #131 */
   emaxps_frame_selection,        /* #132 */
   emaxps_frame_delete,           /* #133 */     
   emaxps_instance_definition,    /* #134 */
   emaxps_inst_frame_select,      /* #135 */
   emaxps_inst_instance_select,   /* #136 */
   emaxps_behavior_definition,    /* #137 */
   emaxps_behav_frame_select,     /* #138 */
   emaxps_behav_behavior_select,  /* #139 */
   /*-------------------------------------*/
   NULL,                          /* #140 */
   NULL,                          /* #141 */
   NULL,                          /* #142 */
   NULL,                          /* #143 */
   NULL,                          /* #144 */
   NULL,                          /* #145 */
   NULL,                          /* #146 */
   NULL,                          /* #147 */
   NULL,                          /* #148 */
   NULL,                          /* #149 */
   /*--------------------Rule-------------*/
   ruleGetRsetsOfKB,              /* #150 */
   ruleGetRulesOfRset,            /* #151 */
   ruleGetBodyOfRule,             /* #152 */
   ruleGetRulesOfRset2,           /* #153 */
   NULL,                          /* #154 */
   NULL,                          /* #155 */
   NULL,                          /* #156 */
   NULL,                          /* #157 */
   NULL,                          /* #158 */
   NULL,                          /* #159 */
   /*-------------------------------------*/
   NULL,                          /* #160 */
   NULL,                          /* #161 */
   NULL,                          /* #162 */
   NULL,                          /* #163 */
   NULL,                          /* #164 */
   NULL,                          /* #165 */
   NULL,                          /* #166 */
   NULL,                          /* #167 */
   NULL,                          /* #168 */
   NULL,                          /* #169 */
   /*------------------Prolog-------------*/
   emaxps_prolog_klauselmengen,   /* #170 */
   emaxps_prolog_select,          /* #171 */
   emaxps_new_axioms,             /* #172 */
   emaxps_new_kbaxiom,            /* #173 */
   emaxps_del_kbaxiom,            /* #174 */
   emaxps_reset_prolog_of_kb,     /* #175 */
   NULL,                          /* #176 */
   NULL,                          /* #177 */
   NULL,                          /* #178 */
   NULL,                          /* #179 */
   /*-------------------------------------*/
   NULL,                          /* #180 */
   NULL,                          /* #181 */
   NULL,                          /* #182 */
   NULL,                          /* #183 */
   NULL,                          /* #184 */
   NULL,                          /* #185 */
   NULL,                          /* #186 */
   NULL,                          /* #187 */
   NULL,                          /* #188 */
   NULL,                          /* #189 */
   /*------------------Consat-------------*/
   emaxps_constraint_def,         /* #190 */
   emaxps_constraint_sel,         /* #190 */
   emaxps_restriction_def,        /* #192 */
   emaxps_restriction_sel,        /* #193 */
   emaxps_csnet_def,              /* #194 */
   emaxps_csnet_sel,              /* #195 */
   NULL,                          /* #196 */
   NULL,                          /* #197 */
   NULL,                          /* #198 */
   NULL,                          /* #199 */
   /*-------------------------------------*/
   NULL,                          /* #200 */
   NULL,                          /* #201 */
   NULL,                          /* #202 */
   NULL,                          /* #203 */
   NULL,                          /* #204 */
   NULL,                          /* #205 */
   NULL,                          /* #206 */
   NULL,                          /* #207 */
   NULL,                          /* #208 */
   NULL,                          /* #209 */
   /*--------------------Task-------------*/
   emaxps_task_list_tasks,        /* #210 */
   emaxps_task_send_body,         /* #211 */
   emaxps_misc_send_others,       /* #212 */
   NULL,                          /* #213 */
   NULL,                          /* #214 */
   NULL,                          /* #215 */
   NULL,                          /* #216 */
   NULL,                          /* #217 */
   NULL,                          /* #218 */
   NULL,                          /* #219 */
   /*-------------------------------------*/
   NULL,                          /* #220 */
   NULL,                          /* #221 */
   NULL,                          /* #222 */
   NULL,                          /* #223 */
   NULL,                          /* #224 */
   NULL,                          /* #225 */
   NULL,                          /* #226 */
   NULL,                          /* #227 */
   NULL,                          /* #228 */
   NULL,                          /* #229 */
#endif /* RUNTIME */
   /*----------------Reserved-------------*/
};

/*** EOF *******************************************/
