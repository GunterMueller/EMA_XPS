###############################################
#   Makefile          EMA-XPS         C-src   #
#                                             #
#   See the file ../Makefile for hints on     #
#   making a binary distribution              #
#                                             #
#   See the file ../util/PORTING for hints    #
#   on adapting to environments not already   #
#   tested by me                              #
###############################################

### Should be changed to the name of
### the LISP Process used (clisp: lisp.run)
### as seen from $(EMAKILL) ...
###    ## hg20080629: war lisp.run
EMALISP = clisp

### Used for publishing in the WWW only ...
### my private settings   (using $$HOME failed!)
###
MIRPATH = ../mirror
EXPPATH = ../../exports

MIR_BAB = Babylon-2.3.tar.gz
MIRCLSP = clisp-2.45.tar.bz2

TGZFILE = emaxps21.tgz
TARFILE = emaxps21.tar

### HTRPATH = HTTP Root Path (local web export dir)
### if relative, add ../ to webdoc!
#
#HTRPATH = ../tmp/http-root
HTRPATH = /D/Hans/MyData/ema-xps/http-root

############### do not edit ###################

CC      = ../util/cc_X11
CCOPTS  = 
BIND    = ../util/ld_X11

HELP_CV = ../util/help-cv
HELP_SH = ../util/help-cv.sh
XRES_SH = ../util/xres-cv.sh
EMAKILL = ../util/ema-kill.sh

CP      = cp
RM      = rm
MV      = mv
TAR     = tar
GZIP    = gzip -9
LS      = ls

#################

EMAROOT = ..
EMAHOME = ../EMA-XPS/lib
EMAKBSD = ../EMA-XPS/kbs
EMADOCD = ../doc

EMA_XPS = $(EMAHOME)/ema-xps
REMAXPS = $(EMAHOME)/rema-xps

L_IMAGE = $(EMAHOME)/ema-xps.mem
RLIMAGE = $(EMAHOME)/rema-xps.mem

XGRF3DP = ../xgrf3/xgrf3d

COMMOBJ = main.o timings.o debugger.o uid.o sysd.o xext.o expd.o ui.o uif.o uiw.o \
          help.o explain.o
RXPSOBJ = $(COMMOBJ) rinit.o rdispatch.o
XPSOBJS = $(COMMOBJ) xinit.o dispatch.o     trace.o \
          frame.o instance.o behavior.o rule.o prolog.o constraint.o \
          csnet.o restriction.o task.o misc.o

INST    = help.de help.en xres.co.de xres.co.en xres.bw.de xres.bw.en
INIT    = _init.cl emalisp remalisp emaXpsGrid.bm emaXpsIcon.bm \
          emaXpsLogo.bm emaXpsSave.bm emaXpsEmpty.bm ui-demo.kb kb-demo.kb EMA-XPS.UI-DEMO

XRES    = _xres _xres.debugger _xres.explain _xres.ui _xres.uid _xres.sysd _xres.expd \
          _xres.help _xres.trace _xres.frame _xres.instance _xres.behavior _xres.rule \
          _xres.prolog _xres.constraint _xres.csnet _xres.restriction _xres.misc _xres.task
HELP    = _help _help.main _help.debugger _help.explain _help.ui _help.explanations \
          _help.trace _help.frame _help.instance _help.behavior _help.rule _help.prolog \
          _help.constraint _help.csnet _help.restriction _help.misc _help.task _help.bql \
          _help.gui _help.blisp _help.kb_constructs _help.unsupported _help.knowledge

#################
### Since updating from clisp of 1994 to clisp May 1996 the LISP compiler seems
### to convert xxx.cl to xxx.c (no xxx.h!) which is C compiled in advance.
### The temporary file xxx.c is removed then.
### Here files like main.lisp (now main.cl) and main.c get naming conflicts.
### Hence the compilable LISP files have been moved to ./cl directory!

LSPRUNB = cl/b3-blisp.bin cl/b3-bql-or.bin cl/b3-bql.bin cl/b3-cnstr.bin cl/b3-editr.bin \
          cl/b3-expln.bin cl/b3-misc.bin cl/b3-oop.bin cl/b3-rules.bin cl/b3-sets.bin \
          cl/b3-ui.bin cl/channels.bin cl/compile.bin cl/debugger.bin cl/dispatch.bin \
          cl/explain.bin cl/inf-eng.bin cl/kb-cfg.bin cl/main.bin cl/sysd.bin cl/ui.bin \
          cl/uid.bin 
LISPBIN = $(LSPRUNB) cl/behavior.bin cl/constraint.bin cl/csnet.bin cl/frame.bin \
          cl/instance.bin cl/misc.bin cl/prolog.bin cl/restriction.bin cl/rule.bin \
          cl/task.bin cl/trace.bin

LSPRUNS = cl/b3-blisp.cl cl/b3-bql-or.cl cl/b3-bql.cl cl/b3-cnstr.cl cl/b3-editr.cl \
          cl/b3-expln.cl cl/b3-misc.cl cl/b3-oop.cl cl/b3-rules.cl cl/b3-sets.cl \
          cl/b3-ui.cl cl/channels.cl cl/compile.cl cl/debugger.cl cl/dispatch.cl \
          cl/explain.cl cl/inf-eng.cl cl/kb-cfg.cl cl/main.cl cl/sysd.cl cl/ui.cl \
          cl/uid.cl
LISPSRC = $(LSPRUNS) cl/behavior.cl cl/constraint.cl cl/csnet.cl cl/frame.cl \
          cl/instance.cl cl/misc.cl cl/prolog.cl cl/restriction.cl cl/rule.cl \
          cl/task.cl cl/trace.cl

LISPINT = make-ema-xps.cl ccload.cl impl-dep.cl excl-emu.cl

READMES = ../README ../COPYRIGHT ../GNU-GPL ../NOTES ../INSTALL
SRCDIRS = README COPYRIGHT GNU-GPL NOTES INSTALL Makefile \
          bin doc mirror src util xgrf3

######### first one ###########################

        ### separated from Xresource inst.
all::
	(cd ..;make mostly-all)

        ### see heading info!
kill::
	@$(EMAKILL) $(EMALISP)

######### C-to-OBJ ############################

.SUFFIXES: .c .o .cl .bin

.c.o:
	@$(CC) $(CCOPTS) -c $<

xinit.o: xinit.c
	@$(CC) $(CCOPTS) -c xinit.c

rinit.o: xinit.c
	@$(CC) $(CCOPTS) -DRUNTIME -c xinit.c -o rinit.o

dispatch.o: dispatch.c
	@$(CC) $(CCOPTS) -c dispatch.c

rdispatch.o: dispatch.c
	@$(CC) $(CCOPTS) -DRUNTIME -c dispatch.c -o rdispatch.o

###### static/dynamic binding #################
	### strip static binaries (1MB less disk space consumption!)

static::
	(rm -f $(XGRF3DP) $(EMA_XPS) $(REMAXPS);\
	EMAXPS_BINDING=static;export EMAXPS_BINDING;cd ..;\
	make xgrf3d-kled;make mostly-all)
	strip $(XGRF3DP) $(EMA_XPS) $(REMAXPS)

dynamic::
	(rm -f $(XGRF3DP) $(EMA_XPS) $(REMAXPS);\
	EMAXPS_BINDING=dynamic;export EMAXPS_BINDING;cd ..;\
	make xgrf3d-kled;make mostly-all)

###### for installation (source) ##############

exe: $(EMA_XPS) $(REMAXPS)

mem: $(L_IMAGE) $(RLIMAGE)

ema-xps-bins: $(EMA_XPS) $(L_IMAGE)

$(EMA_XPS): $(XPSOBJS)
	@$(BIND) -o $@ $(XPSOBJS)

rema-xps-bins: $(REMAXPS) $(RLIMAGE)

$(REMAXPS): $(RXPSOBJ)
	@$(BIND) -o $@ $(RXPSOBJ)

###### LISP to Image ##########################
#
#       WARNING: This part is optimized for CLISP!
#                See ../bin/Makefile, too!
#
#       INFORMATION: Sometimes booting the developer
#                environment gets hanging.
#                In such case type ^C and restart make.
#                Or modify timings.h !

                 ### Dummy-Rule, for dependencies only!
.cl.bin:
	@true

                 ### in case of using another LISP interpreter, del FASL files!
make-ema-xps.cl: bablisp
	-$(RM) -f cl/*.bin
	@touch make-ema-xps.cl
	@echo '*************************************************************'
	@echo '* You have modified src/bablisp, did you?!?                 *'
	@echo '*                                                           *'
	@echo '* You will have to modify the following file, too:  (?!?)   *'
	@echo '*    src/emalisp                                            *'
	@echo '*    src/remalisp                                           *'
	@echo '*    bin/Makefile                                           *'
	@echo '* Finally a (cd src && make inst) might become necessary... *'
	@echo '*************************************************************'

$(L_IMAGE): $(LISPINT) $(LISPBIN) $(LISPSRC) make-develop.cl
	echo '(load "./make-develop.cl")' | ./bablisp
	$(MV) emaxps.image $(L_IMAGE)

$(RLIMAGE): $(LISPINT) $(LSPRUNB) $(LSPRUNS) make-runtime.cl
	echo '(load "./make-runtime.cl")' | ./bablisp
	$(MV) emaxps.image $(RLIMAGE)

###### Preprocessed Files #####################

$(HELP_CV)::
	(cd ../util;make all)

help.de: $(HELP_CV) $(HELP)
	$(HELP_SH) _help $@ -U__english

help.en: $(HELP_CV) $(HELP)
	$(HELP_SH) _help $@ -D__english

xres.co.de: $(XRES) $(HELP_CV)
	$(XRES_SH) _xres $@ -DCOLOR -U__english

xres.bw.de: $(XRES) $(HELP_CV)
	$(XRES_SH) _xres $@ -UCOLOR -U__english

xres.co.en: $(XRES) $(HELP_CV)
	$(XRES_SH) _xres $@ -DCOLOR -D__english

xres.bw.en: $(XRES) $(HELP_CV)
	$(XRES_SH) _xres $@ -UCOLOR -D__english

inst: $(INST) $(INIT)
	$(CP) help.de           $(EMAHOME)/deutsch/ema-xps-help.txt
	$(CP) help.en           $(EMAHOME)/english/ema-xps-help.txt
	$(CP) xres.co.de        $(EMAHOME)/deutsch/EMA-XPS
	$(CP) xres.bw.de        $(EMAHOME)/deutsch/EMA-XPS-BW
	$(CP) xres.co.en        $(EMAHOME)/english/EMA-XPS
	$(CP) xres.bw.en        $(EMAHOME)/english/EMA-XPS-BW
	$(CP) _init.cl          $(EMAHOME)/ema-xps-init.cl
	$(CP) emalisp           $(EMAHOME)
	$(CP) remalisp          $(EMAHOME)
	$(CP) emaXpsGrid.bm     $(EMAHOME)/bitmaps
	$(CP) emaXpsIcon.bm     $(EMAHOME)/bitmaps
	$(CP) emaXpsLogo.bm     $(EMAHOME)/bitmaps
	$(CP) emaXpsSave.bm     $(EMAHOME)/bitmaps
	$(CP) emaXpsEmpty.bm    $(EMAHOME)/bitmaps
	$(CP) XKeysymDB         $(EMAHOME)
	$(CP) ui-demo.kb        $(EMAKBSD)
	$(CP) kb-demo.kb        $(EMAKBSD)
	$(CP) EMA-XPS.UI-DEMO   $(EMAKBSD)
	$(CP) emaXpsIcon.bm     $(EMAKBSD)/xbm/icon
	$(CP) emaXpsLogo.bm     $(EMAKBSD)/xbm/logo

###### Clean and Reinit directory tree ########

less::
	-@$(RM) -f *~ *.lib core 1 2 3 4 5 DEADJOE  ### joe (^KW / ^KR) tmpfiles

clr::
	(cd ..;make clean)

clean::
	(cd ../util;make clean)
	-$(RM) -f *.o $(INST) core *~ 1 2 3 4 5 DEADJOE         ### joe tmpfiles
	-$(RM) -f cl/*.lib cl/*.bin cl/core cl/*~ cl/1 cl/2 cl/3 cl/4 cl/5 cl/DEADJOE

install::
	(cd ..;make install)

###### Prepare transfer to HTTP Server ########

packit::
	make clr
	@(cd $(EMAROOT); \
	  $(RM) -f $(TARFILE) $(TGZFILE); \
	  $(TAR) cvf $(TARFILE) $(SRCDIRS); \
	  echo "*** GNU zipping $(TGZFILE) ***"; \
	  $(GZIP) $(TARFILE); \
	  $(MV) $(TARFILE).gz $(TGZFILE); \
	:)

save: packit expdir
	@(echo "*** transfering $(TGZFILE) to local export dir ***"; \
	  $(MV) $(EMAROOT)/$(TGZFILE) $(EXPPATH)/$(TGZFILE).new; \
	:)
	@cp $(READMES) $(EXPPATH)
	@(cd $(EXPPATH); \
	  (if [ -f $(TGZFILE).bak3 ]; then $(RM) $(TGZFILE).bak3; fi); \
	  (if [ -f $(TGZFILE).bak2 ]; then $(MV) $(TGZFILE).bak2 $(TGZFILE).bak3; fi); \
	  (if [ -f $(TGZFILE).bak1 ]; then $(MV) $(TGZFILE).bak1 $(TGZFILE).bak2; fi); \
	  (if [ -f $(TGZFILE) ]; then $(MV) $(TGZFILE) $(TGZFILE).bak1; fi); \
	  $(MV) $(TGZFILE).new $(TGZFILE); \
	:)
	@(make lsexpdir)

lsexpdir::
	@echo "directory: $(EXPPATH)"
	@(cd $(EXPPATH);$(LS) -al)


web: save webdir webfill webmirror webdoc

expdir: $(EXPPATH)

webdir: $(HTRPATH)

$(HTRPATH)::
	@echo "setting up local web export dir:"
	@echo "[  `/bin/pwd`/$(HTRPATH)  ]"
	mkdir -p $(HTRPATH) $(HTRPATH)/FTP

$(EXPPATH)::
	@echo "setting up local export dir:"
	@echo "[  `/bin/pwd`/$(EXPPATH)  ]"
	mkdir -p $(EXPPATH)

webfill::
	@echo GNU-GPL
	@$(CP) $(EXPPATH)/GNU-GPL      $(HTRPATH)/FTP
	@echo COPYRIGHT
	@$(CP) $(EXPPATH)/COPYRIGHT    $(HTRPATH)/FTP
	@echo INSTALL
	@$(CP) $(EXPPATH)/INSTALL      $(HTRPATH)/FTP
	@echo NOTES
	@$(CP) $(EXPPATH)/NOTES        $(HTRPATH)/FTP
	@echo emaxps21.tgz
	@$(CP) $(EXPPATH)/emaxps21.tgz $(HTRPATH)/FTP/emaxps.tar.gz
	# @echo sunosbin.tgz
	# @$(CP) $(EXPPATH)/sunosbin.tgz $(HTRPATH)/FTP/SunOS.tar.gz
	# @echo linuxbin.tgz
	# @$(CP) $(EXPPATH)/linuxbin.tgz $(HTRPATH)/FTP/LinuxAOUT.tar.gz

webmirror::
	@echo $(MIR_BAB)
	@$(CP) $(MIRPATH)/$(MIR_BAB)   $(HTRPATH)/FTP/babylon.tar.gz
	-chmod 644 $(HTRPATH)/FTP/babylon.tar.gz  2>/dev/null
	@echo $(MIRCLSP)
	@$(CP) $(MIRPATH)/$(MIRCLSP)   $(HTRPATH)/FTP/clisp.tar.gz
	-chmod 644 $(HTRPATH)/FTP/clisp.tar.gz    2>/dev/null

webdoc::
	@echo "HTML doc files"
	@(cd $(EMADOCD)/html; $(CP) -r de en *.gif *.html $(HTRPATH) )

webclean::
	@echo "removing local web export dir:"
	@echo "[  $(HTRPATH)  ]" # `/bin/pwd`/$(HTRPATH)
	rm -rf $(HTRPATH)

### eof
