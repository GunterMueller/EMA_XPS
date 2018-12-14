###############################################
#   Makefile      kled    a graphics utility  #
#                 xgrf3d  the server process  #
###############################################

############### do not edit ###################

           ### -DAPOLLO is a switch in grf3.h !
CCOPTS  = -DAPOLLO
LDOPTSD = -lm
LDOPTS  = -L. -lgrf3 -lm

CC      = ../util/cc_X11
BIND    = ../util/ld_X11

CP      = cp
RANLIB  = ranlib
AR      = ar
RM      = rm

GETDIR  = /ema/hans/Xappl/xgrf3
GETDIR2 = /ema/hans/grfappl/kled
GETSRCS = $(GETDIR)/grf/grf3.h $(GETDIR)/grf/grf3a.c $(GETDIR)/grf/grf3b.c \
          $(GETDIR)/grf/grf3d.c $(GETDIR)/lib/grf3plt.c \
          $(GETDIR)/lib/grf3m.h $(GETDIR)/lib/grf3m.c \
          $(GETDIR)/lib/grf3ipc.h $(GETDIR)/lib/grf3ipc.c \
          $(GETDIR)/lib/grf3mot.h $(GETDIR)/lib/grf3mot.c \
          $(GETDIR)/xgrf3d/Xgrf3.bm $(GETDIR)/xgrf3d/grf3sipc.c \
          $(GETDIR)/xgrf3d/grf3sipc.h $(GETDIR)/xgrf3d/grf3xsrv.c \
          $(GETDIR)/xgrf3d/grf3xsrv.h $(GETDIR)/grf/bsp.c \
          $(GETDIR)/grf/test3.c \
          $(GETDIR2)/kled.c

INSTPATH = ../EMA-XPS/bin

### first one #############

all: libgrf3.a xgrf3d zhp test3 bsp kled

### C-to-OBJ ##############

.SUFFIXES: .c .o

.c.o:
	@$(CC) $(CCOPTS) -c $<

### libgrf3.a #############

libgrf3BINS = grf3d.o grf3a.o grf3b.o grf3plt.o grf3m.o grf3mot.o grf3ipc.o

libgrf3.a: $(libgrf3BINS)
	$(RM) -f $@
	$(AR) rc $@ $(libgrf3BINS)
	$(RANLIB) $@

### xgrf3d ################

xgrf3dBINS = grf3xsrv.o grf3sipc.o

xgrf3d: $(xgrf3dBINS)
	@$(BIND) -o $@ $(xgrf3dBINS) $(LDOPTSD)

### zhp ###################

zhpBINS  = zhp.o libgrf3.a

zhp: $(zhpBINS)
	@$(CC) -o $@ $(zhpBINS) $(LDOPTS)

### test3 #################

test3BINS  = test3.o libgrf3.a

test3: $(test3BINS)
	@$(CC) -o $@ $(test3BINS) $(LDOPTS)

### bsp ###################

bspBINS    = bsp.o libgrf3.a

bsp: $(bspBINS)
	@$(CC) -o $@ $(bspBINS) $(LDOPTS)

### kled ##################

kledBINS   = kled.o libgrf3.a

kled: $(kledBINS)
	@$(CC) -o $@ $(kledBINS) $(LDOPTS)

### for developers only ###

save::
	@(make get;make clean)

get::
	$(CP) $(GETSRCS) .

inst: $(INSTPATH)/kled $(INSTPATH)/xgrf3d

$(INSTPATH)/kled: kled
	$(CP) kled $(INSTPATH)

$(INSTPATH)/xgrf3d: xgrf3d
	$(CP) xgrf3d $(INSTPATH)

### Clean and Reinit directory tree ###

clean::
	-$(RM) -f core *~ *.bak *.BAK *.o *.exe kled xgrf3d test3 bsp zhp libgrf3.a \
	          ../bin/kled ../bin/xgrf3d 1 2 3 4 5 DEADJOE   ### joe tmpfiles

### eof
