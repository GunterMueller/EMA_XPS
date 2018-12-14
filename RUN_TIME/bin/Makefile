###############################################
#   Makefile          EMA-XPS           bin   #
#                                             #
###############################################

### Warning: this is optimized for CLISP
###
#LISP     = clisp
#LISPLOC  = ./bin

############### do not edit ###################

RM       = rm
CP       = cp

INSTPATH = ../EMA-XPS/bin
LISPDST  = ../EMA-XPS/lib

XPS      = $(INSTPATH)/xps
RXPS     = $(INSTPATH)/rxps
UIDEMO   = $(INSTPATH)/ui-demo

PATCHES  = ../util/patches

### install ###

## with Allegro CL 4.1 use:
#inst: inst-starters 

## with CLISP use:
#hg20081111: obsolete:
#   inst: inst-starters inst-lisp
inst: inst-starters 

######

#inst-lisp: $(LISPDST)/$(LISP)

#$(LISPDST)/$(LISP): $(LISPLOC)/$(LISP)
#	$(CP) $(LISPLOC)/$(LISP) $(LISPDST)/$(LISP)

inst-starters: $(XPS) $(RXPS) $(UIDEMO)
	$(PATCHES) `pwd`/../EMA-XPS

$(XPS): xps
	$(CP) xps $(INSTPATH)

$(RXPS): rxps
	$(CP) rxps $(INSTPATH)

$(UIDEMO): ui-demo
	$(CP) ui-demo $(INSTPATH)

### Clean and Reinit directory tree ###

clean::
	-$(RM) -f core *~ 1 2 3 4 5 DEADJOE   ### joe tmpfiles

### eof
