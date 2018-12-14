###############################################
#   Makefile          EMA-XPS           bin   #
#                                             #
#   KNOWN BUGs: html testing is insufficient! #
###############################################

############### do not edit ###################

RM      = rm
CP      = cp

DSTPATH = ../EMA-XPS/man/man1
DOCPATH = ../EMA-XPS/doc

### for installation ###

all: inst-man inst-doc

inst-man: $(DSTPATH)/ema-xps.1

$(DSTPATH)/ema-xps.1: man/ema-xps.1
	$(CP) man/ema-xps.1 $(DSTPATH)/ema-xps.1

inst-doc: $(DOCPATH)/index.html

$(DOCPATH)/index.html: html/index.html
	$(RM) -rf $(DOCPATH)
	$(CP) -r html $(DOCPATH)

### Clean and Reinit directory tree ###

clean::
	-$(RM) -f core *~ 1 2 3 4 5 DEADJOE   ### joe tmpfiles
	-(cd man;$(RM) -f core *~ 1 2 3 4 5 DEADJOE)
	-(cd html;$(RM) -f core *~ 1 2 3 4 5 DEADJOE)
	-(cd html/de;$(RM) -f core *~ 1 2 3 4 5 DEADJOE)
	-(cd html/en;$(RM) -f core *~ 1 2 3 4 5 DEADJOE)

### eof
