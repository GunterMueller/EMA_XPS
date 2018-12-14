###############################################
#   Makefile          EMA-XPS          util   #
###############################################

############### do not edit ###################

RM       = rm
CC       = ../util/cc_X11

HELP_CV  = help-cv.c
CPP_POST = cpp-post.c
PATCHES  = patches.c

INSTPATH = ../EMA-XPS/bin

###

all: help-cv cpp-post patches

help-cv: $(HELP_CV)
	@$(CC) $(HELP_CV) -o $@

cpp-post: $(CPP_POST)
	@$(CC) $(CPP_POST) -o $@

patches: $(PATCHES)
	@$(CC) $(PATCHES) -o $@

###

inst: $(INSTPATH)/patches

$(INSTPATH)/patches: patches
	cp patches $(INSTPATH)

### Clean and Reinit directory tree ###

clean::
	-$(RM) -f core *~ *.o *.exe patches help-cv cpp-post 1 2 3 4 5 DEADJOE   ### joe tmpfiles
	-$(RM) -f online2html

### eof
