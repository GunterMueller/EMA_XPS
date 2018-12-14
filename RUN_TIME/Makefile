###############################################
#   Makefile          EMA-XPS      toplevel   #
#                                             #
#   Hints for making a binary distribution:   #
#   1) You have successfully built EMA-XPS    #
#      from this source distribution          #
#   2) now contact root, if you like to       #
#      install the binary distribution tree   #
#      in /opt/EMA-XPS or modify INSTPATH to  #
#      e.g. your HOME directory (but do NOT   #
#      use $HOME or $$HOME, it won't work!)   #
#      or /tmp or ...                         #
#   3) make install                           #
#   4) cd <location>/EMA-XPS/..               #
#   5) tar cvf $HOME/bin_dist.tar EMA-XPS     #
#   6) cd $HOME                               #
#   7) gzip bin_dist.tar                      #
#   done...                                   #
###############################################

INSTPATH = /opt/EMA-XPS

############### do not edit ###################

DIRS_MAKE = util/dirs-make.sh
DIRS_INST = util/dirs-inst.sh
PATCHES   = util/patches
MOTIF_HDR = util/findXmH.sh
MAKE_LISP = util/makeClispNoReadline.sh
MAKE_BABY = util/makeBabylonImage.sh

RM        = rm

### Installation ###

all: MotifH EMA-XPS-file-system Babylon23 \
	ema-xps-xres xgrf3d-kled patcher inst-man inst-doc mostly-all

MotifH::
	$(MOTIF_HDR)

Babylon23: tmp/Babylon-2.3/babylon.mem

EMA-XPS/bin/clisp: mirror/clisp-2.45.tar.bz2
	$(MAKE_LISP)

tmp/Babylon-2.3/babylon.mem: EMA-XPS/bin/clisp mirror/Babylon-2.3.tar.gz
	$(MAKE_BABY)

install::
	$(DIRS_INST) $(INSTPATH)
	$(PATCHES) $(INSTPATH)
	@echo ">>> Add the appropriate settings to your PATH-variable"
	@echo ">>>    $(INSTPATH)/bin ."


install-man::
	@echo ">>> Add the appropriate settings to your MANPATH-variable"
	@echo ">>>    $(INSTPATH)/man and do a makewhatis ..."

### Utilities ###

mostly-all: ema-xps-bins rema-xps-bins starters

EMA-XPS-file-system::
	$(DIRS_MAKE) ./EMA-XPS
	mkdir -p ./tmp
	@cp README COPYRIGHT GNU-GPL NOTES INSTALL ./EMA-XPS

xgrf3d-kled::
	(cd xgrf3;make;make inst)

## src includes util (hence not needed in patcher):
ema-xps-xres::
	(cd src;make inst)

patcher::
	(cd util;make inst)

ema-xps-bins::
	(cd src;make ema-xps-bins)

rema-xps-bins::
	(cd src;make rema-xps-bins)

starters::
	(cd bin;make inst)

inst-man::
	(cd doc;make inst-man)

inst-doc::
	(cd doc;make inst-doc)

### Clean and Reinit directory tree ###

## src includes util:
clean::
	(cd src;make clean)
	(cd doc;make clean)
	(cd bin;make clean)
	(cd xgrf3;make clean)
	-$(RM) -f core *~ 1 2 3 4 5 DEADJOE
	-$(RM) -rf ./EMA-XPS ./tmp

### eof
