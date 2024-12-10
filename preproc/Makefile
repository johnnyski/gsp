############################################################
# CONIGURATAION STEP 1: CHOOSE COMPILER AND PLATFORM       #
############################################################
#
FC = fort77 +U77 	# HP 
CC = gcc
#FC = f77   		# SGI or Linux

############################################################
# CONIGURATAION STEP 2: CHOOSE THE LIB, BIN AND INC DIRS   #
############################################################
#
# These are the bin, lib and include directories
#
BINDIR = ${HOME}/bin
BINDIR = ${TRMM}

BINDIR = /usr/local/trmm/bin
LIBDIR = /usr/local/trmm/lib
INCDIR = /usr/local/trmm/include

#
# Link flags
#
LDFLAGS = -L. 

############################################################
# CONIGURATAION STEP 3: IDENTIFY INPUT SOURCE COE          #
############################################################
#
# # Input source code
#
p = preproc
o = $(p).o get_site.o dar.o har.o ksc.o kwa.o tft.o  stj.o tam.o time.o dst.o gems.o sfl.o get_current_date.o

############################################################
# NO CONFIGURATION SHOULD BE NECESSARY BELOW THIS LINE     #
############################################################

all: $(p)


$(p): $o
	$(FC) -o $@ $o

clean:
	-rm $o
	-rm $(p)
	-rm $(BINDIR)/$(p)
	-rm $(BINDIR)/$(p).pl

install:
	-cp $(p) $(BINDIR)
	-cp $(p).pl $(BINDIR)




