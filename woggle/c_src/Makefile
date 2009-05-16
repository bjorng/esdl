#  Copyright (c) 2001 Dan Gudmundsson and Patrik Nyblom
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
#     $Id$
#

ifeq ($(OS_FLAG), mingw)
include ../../win32_conf/mingw_vars.mk
LIBS := $(GL_LIBS) -lgdi32 

TARGETDIR = ../../priv

GCC_O	= $(CC) -c $(CFLAGS) $<

ESDL_SRC = ../../c_src

# Optimization currently disabled
#CFLAGS := -g -O2 -funroll-loops -Wall -ffast-math -fomit-frame-pointer \
#         -DWIN32 -D_WIN32 -D__WIN32__ -DFLAVOUR_WOGGLE $(GL_INCS) $(ERL_INCS) \
#	 -I$(ESDL_SRC) -I.
CFLAGS := -g -Wall \
         -DWIN32 -D_WIN32 -D__WIN32__ -DFLAVOUR_WOGGLE $(GL_INCS) $(ERL_INCS) \
	 -I$(ESDL_SRC) -I.

# Files

TSOURCES = woggle_win.c woggle_test.c
SOURCES = \
	woggle_win.c \
	woggle_driver.c \
	woggle_wrapper.c \
	woggle_if.c \
	woggle_compat_if.c \
	$(ESDL_SRC)/esdl_gl.c \
	$(ESDL_SRC)/esdl_glu.c \
	$(ESDL_SRC)/esdl_glext.c \
	$(ESDL_SRC)/esdl_util.c
	 

TOBJECTS = $(TSOURCES:%.c=%.o)
OBJECTS0 = $(SOURCES:%.c=%.o)
OBJECTS = $(OBJECTS0:$(ESDL_SRC)/%.o=%.o)

THEADERS = woggle_win.h
HEADERS = woggle_win.h woggle_driver.h woggle_if.h woggle_if_fp.h\
	$(ESDL_SRC)/esdl_gl.h $(ESDL_SRC)/esdl_gl_fp.h \
	$(ESDL_SRC)/esdl_glext.h $(ESDL_SRC)/esdl_glext_fp.h \
	$(ESDL_SRC)/esdl_glu.h $(ESDL_SRC)/esdl_glu_fp.h \
	$(ESDL_SRC)/esdl_util.h

TTARGET = ./woggle_test.exe
TOBJECTS = ./woggle_test.o woggle_win.o
TARGET = $(TARGETDIR)/woggle_driver.$(SOEXT)
TARGET_JUNK = $(TARGETDIR)/woggle_driver.lib $(TARGETDIR)/woggle_driver.exp

all:  $(TARGET) $(TTARGET) 
	@echo Build done

clean:  
	rm -f $(OBJECTS) $(TOBJECTS)
	rm -f $(TARGET) $(TTARGET) $(TARGET_JUNK)
	rm -f *~ erl_crash.dump

%.o: %.c $(HEADERS)
	$(GCC_O)

%.o: $(ESDL_SRC)/%.c $(HEADERS)
	$(GCC_O)

$(TTARGET): $(TOBJECTS)
	$(CC) $(CFLAGS) $(TOBJECTS) $(LIBS) -o $(TTARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CLINKFLAGS) $(CFLAGS) $(ERL_INCS) $(OBJECTS) $(LIBS) -o $(TARGET)
	rm -f $(TARGET_JUNK)


else

target clean: 
	@echo Only windows for now.

endif




