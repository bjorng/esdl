#  Copyright (c) 2001 Dan Gudmundsson and Patrik Nyblom
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
#     $Id$
#

ifeq ($(OS_FLAG), mingw)
include ../win32_conf/mingw_vars.mk
woggle: 
	(cd c_src; $(MAKE) OS_FLAG=$(OS_FLAG))
	(cd src; $(MAKE) OS_FLAG=$(OS_FLAG))
	(cd test; $(MAKE) OS_FLAG=$(OS_FLAG))

clean:  top_clean
	(cd c_src; $(MAKE)  OS_FLAG=$(OS_FLAG) clean)
	(cd src; $(MAKE)  OS_FLAG=$(OS_FLAG) clean)
	(cd test; $(MAKE)  OS_FLAG=$(OS_FLAG) clean)


top_clean:
	rm -f *~ erl_crash.dump core
	rm -f tools/*~

else

woggle clean: 
	@echo Only windows for now.

endif




