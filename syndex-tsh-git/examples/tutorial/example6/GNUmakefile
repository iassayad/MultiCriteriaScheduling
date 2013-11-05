# (c)INRIA 2001
# WARNING: this file must be processed by GNU make (REQUIRED!)
# SynDEx v6 main makefile for compiling SynDEx-generated executives,
# example6.sdx application, bi-workstation target

# Beginners: please start your reading with the README file.

# $(A) should be the application name
# $(M4) must be the GNU version of m4:
# $(M4PATH) must be the path to the *.m4? files defining SynDEx macros

A  = example6
M4 = m4

export Macros_Path = ../../../macros/Unix_C_TCP
export Libraries_Path = ../../../macros/libraries
export M4PATH = $(Macros_Path):$(Libraries_Path)

CFLAGS = -DDEBUG

# $(VPATH) is searched by make for dependent files not found in $(PWD)
VPATH = $(M4PATH)

# $(A).sdx   is the SynDEx user-specification of the application
# $(A).m4    is the SynDEx-generated architecture file
# xxx.m4     is the SynDEx-generated generic executive for processor xxx
# syndex.m4m defines generic macros for generating makefiles
# <type>.m4m contains compilation rules specific to processor type <type>
# $(A).mk    is the makefile generated from $(A).m4
# syndex.m4x defines generic macros for generating executives
# <type>.m4x defines executive macros specific to processor type <type>
# $(A).m4x   contains user-defined executive macros specific to $(A)
# xxx.src    is the executive generated from xxx.m4 into the src language
# $(xxx.libs) application specific src/obj/libs to be linked to xxx.src
# xxx        is the executable compiled from xxx.src (_root is the launcher)

.PHONY: all clean
all : $(A).mk $(A).run
clean ::
	$(RM) $(A).mk *~ *.o *.a

$(A).mk : $(A).m4 syndex.m4m U.m4m $(A).m4m
	$(M4) $< >$@

root.libs = 
p.libs = 

include $(A).mk
