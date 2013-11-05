# (c)INRIA 2001
# WARNING: this file must be processed by GNU make (REQUIRED!)
# SynDEx v5 main makefile for compiling SynDEx-generated executives,
# equalizer example application, bi-workstation target

# Beginners: please start your reading with the README file.

# $(A) should be the application name
# $(M4) must be the GNU version of m4:
# $(M4PATH) must be the path to the *.m4? files defining SynDEx macros

A  = ega2
M4 = gm4

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

# $(A).mk is a complementary makefile, included at the end of this GNUmakefile.
# $(A).mk defines dependencies and generation commands for all other files.
# $(A).m4 starts with "include(syndex.m4m)" to let m4 "learn" SynDEx generic
# macros for building makefiles; then $(A).m4 continues with the macro-call
# "architecture_(ega2u, version, date)"; the generic `architecture_' macro,
# defined in syndex.m4m, outputs a file header (comments with version/date/...)
# and the $(A).run target, then looks for, and includes if found, the file
# $1.m4m (= ega2u.m4m), which here defines the macros substituting SynDEx
# processor names by internet hostnames; then $(A).m4 continues with
# "processor_(U,root, TCP,L0)" and "processor_(U,p, TCP,L3)"; the generic
# `processor_' macro, defined in syndex.m4m, includes the file $1.m4m (= U.m4m)
# which generates compilation dependencies for a processor of type "U", i.e.
# for a multi-workstation executive programmed in C/UNIX/TCP, translated by m4
# from root.m4 and p.m4; see ega2u.m4x for more on this translation process.

# Application specific dependencies:

# root.m4, after "include(syndex.m4x)" which lets m4 "learn" all the SynDEx
# generic macros, calls the generic macro "processor_(U,root,ega2u,vers,date)"
# which first includes $1.m4x (= U.m4x) to let m4 "learn" SynDEx basic macros
# specific to processor type "U" (UNIX), then generates file header comments,
# then looks for, and includes if found, $3.m4x (= ega2u.m4x) to let m4 "learn"
# user macros specific to the ega2u application, then finally looks for, and
# includes if found, $2.m4x (= root.m4x) which here only includes ega2u.m4m to
# translate SynDEx processor names into internet hostnames for use by rsh.
# Including U.m4x and ega2u.m4x (resp. root.m4x) are considered usual (resp.
# unusual), and as such are (resp. is not) already taken into account in U.m4m
# to generate the correct dependencies in $(A).mk, therefore the following
# dependency reflects the unusual inclusion of root.m4x, and therefore of
# ega2u.m4m, during the generation of root.c (from root.m4):

#root.c : root.m4x basic.m4m

# Architecture heterogeneity: although root and p are both U-type (UNIX/TCP)
# processors, they may use different instruction-set-architectures (Linux/i386,
# Solaris/Sun4, OSF/DecAlpha, etc.); this is the assumption made by U.m4m,
# which separately compiles object files remotely on each processor, and names
# them prefixed by the processor name.  Here equalize.c must be then compiled
# into resp. root.equalize.o and p.equalize.o, which must be passed for linking
# with resp. root and p executables, through the make-variable resp. root.libs
# and p.libs, which appears as dependent of resp. root and p (see ega2u.mk):

root.libs = 
p.libs = 

# Finally, equalize.h is included here by everybody, to generate not only
# root.equalize.o and p.equalize.o, but also by root.root.o and p.p.o,
# which is what the following pattern-matching make-rule means:


include $(A).mk