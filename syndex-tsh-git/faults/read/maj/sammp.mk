# SynDEx-6.7.0 (c)INRIA 2002, 25/6/2004 13:09:44

# Makefile for application sammp

# $(M4) must be the GNU macroprocessor m4
# $(Macros_Path) must be the path to the generic *.m4? macro-files
# $(VPATH) is searched by make for dependent files not found in $(PWD)
VPATH = $(Macros_Path)

.PHONY: sammp.all sammp.run clean
sammp.run : sammp.all # load and run sammp:
       # (command args = processors in loading order)
	./$(sammp.root)


# processor root type=U:
sammp.all : root
sammp.root += root
sammp.root += =aiglon
root : root.root.o $(root.libs)
	S=`$(Macros_Path)/rshcd aiglon "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd aiglon $(CC) $(CFLAGS) $^ $$L -lm -o $@
root.%.o : %.c
	$(Macros_Path)/rshcd aiglon \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
root.c : root.m4 syndex.m4x U.m4x C.m4x sammp.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x aiglon.mnt && ./aiglon.mnt ||\
	$(Macros_Path)/rshcd aiglon "\
	$(RM) root root.c root.root.o aiglon.mnt"
	$(RM) root root.c root.root.o aiglon.mnt


# processor louve type=U:
sammp.all : louve
sammp.root += louve
sammp.root += =louve
louve : louve.louve.o $(louve.libs)
	S=`$(Macros_Path)/rshcd louve "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd louve $(CC) $(CFLAGS) $^ $$L -lm -o $@
louve.%.o : %.c
	$(Macros_Path)/rshcd louve \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
louve.c : louve.m4 syndex.m4x U.m4x C.m4x sammp.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x louve.mnt && ./louve.mnt ||\
	$(Macros_Path)/rshcd louve "\
	$(RM) louve louve.c louve.louve.o louve.mnt"
	$(RM) louve louve.c louve.louve.o louve.mnt




