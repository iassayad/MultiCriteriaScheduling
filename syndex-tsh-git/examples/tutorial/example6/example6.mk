# SynDEx-6.7.0 (c)INRIA 2002, 7/6/2004 15:20:54

# Makefile for application example6

# $(M4) must be the GNU macroprocessor m4
# $(Macros_Path) must be the path to the generic *.m4? macro-files
# $(VPATH) is searched by make for dependent files not found in $(PWD)
VPATH = $(Macros_Path)

.PHONY: example6.all example6.run clean
example6.run : example6.all # load and run example6:
       # (command args = processors in loading order)
	./$(example6.root)



# processor root type=U:
example6.all : root
example6.root += root
example6.root += =yak
root : root.root.o $(root.libs)
	S=`$(Macros_Path)/rshcd yak "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd yak $(CC) $(CFLAGS) $^ $$L -lm -o $@
root.%.o : %.c
	$(Macros_Path)/rshcd yak \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
root.c : root.m4 syndex.m4x U.m4x C.m4x example6.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x yak.mnt && ./yak.mnt ||\
	$(Macros_Path)/rshcd yak "\
	$(RM) root root.c root.root.o yak.mnt"
	$(RM) root root.c root.root.o yak.mnt


# processor pc1 type=U:
example6.all : pc1
example6.root += pc1
example6.root += =ourson
pc1 : pc1.pc1.o $(pc1.libs)
	S=`$(Macros_Path)/rshcd ourson "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd ourson $(CC) $(CFLAGS) $^ $$L -lm -o $@
pc1.%.o : %.c
	$(Macros_Path)/rshcd ourson \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
pc1.c : pc1.m4 syndex.m4x U.m4x C.m4x example6.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x ourson.mnt && ./ourson.mnt ||\
	$(Macros_Path)/rshcd ourson "\
	$(RM) pc1 pc1.c pc1.pc1.o ourson.mnt"
	$(RM) pc1 pc1.c pc1.pc1.o ourson.mnt




