# SynDEx-6.7.0 (c)INRIA 2002, 29/6/2004 09:55:34

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
sammp.root += =yak
root : root.root.o $(root.libs)
	S=`$(Macros_Path)/rshcd yak "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd yak $(CC) $(CFLAGS) $^ $$L -lm -o $@
root.%.o : %.c
	$(Macros_Path)/rshcd yak \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
root.c : root.m4 syndex.m4x U.m4x C.m4x sammp.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x yak.mnt && ./yak.mnt ||\
	$(Macros_Path)/rshcd yak "\
	$(RM) root root.c root.root.o yak.mnt"
	$(RM) root root.c root.root.o yak.mnt


# processor izard type=U:
sammp.all : izard
sammp.root += izard
sammp.root += =izard
izard : izard.izard.o $(izard.libs)
	S=`$(Macros_Path)/rshcd izard "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd izard $(CC) $(CFLAGS) $^ $$L -lm -o $@
izard.%.o : %.c
	$(Macros_Path)/rshcd izard \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
izard.c : izard.m4 syndex.m4x U.m4x C.m4x sammp.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x izard.mnt && ./izard.mnt ||\
	$(Macros_Path)/rshcd izard "\
	$(RM) izard izard.c izard.izard.o izard.mnt"
	$(RM) izard izard.c izard.izard.o izard.mnt


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


# processor ourson type=U:
sammp.all : ourson
sammp.root += ourson
sammp.root += =ourson
ourson : ourson.ourson.o $(ourson.libs)
	S=`$(Macros_Path)/rshcd ourson "uname -sr"` ;\
	case $$S in *SunOS*5.*) L='-lsocket -lnsl' ;; esac ;\
	$(Macros_Path)/rshcd ourson $(CC) $(CFLAGS) $^ $$L -lm -o $@
ourson.%.o : %.c
	$(Macros_Path)/rshcd ourson \
	$(CC) $(CFLAGS) -c $< -o $@ -- $^
ourson.c : ourson.m4 syndex.m4x U.m4x C.m4x sammp.m4x TCP.m4x 
	$(M4) $< >$@
clean ::
	test -x ourson.mnt && ./ourson.mnt ||\
	$(Macros_Path)/rshcd ourson "\
	$(RM) ourson ourson.c ourson.ourson.o ourson.mnt"
	$(RM) ourson ourson.c ourson.ourson.o ourson.mnt




