include(syndex.m4m)dnl
architecture_(repetitioncondition,
SynDEx-6.6.0 (c)INRIA 2002, 22/1/2003 16:16:26
)

processor_(U,root,TCP,x,TCP,y)
processor_(U,ecu1,TCP,x,TCP,y)

connect_(TCP,eth,root,x,ecu1,x)
