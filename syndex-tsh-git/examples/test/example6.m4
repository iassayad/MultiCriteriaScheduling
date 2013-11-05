include(syndex.m4m)dnl
architecture_(example6,
SynDEx-6.7.0 (c)INRIA 2002, 7/6/2004 10:58:19
)

processor_(U,root,TCP,x,TCP,y)
processor_(U,pc1,TCP,x,TCP,y)

connect_(TCP,ppl,root,y,pc1,y)

endarchitecture_