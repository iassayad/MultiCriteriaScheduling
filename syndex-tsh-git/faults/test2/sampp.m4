include(syndex.m4m)dnl
architecture_(sampp,
SynDEx-6.7.0 (c)INRIA 2002, 16/6/2004 09:04:51
)

processor_(U,root,TCP,x,TCP,y)
processor_(U,izard,TCP,x,TCP,y)
processor_(U,ourson,TCP,x,TCP,y)

connect_(TCP,L13_2,izard,y,ourson,x)
connect_(TCP,L13_1,root,x,izard,x)
connect_(TCP,L13,root,y,ourson,y)

endarchitecture_