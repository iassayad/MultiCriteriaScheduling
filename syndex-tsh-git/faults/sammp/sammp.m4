include(syndex.m4m)dnl
architecture_(sammp,
SynDEx-6.7.0 (c)INRIA 2002, 6/9/2004 11:16:58
)

processor_(U,root,TCP,x,TCP,y)
processor_(U,izard,TCP,x,TCP,y)
processor_(U,louve,TCP,x,TCP,y)
processor_(U,ourson,TCP,x,TCP,y)

connect_(TCP,L13,root,y,izard,y,louve,y,ourson,y)

endarchitecture_