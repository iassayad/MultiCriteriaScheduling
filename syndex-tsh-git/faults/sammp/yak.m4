include(syndex.m4x)dnl
include(reliability.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,yak,sammp,
SynDEx-6.7.0 (c)INRIA 2002, 28/6/2004 15:19:40
)


semaphores_(
  Semaphore_Thread_y,
  add_o__yak_y_empty,
  add_o__yak_y_full,
)

alloc_(int,add_o,1)

media_failures_
processors_failures_
 
thread_(TCP,y,root,izard,ourson,yak)
  loadFrom_(root)

  loop_
    reliable_sync_(input1_o,U,izard,ourson)
    reliable_sync_(input2_o,U,izard,ourson)
    Suc1_(add_o__yak_y_empty)
    reliable_recv_(add_o,U,2.,ourson,yak)
    Pre0_(add_o__yak_y_full)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(y)
  int_output(1,add_o)
  Pre1_(add_o__yak_y_empty,y)
  loop_
    Suc0_(add_o__yak_y_full,y)
    int_output(1,add_o)
    Pre1_(add_o__yak_y_empty,y)
  endloop_
  int_output(1,add_o)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
