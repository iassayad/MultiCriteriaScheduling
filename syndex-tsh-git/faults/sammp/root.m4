include(syndex.m4x)dnl
include(reliability.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,root,sammp,
SynDEx-6.7.0 (c)INRIA 2002, 6/9/2004 11:16:58
)


semaphores_(
  Semaphore_Thread_y,
  add_o__root_y_empty,
  add_o__root_y_full,
)

alloc_(int,add_o,1)

media_failures_(0)
processors_failures_(1)

thread_(TCP,y,root,izard,louve,ourson)
  loadDnto_(,izard,louve,ourson)

  loop_
    reliable_sync_(input2_o,U,louve,izard,ourson)
    reliable_sync_(input1_o,U,louve,izard,ourson)
    Suc1_(add_o__root_y_empty)
    reliable_recv_(add_o,U,2.,louve,root)
    Pre0_(add_o__root_y_full)
  endloop_
  saveFrom_(,izard,louve,ourson)
endthread_

main_
  spawn_thread_(y)
  int_output(1,add_o)
  Pre1_(add_o__root_y_empty,y)
  loop_
    Suc0_(add_o__root_y_full,y)
    int_output(1,add_o)
    Pre1_(add_o__root_y_empty,y)
  endloop_
  int_output(1,add_o)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
