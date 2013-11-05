include(syndex.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,izard,sampp,
SynDEx-6.7.0 (c)INRIA 2002, 15/6/2004 07:48:52
)


semaphores_(
  Semaphore_Thread_x,
  Semaphore_Thread_y,
  Sub_o__izard_y_empty,
  Sub_o__izard_y_full,
  input2_o__izard_x_empty,
  input2_o__izard_x_full,
  input1_o__izard_x_empty,
  input1_o__izard_x_full,
)


alloc_(int,input1_o,1)
alloc_(int,input2_o,1)
alloc_(int,Sub_o,1)


thread_(TCP,x,root,izard)
  loadFrom_(root)

  loop_
    Suc1_(input1_o__izard_x_empty)
    recv_(input1_o,U,root,izard)
    Pre0_(input1_o__izard_x_full)
    Suc1_(input2_o__izard_x_empty)
    recv_(input2_o,U,root,izard)
    Pre0_(input2_o__izard_x_full)
  endloop_
  saveUpto_(root)
endthread_

thread_(TCP,y,,izard,ourson)
  Pre0_(Sub_o__izard_y_empty)
  loop_
    Suc1_(Sub_o__izard_y_full)
    send_(Sub_o,U,izard,ourson)
    Pre0_(Sub_o__izard_y_empty)
  endloop_
endthread_

main_
  spawn_thread_(x)
  spawn_thread_(y)
  Pre1_(input1_o__izard_x_empty,x)
  Pre1_(input2_o__izard_x_empty,x)
  loop_
    Suc0_(input1_o__izard_x_full,x)
    Suc0_(input2_o__izard_x_full,x)
    Suc0_(Sub_o__izard_y_empty,y)
    int_Arit_sub(1,input1_o,input2_o,Sub_o)
    Pre1_(input1_o__izard_x_empty,x)
    Pre1_(input2_o__izard_x_empty,x)
    Pre1_(Sub_o__izard_y_full,y)
  endloop_
  wait_endthread_(Semaphore_Thread_x)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
