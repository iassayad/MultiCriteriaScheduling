include(syndex.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,root,sampp,
SynDEx-6.7.0 (c)INRIA 2002, 15/6/2004 07:48:52
)


semaphores_(
  Semaphore_Thread_y,
  Semaphore_Thread_x,
  input2_o__root_x_empty,
  input2_o__root_x_full,
  input1_o__root_x_empty,
  input1_o__root_x_full,
  input2_o__root_y_empty,
  input2_o__root_y_full,
  Mul_o__root_y_empty,
  Mul_o__root_y_full,
  input1_o__root_y_empty,
  input1_o__root_y_full,
)


alloc_(int,Mul_o,1)
alloc_(int,input1_o,1)
alloc_(int,input2_o,1)


thread_(TCP,y,root,ourson)
  loadDnto_(,ourson)
  Pre0_(input1_o__root_y_empty)
  Pre0_(input2_o__root_y_empty)
  loop_
    Suc1_(input1_o__root_y_full)
    send_(input1_o,U,root,ourson)
    Pre0_(input1_o__root_y_empty)
    Suc1_(input2_o__root_y_full)
    send_(input2_o,U,root,ourson)
    Pre0_(input2_o__root_y_empty)
    Suc1_(Mul_o__root_y_empty)
    recv_(Mul_o,U,ourson,root)
    Pre0_(Mul_o__root_y_full)
  endloop_
  saveFrom_(,ourson)
endthread_

thread_(TCP,x,root,izard)
  loadDnto_(,izard)
  Pre0_(input1_o__root_x_empty)
  Pre0_(input2_o__root_x_empty)
  loop_
    Suc1_(input1_o__root_x_full)
    send_(input1_o,U,root,izard)
    Pre0_(input1_o__root_x_empty)
    Suc1_(input2_o__root_x_full)
    send_(input2_o,U,root,izard)
    Pre0_(input2_o__root_x_empty)
  endloop_
  saveFrom_(,izard)
endthread_

main_
  spawn_thread_(y)
  spawn_thread_(x)
  int_input(1,input1_o)
  int_input(1,input2_o)
  int_output(1,Mul_o)
  Pre1_(Mul_o__root_y_empty,y)
  loop_
    Suc0_(input1_o__root_x_empty,x)
    Suc0_(input1_o__root_y_empty,y)
    int_input(1,input1_o)
    Pre1_(input1_o__root_x_full,x)
    Pre1_(input1_o__root_y_full,y)
    Suc0_(input2_o__root_x_empty,x)
    Suc0_(input2_o__root_y_empty,y)
    int_input(1,input2_o)
    Pre1_(input2_o__root_x_full,x)
    Pre1_(input2_o__root_y_full,y)
    Suc0_(Mul_o__root_y_full,y)
    int_output(1,Mul_o)
    Pre1_(Mul_o__root_y_empty,y)
  endloop_
  int_input(1,input1_o)
  int_input(1,input2_o)
  int_output(1,Mul_o)
  wait_endthread_(Semaphore_Thread_y)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_
