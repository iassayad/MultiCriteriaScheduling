include(syndex.m4x)dnl
include(failures.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,root,sampp,
SynDEx-6.7.0 (c)INRIA 2002, 16/6/2004 09:04:51
)


semaphores_(
  Semaphore_Thread_y,
  Semaphore_Thread_x,
  input2_o__root_x_empty,
  input2_o__root_x_full,
  input1_o__root_x_empty,
  input1_o__root_x_full,
  Mul_1_o__root_x_empty,
  Mul_1_o__root_x_full,
  input2_o__root_y_empty,
  input2_o__root_y_full,
  Mul_o__root_y_empty,
  Mul_o__root_y_full,
  input1_o__root_y_empty,
  input1_o__root_y_full,
)


alloc_(int,Mul_o,1)
alloc_(int,Mul_1_o,1)
alloc_(int,input1_o,1)
alloc_(int,input2_o,1)


thread_(TCP,y,root,ourson)
  loadDnto_(,ourson)

  loop_
    Suc1_(input1_o__root_y_full)
    send_(input1_o,U,root,ourson)
    Suc1_(input2_o__root_y_full)
    send_(input2_o,U,root,ourson)
    recv_(Mul_o,U,ourson,root)
    Pre0_(Mul_o__root_y_full)
    clock_synchronisation_
endloop_
  saveFrom_(,ourson)
endthread_

thread_(TCP,x,root,izard)
  loadDnto_(,izard)

  loop_
    Suc1_(input1_o__root_x_full)
    send_(input1_o,U,root,izard)
    Suc1_(input2_o__root_x_full)
    send_(input2_o,U,root,izard)
    recv_(Mul_1_o,U,izard,root)
    Pre0_(Mul_1_o__root_x_full)
    clock_synchronisation_
endloop_
  saveFrom_(,izard)
endthread_

main_
  spawn_thread_(y)
  spawn_thread_(x)
  int_input(1,input1_o)
  int_input(1,input2_o)
  int_output(1,Mul_1_o)
  int_output(1,Mul_o)

  loop_
    int_input(1,input1_o)
    Pre1_(input1_o__root_x_full,x)
    Pre1_(input1_o__root_y_full,y)
    int_input(1,input2_o)
    Pre1_(input2_o__root_x_full,x)
    Pre1_(input2_o__root_y_full,y)
    Suc0_(Mul_1_o__root_x_full,x)
    int_output(1,Mul_1_o)
    Suc0_(Mul_o__root_y_full,y)
    int_output(1,Mul_o)
    clock_synchronisation_
endloop_
  int_input(1,input1_o)
  int_input(1,input2_o)
  int_output(1,Mul_1_o)
  int_output(1,Mul_o)
  wait_endthread_(Semaphore_Thread_y)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_
