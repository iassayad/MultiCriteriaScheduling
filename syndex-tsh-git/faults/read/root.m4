include(syndex.m4x)dnl
include(fault_tolerance.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,root,sammp,
SynDEx-6.7.0 (c)INRIA 2002, 25/6/2004 13:09:44
)


semaphores_(
  Semaphore_Thread_y,
  input1_o__root_y_empty,
  input1_o__root_y_full,
  add_o__root_y_empty,
  add_o__root_y_full,
  input2_o__root_y_empty,
  input2_o__root_y_full,
)


alloc_(int,add_o,1)
alloc_(int,input1_o,1)
alloc_(int,input2_o,1)


thread_(TCP,y,root,louve)
  loadDnto_(,louve)
  Pre0_(input2_o__root_y_empty)
  Pre0_(input1_o__root_y_empty)
  loop_
    Suc1_(input2_o__root_y_full)
    send_(input2_o,U,root,louve)
    Pre0_(input2_o__root_y_empty)
    Suc1_(input1_o__root_y_full)
    send_(input1_o,U,root,louve)
    Pre0_(input1_o__root_y_empty)
    Suc1_(add_o__root_y_empty)
    recv_(add_o,U,louve,root)
    Pre0_(add_o__root_y_full)
  endloop_
  saveFrom_(,louve)
endthread_

main_
  spawn_thread_(y)
  int_input(1,input1_o)
  sensor2(input2_o)
  int_output(1,add_o)
  Pre1_(add_o__root_y_empty,y)
  loop_
    Suc0_(input1_o__root_y_empty,y)
    int_input(1,input1_o)
    Pre1_(input1_o__root_y_full,y)
    Suc0_(input2_o__root_y_empty,y)
    sensor2(input2_o)
    Pre1_(input2_o__root_y_full,y)
    Suc0_(add_o__root_y_full,y)
    int_output(1,add_o)
    Pre1_(add_o__root_y_empty,y)
  endloop_
  int_input(1,input1_o)
  sensor2(input2_o)
  int_output(1,add_o)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
