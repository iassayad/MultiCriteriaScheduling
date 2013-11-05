include(syndex.m4x)dnl
include(fault_tolerance.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,louve,sammp,
SynDEx-6.7.0 (c)INRIA 2002, 25/6/2004 13:09:44
)


semaphores_(
  Semaphore_Thread_y,
  add_o__louve_y_empty,
  add_o__louve_y_full,
  input2_o__louve_y_empty,
  input2_o__louve_y_full,
  input1_o__louve_y_empty,
  input1_o__louve_y_full,
)


alloc_(int,input1_o,1)
alloc_(int,input2_o,1)
alloc_(int,add_o,1)


thread_(TCP,y,root,louve)
  loadFrom_(root)
  Pre0_(add_o__louve_y_empty)
  loop_
    Suc1_(input2_o__louve_y_empty)
    recv_(input2_o,U,root,louve)
    Pre0_(input2_o__louve_y_full)
    Suc1_(input1_o__louve_y_empty)
    recv_(input1_o,U,root,louve)
    Pre0_(input1_o__louve_y_full)
    Suc1_(add_o__louve_y_full)
    send_(add_o,U,louve,root)
    Pre0_(add_o__louve_y_empty)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(y)
  Pre1_(input1_o__louve_y_empty,y)
  Pre1_(input2_o__louve_y_empty,y)
  loop_
    Suc0_(input1_o__louve_y_full,y)
    Suc0_(input2_o__louve_y_full,y)
    Suc0_(add_o__louve_y_empty,y)
    addition(input1_o,input2_o,add_o)
    Pre1_(input1_o__louve_y_empty,y)
    Pre1_(input2_o__louve_y_empty,y)
    Pre1_(add_o__louve_y_full,y)
  endloop_
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
