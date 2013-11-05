include(syndex.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,root,example6,
SynDEx-6.7.0 (c)INRIA 2002, 7/6/2004 15:20:54
)


semaphores_(
  Semaphore_Thread_y,
  In_o__root_y_empty,
  In_o__root_y_full,
  mul_o__root_y_empty,
  mul_o__root_y_full,
  add_o__root_y_empty,
  add_o__root_y_full,
)


alloc_(int,add_o,1)
alloc_(int,mul_o,1)
alloc_(int,In_o,1)


thread_(TCP,y,root,pc1)
  loadDnto_(,pc1)
  Pre0_(In_o__root_y_empty)
  loop_
    Suc1_(In_o__root_y_full)
    send_(In_o,U,root,pc1)
    Pre0_(In_o__root_y_empty)
    Suc1_(add_o__root_y_empty)
    recv_(add_o,U,pc1,root)
    Pre0_(add_o__root_y_full)
    Suc1_(mul_o__root_y_empty)
    recv_(mul_o,U,pc1,root)
    Pre0_(mul_o__root_y_full)
  endloop_
  saveFrom_(,pc1)
endthread_

main_
  spawn_thread_(y)
  int_input(1,In_o)
  int_output(1,mul_o)
  int_output(1,add_o)
  Pre1_(mul_o__root_y_empty,y)
  Pre1_(add_o__root_y_empty,y)
  loop_
    Suc0_(In_o__root_y_empty,y)
    int_input(1,In_o)
    Pre1_(In_o__root_y_full,y)
    Suc0_(mul_o__root_y_full,y)
    int_output(1,mul_o)
    Pre1_(mul_o__root_y_empty,y)
    Suc0_(add_o__root_y_full,y)
    int_output(1,add_o)
    Pre1_(add_o__root_y_empty,y)
  endloop_
  int_input(1,In_o)
  int_output(1,mul_o)
  int_output(1,add_o)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
