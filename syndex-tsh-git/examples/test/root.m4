include(syndex.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,root,example6,
SynDEx-6.7.0 (c)INRIA 2002, 7/6/2004 10:58:19
)


semaphores_(
  Semaphore_Thread_y,
  mul_o__root_y_empty,
  mul_o__root_y_full,
  conv_o__root_y_empty,
  conv_o__root_y_full,
)


alloc_(int,mul_o,1)
alloc_(int,conv_o,1)
alloc_(int,cste2_root_o,1)


thread_(TCP,y,root,pc1)
  loadDnto_(,pc1)
  Pre0_(conv_o__root_y_empty)
  loop_
    Suc1_(conv_o__root_y_full)
    send_(conv_o,U,root,pc1)
    Pre0_(conv_o__root_y_empty)
    Suc1_(mul_o__root_y_empty)
    recv_(mul_o,U,pc1,root)
    Pre0_(mul_o__root_y_full)
  endloop_
  saveFrom_(,pc1)
endthread_

main_
  spawn_thread_(y)
  int_cst(1,2,cste2_root_o)
  int_output(1,mul_o)
  Pre1_(mul_o__root_y_empty,y)
  loop_
    Suc0_(conv_o__root_y_empty,y)
    conv(cste2_root_o,conv_o)
    Pre1_(conv_o__root_y_full,y)
    Suc0_(mul_o__root_y_full,y)
    int_output(1,mul_o)
    Pre1_(mul_o__root_y_empty,y)
  endloop_
  int_output(1,mul_o)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
