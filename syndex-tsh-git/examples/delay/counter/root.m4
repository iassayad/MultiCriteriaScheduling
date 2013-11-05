include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,root,counter,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:45:52
)


semaphores_(
  Semaphore_Thread_x,
  counter_Z_o__root_x_empty,
  counter_Z_o__root_x_full,
  counter_add_o__root_x_empty,
  counter_add_o__root_x_full,
)


alloc_(int,counter_Z_o,1)
alloc_(int,counter_cst_root_o,1)
alloc_(int,counter_add_o,1)

thread_(TCP,x,root,pc1)
  loadDnto_(,pc1)
  Pre0_(counter_add_o__root_x_empty)
  Pre0_(counter_Z_o__root_x_full)
  loop_
    Suc1_(counter_add_o__root_x_full)
    send_(counter_add_o,U,root,pc1)
    Pre0_(counter_add_o__root_x_empty)
    Suc1_(counter_Z_o__root_x_empty)
    recv_(counter_Z_o,U,pc1,root)
    Pre0_(counter_Z_o__root_x_full)
  endloop_
  saveFrom_(,pc1)
endthread_

main_
  spawn_thread_(x)
  int_cst(1,1,counter_cst_root_o)
  int_Arit_add(1,counter_Z_o,counter_cst_root_o,counter_add_o)
  int_output(1,counter_add_o)
  int_delay(1,0,counter_add_o,counter_Z_o)

  loop_
    Suc0_(counter_Z_o__root_x_full,x)
    Suc0_(counter_add_o__root_x_empty,x)
    int_Arit_add(1,counter_Z_o,counter_cst_root_o,counter_add_o)
    Pre1_(counter_Z_o__root_x_empty,x)
    Pre1_(counter_add_o__root_x_full,x)
    int_output(1,counter_add_o)
  endloop_
  int_cst(1,1,counter_cst_root_o)
  int_Arit_add(1,counter_Z_o,counter_cst_root_o,counter_add_o)
  int_output(1,counter_add_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_