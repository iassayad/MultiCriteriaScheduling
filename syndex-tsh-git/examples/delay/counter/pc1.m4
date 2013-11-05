include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,pc1,counter,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:45:52
)


semaphores_(
  Semaphore_Thread_x,
  counter_Z_o__pc1_x_empty,
  counter_Z_o__pc1_x_full,
  counter_add_o__pc1_x_empty,
  counter_add_o__pc1_x_full,
)


alloc_(int,counter_add_o,1)
alloc_(int,counter_Z_o,1)
alloc_(int,counter_Z_buf,1)

thread_(TCP,x,root,pc1)
  loadFrom_(root)
  Pre0_(counter_Z_o__pc1_x_empty)
  loop_
    Suc1_(counter_add_o__pc1_x_empty)
    recv_(counter_add_o,U,root,pc1)
    Pre0_(counter_add_o__pc1_x_full)
    Suc1_(counter_Z_o__pc1_x_full)
    send_(counter_Z_o,U,pc1,root)
    Pre0_(counter_Z_o__pc1_x_empty)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(x)
  int_delay(1,0,counter_add_o,counter_Z_o)
  Pre1_(counter_add_o__pc1_x_empty,x)
  loop_
    Suc0_(counter_add_o__pc1_x_full,x)
    Suc0_(counter_Z_o__pc1_x_empty,x)
    memory_copy_(1,0,counter_add_o,counter_Z_o)
    Pre1_(counter_add_o__pc1_x_empty,x)
    Pre1_(counter_Z_o__pc1_x_full,x)
  endloop_
  int_delay(1,0,counter_add_o,counter_Z_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_