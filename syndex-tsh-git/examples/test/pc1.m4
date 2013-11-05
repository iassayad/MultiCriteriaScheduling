include(syndex.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,pc1,example6,
SynDEx-6.7.0 (c)INRIA 2002, 7/6/2004 10:58:19
)


semaphores_(
  Semaphore_Thread_y,
  conv_o__pc1_y_empty,
  conv_o__pc1_y_full,
  mul_o__pc1_y_empty,
  mul_o__pc1_y_full,
)


alloc_(int,conv_o,1)
alloc_(int,add_o,1)
alloc_(int,In_o,1)
alloc_(int,mul_o,1)
alloc_(int,cste2_pc1_o,1)


thread_(TCP,y,root,pc1)
  loadFrom_(root)
  Pre0_(mul_o__pc1_y_empty)
  loop_
    Suc1_(conv_o__pc1_y_empty)
    recv_(conv_o,U,root,pc1)
    Pre0_(conv_o__pc1_y_full)
    Suc1_(mul_o__pc1_y_full)
    send_(mul_o,U,pc1,root)
    Pre0_(mul_o__pc1_y_empty)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(y)
  int_cst(1,2,cste2_pc1_o)
  int_input(1,In_o)
  int_output(1,add_o)
  Pre1_(conv_o__pc1_y_empty,y)
  loop_
    int_input(1,In_o)
    int_Arit_add(1,In_o,cste2_pc1_o,add_o)
    Suc0_(conv_o__pc1_y_full,y)
    Suc0_(mul_o__pc1_y_empty,y)
    int_Arit_mul(1,In_o,conv_o,mul_o)
    Pre1_(conv_o__pc1_y_empty,y)
    Pre1_(mul_o__pc1_y_full,y)
    int_output(1,add_o)
  endloop_
  int_input(1,In_o)
  int_output(1,add_o)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
