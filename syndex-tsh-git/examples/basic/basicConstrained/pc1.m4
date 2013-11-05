include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,pc1,basic,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:39:31
)


semaphores_(
  Semaphore_Thread_x,
  algoBasic_mul_o__pc1_x_empty,
  algoBasic_mul_o__pc1_x_full,
  algoBasic_add_o__pc1_x_empty,
  algoBasic_add_o__pc1_x_full,
  algoBasic_input_o__pc1_x_empty,
  algoBasic_input_o__pc1_x_full,
)


alloc_(int,algoBasic_input_o,1)
alloc_(int,algoBasic_mul_o,1)
alloc_(int,algoBasic_cste_pc1_o,1)
alloc_(int,algoBasic_add_o,1)

thread_(TCP,x,root,pc1)
  loadFrom_(root)
  Pre0_(algoBasic_add_o__pc1_x_empty)
  loop_
    Suc1_(algoBasic_input_o__pc1_x_empty)
    recv_(algoBasic_input_o,U,root,pc1)
    Pre0_(algoBasic_input_o__pc1_x_full)
    Suc1_(algoBasic_add_o__pc1_x_full)
    send_(algoBasic_add_o,U,pc1,root)
    Pre0_(algoBasic_add_o__pc1_x_empty)
    Suc1_(algoBasic_mul_o__pc1_x_empty)
    recv_(algoBasic_mul_o,U,root,pc1)
    Pre0_(algoBasic_mul_o__pc1_x_full)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(x)
  int_cst(1,2,algoBasic_cste_pc1_o)
  int_Arit_add(1,algoBasic_input_o,algoBasic_cste_pc1_o,algoBasic_add_o)
  int_output(1,algoBasic_mul_o)
  Pre1_(algoBasic_input_o__pc1_x_empty,x)
  Pre1_(algoBasic_mul_o__pc1_x_empty,x)
  loop_
    Suc0_(algoBasic_input_o__pc1_x_full,x)
    Suc0_(algoBasic_add_o__pc1_x_empty,x)
    int_Arit_add(1,algoBasic_input_o,algoBasic_cste_pc1_o,algoBasic_add_o)
    Pre1_(algoBasic_input_o__pc1_x_empty,x)
    Pre1_(algoBasic_add_o__pc1_x_full,x)
    Suc0_(algoBasic_mul_o__pc1_x_full,x)
    int_output(1,algoBasic_mul_o)
    Pre1_(algoBasic_mul_o__pc1_x_empty,x)
  endloop_
  int_cst(1,2,algoBasic_cste_pc1_o)
  int_Arit_add(1,algoBasic_input_o,algoBasic_cste_pc1_o,algoBasic_add_o)
  int_output(1,algoBasic_mul_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_