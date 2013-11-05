include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,root,basic,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:39:15
)


semaphores_(
  Semaphore_Thread_x)


alloc_(int,algoBasic_cste2_root_o,1)
alloc_(int,algoBasic_cste1_root_o,1)
alloc_(int,algoBasic_mul_o,1)

thread_(TCP,x,root,pc1)
  loadDnto_(,pc1)

  loop_
  endloop_
  saveFrom_(,pc1)
endthread_

main_
  spawn_thread_(x)
  int_cst(1,5,algoBasic_cste2_root_o)
  int_cst(1,2,algoBasic_cste1_root_o)
  int_Arit_mul(1,algoBasic_cste1_root_o,algoBasic_cste2_root_o,algoBasic_mul_o)
  int_output(1,algoBasic_mul_o)

  loop_
    int_Arit_mul(1,algoBasic_cste1_root_o,algoBasic_cste2_root_o,algoBasic_mul_o)
    int_output(1,algoBasic_mul_o)
  endloop_
  int_cst(1,5,algoBasic_cste2_root_o)
  int_cst(1,2,algoBasic_cste1_root_o)
  int_Arit_mul(1,algoBasic_cste1_root_o,algoBasic_cste2_root_o,algoBasic_mul_o)
  int_output(1,algoBasic_mul_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_