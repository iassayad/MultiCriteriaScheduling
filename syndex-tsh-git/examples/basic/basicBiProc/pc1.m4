include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,pc1,basic,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:39:15
)


semaphores_(
  Semaphore_Thread_x)


alloc_(int,algoBasic_cste2_pc1_o,1)
alloc_(int,algoBasic_cste1_pc1_o,1)
alloc_(int,algoBasic_add_o,1)

thread_(TCP,x,root,pc1)
  loadFrom_(root)

  loop_
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(x)
  int_cst(1,5,algoBasic_cste2_pc1_o)
  int_cst(1,2,algoBasic_cste1_pc1_o)
  int_Arit_add(1,algoBasic_cste1_pc1_o,algoBasic_cste2_pc1_o,algoBasic_add_o)
  int_output(1,algoBasic_add_o)

  loop_
    int_Arit_add(1,algoBasic_cste1_pc1_o,algoBasic_cste2_pc1_o,algoBasic_add_o)
    int_output(1,algoBasic_add_o)
  endloop_
  int_cst(1,5,algoBasic_cste2_pc1_o)
  int_cst(1,2,algoBasic_cste1_pc1_o)
  int_Arit_add(1,algoBasic_cste1_pc1_o,algoBasic_cste2_pc1_o,algoBasic_add_o)
  int_output(1,algoBasic_add_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_