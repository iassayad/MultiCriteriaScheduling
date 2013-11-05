include(syndex.m4x)dnl
include(failures.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,izard,sampp,
SynDEx-6.7.0 (c)INRIA 2002, 16/6/2004 09:04:51
)


semaphores_(
  Semaphore_Thread_x,
  Semaphore_Thread_y,
  input2_o__izard_x_empty,
  input2_o__izard_x_full,
  Mul_1_o__izard_x_empty,
  Mul_1_o__izard_x_full,
  input1_o__izard_x_empty,
  input1_o__izard_x_full,
)


alloc_(int,input1_o,1)
alloc_(int,input2_o,1)
alloc_(int,Mul_1_o,1)


thread_(TCP,x,root,izard)
  loadFrom_(root)

  loop_
    recv_(input1_o,U,root,izard)
    Pre0_(input1_o__izard_x_full)
    recv_(input2_o,U,root,izard)
    Pre0_(input2_o__izard_x_full)
    Suc1_(Mul_1_o__izard_x_full)
    send_(Mul_1_o,U,izard,root)
    clock_synchronisation_
endloop_
  saveUpto_(root)
endthread_

thread_(TCP,y,,izard,ourson)

  loop_
    clock_synchronisation_
endloop_
endthread_

main_
  spawn_thread_(x)
  spawn_thread_(y)

  loop_
    Suc0_(input1_o__izard_x_full,x)
    Suc0_(input2_o__izard_x_full,x)
    int_Arit_mul(1,input1_o,input2_o,Mul_1_o)
    Pre1_(Mul_1_o__izard_x_full,x)
    clock_synchronisation_
endloop_
  wait_endthread_(Semaphore_Thread_x)
  wait_endthread_(Semaphore_Thread_y)
endmain_

endprocessor_
