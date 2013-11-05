include(syndex.m4x)dnl
include(failures.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,ourson,sampp,
SynDEx-6.7.0 (c)INRIA 2002, 16/6/2004 09:04:51
)


semaphores_(
  Semaphore_Thread_y,
  Semaphore_Thread_x,
  input1_o__ourson_y_empty,
  input1_o__ourson_y_full,
  input2_o__ourson_y_empty,
  input2_o__ourson_y_full,
  Mul_o__ourson_y_empty,
  Mul_o__ourson_y_full,
)


alloc_(int,input2_o,1)
alloc_(int,input1_o,1)
alloc_(int,Mul_o,1)


thread_(TCP,y,root,ourson)
  loadFrom_(root)

  loop_
    recv_(input1_o,U,root,ourson)
    Pre0_(input1_o__ourson_y_full)
    recv_(input2_o,U,root,ourson)
    Pre0_(input2_o__ourson_y_full)
    Suc1_(Mul_o__ourson_y_full)
    send_(Mul_o,U,ourson,root)
    clock_synchronisation_
endloop_
  saveUpto_(root)
endthread_

thread_(TCP,x,,izard,ourson)

  loop_
    clock_synchronisation_
endloop_
endthread_

main_
  spawn_thread_(y)
  spawn_thread_(x)

  loop_
    Suc0_(input1_o__ourson_y_full,y)
    Suc0_(input2_o__ourson_y_full,y)
    int_Arit_mul(1,input1_o,input2_o,Mul_o)
    Pre1_(Mul_o__ourson_y_full,y)
    clock_synchronisation_
endloop_
  wait_endthread_(Semaphore_Thread_y)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_
