include(syndex.m4x)dnl
include(int.m4x)dnl
include(u.m4x)dnl

processor_(U,ourson,sampp,
SynDEx-6.7.0 (c)INRIA 2002, 15/6/2004 07:48:52
)


semaphores_(
  Semaphore_Thread_y,
  Semaphore_Thread_x,
  Sub_o__ourson_x_empty,
  Sub_o__ourson_x_full,
  input1_o__ourson_y_empty,
  input1_o__ourson_y_full,
  input2_o__ourson_y_empty,
  input2_o__ourson_y_full,
  Mul_o__ourson_y_empty,
  Mul_o__ourson_y_full,
)


alloc_(int,input2_o,1)
alloc_(int,input1_o,1)
alloc_(int,Sub_o,1)
alloc_(int,Mul_o,1)
alloc_(int,Add_o,1)


thread_(TCP,y,root,ourson)
  loadFrom_(root)
  Pre0_(Mul_o__ourson_y_empty)
  loop_
    Suc1_(input1_o__ourson_y_empty)
    recv_(input1_o,U,root,ourson)
    Pre0_(input1_o__ourson_y_full)
    Suc1_(input2_o__ourson_y_empty)
    recv_(input2_o,U,root,ourson)
    Pre0_(input2_o__ourson_y_full)
    Suc1_(Mul_o__ourson_y_full)
    send_(Mul_o,U,ourson,root)
    Pre0_(Mul_o__ourson_y_empty)
  endloop_
  saveUpto_(root)
endthread_

thread_(TCP,x,,izard,ourson)

  loop_
    Suc1_(Sub_o__ourson_x_empty)
    recv_(Sub_o,U,izard,ourson)
    Pre0_(Sub_o__ourson_x_full)
  endloop_
endthread_

main_
  spawn_thread_(y)
  spawn_thread_(x)
  Pre1_(input1_o__ourson_y_empty,y)
  Pre1_(input2_o__ourson_y_empty,y)
  Pre1_(Sub_o__ourson_x_empty,x)
  loop_
    Suc0_(input1_o__ourson_y_full,y)
    Suc0_(input2_o__ourson_y_full,y)
    addition(input1_o,input2_o,Add_o)
    Pre1_(input1_o__ourson_y_empty,y)
    Pre1_(input2_o__ourson_y_empty,y)
    Suc0_(Sub_o__ourson_x_full,x)
    Suc0_(Mul_o__ourson_y_empty,y)
    int_Arit_mul(1,Add_o,Sub_o,Mul_o)
    Pre1_(Sub_o__ourson_x_empty,x)
    Pre1_(Mul_o__ourson_y_full,y)
  endloop_
  wait_endthread_(Semaphore_Thread_y)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_
