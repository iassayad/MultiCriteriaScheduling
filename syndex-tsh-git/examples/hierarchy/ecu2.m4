include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,ecu2,hierarchy,
SynDEx-6.5.2 (c)INRIA 2002, 18/11/2002 10:33:21
)


semaphores_(
  Semaphore_Thread_x,
  Main_C_sub_o__ecu2_x_empty,
  Main_C_sub_o__ecu2_x_full,
  Main_C_B1_A1_add_o__ecu2_x_empty,
  Main_C_B1_A1_add_o__ecu2_x_full,
  Main_C_B2_A1_add_o__ecu2_x_empty,
  Main_C_B2_A1_add_o__ecu2_x_full,
  Main_i2_o__ecu2_x_empty,
  Main_i2_o__ecu2_x_full,
  Main_i1_o__ecu2_x_empty,
  Main_i1_o__ecu2_x_full,
)


alloc_(int,Main_i1_o,1)
alloc_(int,Main_i2_o,1)
alloc_(int,Main_C_sub_o,1)
alloc_(int,Main_C_B1_zero_ecu2_o,1)
alloc_(int,Main_C_B2_zero_ecu2_o,1)
alloc_(int,Main_C_B1_A1_add_o,1)
alloc_(int,Main_C_B2_A1_add_o,1)

thread_(TCP,x,root,ecu1,ecu2)
  loadFrom_(root)
  Pre0_(Main_C_B2_A1_add_o__ecu2_x_empty)
  Pre0_(Main_C_B1_A1_add_o__ecu2_x_empty)
  loop_
    Suc1_(Main_i1_o__ecu2_x_empty)
    recv_(Main_i1_o,U,root,ecu2)
    Pre0_(Main_i1_o__ecu2_x_full)
    Suc1_(Main_i2_o__ecu2_x_empty)
    recv_(Main_i2_o,U,root,ecu2,ecu1)
    Pre0_(Main_i2_o__ecu2_x_full)
    sync_(int,1,U,root,ecu1)
    Suc1_(Main_C_B2_A1_add_o__ecu2_x_full)
    send_(Main_C_B2_A1_add_o,U,ecu2,ecu1)
    Pre0_(Main_C_B2_A1_add_o__ecu2_x_empty)
    Suc1_(Main_C_B1_A1_add_o__ecu2_x_full)
    send_(Main_C_B1_A1_add_o,U,ecu2,ecu1)
    Pre0_(Main_C_B1_A1_add_o__ecu2_x_empty)
    Suc1_(Main_C_sub_o__ecu2_x_empty)
    recv_(Main_C_sub_o,U,ecu1,ecu2)
    Pre0_(Main_C_sub_o__ecu2_x_full)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(x)
  int_cst(0,Main_C_B1_zero_ecu2_o)
  int_cst(0,Main_C_B2_zero_ecu2_o)
  int_Arit_add(Main_i1_o,Main_C_B1_zero_ecu2_o,Main_C_B1_A1_add_o)
  int_Arit_add(Main_i2_o,Main_C_B2_zero_ecu2_o,Main_C_B2_A1_add_o)
  int_output(Main_C_sub_o)
  Pre1_(Main_i1_o__ecu2_x_empty,x)
  Pre1_(Main_i2_o__ecu2_x_empty,x)
  Pre1_(Main_C_sub_o__ecu2_x_empty,x)
  loop_
    Suc0_(Main_i1_o__ecu2_x_full,x)
    Suc0_(Main_C_B1_A1_add_o__ecu2_x_empty,x)
    int_Arit_add(Main_i1_o,Main_C_B1_zero_ecu2_o,Main_C_B1_A1_add_o)
    Pre1_(Main_i1_o__ecu2_x_empty,x)
    Pre1_(Main_C_B1_A1_add_o__ecu2_x_full,x)
    Suc0_(Main_i2_o__ecu2_x_full,x)
    Suc0_(Main_C_B2_A1_add_o__ecu2_x_empty,x)
    int_Arit_add(Main_i2_o,Main_C_B2_zero_ecu2_o,Main_C_B2_A1_add_o)
    Pre1_(Main_i2_o__ecu2_x_empty,x)
    Pre1_(Main_C_B2_A1_add_o__ecu2_x_full,x)
    Suc0_(Main_C_sub_o__ecu2_x_full,x)
    int_output(Main_C_sub_o)
    Pre1_(Main_C_sub_o__ecu2_x_empty,x)
  endloop_
  int_cst(0,Main_C_B1_zero_ecu2_o)
  int_cst(0,Main_C_B2_zero_ecu2_o)
  int_Arit_add(Main_i1_o,Main_C_B1_zero_ecu2_o,Main_C_B1_A1_add_o)
  int_Arit_add(Main_i2_o,Main_C_B2_zero_ecu2_o,Main_C_B2_A1_add_o)
  int_output(Main_C_sub_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_