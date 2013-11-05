include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,root,conditionnested,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:44:55
)


semaphores_(
  Semaphore_Thread_x,
  top_c_add2_o_top_i1_o_2_root_x_empty,
  top_c_add2_o_top_i1_o_2_root_x_full,
  top_c_CondI_i2_c_i2_o__root_x_empty,
  top_c_CondI_i2_c_i2_o__root_x_full,
  top_i1_o__root_x_empty,
  top_i1_o__root_x_full,
)


alloc_(int,top_i2_o,1)
alloc_(int,top_i1_o,1)
alloc_(int,top_c_CondI_i2_c_i2_o,1)
alloc_(int,top_c_add2_o,1)

thread_(TCP,x,root,u1)
  loadDnto_(,u1)
  Pre0_(top_i1_o__root_x_empty)
  Pre0_(top_c_CondI_i2_c_i2_o__root_x_empty)
  Pre0_(top_c_add2_o_top_i1_o_2_root_x_empty)
  loop_
    Suc1_(top_i1_o__root_x_full)
    send_(top_i1_o,U,root,u1)
    Pre0_(top_i1_o__root_x_empty)
    Suc1_(top_c_CondI_i2_c_i2_o__root_x_full)
    send_(top_c_CondI_i2_c_i2_o,U,root,u1)
    Pre0_(top_c_CondI_i2_c_i2_o__root_x_empty)
    Suc1_(top_c_add2_o_top_i1_o_2_root_x_full)
    if_(top_i1_o,2)
      send_(top_c_add2_o,U,root,u1)
    endif_
    Pre0_(top_c_add2_o_top_i1_o_2_root_x_empty)
  endloop_
  saveFrom_(,u1)
endthread_

main_
  spawn_thread_(x)
  int_input(1,top_i2_o)
  int_input(1,top_i1_o)
  int_Arit_add(1,top_c_CondI_i2_c_i2_o,top_i1_o,top_c_add2_o)

  loop_
    int_input(1,top_i2_o)
    Suc0_(top_i1_o__root_x_empty,x)
    int_input(1,top_i1_o)
    Pre1_(top_i1_o__root_x_full,x)
    Suc0_(top_c_CondI_i2_c_i2_o__root_x_empty,x)
    CondI(top_i2_o,top_c_CondI_i2_c_i2_o)
    Pre1_(top_c_CondI_i2_c_i2_o__root_x_full,x)
    Suc0_(top_c_add2_o_top_i1_o_2_root_x_empty,x)
    if_(top_i1_o,2)
      int_Arit_add(1,top_c_CondI_i2_c_i2_o,top_i1_o,top_c_add2_o)
    endif_
    Pre1_(top_c_add2_o_top_i1_o_2_root_x_full,x)
  endloop_
  int_input(1,top_i2_o)
  int_input(1,top_i1_o)
  int_Arit_add(1,top_c_CondI_i2_c_i2_o,top_i1_o,top_c_add2_o)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_