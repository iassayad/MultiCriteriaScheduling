include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,u1,conditionnested,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:44:55
)


semaphores_(
  Semaphore_Thread_x,
  top_c_add2_o_top_i1_o_2_u1_x_empty,
  top_c_add2_o_top_i1_o_2_u1_x_full,
  top_c_CondI_i2_c_i2_o__u1_x_empty,
  top_c_CondI_i2_c_i2_o__u1_x_full,
  top_i1_o__u1_x_empty,
  top_i1_o__u1_x_full,
)


alloc_(int,top_i1_o,1)
alloc_(int,top_c_CondI_i2_c_i2_o,1)
alloc_(int,top_c_add2_o,1)
alloc_(int,top_c_deux_u1_o,1)
alloc_(int,top_c_imbr_addimbr1_o,1)
alloc_(int,top_c_imbr_div2_o,1)
alloc_(int,top_c_imbr_mulimbr1_o,1)
alloc_(int,top_c_CondO_imbr_c_imbr_c,1)
alloc_(int,top_CondO_c_b_c_b,1)

thread_(TCP,x,root,u1)
  loadFrom_(root)

  loop_
    Suc1_(top_i1_o__u1_x_empty)
    recv_(top_i1_o,U,root,u1)
    Pre0_(top_i1_o__u1_x_full)
    Suc1_(top_c_CondI_i2_c_i2_o__u1_x_empty)
    recv_(top_c_CondI_i2_c_i2_o,U,root,u1)
    Pre0_(top_c_CondI_i2_c_i2_o__u1_x_full)
    Suc1_(top_c_add2_o_top_i1_o_2_u1_x_empty)
    if_(top_i1_o,2)
      recv_(top_c_add2_o,U,root,u1)
    endif_
    Pre0_(top_c_add2_o_top_i1_o_2_u1_x_full)
  endloop_
  saveUpto_(root)
endthread_

main_
  spawn_thread_(x)
  int_cst(1,2,top_c_deux_u1_o)
  int_Arit_add(1,top_c_CondI_i2_c_i2_o,top_i1_o,top_c_imbr_addimbr1_o)
  int_Arit_div(1,top_i1_o,top_c_deux_u1_o,top_c_imbr_div2_o)
  int_Arit_mul(1,top_c_imbr_addimbr1_o,top_c_deux_u1_o,top_c_imbr_mulimbr1_o)
  int_output(1,top_CondO_c_b_c_b)
  Pre1_(top_c_CondI_i2_c_i2_o__u1_x_empty,x)
  Pre1_(top_i1_o__u1_x_empty,x)
  Pre1_(top_c_add2_o_top_i1_o_2_u1_x_empty,x)
  loop_
    Suc0_(top_c_CondI_i2_c_i2_o__u1_x_full,x)
    Suc0_(top_i1_o__u1_x_full,x)
    if_(top_i1_o,1)
      switch_(top_c_CondI_i2_c_i2_o)
        case_(1)
          int_Arit_add(1,top_c_CondI_i2_c_i2_o,top_i1_o,top_c_imbr_addimbr1_o)
        endcase_
        case_(2)
          int_Arit_div(1,top_i1_o,top_c_deux_u1_o,top_c_imbr_div2_o)
        endcase_
      endswitch_
    endif_
    if_(top_i1_o,1)
      if_(top_c_CondI_i2_c_i2_o,1)
        int_Arit_mul(1,top_c_imbr_addimbr1_o,top_c_deux_u1_o,top_c_imbr_mulimbr1_o)
      endif_
    endif_
    if_(top_i1_o,1)
      CondO(top_c_CondI_i2_c_i2_o,4,top_c_deux_u1_o,3,top_i1_o,1,top_c_imbr_mulimbr1_o,2,top_c_imbr_div2_o,top_c_CondO_imbr_c_imbr_c)
    endif_
    Pre1_(top_c_CondI_i2_c_i2_o__u1_x_empty,x)
    Suc0_(top_c_add2_o_top_i1_o_2_u1_x_full,x)
    CondO(top_i1_o,2,top_c_add2_o,1,top_c_CondO_imbr_c_imbr_c,top_CondO_c_b_c_b)
    Pre1_(top_c_add2_o_top_i1_o_2_u1_x_empty,x)
    Pre1_(top_i1_o__u1_x_empty,x)
    int_output(1,top_CondO_c_b_c_b)
  endloop_
  int_cst(1,2,top_c_deux_u1_o)
  int_Arit_add(1,top_c_CondI_i2_c_i2_o,top_i1_o,top_c_imbr_addimbr1_o)
  int_Arit_div(1,top_i1_o,top_c_deux_u1_o,top_c_imbr_div2_o)
  int_Arit_mul(1,top_c_imbr_addimbr1_o,top_c_deux_u1_o,top_c_imbr_mulimbr1_o)
  int_output(1,top_CondO_c_b_c_b)
  wait_endthread_(Semaphore_Thread_x)
endmain_

endprocessor_