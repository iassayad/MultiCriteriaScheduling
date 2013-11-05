include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,root,condition,
SynDEx-6.6.0 (c)INRIA 2002, 29/1/2003 14:41:39
)




alloc_(int,top_cst2_root_o,1)
alloc_(int,top_cst1_root_o,1)
alloc_(int,top_x_o,1)
alloc_(int,top_c_mul4_o,1)
alloc_(int,top_c_add3_o,1)
alloc_(int,top_CondO_c_o_c_o,1)

main_
  int_cst(1,3,top_cst2_root_o)
  int_cst(1,2,top_cst1_root_o)
  int_input(1,top_x_o)
  int_Arit_mul(1,top_cst1_root_o,top_cst2_root_o,top_c_mul4_o)
  int_Arit_add(1,top_cst1_root_o,top_cst2_root_o,top_c_add3_o)
  int_output(1,top_CondO_c_o_c_o)

  loop_
    int_input(1,top_x_o)
    switch_(top_x_o)
      case_(3)
        int_Arit_add(1,top_cst1_root_o,top_cst2_root_o,top_c_add3_o)
      endcase_
      case_(4)
        int_Arit_mul(1,top_cst1_root_o,top_cst2_root_o,top_c_mul4_o)
      endcase_
    endswitch_
    CondO(top_x_o,1,top_cst1_root_o,2,top_cst2_root_o,4,top_c_mul4_o,3,top_c_add3_o,top_CondO_c_o_c_o)
    int_output(1,top_CondO_c_o_c_o)
  endloop_
  int_cst(1,3,top_cst2_root_o)
  int_cst(1,2,top_cst1_root_o)
  int_input(1,top_x_o)
  int_Arit_mul(1,top_cst1_root_o,top_cst2_root_o,top_c_mul4_o)
  int_Arit_add(1,top_cst1_root_o,top_cst2_root_o,top_c_add3_o)
  int_output(1,top_CondO_c_o_c_o)
endmain_

endprocessor_