include(syndex.m4x)
include(int.m4x)
include(u.m4x)
processor_(U,root,basic,
SynDEx-6.6.0 (c)INRIA 2002, 22/1/2003 15:01:57
)




alloc_(int,algoBasic_cste_root_o,1)
alloc_(int,algoBasic_input_o,1)
alloc_(int,algoBasic_add_o,1)
alloc_(int,algoBasic_mul_o,1)

main_
  int_cst(1,2,algoBasic_cste_root_o)
  int_input(1,algoBasic_input_o)
  int_Arit_add(1,algoBasic_input_o,algoBasic_cste_root_o,algoBasic_add_o)
  int_Arit_mul(1,algoBasic_input_o,algoBasic_cste_root_o,algoBasic_mul_o)
  int_output(1,algoBasic_add_o)
  int_output(1,algoBasic_mul_o)

  loop_
    int_input(1,algoBasic_input_o)
    int_Arit_add(1,algoBasic_input_o,algoBasic_cste_root_o,algoBasic_add_o)
    int_Arit_mul(1,algoBasic_input_o,algoBasic_cste_root_o,algoBasic_mul_o)
    int_output(1,algoBasic_add_o)
    int_output(1,algoBasic_mul_o)
  endloop_
  int_cst(1,2,algoBasic_cste_root_o)
  int_input(1,algoBasic_input_o)
  int_Arit_add(1,algoBasic_input_o,algoBasic_cste_root_o,algoBasic_add_o)
  int_Arit_mul(1,algoBasic_input_o,algoBasic_cste_root_o,algoBasic_mul_o)
  int_output(1,algoBasic_add_o)
  int_output(1,algoBasic_mul_o)
endmain_

endprocessor_