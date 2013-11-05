/*************************************************************************/
/*                                                                       */
/*                               SynDEx 6                                */
/*                                                                       */
/*                          Christophe Macabiau                          */
/*                                                                       */
/*                     Projet Ostre, INRIA Rocquencourt                  */
/*                                                                       */
/*   Copyright 2000 Institut National de Recherche en Informatique et    */
/*   en Automatique.  All rights reserved.  This file is distributed     */
/*   under the terms of the Q Public License version 1.0.                */
/*                                                                       */
/*************************************************************************/

/* To debug the parser behaviour, affect variable OCAMRUNPARAM to 'p'. */
/* This will print a trace of the tokens recognized and rules reduced. */
/* Do the affectation just before runing SynDEx (but not at compilation). */
/* For some strange reasons, using a script to run SynDEx and not directly syndex.bin won't print the trace. */
/* In you shell, type "export OCAMLRUNPARAM='p'" (bash) or "setenv OCAMLRUNPARAM='p'" (tcsh) */
/* You should then add option -v to ocamlyacc in the Makefile and look at the grammar in parser.output */

%{
  open Types
  let archi_lib = ref ""
  let archi_name = ref ""
  let data_of_operation_port (operation,port) dir =
    Adequationtypes.data_get !Sdx_lexer.adequation_graph operation port dir
  let operator_of_name operator_name =
    Architecture.operator_reference !archi_lib !archi_name operator_name
  let medium_of_name medium_name =
    Architecture.media_reference !archi_lib !archi_name medium_name
%}

/*---- Base value types ----*/
%token <string> NAME
%token <string> STRING
%token <int*int> COORD2D
%token <float> FLOAT
/*---- General application information----*/
%token INCLUDE
%token DEF MAIN APPLICATION DESCRIPTION XSC
%token CODE_PHASES INIT_SEQ LOOP_SEQ END_SEQ
%token CONSTRAINT RELATIVE ABSOLUTE UNION DISJUNCTION
%token SYNDEX_VERSION
/*---- Algorithm ----*/
%token CONSTANT SENSOR ACTUATOR MEMORY ALGORITHM INTERNAL CONDITIONS REFERENCES DEPENDANCES IN OUT INIT
%token ATTACH_ALL ATTACH_REF ATTACH_CONDI ATTACH_CONDO ATTACH_EXPLODE ATTACH_IMPLODE
%token STRONGPRECEDENCEDATA WEAKPRECEDENCEDATA PRECEDENCE DATA
/*---- Architecture ----*/
%token ARCHITECTURE OPERATOR MEDIA OPERATORS GATE MEDIAS CONNECTIONS SAMPP SAMMP RAM BROADCAST NOBROADCAST
%token EXTRA_DURATIONS_OPERATOR EXTRA_DURATIONS_MEDIA
/*---- Adequation result ----*/
%token SCHEDULES OPERATION_SCHEDULED SCHEDULED
%token CALCUL COMMUNICATION SEND RECEIVE SYNC SEND_SYNCHRO RECEIVE_SYNCHRO READ WRITE
%token IHM CONDI CONDO EXPLODE IMPLODE SYNCHRO_CONSTANT
%token PORTS COND_LEVEL SCHEDULE_DEPENDENCES SCHEDULE_CONDITIONS
/*---- Common symbols ----*/
%token ON AT TO EQU COMMENT TRUE FALSE BACKSLASH EOF
%token BAR DIV MINUS LDIM RDIM LARG RARG LLIST RLIST LPAR RPAR AND COL COMMA SCOL DOT
%left MINUS
%left DIV

%type <Types.parse_ret> file
%start file

%%

def_description:
  | DESCRIPTION COL STRING {$3}
  | {""};
  
  ref_description:
  | STRING {$1}
  | {""};
  
  comment: COMMENT {};

  boolean:
  | FALSE {0}
  | TRUE {1};

  int: FLOAT {int_of_float $1};

  integer:
  | int {$1}
  | MINUS int {-$2};

  name_list:
  | {[]}
  | name_list NAME {$1 @ [$2]};

  ref_type:
  | NAME {(!Sdx_lexer.lib,$1)}
  | NAME DIV NAME {($1,$3)};

  ref_type_name: ref_type DOT NAME {($1,$3)};

  ref_type_name_list:
  | {[]}
  | ref_type_name_list ref_type_name {$1 @ [$2]};

  reference_path:
  | {[]}
  | reference_path BACKSLASH {"" :: $1}
  | reference_path BACKSLASH NAME {$1 @ [$3]};

  attachement_type:
  | ATTACH_ALL {AttachAll}
  | ATTACH_REF {AttachRef}
  | ATTACH_CONDI {AttachCondI}
  | ATTACH_CONDO {AttachCondO}
  | ATTACH_EXPLODE {AttachExplode}
  | ATTACH_IMPLODE {AttachImplode};

  operation_attached:
  | LDIM reference_path RDIM {($2,AttachRef)}
  | LDIM reference_path COMMA attachement_type RDIM {($2,$4)};

  operation_attached_list:
  | {[]}
  | operation_attached_list operation_attached {$1 @ [$2]};
  
  expression: {Parserexpression.expression Lexerexpression.lexer !Parserexpressioninit.lexerbuffer};
  
  arg_names:
  | {[]}
  | LARG arg_names_continue RARG {$2};

  arg_names_continue:
  | NAME {[$1]} 
  | arg_names_continue SCOL NAME {$1 @ [$3]};

  arg_values:
  | {[]}
  | LARG arg_values_continue RARG {$2};

  arg_values_continue:
  | expression {[$1]} 
  | arg_values_continue SCOL expression {$1 @ [$3]};

  dimension:
  | {Symbolic.Float 1.}
  | LDIM expression RDIM {$2};

  range:
  | {(Symbolic.Float 1.,Symbolic.Float 1.)}
  | LDIM expression RDIM {(Symbolic.Float 1.,$2)}
  | LDIM expression DOT DOT expression RDIM {($2,$5)};

  coord2d: integer COMMA integer {Coord.Coord2d ($1,$3)};

  rank:
  | {0}
  | int {$1};

  position:
  | {Coord.No_coord2d}
  | AT coord2d {$2};

  dim_window: 
  | {Coord.No_coord2d}
  | coord2d {$1};

  version: SYNDEX_VERSION COL STRING {};

  code_phase:
  | INIT_SEQ {InitSeq}
  | LOOP_SEQ {LoopSeq}
  | END_SEQ {EndSeq};

  code_phase_list:
  | {[]}
  | code_phase_list code_phase {$1 @ [$2]}

  code_phases:
  | {[LoopSeq]}
  | CODE_PHASES COL code_phase_list SCOL {$3};


  /* Algorithm */

  init_port:
  | {Port.Data_Port}
  | INIT {Port.Init_Memory_Port};

  in_port: IN NAME dimension NAME init_port rank position SCOL {($4,Port.In,$2,$3,$5,$6,$7)};
  out_port: OUT NAME dimension NAME rank position SCOL {($4,Port.Out,$2,$3,Port.Data_Port,$5,$6)};

  in_port_list:
  | {[]}
  | in_port_list in_port {$1 @ [$2]};

  out_port_list:
  | {[]}
  | out_port_list out_port {$1 @ [$2]};

  port_list:
  | {[]}
  | port_list in_port {$1 @ [$2]}
  | port_list out_port {$1 @ [$2]};

  ref_port:
  | NAME {("",$1)}
  | NAME DOT NAME {($1,$3)};

  ref: NAME {($1,"")};

  dependence:
  | STRONGPRECEDENCEDATA ref_port TO ref_port SCOL {($2,$4,Strong_Precedence_Data)}
  | WEAKPRECEDENCEDATA ref_port TO ref_port SCOL {($2,$4,Weak_Precedence_Data)}
  | PRECEDENCE ref TO ref SCOL {($2,$4,Precedence)}
  | DATA ref_port TO ref_port SCOL {($2,$4,Data)};

  dependence_list:
  | {[]}
  | dependence_list dependence {$1 @ [$2]};

  reference_algo: ref_type arg_values dimension NAME position ref_description SCOL
    {$4,$1,$2,$5,(int_of_string (Symbolic.string_of_expression $3)),$6};

  reference_algo_list:
  | {[]}
  | reference_algo_list reference_algo {$1 @ [$2]};

  condition:
  | {("",1)}
  | boolean {("",$1)}
  | NAME EQU integer {($1,$3)};

  conditions: CONDITIONS COL condition SCOL {$3};

  references: REFERENCES COL reference_algo_list {$3};

  dependences: DEPENDANCES COL dependence_list {$3};

  conds_refs_dpds: conditions references dependences {($1,$2,$3)};

  conds_refs_dpds_list:
  | {[]}
  | conds_refs_dpds_list conds_refs_dpds {$1 @ [$2]};

  internal: DEF INTERNAL NAME arg_names COL port_list
    {Algorithm.algorithm_create "" $3 Internal $4 $6 [] [] Coord.No_coord2d ""};

  constant: DEF CONSTANT NAME arg_names dim_window COL out_port_list def_description
    {Algorithm.constant_create !Sdx_lexer.lib $3 $4 $7 $5 $8};

  sensor: DEF SENSOR NAME arg_names dim_window COL out_port_list def_description
    {Algorithm.sensor_create !Sdx_lexer.lib $3 $4 $7 $5 $8};

  actuator: DEF ACTUATOR NAME arg_names dim_window COL in_port_list def_description
    {Algorithm.actuator_create !Sdx_lexer.lib $3 $4 $7 $5 $8};

  memory: DEF MEMORY NAME range arg_names dim_window COL port_list def_description
    {Algorithm.memory_create !Sdx_lexer.lib $3 $5 $8 $6 $9 $4};

  algorithm: DEF ALGORITHM NAME arg_names dim_window COL port_list conds_refs_dpds_list code_phases def_description
    {Algorithm.operation_create !Sdx_lexer.lib $3 $4 $7 $8 $5 $10 $9};

  algo:
  | internal {}
  | constant {}
  | sensor {}
  | actuator {}
  | memory {}
  | algorithm {};

  /* Architecture */

  bus_type:
  | SAMPP SCOL {SamPP}
  | SAMMP SCOL {SamMP}
  | RAM SCOL {Ram};

  broadcast:
  | {true}
  | BROADCAST {true}
  | NOBROADCAST {false};

  gate: GATE NAME NAME SCOL {($2,$3)};

  gate_list:
  | {[]}
  | gate_list gate {$1 @ [$2]};

  duration: ref_type EQU FLOAT SCOL {($1,$3)};

  duration_list:
  | {[]}
  | duration_list duration {$1 @ [$2]};

  gateref: NAME DOT NAME {($1,$3)};

  operatorref: ref_type NAME position SCOL {($2,$1,$3)};

  operatorref_list:
  | {[]}
  | operatorref_list operatorref {$1 @ [$2]};

  mediaref: ref_type NAME broadcast position SCOL {($2,$1,$3,$4)};

  mediaref_list:
  | {[]}
  | mediaref_list mediaref {$1 @ [$2]};

  connection: gateref NAME SCOL {($1,$2)};

  connection_list:
  | {[]}
  | connection_list connection {$1 @ [$2]};

  operators: OPERATORS COL operatorref_list {$3};

  medias: MEDIAS COL mediaref_list {$3};

  connections: CONNECTIONS COL connection_list {$3};

  main_operator:
  | {""}
  | MAIN OPERATOR NAME SCOL {$3};

  extra_durations_operator: EXTRA_DURATIONS_OPERATOR ref_type COL duration_list
    {Architecture.operator_durations_add $2 $4};

  extra_durations_media: EXTRA_DURATIONS_MEDIA ref_type COL duration_list
    {Architecture.media_durations_add $2 $4};

  operator: DEF OPERATOR NAME COL gate_list duration_list def_description code_phases
    {Architecture.operator_definition_create !Sdx_lexer.lib $3 $5 $6 $7 $8};

  media: DEF MEDIA NAME COL bus_type duration_list def_description
    {Architecture.media_definition_create !Sdx_lexer.lib $3 $5 $6 $7};

  architecture: DEF ARCHITECTURE NAME dim_window COL operators main_operator medias connections def_description
    {Architecture.architecture_create !Sdx_lexer.lib $3 $6 $7 $8 $9 $4 $10};

  archi:
  | operator {}
  | media {}
  | extra_durations_operator {}
  | extra_durations_media {}
  | architecture {};

  /* Application */

  main:
  | MAIN ALGORITHM ref_type arg_values SCOL {Algorithm.algo_main_set $3 $4}
  | MAIN ARCHITECTURE ref_type SCOL {Architecture.archi_main_set $3};

  xsc_definition: XSC NAME COL operation_attached_list SCOL {Application.xsc_define $2 $4};

  operationonproc: CONSTRAINT COL reference_path ON ref_type_name_list SCOL
    {Application.constraints_create ($3,$5)};

  absoluteconstraint: ABSOLUTE CONSTRAINT COL NAME ON ref_type_name_list SCOL
    {Application.xsc_absolute_constraint_create ($4,$6)};

  relativeconstraint_type:
  | UNION {Union}
  | DISJUNCTION {Disjunction};

  relativeconstraint: RELATIVE CONSTRAINT COL relativeconstraint_type name_list SCOL
    {Application.xsc_relative_constraint_create ($4,$5)};

  constraints:
  | operationonproc {}
  | absoluteconstraint {}
  | relativeconstraint {};

  descr:
    APPLICATION def_description {Application.set_application_description $2};
  
  /*---- Adequation result ----*/

  calcul_path: 
  | DIV DIV NAME {""::[$3]}
  | DIV NAME {[$2]} 
  | calcul_path DIV NAME {$1 @ [$3]};
  
  communication_name:
  | NAME {$1}
  | communication_name COMMA NAME {$1^","^$3}

  string_path:
  | DIV DIV NAME {"//"^$3}
  | DIV NAME {"/"^$2} 
  | string_path DIV NAME {$1^"/"^$3};

  communication_path_not_repeated:
  | DIV communication_name LPAR string_path DOT NAME RPAR {$2^"("^$4^"."^$6^")"};

  communication_path:
  | communication_path_not_repeated {$1}
  | communication_path_not_repeated NAME {$1^$2}

  operation_path:
  | calcul_path {$1}
  | communication_path {[$1]}

  operator_list:
  | NAME {let operator = operator_of_name $1 in
	    [operator]}
  | operator_list COMMA NAME {let operator = operator_of_name $3 in
				$1 @ [operator]};
  
  receivers:
    LPAR operator_list RPAR {$2};

  operation_port:
    operation_path DOT NAME {$1,$3}

  calcul_class: 
  | CONSTANT {Constant} 
  | SENSOR {Sensor} 
  | ACTUATOR {Actuator} 
  | MEMORY {Memory (Symbolic.Float 1.,Symbolic.Float 1.)} 
  | ALGORITHM {Operation} 
  | INTERNAL {Internal}; 
  
  communication_class: 
  | SEND NAME receivers operation_port
      {let data = data_of_operation_port $4 Port.Out in
       let operator = operator_of_name $2 in
	 Adequationtypes.Send (data,operator,$3)}
  | RECEIVE NAME receivers NAME operation_port
      {let data = data_of_operation_port $5 Port.Out in
       let sender = operator_of_name $2 in
       let executor = operator_of_name $4 in
	 Adequationtypes.Receive (data,sender,$3,executor)}
  | SYNC NAME receivers NAME operation_port
      {let data = data_of_operation_port $5 Port.Out in
       let sender = operator_of_name $2 in
       let executor = operator_of_name $4 in
	 Adequationtypes.Sync (data,sender,$3,executor)}
  | SEND_SYNCHRO NAME NAME
      {let sender = operator_of_name $2 in
       let receiver = operator_of_name $3 in
	 Adequationtypes.Send_Synchro (sender,receiver)}
  | RECEIVE_SYNCHRO NAME NAME
      {let sender = operator_of_name $2 in
       let receiver = operator_of_name $3 in
	 Adequationtypes.Receive_Synchro (sender,receiver)}
  | WRITE NAME operation_port
      {let data = data_of_operation_port $3 Port.Out in
       let writer = operator_of_name $2 in
	 Adequationtypes.Write (data,writer)}
  | READ NAME operation_port
      {let data = data_of_operation_port $3 Port.Out in
       let reader = operator_of_name $2 in
	 Adequationtypes.Read (data,reader)};
  
  opn_class: 
  | CALCUL calcul_class ref_type {Adequationtypes.Calcul ($2,(fst $3),(snd $3))} 
  | COMMUNICATION communication_class {Adequationtypes.Communication $2};
   
   origin: 
   | IHM operation_path {Adequationtypes.Ihm $2}
   | CONDI operation_path {Adequationtypes.Condition_In $2}
   | CONDO operation_path {Adequationtypes.Condition_Out $2}
   | EXPLODE operation_path {Adequationtypes.Explode $2}
   | IMPLODE operation_path {Adequationtypes.Implode $2}
   | SYNCHRO_CONSTANT operation_path {Adequationtypes.Synchro_Constant $2};

   opn_title:
     opn_class arg_values LPAR origin RPAR SCOL {$1,$2,$4}; 

   operator_class:
   | OPERATOR NAME {let operator = operator_of_name $2 in
		      Operator operator}
   | MEDIA NAME {let medium = medium_of_name $2 in
		   Media medium};

   schedule_place: 
     SCHEDULED COL operator_class integer FLOAT integer {$3,$4,$5,$6}; 

   adeq_condition:
     operation_port EQU integer {let data = data_of_operation_port $1 Port.Out in
				   (Some data),(Some $3)};
   adeq_cond_list:
   | adeq_condition {[$1]}
   | adeq_cond_list AND adeq_condition {$1 @ [$3]}

   adeq_conditions:
   | CONDITIONS COL boolean {[None,None]}
   | CONDITIONS COL adeq_cond_list {$3};

   dir:
   | IN {Port.In}
   | OUT {Port.Out};

   adeq_port:
     dir NAME LDIM int RDIM NAME integer SCOL
     {Adequationtypes.new_port $6 $1 $2 $4 Port.Data_Port $7};

   adeq_port_list: 
   | {[]}
   | adeq_port_list adeq_port {$1 @[$2]};

   adeq_ports:
     PORTS COL adeq_port_list {$3@(Adequationtypes.precedence_ports ())};

   adeq_dependence:
   | STRONGPRECEDENCEDATA COND_LEVEL EQU int operation_port TO operation_port adeq_conditions SCOL
       {let sopn,sprt = data_of_operation_port $5 Port.Out in
	let dopn,dprt = data_of_operation_port $7 Port.In in
	  Adequationtypes.dependence_add sopn sprt dopn dprt ((Adequationtypes.Condition $4),Strong_Precedence_Data) $8 true}
   | STRONGPRECEDENCEDATA operation_port TO operation_port adeq_conditions SCOL
       {let sopn,sprt = data_of_operation_port $2 Port.Out in
	let dopn,dprt = data_of_operation_port $4 Port.In in
	  Adequationtypes.dependence_add sopn sprt dopn dprt (Adequationtypes.Data,Strong_Precedence_Data) $5 true}
   | PRECEDENCE operation_path TO operation_path adeq_conditions SCOL
       {let sopn = Adequationtypes.operation_get_of_path !Sdx_lexer.adequation_graph $2 in
	let sprt = Adequationtypes.find_precedence_port sopn Port.Out in
	let dopn = Adequationtypes.operation_get_of_path !Sdx_lexer.adequation_graph $4 in
	let dprt = Adequationtypes.find_precedence_port dopn Port.In in
	  Adequationtypes.dependence_add sopn sprt dopn dprt (Adequationtypes.Data,Precedence) $5 true}

   adeq_dpd_list: 
   | {[]}
   | adeq_dpd_list adeq_dependence {$1 @ [$2]};

   adeq_dpds:
     SCHEDULE_DEPENDENCES COL adeq_dpd_list {$3};

   operation_condition:
   | operation_path adeq_conditions {Adequationtypes.operation_cond_set !Sdx_lexer.adequation_graph $1 $2};

   operation_condition_list:
   | {[]}
   | operation_condition_list operation_condition {$1 @ [$2]};

   adeq_operations_conditions:
   | SCHEDULE_CONDITIONS operation_condition_list {$2}

   operation_scheduled:
     OPERATION_SCHEDULED operation_path COL opn_title schedule_place adeq_ports
     {let opn_class,args,origin = $4 in
      let opn_operator,rank,esfs,adequation_order = $5 in
      let operation = Adequationtypes.new_opn $2 opn_class args $6 [] []
			[] (Some opn_operator) rank esfs true origin None "" "" "" [LoopSeq] in
	operation.Adequationtypes.t_opn_adequation_order <- adequation_order;
	Adequationtypes.operation_add !Sdx_lexer.adequation_graph operation};
   
   schedules:
     SCHEDULES COL {let arclib,arcname = Application.archi_main_get () in
	       archi_lib := arclib;
	       archi_name := arcname;};

  /*---- Global commands ----*/
  command:
  | version {}
  | descr {}
  | algo {}
  | archi {}
  | main {}
  | schedules {}
  | operation_scheduled {}
  | adeq_dpds {}
  | adeq_operations_conditions {}
  | xsc_definition {}
  | constraints {}
  | comment {};

  command_list:
  | {}
  | command_list command {};

  fileinclude: INCLUDE STRING SCOL {$2};

  file: 
  | command_list fileinclude {Include $2}
  | command_list EOF {Done};
