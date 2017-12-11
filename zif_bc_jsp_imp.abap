INTERFACE zif_bc_jsp_imp
  PUBLIC .

  CONSTANTS: c_meth_casss TYPE seocpdname VALUE 'CALL_AND_SAVE_SS',
             c_meth_exeit TYPE seocpdname VALUE 'EXECUTE_ITEM',
             c_meth_exemi TYPE seocpdname VALUE 'EXECUTE_MILESTONE',
             c_meth_rassp TYPE seocpdname VALUE 'READ_AND_SAVE_SPLIT',
             c_meth_setmo TYPE seocpdname VALUE 'SET_MODEL'.

  METHODS call_and_save_ss
    RAISING
      zcx_bc_class_method.

  METHODS delete
    IMPORTING
      !it_jphid_rng TYPE zcl_bc_jsp_model=>tt_jphid_rng.

  METHODS execute_item
    IMPORTING
      !iv_jppos TYPE zbcd_jsp_jppos
    RAISING
      zcx_bc_class_method.

  METHODS execute_milestone
    IMPORTING
      !iv_milst TYPE zbcd_jsp_milst
    RAISING
      zcx_bc_class_method.

  METHODS read_and_save_split
    EXPORTING
      !ev_last_pos TYPE zbcd_jsp_jppos
    RAISING
      zcx_bc_class_method.

  METHODS set_model
    IMPORTING
      !io_model TYPE REF TO zcl_bc_jsp_model.

ENDINTERFACE.