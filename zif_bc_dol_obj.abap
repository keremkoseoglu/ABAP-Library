INTERFACE ZIF_BC_DOL_OBJ
  PUBLIC .

  CONSTANTS: c_pgmid_r3tr TYPE pgmid VALUE 'R3TR'.

  METHODS get_object_txt
    IMPORTING
      !iv_pgmid            TYPE zbcs_dol_list-pgmid DEFAULT c_pgmid_r3tr
      !iv_object           TYPE zbcs_dol_list-object
    RETURNING
      VALUE(rv_object_txt) TYPE zbcs_dol_list-object_txt.

  METHODS get_ddtext
    IMPORTING
      !iv_pgmid        TYPE zbcs_dol_list-pgmid DEFAULT c_pgmid_r3tr
      !iv_object       TYPE zbcs_dol_list-object
      !iv_obj_name     TYPE zbcs_dol_list-obj_name
    RETURNING
      VALUE(rv_ddtext) TYPE zbcs_dol_list-ddtext.

  METHODS is_deleted
    IMPORTING
      !iv_pgmid         TYPE zbcs_dol_list-pgmid DEFAULT c_pgmid_r3tr
      !iv_object        TYPE zbcs_dol_list-object
      !iv_obj_name      TYPE zbcs_dol_list-obj_name
    RETURNING
      VALUE(rv_deleted) TYPE abap_bool.

ENDINTERFACE.