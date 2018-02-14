CLASS zcl_fi_company DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_def,
        bukrs TYPE t001-bukrs,
        waers TYPE t001-waers,
      END OF t_def .

    DATA gs_def TYPE t_def READ-ONLY .

    CLASS-METHODS get_instance
      IMPORTING
        !iv_bukrs     TYPE bukrs
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_fi_company
      RAISING
        zcx_fi_company_code_def .

    METHODS:
      get_kokrs RETURNING VALUE(rv_kokrs) TYPE tka02-kokrs,
      get_mat_control_record RETURNING VALUE(rs_marv) TYPE marv.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_tabname_t001 TYPE tabname VALUE 'T001'.

    TYPES:

      BEGIN OF t_lazy_flg,
        kokrs TYPE abap_bool,
        marv  TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        kokrs TYPE tka02-kokrs,
        marv  TYPE marv,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_multiton,
        bukrs TYPE bukrs,
        obj   TYPE REF TO zcl_fi_company,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS bukrs.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA gs_lazy TYPE t_lazy.

ENDCLASS.



CLASS ZCL_FI_COMPANY IMPLEMENTATION.


  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS bukrs = iv_bukrs
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( bukrs = iv_bukrs ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE bukrs waers
        INTO CORRESPONDING FIELDS OF ls_multiton-obj->gs_def
        FROM t001
        WHERE bukrs EQ ls_multiton-bukrs.

      IF sy-subrc NE 0.

        RAISE EXCEPTION TYPE zcx_fi_company_code_def
          EXPORTING
            bukrs    = ls_multiton-bukrs
            previous = NEW zcx_bc_table_content(
              textid   = zcx_bc_table_content=>value_missing
              objectid = CONV #( ls_multiton-bukrs )
              tabname  = c_tabname_t001
            ).

      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_kokrs.

    IF gs_lazy-flg-kokrs IS INITIAL.
      SELECT SINGLE kokrs INTO @gs_lazy-val-kokrs FROM tka02 WHERE bukrs EQ @gs_def-bukrs ##WARN_OK.
      gs_lazy-flg-kokrs = abap_true.
    ENDIF.

    rv_kokrs = gs_lazy-val-kokrs.

  ENDMETHOD.


  METHOD get_mat_control_record.

    IF gs_lazy-flg-marv EQ abap_false.

      SELECT SINGLE *
        FROM marv
        WHERE bukrs EQ @gs_def-bukrs
        INTO @gs_lazy-val-marv.

      gs_lazy-flg-marv = abap_true.
    ENDIF.

    rs_marv = gs_lazy-val-marv.

  ENDMETHOD.
ENDCLASS.