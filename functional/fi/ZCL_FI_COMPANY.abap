CLASS zcl_fi_company DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_def,
             bukrs TYPE t001-bukrs,
             land1 TYPE t001-land1,
             waers TYPE t001-waers,
             butxt TYPE t001-butxt,
           END OF t_def.

    DATA gs_def TYPE t_def READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING iv_bukrs      TYPE bukrs
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_fi_company
      RAISING   zcx_fi_company_code_def.

    METHODS get_kokrs              RETURNING VALUE(rv_kokrs) TYPE tka02-kokrs.
    METHODS get_mat_control_record RETURNING VALUE(rs_marv)  TYPE marv.

    METHODS get_tax_code
      RETURNING VALUE(stcd2) TYPE stcd2
      RAISING   zcx_fi_company_code_def.

    METHODS get_currency_type_set RETURNING VALUE(result) TYPE REF TO zif_fi_currency_type_set.

    METHODS get_ekorgs            RETURNING VALUE(result) TYPE zmmtt_ekorg.

  PRIVATE SECTION.
    CONSTANTS c_tabname_t001 TYPE tabname VALUE 'T001'.

    TYPES: BEGIN OF t_lazy_flg,
             kokrs  TYPE abap_bool,
             marv   TYPE abap_bool,
             stcd2  TYPE abap_bool,
             ekorgs TYPE abap_bool,
           END OF t_lazy_flg,

           BEGIN OF t_lazy_val,
             kokrs  TYPE tka02-kokrs,
             marv   TYPE marv,
             stcd2  TYPE stcd2,
             ekorgs TYPE zmmtt_ekorg,
           END OF t_lazy_val,

           BEGIN OF t_lazy,
             flg TYPE t_lazy_flg,
             val TYPE t_lazy_val,
           END OF t_lazy,

           BEGIN OF t_multiton,
             bukrs TYPE bukrs,
             obj   TYPE REF TO zcl_fi_company,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS bukrs.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA gs_lazy TYPE t_lazy.

ENDCLASS.


CLASS zcl_fi_company IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key
                        COMPONENTS bukrs = iv_bukrs ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( bukrs = iv_bukrs ).
      ls_multiton-obj = NEW #( ).

      SELECT SINGLE bukrs, land1, waers, butxt
             FROM t001
             WHERE bukrs = @ls_multiton-bukrs
             INTO CORRESPONDING FIELDS OF @ls_multiton-obj->gs_def.

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_fi_company_code_def(
                                bukrs    = ls_multiton-bukrs
                                previous = NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>value_missing
                                                                     objectid = CONV #( ls_multiton-bukrs )
                                                                     tabname  = c_tabname_t001 ) ).
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD get_kokrs.
    IF gs_lazy-flg-kokrs IS INITIAL.
      SELECT SINGLE kokrs INTO @gs_lazy-val-kokrs
             FROM tka02
             WHERE bukrs = @gs_def-bukrs ##WARN_OK. "#EC CI_NOORDER

      gs_lazy-flg-kokrs = abap_true.
    ENDIF.

    rv_kokrs = gs_lazy-val-kokrs.
  ENDMETHOD.

  METHOD get_mat_control_record.
    IF gs_lazy-flg-marv = abap_false.
      SELECT SINGLE * FROM marv
             WHERE bukrs = @gs_def-bukrs
             INTO @gs_lazy-val-marv.

      gs_lazy-flg-marv = abap_true.
    ENDIF.

    rs_marv = gs_lazy-val-marv.
  ENDMETHOD.

  METHOD get_tax_code.
    IF gs_lazy-flg-stcd2 = abap_false.
      DATA(kunnr) = CONV kunnr( |Q{ gs_def-bukrs }| ).

      SELECT SINGLE stcd2 FROM kna1
             WHERE kunnr = @kunnr
             INTO @gs_lazy-val-stcd2.

      IF gs_lazy-val-stcd2 IS INITIAL.
        RAISE EXCEPTION NEW zcx_fi_company_code_def( textid = zcx_fi_company_code_def=>cant_determine_tax_code
                                                     bukrs  = gs_def-bukrs ).
      ENDIF.

      gs_lazy-flg-stcd2 = abap_true.
    ENDIF.

    stcd2 = gs_lazy-val-stcd2.
  ENDMETHOD.

  METHOD get_currency_type_set.
    result = CAST #( zcl_fi_comp_currency_type_set=>get_instance( gs_def-bukrs ) ).
  ENDMETHOD.

  METHOD get_ekorgs.
    IF gs_lazy-flg-ekorgs = abap_false.
      SELECT FROM t024e
             FIELDS ekorg
             WHERE bukrs = @gs_def-bukrs
             INTO TABLE @gs_lazy-val-ekorgs.

      gs_lazy-flg-ekorgs = abap_true.
    ENDIF.

    result = gs_lazy-val-ekorgs.
  ENDMETHOD.
ENDCLASS.