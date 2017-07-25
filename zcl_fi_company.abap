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
        zcx_bc_table_content .

    methods get_kokrs returning value(rv_kokrs) type tka02-kokrs.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_tabname_t001 TYPE tabname VALUE 'T001'.

    TYPES:

      begin of t_lazy_flg,
        kokrs type abap_bool,
      end of t_lazy_flg,

      begin of t_lazy_val,
        kokrs type tka02-kokrs,
      end of t_lazy_val,

      begin of t_lazy,
        flg type t_lazy_flg,
        val type t_lazy_val,
      end of t_lazy,

      BEGIN OF t_multiton,
        bukrs TYPE bukrs,
        obj   TYPE REF TO zcl_fi_company,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS bukrs.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    data gs_lazy type t_lazy.

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
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>value_missing
            objectid = CONV #( ls_multiton-bukrs )
            tabname  = c_tabname_t001.
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.


  method get_kokrs.

    if gs_lazy-flg-kokrs is initial.
      select single kokrs into @gs_lazy-val-kokrs from tka02 where bukrs eq @gs_def-bukrs ##WARN_OK.
      gs_lazy-flg-kokrs = Abap_true.
    endif.

    rv_kokrs = gs_lazy-val-kokrs.

  endmethod.
ENDCLASS.