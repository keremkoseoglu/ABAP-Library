CLASS zcl_mm_material_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gv_mtart TYPE mtart READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_mtart     TYPE mtart
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_material_type
        RAISING   zcx_bc_table_content,

      is_mtart_stocked__safe
        IMPORTING !iv_mtart         TYPE mtart
        RETURNING VALUE(rv_stocked) TYPE abap_bool.

    METHODS:
      is_stocked RETURNING VALUE(rv_stocked) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        mtart TYPE mtart,
        obj   TYPE REF TO zcl_mm_material_type,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS mtart,

      BEGIN OF t_lazy_flg,
        stocked TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        stocked TYPE abap_bool,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'T134'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    DATA:
      gs_lazy TYPE t_lazy.

    METHODS:
      constructor
        IMPORTING !iv_mtart TYPE mtart
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_mm_material_type IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE mtart
      FROM t134
      WHERE mtart EQ @iv_mtart
      INTO @gv_mtart.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_mtart )
          tabname  = c_tabname_def.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        mtart = iv_mtart
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( mtart = iv_mtart ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-mtart ).
        CATCH zcx_bc_table_content INTO ls_mt-cx ##no_handler.
      ENDTRY.

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

  METHOD is_mtart_stocked__safe.

    TRY.
        rv_stocked = get_instance( iv_mtart )->is_stocked( ).
      CATCH cx_root ##no_Handler .
    ENDTRY.

  ENDMETHOD.

  METHOD is_stocked.

    IF gs_lazy-flg-stocked EQ abap_false.

      SELECT SINGLE mandt
        FROM t134m
        WHERE
          mtart EQ @gv_mtart AND
          mengu EQ @space
        INTO @sy-mandt ##write_ok. "#EC CI_NOORDER

      gs_lazy-val-stocked = xsdbool( sy-subrc NE 0 ).
      gs_lazy-flg-stocked = abap_true.
    ENDIF.

    rv_stocked = gs_lazy-val-stocked.

  ENDMETHOD.

ENDCLASS.