CLASS zcl_mm_purch_org DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_def TYPE t024e READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_ekorg     TYPE ekorg
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_purch_org
        RAISING   zcx_bc_table_content,

      get_text_safe
        importing !iv_ekorg type ekorg
        returning value(rv_text) type EKOTX.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        ekorg TYPE ekorg,
        cx    TYPE REF TO zcx_bc_table_content,
        obj   TYPE REF TO zcl_mm_purch_org,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS ekorg.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'T024E'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !iv_ekorg TYPE ekorg
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_mm_purch_org IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      FROM t024e
      WHERE ekorg EQ @iv_ekorg
      INTO CORRESPONDING FIELDS OF @gs_def.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_ekorg )
          tabname  = c_tabname_def.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        ekorg = iv_ekorg
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( ekorg = iv_ekorg ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-ekorg ).
        CATCH zcx_bc_table_content INTO ls_mt-cx ##no_Handler .
      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

  method get_text_safe.

    try.
        rv_text = get_instance( iv_ekorg )->gs_Def-ekotx.
      catch cx_root ##no_handler .
    endtry.

  endmethod.

ENDCLASS.