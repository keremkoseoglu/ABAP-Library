CLASS zcl_bc_uom DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_def TYPE t006 READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_msehi     TYPE msehi
        RETURNING VALUE(ro_uom) TYPE REF TO zcl_bc_uom
        RAISING   zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        msehi TYPE msehi,
        cx    TYPE REF TO zcx_bc_table_content,
        obj   TYPE REF TO zcl_bc_uom,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS msehi.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'T006'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !iv_msehi TYPE msehi
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_bc_uom IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      FROM t006
      WHERE msehi EQ @iv_msehi
      INTO @gs_def.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_msehi )
          tabname  = c_tabname_def.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        msehi = iv_msehi
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton(
        msehi = iv_msehi
      ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-msehi ).
        CATCH zcx_bc_table_content INTO ls_mt-cx ##no_Handler .
      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_uom = <ls_mt>-obj.

  ENDMETHOD.

ENDCLASS.