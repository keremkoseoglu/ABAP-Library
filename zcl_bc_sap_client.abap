CLASS zcl_bc_sap_client DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CONSTANTS c_category_customizing TYPE t000-cccategory VALUE 'C'.

    DATA gs_def TYPE t000.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_mandt        TYPE symandt DEFAULT sy-mandt
        RETURNING VALUE(ro_client) TYPE REF TO zcl_bc_sap_client
        RAISING   zcx_bc_table_content.

    METHODS:
      ensure_customizing_client RAISING zcx_bc_sap_client,
      is_customizing_client RETURNING VALUE(rv_cust) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        mandt TYPE sy-mandt,
        obj   TYPE REF TO zcl_bc_sap_client,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS mandt.

    CONSTANTS c_tabname_def TYPE tabname VALUE 'T000'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

ENDCLASS.



CLASS zcl_bc_sap_client IMPLEMENTATION.

  METHOD ensure_customizing_client.

    CHECK is_customizing_client( ) EQ abap_false.

    RAISE EXCEPTION TYPE zcx_bc_sap_client
      EXPORTING
        textid = zcx_bc_sap_client=>not_customizing_client
        mandt  = gs_def-mandt.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS mandt = iv_mandt
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( mandt = iv_mandt ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE * INTO @ls_multiton-obj->gs_def
        FROM t000
        WHERE mandt EQ @ls_multiton-mandt.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-mandt )
            tabname  = c_tabname_def.
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_client = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD is_customizing_client.
    rv_cust = xsdbool( gs_def-cccategory EQ c_category_customizing ).
  ENDMETHOD.
ENDCLASS.