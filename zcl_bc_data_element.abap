CLASS zcl_bc_data_element DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_def TYPE dd04l READ-ONLY,
      gs_txt TYPE dd04t READ-ONLY.

    CLASS-METHODS:

      get_instance
        IMPORTING !iv_rollname   TYPE rollname
        RETURNING VALUE(ro_dtel) TYPE REF TO zcl_bc_data_element
        RAISING   zcx_bc_table_content,

      get_text_safe
        importing !iv_rollname type rollname
        returning value(rv_text) type ddtext.

    METHODS:

      get_Domain
        returning value(ro_domain) type ref to zcl_Bc_Abap_domain
        raising   zcx_Bc_table_Content,

      get_text
        IMPORTING !iv_worst_case_rollname TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rv_text)          TYPE ddtext,

      validate_value
        importing
          !iv_value type val_single
        raising
          zcx_bc_domain
          zcx_bc_table_Content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        rollname TYPE rollname,
        obj      TYPE REF TO zcl_bc_data_element,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS rollname.

    CONSTANTS c_tabname_main TYPE tabname VALUE 'DD04L'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

ENDCLASS.



CLASS ZCL_BC_DATA_ELEMENT IMPLEMENTATION.

  method get_domain.
    ro_domain = zcl_Bc_abap_Domain=>get_instance( gs_Def-domname ).
  endmethod.

  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS rollname = iv_rollname
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( rollname = iv_rollname ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE *
        INTO @ls_multiton-obj->gs_def
        FROM dd04l
        WHERE rollname EQ @ls_multiton-rollname
        ##WARN_OK.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-rollname )
            tabname  = c_tabname_main.
      ENDIF.

      SELECT SINGLE *
        INTO @ls_multiton-obj->gs_txt
        FROM dd04t
        WHERE
          rollname   EQ @ls_multiton-rollname AND
          ddlanguage EQ @sy-langu
        ##WARN_OK.

      IF sy-subrc NE 0.

        SELECT SINGLE *
          INTO @ls_multiton-obj->gs_txt
          FROM dd04t
          WHERE rollname EQ @ls_multiton-rollname
          ##WARN_OK.

      ENDIF.

      INSERT ls_multiton
        INTO TABLE gt_multiton
        ASSIGNING <ls_multiton>.

    ENDIF.

    ro_dtel = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_text.

    rv_text = COND #(
      WHEN gs_txt-ddtext IS NOT INITIAL THEN gs_txt-ddtext
      WHEN gs_txt-reptext IS NOT INITIAL THEN gs_txt-reptext
      WHEN gs_txt-scrtext_l IS NOT INITIAL THEN gs_txt-scrtext_l
      WHEN gs_txt-scrtext_m IS NOT INITIAL THEN gs_txt-scrtext_m
      WHEN gs_txt-scrtext_s IS NOT INITIAL THEN gs_txt-scrtext_s
      WHEN iv_worst_case_rollname EQ abap_true THEN gs_def-rollname
    ).

  ENDMETHOD.

  method get_text_safe.

    try.
        data(lv_ddtext) = zcl_bc_data_element=>get_instance( iv_rollname )->get_text( ).
      catch cx_root.
        lv_ddtext = iv_rollname.
    endtry.

  endmethod.

  method validate_value.
    get_domain( )->validate_value( iv_value ).
  endmethod.

ENDCLASS.