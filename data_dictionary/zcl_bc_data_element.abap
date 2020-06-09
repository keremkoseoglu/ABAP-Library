CLASS zcl_bc_data_element DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_bc_multiton.

    CONSTANTS:
      c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DATA_ELEMENT'.

    DATA:
      gs_def TYPE dd04l READ-ONLY,
      gs_txt TYPE dd04t READ-ONLY.

    CLASS-METHODS:
      get_shortest_text_safe
        IMPORTING !iv_rollname   TYPE rollname
        RETURNING VALUE(rv_text) TYPE ddtext,

      get_text_safe
        IMPORTING !iv_rollname   TYPE rollname
        RETURNING VALUE(rv_text) TYPE ddtext.

    METHODS:

      constructor
        IMPORTING !iv_rollname   TYPE rollname
        RAISING   zcx_bc_table_content,

      get_domain
        RETURNING VALUE(ro_domain) TYPE REF TO zcl_bc_abap_domain
        RAISING   zcx_bc_table_content,

      get_shortest_text
        IMPORTING !iv_worst_case_rollname TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rv_text)          TYPE ddtext,

      get_text
        IMPORTING !iv_worst_case_rollname TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rv_text)          TYPE ddtext,

      validate_value
        IMPORTING
          !iv_value TYPE val_single
        RAISING
          zcx_bc_domain
          zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flg,
        shortest_text TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        shortest_text TYPE string,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_multiton,
        rollname TYPE rollname,
        obj      TYPE REF TO zcl_bc_data_element,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS rollname.

    CONSTANTS c_tabname_main TYPE tabname VALUE 'DD04L'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA gs_lazy TYPE t_lazy.

ENDCLASS.



CLASS zcl_bc_data_element IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      INTO @gs_def
      FROM dd04l
      WHERE rollname EQ @iv_rollname
      ##WARN_OK.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_rollname )
          tabname  = c_tabname_main.
    ENDIF.

    SELECT SINGLE *
      INTO @gs_txt
      FROM dd04t
      WHERE
        rollname   EQ @iv_rollname AND
        ddlanguage EQ @sy-langu
      ##WARN_OK.

    IF sy-subrc NE 0.

      SELECT SINGLE *
        INTO @gs_txt
        FROM dd04t
        WHERE rollname EQ @iv_rollname
        ##WARN_OK.

    ENDIF.

  ENDMETHOD.

  METHOD get_domain.
    ro_domain = zcl_bc_abap_domain=>get_instance( gs_def-domname ).
  ENDMETHOD.

  METHOD get_shortest_text.

    IF gs_lazy-flg-shortest_text EQ abap_false.

      gs_lazy-val-shortest_text = zcl_bc_text_toolkit=>get_shortest_text(
        VALUE #(
          ( CONV #( gs_txt-ddtext ) )
          ( CONV #( gs_txt-reptext ) )
          ( CONV #( gs_txt-scrtext_l ) )
          ( CONV #( gs_txt-scrtext_m ) )
          ( CONV #( gs_txt-scrtext_s ) )
        )
      ).

      gs_lazy-flg-shortest_text = abap_true.

    ENDIF.

    rv_text = COND #(
      WHEN gs_lazy-val-shortest_text IS NOT INITIAL
      THEN gs_lazy-val-shortest_text
      ELSE COND #(
        WHEN iv_worst_case_rollname EQ abap_true
        THEN gs_def-rollname
        ELSE space
      )
    ).

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

  METHOD get_shortest_text_safe.

    TRY.
        rv_Text = cast zcl_bc_data_element(
            zcl_bc_multiton=>get_obj(
              iv_clsname  = zcl_bc_data_element=>c_clsname_me
              iv_objectid = conv #( iv_rollname )
            )
          )->get_shortest_Text( ).
      CATCH cx_root.
        rv_text = iv_rollname.
    ENDTRY.

  ENDMETHOD.

  METHOD get_text_safe.

    TRY.

        rv_Text = cast zcl_bc_data_element(
            zcl_bc_multiton=>get_obj(
              iv_clsname  = zcl_bc_data_element=>c_clsname_me
              iv_objectid = conv #( iv_rollname )
            )
          )->get_Text( ).

      CATCH cx_root.
        rv_text = iv_rollname.
    ENDTRY.

  ENDMETHOD.


  METHOD validate_value.
    get_domain( )->validate_value( iv_value ).
  ENDMETHOD.

  method zif_bc_multiton~get_instance.

    try.
        ro_obj ?= new zcl_bc_data_element( conv #( iv_objectid ) ).
      catch cx_root into datA(lo_Diaper).

        raise exception type cx_sy_create_object_error
          EXPORTING
            textid    = cx_sy_create_object_error=>cx_sy_create_object_error
            previous  = lo_Diaper
            classname = conv #( c_clsname_me ).

    endtry.

  endmethod.

ENDCLASS.