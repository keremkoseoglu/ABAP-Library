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

      get_shortest_text_safe
        IMPORTING !iv_rollname   TYPE rollname
        RETURNING VALUE(rv_text) TYPE ddtext,

      get_text_safe
        IMPORTING !iv_rollname   TYPE rollname
        RETURNING VALUE(rv_text) TYPE ddtext.

    METHODS:

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


  METHOD get_domain.
    ro_domain = zcl_bc_abap_domain=>get_instance( gs_def-domname ).
  ENDMETHOD.


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
        rv_text = zcl_bc_data_element=>get_instance( iv_rollname )->get_shortest_text( ).
      CATCH cx_root.
        rv_text = iv_rollname.
    ENDTRY.

  ENDMETHOD.

  METHOD get_text_safe.

    TRY.
        rv_text = zcl_bc_data_element=>get_instance( iv_rollname )->get_text( ).
      CATCH cx_root.
        rv_text = iv_rollname.
    ENDTRY.

  ENDMETHOD.


  METHOD validate_value.
    get_domain( )->validate_value( iv_value ).
  ENDMETHOD.
ENDCLASS.