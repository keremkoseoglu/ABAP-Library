CLASS zcl_bc_abap_domain DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_line,
        value TYPE val_single,
        text  TYPE val_text,
      END OF t_line,

      tt_line
        TYPE HASHED TABLE OF t_line
        WITH UNIQUE KEY primary_key COMPONENTS value.

    DATA gs_def TYPE dd01l READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_domname      TYPE domname
        RETURNING VALUE(ro_domain) TYPE REF TO zcl_bc_abap_domain
        RAISING   zcx_bc_table_content,

      get_value_text_safe
        IMPORTING
          !iv_domname    TYPE domname
          !iv_value      TYPE val_single
        RETURNING
          VALUE(rv_text) TYPE val_text.

    METHODS:
      get_text RETURNING VALUE(rv_text) TYPE ddtext,

      get_value_line
        IMPORTING !iv_value      TYPE val_single
        RETURNING VALUE(rs_line) TYPE t_line
        RAISING   zcx_bc_domain,

      get_value_tab RETURNING VALUE(rt_tab) TYPE tt_line,

      get_value_text
        IMPORTING !iv_value      TYPE val_single
        RETURNING VALUE(rv_text) TYPE val_text
        RAISING   zcx_bc_domain,

      validate_value
        IMPORTING
          !iv_value TYPE val_single
        RAISING
          zcx_bc_domain
          zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:

      BEGIN OF t_lazy_flag,
        text_read       TYPE abap_bool,
        value_text_read TYPE abap_bool,
      END OF t_lazy_flag,

      BEGIN OF t_multiton,
        domname TYPE domname,
        obj     TYPE REF TO zcl_bc_abap_domain,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS domname.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'DD01L'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    DATA:
      gs_lazy_flag TYPE t_lazy_flag,
      gt_line      TYPE tt_line,
      gv_text      TYPE ddtext.

    METHODS:
      ensure_text_read,
      ensure_value_read.

ENDCLASS.



CLASS zcl_bc_abap_domain IMPLEMENTATION.


  METHOD ensure_text_read.

    CHECK gs_lazy_flag-text_read EQ abap_false.

    SELECT SINGLE ddtext
      INTO @gv_text
      FROM dd01t
      WHERE
        domname EQ @gs_def-domname AND
        ddlanguage EQ @sy-langu
      ##WARN_OK.

    IF sy-subrc NE 0.
      SELECT SINGLE ddtext
        INTO @gv_text
        FROM dd01t
        WHERE domname EQ @gs_def-domname
        ##WARN_OK.
    ENDIF.

    gs_lazy_flag-text_read = abap_true.

  ENDMETHOD.


  METHOD ensure_value_read.

    CHECK gs_lazy_flag-value_text_read EQ abap_false.

    SELECT
      dd07l~domvalue_l AS value,
      dd07t~ddtext AS text
      INTO CORRESPONDING FIELDS OF TABLE @gt_line
      FROM
        dd07l
        LEFT JOIN dd07t ON dd07t~domname    EQ dd07l~domname
                       AND dd07t~ddlanguage EQ @sy-langu
                       AND dd07t~as4local   EQ dd07l~as4local
                       AND dd07t~valpos     EQ dd07l~valpos
                       AND dd07t~as4vers    EQ dd07l~as4vers
        WHERE dd07l~domname EQ @gs_def-domname.

    gs_lazy_flag-value_text_read = abap_true.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS domname = iv_domname
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( domname = iv_domname ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE *
        INTO @ls_multiton-obj->gs_def
        FROM dd01l
        WHERE domname EQ @ls_multiton-domname
        ##WARN_OK.

      IF sy-subrc NE 0.

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-domname )
            tabname  = c_tabname_def.

      ENDIF.

      INSERT ls_multiton
        INTO TABLE gt_multiton
        ASSIGNING <ls_multiton>.

    ENDIF.

    ro_domain = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_text.
    ensure_text_read( ).
    rv_text = gv_text.
  ENDMETHOD.


  METHOD get_value_line.

    ensure_value_read( ).

    ASSIGN gt_line[
      KEY primary_key
      COMPONENTS value = iv_value
    ] TO FIELD-SYMBOL(<ls_line>).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_domain
        EXPORTING
          textid     = zcx_bc_domain=>invalid_value
          domname    = gs_def-domname
          domvalue_l = iv_value.
    ENDIF.

    rs_line = <ls_line>.

  ENDMETHOD.


  METHOD get_value_tab.
    ensure_value_read( ).
    rt_tab = gt_line.
  ENDMETHOD.


  METHOD get_value_text.
    rv_text = get_value_line( iv_value )-text.
  ENDMETHOD.

  method get_value_text_safe.

    try.
        rv_Text = get_instance( iv_domname )->get_value_text( iv_value ).
      catch cx_root .
        rv_text = iv_value.
    endtry.

  endmethod.

  METHOD validate_value.

    DATA lv_dummy TYPE string.

    ensure_value_read( ).

    IF gt_line IS NOT INITIAL.
      get_value_line( iv_value ).
      RETURN.
    ENDIF.

    IF gs_def-entitytab IS NOT INITIAL.

      DATA(lt_key) = zcl_bc_abap_table=>get_instance( gs_def-entitytab )->get_key_fields( iv_with_mandt = abap_false ).
      DATA(lv_key) = lt_key[ 1 ]-fieldname.

      DATA(lv_where) = |{ lv_key } EQ '{ iv_value }'|.

      SELECT SINGLE (lv_key)
        INTO lv_dummy
        FROM (gs_def-entitytab)
        WHERE (lv_where).

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_domain
          EXPORTING
            textid     = zcx_bc_domain=>invalid_value
            domname    = gs_def-domname
            domvalue_l = iv_value.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.