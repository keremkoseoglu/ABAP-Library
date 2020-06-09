CLASS zcl_bc_t100_to_string DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_t100_to_string.

    METHODS:
      conv_t100_to_string
        IMPORTING !is_t100         TYPE sccms_t100_message
        RETURNING VALUE(rv_string) TYPE string,

      parse_msgv
        IMPORTING !iv_msgv       TYPE symsgv
        RETURNING VALUE(rv_msgv) TYPE symsgv.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_msgv_cache,
        input  TYPE symsgv,
        output TYPE symsgv,
      END OF t_msgv_cache,

      tt_msgv_cache
        TYPE HASHED TABLE OF t_msgv_cache
        WITH UNIQUE KEY primary_key COMPONENTS input,

      BEGIN OF t_t100_cache,
        t100   TYPE sccms_t100_message,
        output TYPE string,
      END OF t_t100_cache,

      tt_t100_cache
        TYPE HASHED TABLE OF t_t100_cache
        WITH UNIQUE KEY primary_key COMPONENTS t100.

    CONSTANTS:
      c_regex_date TYPE string VALUE '20[1-9][0-9][0-1][0-9][0-3][0-9]'.

    DATA:
      go_date_regex TYPE REF TO cl_abap_regex,
      gt_msgv_cache TYPE tt_msgv_cache,
      gt_t100_cache TYPE tt_t100_cache.

    CLASS-DATA:
      go_singleton TYPE REF TO zcl_bc_t100_to_string.

    METHODS:
      get_date_regex RETURNING VALUE(ro_regex) TYPE REF TO cl_abap_regex.

ENDCLASS.



CLASS zcl_bc_t100_to_string IMPLEMENTATION.

  METHOD conv_t100_to_string.

    ASSIGN gt_t100_cache[
        KEY primary_key COMPONENTS
        t100 = is_t100
      ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_t100_cache( t100 = is_t100 ).

      DATA(lv_msgv1) = parse_msgv( ls_cache-t100-msgv1 ).
      DATA(lv_msgv2) = parse_msgv( ls_cache-t100-msgv2 ).
      DATA(lv_msgv3) = parse_msgv( ls_cache-t100-msgv3 ).
      DATA(lv_msgv4) = parse_msgv( ls_cache-t100-msgv4 ).

      MESSAGE
        ID ls_cache-t100-msgid
        TYPE ls_cache-t100-msgty
        NUMBER ls_cache-t100-msgno
        WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
        INTO ls_cache-output.

      INSERT ls_cache
        INTO TABLE gt_t100_cache
        ASSIGNING <ls_cache>.

    ENDIF.

    rv_string = <ls_cache>-output.

  ENDMETHOD.

  METHOD get_instance.

    IF go_singleton IS INITIAL.
      go_singleton = NEW #( ).
    ENDIF.

    ro_obj = go_singleton.

  ENDMETHOD.

  METHOD get_date_regex.

    IF go_date_regex IS INITIAL.

      go_date_regex = NEW #(
        pattern       = c_regex_date
        ignore_case   = abap_false
        no_submatches = abap_true
      ).

    ENDIF.

    ro_regex = go_date_regex.

  ENDMETHOD.

  METHOD parse_msgv.

    ASSIGN gt_msgv_cache[
        KEY primary_key COMPONENTS
        input = iv_msgv
      ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_msgv_cache(
        input  = iv_msgv
        output = iv_msgv
      ).

      SHIFT ls_cache-output LEFT DELETING LEADING space.

      IF get_date_regex( )->create_matcher( text = ls_cache-output )->match( ) EQ abap_true.
        ls_cache-output = |{ ls_cache-output+6(2) }.{ ls_cache-output+4(2) }.{ ls_cache-output+0(4) }|.
      ENDIF.

      INSERT ls_cache
        INTO TABLE gt_msgv_cache
        ASSIGNING <ls_cache>.

    ENDIF.

    rv_msgv = <ls_cache>-output.


  ENDMETHOD.

ENDCLASS.