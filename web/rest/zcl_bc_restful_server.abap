CLASS zcl_bc_restful_server DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS accept_json_input
      IMPORTING apiid              TYPE zbct_restful_001-apiid
                json_input         TYPE string
      RETURNING VALUE(json_output) TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS get_clean_apiid
      IMPORTING dirty        TYPE zbct_restful_001-apiid
      RETURNING VALUE(clean) TYPE zbct_restful_001-apiid.

    METHODS exception_to_json
      IMPORTING !exception  TYPE REF TO cx_root
      RETURNING VALUE(json) TYPE string.

    METHODS extract_exception_texts
      IMPORTING !exception TYPE REF TO cx_root
      CHANGING  texts      TYPE string_t.

ENDCLASS.


CLASS zcl_bc_restful_server IMPLEMENTATION.
  METHOD get_clean_apiid.
    clean = dirty.

    zcl_bc_text_toolkit=>remove_non_alphanum_chars(
      EXPORTING iv_valid_chars = |{ zcl_bc_text_toolkit=>c_alphanumeric }-_./,;|
      CHANGING  cv_text        = clean ).
  ENDMETHOD.

  METHOD accept_json_input.
    TRY.
        DATA(clean_apiid) = get_clean_apiid( apiid ).
        DATA(api) = NEW zcl_bc_restful_api( clean_apiid ).

        json_output = api->imp->accept_json_input( json_input = json_input
                                                   apiid      = clean_apiid ).

      CATCH cx_root INTO DATA(diaper).
        json_output = exception_to_json( diaper ).
    ENDTRY.
  ENDMETHOD.

  METHOD exception_to_json.
    DATA(error_texts) = VALUE string_t( ).

    extract_exception_texts( EXPORTING exception = exception
                             CHANGING  texts     = error_texts ).

    json = |\{ "errors": [ |.

    LOOP AT error_texts ASSIGNING FIELD-SYMBOL(<error_text>).
      IF sy-tabix > 1.
        json = |{ json }, |.
      ENDIF.

      json = |{ json } "{ zcl_bc_json_toolkit=>get_json_text( <error_text> ) }"|.
    ENDLOOP.

    json = |{ json } ] \}|.
  ENDMETHOD.

  METHOD extract_exception_texts.
    CHECK exception IS NOT INITIAL.
    APPEND exception->get_text( ) TO texts.

    extract_exception_texts( EXPORTING exception = exception->previous
                             CHANGING  texts     = texts ).
  ENDMETHOD.
ENDCLASS.