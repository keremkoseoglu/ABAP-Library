CLASS zcl_bc_html_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES string_list TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    CONSTANTS max_line_len TYPE i VALUE 255. " w3html_tab'dan geliyor

    METHODS reset_html.

    METHODS open_html
      RAISING   zcx_bc_html.

    METHODS close_html RAISING zcx_bc_html.

    METHODS append_html
      IMPORTING !html TYPE clike
      RAISING   zcx_bc_html.

    METHODS append_htmls
      IMPORTING !htmls TYPE string_list
      RAISING   zcx_bc_html.

    METHODS append_text
      IMPORTING !text TYPE clike
      RAISING   zcx_bc_html.

    METHODS append_t100_msg
      IMPORTING !msgid TYPE symsgid DEFAULT zcl_sd_sofo_wf_toolkit=>msgid-sofo
                !msgno TYPE symsgno
                !msgv1 TYPE any OPTIONAL
                !msgv2 TYPE any OPTIONAL
                !msgv3 TYPE any OPTIONAL
                !msgv4 TYPE any OPTIONAL
      RAISING   zcx_bc_html.

    METHODS append_line_break RAISING zcx_bc_html.

    METHODS append_w3htmls IMPORTING !w3htmls TYPE w3html_tab.

    METHODS append_value
      IMPORTING !value    TYPE any
                !waers    TYPE waers OPTIONAL
                !meins    TYPE meins OPTIONAL
                !decimals TYPE i     OPTIONAL
      RAISING   zcx_bc_html.

    METHODS append_texts_as_numbered_list
      IMPORTING !texts TYPE string_list
      RAISING   zcx_bc_html.

    METHODS get_html_itab RETURNING VALUE(result) TYPE w3html_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF html_snippet,
                 line_break TYPE char8 VALUE '<br><br>',
               END OF html_snippet.

    DATA html TYPE w3html_tab.

    METHODS append_clike
      IMPORTING !text   TYPE clike
                !escape TYPE abap_bool DEFAULT abap_true
      RAISING   zcx_bc_html.
ENDCLASS.



CLASS zcl_bc_html_builder IMPLEMENTATION.
  METHOD reset_html.
    CLEAR html.
  ENDMETHOD.


  METHOD open_html.
    append_html( |<html><body>| ).
  ENDMETHOD.


  METHOD close_html.
    append_html( |</body></html>| ).
  ENDMETHOD.


  METHOD append_html.
    append_clike( text   = html
                  escape = abap_false ).
  ENDMETHOD.


  METHOD append_htmls.
    LOOP AT htmls REFERENCE INTO DATA(html).
      append_html( html   = html->* ).
    ENDLOOP.
  ENDMETHOD.


  METHOD append_text.
    append_clike( text   = text
                  escape = abap_true ).
  ENDMETHOD.


  METHOD append_t100_msg.
    DATA msg_text TYPE string.

    MESSAGE ID      msgid
            TYPE    ycl_simbal=>msgty-status
            NUMBER  msgno
            WITH    msgv1 msgv2 msgv3 msgv4
            INTO    msg_text.

    append_text( msg_text ).
  ENDMETHOD.


  METHOD append_line_break.
    append_html( me->html_snippet-line_break ).
  ENDMETHOD.


  METHOD append_w3htmls.
    APPEND LINES OF w3htmls TO me->html.
  ENDMETHOD.


  METHOD append_value.
    DATA out_text TYPE text200.

    IF waers IS NOT INITIAL.
      DATA(w_decimals) = COND i( WHEN decimals IS NOT INITIAL
                                 THEN decimals
                                 ELSE 2 ).

      WRITE value TO out_text LEFT-JUSTIFIED DECIMALS w_decimals CURRENCY waers.

    ELSEIF meins IS NOT INITIAL.
      WRITE value TO out_text LEFT-JUSTIFIED UNIT meins.
    ELSE.
      IF decimals IS INITIAL.
        WRITE value TO out_text LEFT-JUSTIFIED.
      ELSE.
        WRITE value TO out_text DECIMALS decimals LEFT-JUSTIFIED.
      ENDIF.
    ENDIF.

    append_text( out_text ).
  ENDMETHOD.


  METHOD append_texts_as_numbered_list.
    append_html( '<ol>' ).

    LOOP AT texts REFERENCE INTO DATA(text).
      append_html( '<li>' ).
      append_text( text->* ).
      append_html( '</li>' ).
    ENDLOOP.

    append_html( '</ol>' ).
  ENDMETHOD.


  METHOD get_html_itab.
    result = me->html.
  ENDMETHOD.


  METHOD append_clike.
    DATA(appendable_string) =
      SWITCH string( escape WHEN abap_true
                            THEN cl_http_utility=>escape_html( CONV #( text ) )
                            ELSE text ).

    DATA(html_entry) = VALUE w3html( ).

    TRY.
        MOVE EXACT appendable_string TO html_entry-line.
      CATCH cx_root INTO DATA(move_error).
        RAISE EXCEPTION NEW zcx_bc_html( textid = zcx_bc_html=>text_too_long
                                         text   = text ).
    ENDTRY.

    APPEND html_entry TO me->html.
  ENDMETHOD.
ENDCLASS.