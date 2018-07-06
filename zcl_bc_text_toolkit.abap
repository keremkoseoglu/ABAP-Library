CLASS zcl_bc_text_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_string TYPE STANDARD TABLE OF string.

    CLASS-METHODS:
      get_shortest_text
        IMPORTING
          !it_candidate       TYPE tt_string
          !iv_ignore_if_empty TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(rv_shortest)  TYPE string,

      remove_text_in_string
        IMPORTING
          !iv_string       TYPE clike
          !iv_remove       TYPE clike
        RETURNING
          VALUE(rv_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_text_and_len,
        text TYPE string,
        len  TYPE i,
      END OF t_text_and_len,

      tt_text_and_len TYPE STANDARD TABLE OF t_text_and_len WITH DEFAULT KEY.

ENDCLASS.



CLASS zcl_bc_text_toolkit IMPLEMENTATION.

  METHOD get_shortest_text.

    DATA(lt_tl) = VALUE tt_text_and_len(
      FOR lv_candidate IN it_candidate
      (
        text = lv_candidate
        len  = strlen( lv_candidate )
      )
    ).

    IF iv_ignore_if_empty EQ abap_true.
      DELETE lt_tl WHERE text IS INITIAL.
    ENDIF.

    IF lt_tl IS NOT INITIAL.
      SORT lt_tl BY len.
      rv_shortest = lt_tl[ 1 ]-text.
    ENDIF.

  ENDMETHOD.

  METHOD remove_text_in_string.
    rv_result = iv_string.
    REPLACE ALL OCCURRENCES OF iv_remove IN rv_result WITH space.
  ENDMETHOD.

ENDCLASS.