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

      remove_non_alphanum_chars
        importing !iv_valid_chars type clike optional
        changing  !cv_text        type clike,

      remove_text_in_string
        IMPORTING
          !iv_string       TYPE clike
          !iv_remove       TYPE clike
        RETURNING
          VALUE(rv_result) TYPE string,

      replace_turkish_characters CHANGING !cv_text TYPE clike.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_text_and_len,
        text TYPE string,
        len  TYPE i,
      END OF t_text_and_len,

      tt_text_and_len TYPE STANDARD TABLE OF t_text_and_len WITH DEFAULT KEY.

    constants:
      c_alphanumeric type string value `1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM `.

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

  method remove_non_alphanum_chars.

    data(lv_output) = conv string( space ).
    data(lv_length) = strlen( cv_text ).
    data(lv_pos) = 0.

    while lv_pos lt lv_length.
      data(lv_char) = conv string( cv_text+lv_pos(1) ).
      if lv_char ca c_alphanumeric or
         ( iv_valid_chars is not initial and
           lv_char ca iv_valid_Chars
         ).
        lv_output = |{ lv_output }{ lv_char }|.
      else.
        lv_output = |{ lv_output } |.
      endif.

      add 1 to lv_pos.

    endwhile.

    cv_text = lv_output.

  endmethod.

  METHOD remove_text_in_string.
    rv_result = iv_string.
    REPLACE ALL OCCURRENCES OF iv_remove IN rv_result WITH space.
  ENDMETHOD.

  METHOD replace_turkish_characters.

    REPLACE ALL OCCURRENCES OF:
      'ı' IN cv_text WITH 'i',
      'ğ' IN cv_text WITH 'g',
      'Ğ' IN cv_text WITH 'G',
      'ü' IN cv_text WITH 'u',
      'Ü' IN cv_text WITH 'U',
      'ş' IN cv_text WITH 's',
      'Ş' IN cv_text WITH 'S',
      'İ' IN cv_text WITH 'I',
      'ö' IN cv_text WITH 'o',
      'Ö' IN cv_text WITH 'O',
      'ç' IN cv_text WITH 'c',
      'Ç' IN cv_text WITH 'C'.

  ENDMETHOD.

ENDCLASS.