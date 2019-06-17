CLASS zcl_bc_text_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_string TYPE STANDARD TABLE OF string .

    CLASS-METHODS get_shortest_text
      IMPORTING
        !it_candidate       TYPE tt_string
        !iv_ignore_if_empty TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_shortest)  TYPE string .

    CLASS-METHODS get_text_after_separator
      IMPORTING
        !iv_text       TYPE clike
        !iv_separator  TYPE clike
      RETURNING
        VALUE(rv_text) TYPE string.

    CLASS-METHODS is_string_an_integer
      IMPORTING
        !iv_string   TYPE string
      RETURNING
        VALUE(rv_is) TYPE abap_bool .
    CLASS-METHODS remove_non_alphanum_chars
      IMPORTING
        !iv_valid_chars TYPE clike OPTIONAL
      CHANGING
        !cv_text        TYPE clike .
    CLASS-METHODS remove_text_in_string
      IMPORTING
        !iv_string       TYPE clike
        !iv_remove       TYPE clike
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS replace_turkish_characters
      CHANGING
        !cv_text TYPE clike .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_text_and_len,
        text TYPE string,
        len  TYPE i,
      END OF t_text_and_len,

      tt_text_and_len TYPE STANDARD TABLE OF t_text_and_len WITH DEFAULT KEY.

    CONSTANTS:
      c_alphanumeric TYPE string VALUE `1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM `.
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

  METHOD get_text_after_separator.

    DATA lt_split TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CHECK iv_text IS NOT INITIAL.

    SPLIT iv_text AT iv_separator INTO TABLE lt_split.

    IF lines( lt_split ) LE 0.
      RETURN.
    ENDIF.

    rv_text = lt_split[ lines( lt_split ) ].

  ENDMETHOD.

  METHOD is_string_an_integer.

    DATA lv_int TYPE i.

    CHECK iv_string IS NOT INITIAL.

    TRY.
        MOVE EXACT iv_string TO lv_int.
        rv_is = abap_true.
      CATCH cx_root.
        rv_is = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD remove_non_alphanum_chars.

    DATA(lv_output) = CONV string( space ).
    DATA(lv_length) = strlen( cv_text ).
    DATA(lv_pos) = 0.

    WHILE lv_pos LT lv_length.
      DATA(lv_char) = CONV string( cv_text+lv_pos(1) ).
      IF lv_char CA c_alphanumeric OR
         ( iv_valid_chars IS NOT INITIAL AND
           lv_char CA iv_valid_chars
         ).
        lv_output = |{ lv_output }{ lv_char }|.
      ELSE.
        lv_output = |{ lv_output } |.
      ENDIF.

      ADD 1 TO lv_pos.

    ENDWHILE.

    cv_text = lv_output.

  ENDMETHOD.


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