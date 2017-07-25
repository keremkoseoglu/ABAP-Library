CLASS zcl_bc_json_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      is_initial
        IMPORTING !iv_json_value    TYPE clike
        RETURNING VALUE(rv_initial) TYPE abap_bool,

      is_null
        IMPORTING !iv_json_value TYPE clike
        RETURNING VALUE(rv_null) TYPE abap_bool,

      get_json_date
        IMPORTING !iv_timestamp  TYPE clike
        RETURNING VALUE(rv_date) TYPE dats,

      get_json_time
        IMPORTING !iv_timestamp  TYPE clike
        RETURNING VALUE(rv_time) TYPE tims,

      parse_json_bool
        IMPORTING !iv_bool       TYPE clike
        RETURNING VALUE(rv_bool) TYPE abap_bool,

      parse_json_timestamp
        IMPORTING
          !iv_timestamp TYPE clike
        EXPORTING
          !ev_date      TYPE dats
          !ev_time      TYPE tims.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_json_null_max TYPE text4 VALUE 'NULL',
      c_json_null_min TYPE text4 VALUE 'null'.

ENDCLASS.



CLASS zcl_bc_json_toolkit IMPLEMENTATION.

  METHOD get_json_date.

    parse_json_timestamp(
      EXPORTING iv_timestamp = iv_timestamp
      IMPORTING ev_date      = rv_date
    ).

  ENDMETHOD.

  METHOD get_json_time.

    parse_json_timestamp(
      EXPORTING iv_timestamp = iv_timestamp
      IMPORTING ev_time      = rv_time
    ).

  ENDMETHOD.

  METHOD parse_json_bool.
    rv_bool = boolc( iv_bool EQ 'true' OR iv_bool EQ 'TRUE' ).
  ENDMETHOD.

  METHOD is_initial.
    rv_initial = xsdbool(
        iv_json_value IS INITIAL OR
        is_null( iv_json_value ) EQ abap_true
    ).
  ENDMETHOD.

  METHOD is_null.

    rv_null = xsdbool(
        iv_json_value EQ c_json_null_max OR
        iv_json_value EQ c_json_null_min
    ).

  ENDMETHOD.

  METHOD parse_json_timestamp.

    "2016-05-12T16:55:55.023+0300

    CLEAR:
        ev_date,
        ev_time.

    CHECK is_initial( iv_timestamp ) EQ abap_false.

    ev_date = |{ iv_timestamp+0(4) }{ iv_timestamp+5(2) }{ iv_timestamp+8(2) }|.

    CHECK (
        ev_time IS REQUESTED AND
        strlen( iv_timestamp ) GT 10
    ).

    ev_time = |{ iv_timestamp+11(2) }{ iv_timestamp+14(2) }{ iv_timestamp+17(2) }|.

  ENDMETHOD.


ENDCLASS.