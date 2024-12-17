CLASS zcl_sd_division_set DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sd_division_set.

    METHODS constructor IMPORTING sparts TYPE tdt_spart OPTIONAL.

  PRIVATE SECTION.
    DATA sparts TYPE tdt_spart.
ENDCLASS.


CLASS zcl_sd_division_set IMPLEMENTATION.
  METHOD constructor.
    me->sparts = sparts.
  ENDMETHOD.

  METHOD zif_sd_division_set~collect.
    CHECK NOT line_exists( me->sparts[ table_line = spart ] ).
    APPEND spart TO sparts.
  ENDMETHOD.

  METHOD zif_sd_division_set~to_range.
    result = COND #( WHEN me->sparts IS INITIAL
                     THEN VALUE #( ( sign   = ycl_addict_toolkit=>sign-exclude
                                     option = ycl_addict_toolkit=>option-cp
                                     low    = '*' ) )
                     ELSE VALUE #( FOR GROUPS _gr OF _sp IN sparts
                                   GROUP BY _sp
                                   ( sign   = ycl_addict_toolkit=>sign-include
                                     option = ycl_addict_toolkit=>option-eq
                                     low    = _gr ) ) ).
  ENDMETHOD.
ENDCLASS.