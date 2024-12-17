CLASS zcl_bc_finite_year_period_rng DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_year_period_range.

    CLASS-METHODS get_instance
      IMPORTING jahrper_from  TYPE jahrper
                jahrper_to    TYPE jahrper
      RETURNING VALUE(result) TYPE REF TO zcl_bc_finite_year_period_rng
      RAISING   zcx_bc_period_range.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             jahrper_from TYPE jahrper,
             jahrper_to   TYPE jahrper,
             rng          TYPE REF TO zcl_bc_finite_year_period_rng,
             error        TYPE REF TO zcx_bc_period_range,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
           WITH UNIQUE KEY primary_key COMPONENTS jahrper_from jahrper_to.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: jahrper_from     TYPE jahrper,
          jahrper_to       TYPE jahrper,
          month_ends       TYPE datum_tab,
          month_ends_built TYPE abap_bool.

    METHODS constructor
      IMPORTING jahrper_from TYPE jahrper
                jahrper_to   TYPE jahrper
      RAISING   zcx_bc_period_range.

    METHODS get_month_ends
      RETURNING VALUE(result) TYPE REF TO datum_tab
      RAISING   zcx_bc_period_range.

    METHODS raise_period_range_error
      IMPORTING textid    TYPE scx_t100key
                !previous TYPE REF TO cx_root OPTIONAL
      RAISING   zcx_bc_period_range.

ENDCLASS.


CLASS zcl_bc_finite_year_period_rng IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_bc_finite_year_period_rng=>multitons[ KEY primary_key
                                                                    jahrper_from = jahrper_from
                                                                    jahrper_to   = jahrper_to ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE multiton_dict( jahrper_from = jahrper_from
                                            jahrper_to   = jahrper_to ).

        TRY.
            new_mt-rng = NEW #( jahrper_from = new_mt-jahrper_from
                                jahrper_to   = new_mt-jahrper_to ).
          CATCH zcx_bc_period_range INTO new_mt-error ##NO_HANDLER.
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_bc_finite_year_period_rng=>multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->error IS NOT INITIAL.
      RAISE EXCEPTION mt->error.
    ENDIF.

    result = mt->rng.
  ENDMETHOD.

  METHOD constructor.
    IF jahrper_from IS INITIAL OR jahrper_to IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_period_range( textid = zcx_bc_period_range=>period_range_incomplete ).
    ENDIF.

    IF jahrper_from > jahrper_to.
      raise_period_range_error( zcx_bc_period_range=>period_greater ).
    ENDIF.

    me->jahrper_from = jahrper_from.
    me->jahrper_to   = jahrper_to.
  ENDMETHOD.

  METHOD get_month_ends.
    DATA last_day TYPE dats.

    IF me->month_ends_built = abap_false.
      CLEAR me->month_ends.

      DATA(jahrper_cursor) = me->jahrper_from.

      WHILE jahrper_cursor <= me->jahrper_to.
        TRY.
            DATA(jahrper_date) = CONV sydatum( |{ jahrper_cursor+0(4) }{ jahrper_cursor+5(2) }01| ).
            CLEAR last_day.

            ##FM_SUBRC_OK
            CALL FUNCTION 'LAST_DAY_OF_MONTHS'
              EXPORTING  day_in            = jahrper_date
              IMPORTING  last_day_of_month = last_day
              EXCEPTIONS day_in_no_date    = 1
                         OTHERS            = 2.

            ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_OF_MONTHS' ).

            IF last_day IS INITIAL.
              RAISE EXCEPTION NEW zcx_bc_period( textid = zcx_bc_period=>cant_find_last_day
                                                 period = |{ jahrper_cursor+0(4) }-{ jahrper_cursor+5(2) }| ).
            ENDIF.

          CATCH cx_root INTO DATA(last_day_error).
            raise_period_range_error( textid   = zcx_bc_period_range=>month_end_determination_fail
                                      previous = last_day_error ).
        ENDTRY.

        APPEND last_day TO me->month_ends.
        jahrper_cursor += 1.
      ENDWHILE.

      me->month_ends_built = abap_true.
    ENDIF.

    result = REF #( me->month_ends ).
  ENDMETHOD.

  METHOD raise_period_range_error.
    RAISE EXCEPTION NEW zcx_bc_period_range( textid   = textid
                                             previous = previous
                                             bdatj1   = jahrper_from+0(4)
                                             poper1   = jahrper_from+4(3)
                                             bdatj2   = jahrper_to+0(4)
                                             poper2   = jahrper_to+4(3) ).
  ENDMETHOD.

  METHOD zif_bc_year_period_range~get_month_ends.
    result = get_month_ends( )->*.
  ENDMETHOD.
ENDCLASS.