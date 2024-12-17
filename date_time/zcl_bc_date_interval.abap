CLASS zcl_bc_date_interval DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_date_interval.

    CLASS-METHODS get_instance
      IMPORTING begda         TYPE begda
                endda         TYPE endda
      RETURNING VALUE(result) TYPE REF TO zcl_bc_date_interval
      RAISING   zcx_bc_date_range.

    CLASS-METHODS get_instance_by_sel_rng
      IMPORTING date_rng      TYPE trgr_date
      RETURNING VALUE(result) TYPE REF TO zcl_bc_date_interval
      RAISING   zcx_bc_date_range.

    CLASS-METHODS get_instance_by_single_date
      IMPORTING !date         TYPE sydatum
      RETURNING VALUE(result) TYPE REF TO zcl_bc_date_interval
      RAISING   zcx_bc_date_range.

  PRIVATE SECTION.
    TYPES: BEGIN OF mt_dict,
             begda TYPE begda,
             endda TYPE endda,
             obj   TYPE REF TO zcl_bc_date_interval,
             error TYPE REF TO zcx_bc_date_range,
           END OF mt_dict,

           mt_set TYPE HASHED TABLE OF mt_dict
           WITH UNIQUE KEY primary_key COMPONENTS begda endda.

    CLASS-DATA mts TYPE mt_set.

    DATA: begda TYPE begda,
          endda TYPE endda,
          fmfd  TYPE datum,
          lmld  TYPE datum.

    METHODS constructor
      IMPORTING begda TYPE begda
                endda TYPE endda
      RAISING   zcx_bc_date_range.
ENDCLASS.


CLASS zcl_bc_date_interval IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_bc_date_interval=>mts[ KEY primary_key
                                                     begda = begda
                                                     endda = endda ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE mt_dict( begda = begda
                                      endda = endda ).

        TRY.
            new_mt-obj = NEW #( begda = new_mt-begda
                                endda = new_mt-endda ).
          CATCH zcx_bc_date_range INTO new_mt-error ##NO_HANDLER.
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_bc_date_interval=>mts REFERENCE INTO mt.
    ENDTRY.

    IF mt->error IS NOT INITIAL.
      RAISE EXCEPTION mt->error.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_instance_by_sel_rng.
    ASSERT lines( date_rng ) = 1.

    DATA(date_entry) = REF #( date_rng[ 1 ] ).

    ASSERT     date_entry->sign  = ycl_addict_toolkit=>sign-include
           AND date_entry->low  IS NOT INITIAL.

    DATA(begda) = date_entry->low.

    DATA(endda) = SWITCH endda( date_entry->option
                                WHEN ycl_addict_toolkit=>option-eq THEN begda
                                WHEN ycl_addict_toolkit=>option-bt THEN date_entry->high ).

    ASSERT endda IS NOT INITIAL.

    result = get_instance( begda = begda
                           endda = endda ).
  ENDMETHOD.

  METHOD get_instance_by_single_date.
    result = get_instance( begda = date
                           endda = date ).
  ENDMETHOD.

  METHOD zif_bc_date_interval~get_first_date.
    result = me->begda.
  ENDMETHOD.

  METHOD zif_bc_date_interval~get_last_date.
    result = me->endda.
  ENDMETHOD.

  METHOD constructor.
    IF begda IS INITIAL OR endda IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_date_range( textid = zcx_bc_date_range=>begin_or_end_missing ).
    ENDIF.

    IF begda > endda.
      RAISE EXCEPTION NEW zcx_bc_date_range( textid     = zcx_bc_date_range=>begin_gt_end
                                             date_begin = begda
                                             date_end   = endda ).
    ENDIF.

    me->begda = begda.
    me->endda = endda.
  ENDMETHOD.

  METHOD zif_bc_date_interval~get_first_month_first_day.
    IF me->fmfd IS INITIAL.
      me->fmfd = |{ me->begda+0(6) }01|.
    ENDIF.

    result = me->fmfd.
  ENDMETHOD.

  METHOD zif_bc_date_interval~get_last_month_last_day.
    IF me->lmld IS INITIAL.
      TRY.
          ##FM_SUBRC_OK
          CALL FUNCTION 'LAST_DAY_OF_MONTHS'
            EXPORTING  day_in            = me->endda
            IMPORTING  last_day_of_month = me->lmld
            EXCEPTIONS day_in_no_date    = 1
                       OTHERS            = 2.

          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_OF_MONTHS' ).

        CATCH cx_root INTO DATA(func_error).
          RAISE EXCEPTION NEW zcx_bc_date( textid   = zcx_bc_date=>cant_find_period_last_day
                                           previous = func_error
                                           datum    = me->endda ).
      ENDTRY.
    ENDIF.

    result = me->lmld.
  ENDMETHOD.
ENDCLASS.