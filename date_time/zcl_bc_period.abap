CLASS zcl_bc_period DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING jahrper       TYPE jahrper
                periv         TYPE periv DEFAULT 'K4'
      RETURNING VALUE(result) TYPE REF TO zcl_bc_period
      RAISING   zcx_bc_period.

    METHODS get_date_interval RETURNING VALUE(result) TYPE REF TO zif_bc_date_interval
                              RAISING   zcx_bc_period.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             jahrper TYPE jahrper,
             periv   TYPE periv,
             obj     TYPE REF TO zcl_bc_period,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map
                        WITH UNIQUE KEY primary_key COMPONENTS jahrper periv.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: jahrper             TYPE jahrper,
          periv               TYPE periv,
          date_interval_cache TYPE REF TO zif_bc_date_interval,
          endda_cache         TYPE endda.

    METHODS constructor IMPORTING jahrper TYPE jahrper
                                  periv   TYPE periv
                        RAISING   zcx_bc_period.

    METHODS get_begda RETURNING VALUE(result) TYPE begda.

    METHODS obtain_endda RETURNING VALUE(result) TYPE REF TO endda
                         RAISING   zcx_bc_period.

    METHODS get_gjahr     RETURNING VALUE(result) TYPE gjahr.

    METHODS get_monat     RETURNING VALUE(result) TYPE monat.

    METHODS get_poper     RETURNING VALUE(result) TYPE poper.

    METHODS get_alv_perio RETURNING VALUE(result) TYPE wahd_alv_perio.
ENDCLASS.


CLASS zcl_bc_period IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key
                      jahrper = jahrper
                      periv   = periv ]
           TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      INSERT VALUE #( jahrper = jahrper
                      periv   = periv
                      obj     = NEW #( jahrper = jahrper
                                       periv   = periv ) )
             INTO TABLE multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_date_interval.
    IF me->date_interval_cache IS INITIAL.
      DATA(begda) = get_begda( ).
      DATA(endda) = obtain_endda( ).

      TRY.
          me->date_interval_cache = CAST #( zcl_bc_date_interval=>get_instance( begda = begda
                                                                                endda = endda->* ) ).

        CATCH zcx_bc_date_range INTO DATA(data_rng_error).
          RAISE EXCEPTION NEW zcx_bc_period( textid   = zcx_bc_period=>date_range_build_failed
                                             period   = get_alv_perio( )
                                             previous = data_rng_error ).
      ENDTRY.
    ENDIF.

    result = me->date_interval_cache.
  ENDMETHOD.

  METHOD constructor.
    IF jahrper IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_period( textid = zcx_bc_period=>perid_initial ).
    ENDIF.

    IF periv IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_period( textid = zcx_bc_period=>periv_initial ).
    ENDIF.

    me->jahrper = jahrper.
    me->periv   = periv.
  ENDMETHOD.

  METHOD get_begda.
    result = |{ get_gjahr( ) }{ get_monat( ) }01|.
  ENDMETHOD.

  METHOD obtain_endda.
    result = REF #( me->endda_cache ).
    CHECK result->* IS INITIAL.

    TRY.
        DATA(gjahr) = get_gjahr( ).
        DATA(poper) = get_poper( ).

        ##FM_SUBRC_OK
        CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
          EXPORTING  i_gjahr        = gjahr
                     i_periv        = me->periv
                     i_poper        = poper
          IMPORTING  e_date         = result->*
          EXCEPTIONS input_false    = 1
                     t009_notfound  = 2
                     t009b_notfound = 3
                     OTHERS         = 4.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_IN_PERIOD_GET' ).

      CATCH ycx_addict_function_subrc INTO DATA(func_error).
        RAISE EXCEPTION NEW zcx_bc_period( textid   = zcx_bc_period=>cant_find_last_day
                                           period   = get_alv_perio( )
                                           previous = func_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_gjahr.
    result = me->jahrper+0(4).
  ENDMETHOD.

  METHOD get_monat.
    result = me->jahrper+5(2).
  ENDMETHOD.

  METHOD get_poper.
    result = me->jahrper+4(3).
  ENDMETHOD.

  METHOD get_alv_perio.
    result = |{ get_gjahr( ) }{ get_monat( ) }|.
  ENDMETHOD.
ENDCLASS.