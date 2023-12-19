CLASS zcl_bc_applog_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO zcl_bc_applog_gui.

    METHODS show_latest_log
      IMPORTING !object   TYPE balobj_d
                subobject TYPE balsubobj
                extnumber TYPE balnrext OPTIONAL
      RAISING   zcx_bc_log_disp.

  PRIVATE SECTION.
    TYPES extnumber_range TYPE RANGE OF balnrext.

    CLASS-DATA singleton TYPE REF TO zcl_bc_applog_gui.
ENDCLASS.


CLASS zcl_bc_applog_gui IMPLEMENTATION.
  METHOD get_instance.
    IF zcl_bc_applog_gui=>singleton IS INITIAL.
      zcl_bc_applog_gui=>singleton = NEW #( ).
    ENDIF.

    result = zcl_bc_applog_gui=>singleton.
  ENDMETHOD.

  METHOD show_latest_log.
    data date_txt type char10.

    DATA(extnumber_rng) = COND extnumber_range( WHEN extnumber IS INITIAL
                                                THEN VALUE #( )
                                                ELSE VALUE #( ( sign   = ycl_addict_toolkit=>sign-include
                                                                option = ycl_addict_toolkit=>option-eq
                                                                low    = extnumber ) ) ).

    SELECT SINGLE FROM balhdr
           FIELDS lognumber, aldate, altime
           WHERE  lognumber = ( SELECT MAX( lognumber ) FROM balhdr AS _bh2
                                WHERE  object     = @object     AND
                                       subobject  = @subobject  AND
                                       extnumber IN @extnumber_rng )
           INTO   @DATA(log).

    CHECK log IS NOT INITIAL.

    write log-aldate to date_txt.

    DATA(bdc) = NEW ycl_addict_bdc( ).

    bdc->add_scr( prg = 'SAPLSLG3'
                  dyn = '0100' ).

    bdc->add_fld( nam = 'BDC_OKCODE'
                  val = '=SELE' ).

    bdc->add_fld( nam = 'BALHDR-OBJECT'
                  val = CONV #( object ) ).

    bdc->add_fld( nam = 'BALHDR-SUBOBJECT'
                  val = CONV #( subobject ) ).

    bdc->add_fld( nam = 'BALHDR-EXTNUMBER'
                  val = CONV #( extnumber ) ).

    bdc->add_fld( nam = 'BALHDR-ALDATE'
                  val = CONV #( date_txt ) ).

    bdc->add_fld( nam = 'BALHDR-ALTIME'
                  val = CONV #( log-altime ) ).

    bdc->add_fld( nam = '*BALHDR-ALDATE'
                  val = CONV #( date_txt ) ).

    bdc->add_fld( nam = '*BALHDR-ALTIME'
                  val = CONV #( log-altime ) ).

    TRY.
        bdc->submit( tcode  = 'SLG1'
                     option = VALUE #( dismode = ycl_addict_bdc=>dismode-error ) ).

      CATCH cx_root INTO DATA(bdc_error).
        RAISE EXCEPTION NEW zcx_bc_log_disp( textid    = zcx_bc_log_disp=>cant_disp_bdc
                                             previous  = bdc_error
                                             object    = object
                                             subobject = subobject ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.