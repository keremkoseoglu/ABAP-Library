CLASS zcl_bc_logoff_user_sm04 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_bc_logoff_user.

    CONSTANTS: BEGIN OF class,
                 me TYPE seocpdname VALUE 'ZCL_BC_LOGOFF_USER_SM04',
               END OF class.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF state_dict,
             user TYPE REF TO xubname,
             bdc  TYPE REF TO zcl_bc_bdc,
           END OF state_dict.

    CONSTANTS: BEGIN OF tcode,
                 sm04 TYPE sytcode VALUE 'SM04',
               END OF tcode.

    DATA state TYPE state_dict.

    METHODS build_bdc.
    METHODS run_bdc RAISING zcx_bc_symsg.
ENDCLASS.



CLASS zcl_bc_logoff_user_sm04 IMPLEMENTATION.
  METHOD build_bdc.
    " Hazırlık
    me->state-bdc = NEW #( ).

    " Ekrana girip, filtreye bas
    me->state-bdc->add_scr( iv_prg = 'SAPMSSY0'
                            iv_dyn = '0120' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=&ILT' ).

    " Filtre Popup: Username seç ve sola aktar
    me->state-bdc->add_scr( iv_prg = 'SAPLSKBH'
                            iv_dyn = '0830' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=WLSE' ).

    me->state-bdc->add_fld( iv_nam = 'GT_FIELD_LIST-MARK(02)'
                            iv_val = 'X' ).

    " Filtre Popup: Devam
    me->state-bdc->add_scr( iv_prg = 'SAPLSKBH'
                            iv_dyn = '0830' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=CONT' ).

    " Popup'ta filtre değeri gir (USER)
    me->state-bdc->add_scr( iv_prg = 'SAPLSSEL'
                            iv_dyn = '1104' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=CRET' ).

    me->state-bdc->add_fld( iv_nam = '%%DYN001-LOW'
                            iv_val = CONV #( me->state-user->* ) ).

    " Ana liste: SELECT ALL
    me->state-bdc->add_scr( iv_prg = 'SAPMSSY0'
                            iv_dyn = '0120' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=&ALL' ).

    " Tüm oturumları kapat
    me->state-bdc->add_scr( iv_prg = 'SAPMSSY0'
                            iv_dyn = '0120' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=UDELGN' ).

    " Emin misin? evet
    me->state-bdc->add_scr( iv_prg = 'SAPLSPO1'
                            iv_dyn = '0500' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=OPT1' ).

    " Çıkış
    me->state-bdc->add_scr( iv_prg = 'SAPMSSY0'
                            iv_dyn = '0120' ).

    me->state-bdc->add_fld( iv_nam = 'BDC_OKCODE'
                            iv_val = '=&F03' ).
  ENDMETHOD.


  METHOD run_bdc.
    ASSERT me->state-bdc IS NOT INITIAL.

    DATA(options) =
      VALUE ctu_params( dismode  = 'N'
                        updmode  = 'S'
                        defsize  = abap_true ).

    me->state-bdc->submit(
      EXPORTING iv_tcode  = me->tcode-sm04
                is_option = options
      IMPORTING et_msg    = DATA(bdc_messages) ).

    LOOP AT bdc_messages REFERENCE INTO DATA(error_msg) WHERE msgtyp IN ycl_simbal=>get_crit_msgty_range( ).
      DATA(error_txt) = CONV string( space ).

      MESSAGE ID      error_msg->msgid
              TYPE    ycl_simbal=>msgty-status
              NUMBER  error_msg->msgnr
              WITH    error_msg->msgv1 error_msg->msgv2
                      error_msg->msgv3 error_msg->msgv4
              INTO    error_txt.

      RAISE EXCEPTION zcx_bc_symsg=>get_instance( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_bc_logoff_user~logoff_user.
    TRY.
        me->state = VALUE #( user = REF #( user ) ).
        build_bdc( ).
        run_bdc( ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW ycx_addict_class_method( textid   = ycx_addict_class_method=>unexpected_error
                                                     previous = diaper
                                                     class    = ycl_addict_class=>get_class_name( me )
                                                     method   = zif_bc_logoff_user=>method-logoff_user ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.