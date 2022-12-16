CLASS zcl_bc_logoff_user_th_del DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_bc_logoff_user.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_logoff_user_th_del IMPLEMENTATION.
  METHOD zif_bc_logoff_user~logoff_user.
    TRY.
        CALL FUNCTION 'DEQUEUE_ALL'
          EXPORTING
            _synchron = abap_true.

        ##FM_SUBRC_OK
        CALL FUNCTION 'TH_DELETE_USER'
          EXPORTING
            user            = user
            client          = client
          EXCEPTIONS
            authority_error = 1
            OTHERS          = 2.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TH_DELETE_USER' ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW ycx_addict_class_method( textid   = ycx_addict_class_method=>unexpected_error
                                                     previous = diaper
                                                     class    = ycl_addict_class=>get_class_name( me )
                                                     method   = zif_bc_logoff_user=>method-logoff_user ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.