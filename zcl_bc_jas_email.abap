CLASS zcl_bc_jas_email DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bc_jas_carrier.

    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_JAS_EMAIL'.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_jas_email IMPLEMENTATION.

  METHOD zif_bc_jas_carrier~send_message.

    TRY.

        zcl_bc_mail_facade=>send_email(
          it_to      = it_to
          iv_subject = CONV #( iv_subject )
          it_body    = VALUE #( ( line = iv_body ) )
        ).

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = zif_bc_jas_carrier=>c_meth_sm.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.