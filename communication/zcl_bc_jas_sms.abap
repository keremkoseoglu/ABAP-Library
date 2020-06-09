CLASS zcl_bc_jas_sms DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bc_jas_carrier.

    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_JAS_SMS'.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_jas_sms IMPLEMENTATION.

  METHOD zif_bc_jas_carrier~send_message.

    TRY.

        zcl_bc_sms_facade=>send_sms(
          is_recipients  = VALUE #( users = it_to )
          iv_message     = iv_body
          is_sms_options = VALUE #(
            tolerate_phone_number_error = abap_true
            tolerate_no_recipient       = abap_false
            tolerate_text_truncate      = abap_true
          )
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