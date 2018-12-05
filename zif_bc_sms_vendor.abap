interface ZIF_BC_SMS_VENDOR
  public .

  types:
    tt_mobile_number type standard table of AD_MBNMBR1 with default key,

    begin of t_sms_options,
      tolerate_phone_number_error type abap_bool,
      tolerate_no_Recipient       type abap_bool,
      tolerate_text_truncate      type abap_bool,
    end of t_sms_options.

  constants:
    c_clsname_me type seoclsname value 'ZIF_BC_SMS_VENDOR'.

  methods:
    send_sms
      importing
        !it_recipients type tt_mobile_number
        !iv_message    type clike
        !is_options    type t_sms_options
      raising
        zcx_bc_sms_send.

endinterface.