CLASS zcl_bc_sms_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_recipient,
        phones TYPE zif_bc_sms_vendor=>tt_mobile_number,
        users  TYPE rke_userid,
      END OF t_recipient.

    CLASS-METHODS:
      send_sms
        IMPORTING
          !is_recipients  TYPE t_recipient
          !iv_message     TYPE clike
          !io_sms_vendor  TYPE REF TO zif_bc_sms_vendor OPTIONAL
          !is_sms_options TYPE zif_bc_sms_vendor=>t_sms_options OPTIONAL
        RAISING
          zcx_bc_sms_send
          zcx_bc_table_content
          zcx_bc_user_master_data.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_fnam_clsname   TYPE fieldname VALUE 'CLSNAME',
      c_tabname_vendor TYPE tabname   VALUE 'ZBCT_SMS_VENDOR'.

    CLASS-DATA:
      go_default_vendor TYPE REF TO zif_bc_sms_vendor.

    CLASS-METHODS:
      build_phone_list
        IMPORTING
          !is_recipients   TYPE t_recipient
          !is_sms_options  TYPE zif_bc_sms_vendor=>t_sms_options
        RETURNING
          VALUE(rt_phones) TYPE zif_bc_sms_vendor=>tt_mobile_number
        RAISING
          zcx_bc_sms_send
          zcx_bc_user_master_data,

      get_default_sms_vendor
        RETURNING VALUE(ro_vendor) TYPE REF TO zif_bc_sms_vendor
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_bc_sms_facade IMPLEMENTATION.

  METHOD build_phone_list.

    " Doğrudan gönderilen numaralar """""""""""""""""""""""""""""""""

    APPEND LINES OF is_recipients-phones TO rt_phones.

    " SAP kullanıcılarının numaraları """""""""""""""""""""""""""""""

    LOOP AT is_recipients-users ASSIGNING FIELD-SYMBOL(<lv_user>).

      TRY.

          DATA(lv_mobile) = zcl_bc_sap_user=>get_instance(
              <lv_user>
            )->get_mobile_number(
              iv_tolerate_missing_number = is_sms_options-tolerate_phone_number_error
            ).

          APPEND lv_mobile TO rt_phones.

        CATCH cx_root INTO DATA(lo_diaper).

          CHECK is_sms_options-tolerate_phone_number_error EQ abap_false.

          RAISE EXCEPTION TYPE zcx_bc_user_master_data
            EXPORTING
              textid = zcx_bc_user_master_data=>mobile_missing
              uname  = <lv_user>.

      ENDTRY.

    ENDLOOP.

    " Dönüş """""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DELETE rt_phones WHERE table_line IS INITIAL.
    SORT rt_phones.
    DELETE ADJACENT DUPLICATES FROM rt_phones.

  ENDMETHOD.

  METHOD get_default_sms_vendor.

    DATA lo_obj TYPE REF TO object.

    IF go_default_vendor IS INITIAL.

      SELECT SINGLE clsname
        FROM zbct_sms_vendor
        WHERE sysid EQ @sy-sysid
        INTO @DATA(lv_clsname).

      IF lv_clsname IS INITIAL.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = |{ sy-sysid }|
            tabname  = c_tabname_vendor.
      ENDIF.

      TRY.
          CREATE OBJECT lo_obj TYPE (lv_clsname).
          go_default_vendor ?= lo_obj.
        CATCH cx_root INTO DATA(lo_diaper).
          RAISE EXCEPTION TYPE zcx_bc_table_content
            EXPORTING
              textid    = zcx_bc_table_content=>value_invalid
              objectid  = |{ sy-sysid }|
              tabname   = c_tabname_vendor
              fieldname = c_fnam_clsname.
      ENDTRY.

    ENDIF.

    ro_vendor = go_default_vendor.

  ENDMETHOD.

  METHOD send_sms.

    IF iv_message IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_sms_send
        EXPORTING
          textid = zcx_bc_sms_send=>empty_body.
    ENDIF.

    DATA(lt_phones) = build_phone_list(
      is_recipients  = is_recipients
      is_sms_options = is_sms_options
    ).

    IF lt_phones IS INITIAL.
      IF is_sms_options-tolerate_no_recipient EQ abap_false.
        RAISE EXCEPTION TYPE zcx_bc_sms_send
          EXPORTING
            textid = zcx_bc_sms_send=>no_recipient.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    IF io_sms_vendor IS NOT INITIAL.
      DATA(lo_sms_vendor) = io_sms_vendor.
    ELSE.
      lo_sms_vendor = get_default_sms_vendor( ).
    ENDIF.

    lo_sms_vendor->send_sms(
      it_recipients = lt_phones
      iv_message    = iv_message
      is_options    = is_sms_options
    ).

  ENDMETHOD.

ENDCLASS.