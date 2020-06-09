CLASS zcl_bc_sms_mobildev DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bc_sms_vendor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_state,
        message    TYPE string,
        options    TYPE zif_bc_sms_vendor=>t_sms_options,
        recipients TYPE zif_bc_sms_vendor=>tt_mobile_number,
      END OF t_state,

      BEGIN OF t_config,
        user TYPE string,
        pass TYPE string,
        clnt TYPE string,
        orig TYPE string,
        port TYPE prx_logical_port_name,
      END OF t_config.

    CONSTANTS:
      BEGIN OF c_parval,
        client     TYPE zbct_par_val-pname VALUE 'MOBILDEV_CLNT',
        user       TYPE zbct_par_val-pname VALUE 'MOBILDEV_USER',
        originator TYPE zbct_par_val-pname VALUE 'MOBILDEV_ORIG',
        password   TYPE zbct_par_val-pname VALUE 'MOBILDEV_PASS',
        pattern    TYPE zbct_par_val-pname VALUE 'MOBILDEV_*',
        port       TYPE zbct_par_val-pname VALUE 'MOBILDEV_PORT',
      END OF c_parval,

      c_char_limit     TYPE i VALUE 153,
      c_status_success TYPE zbc_mobildevsmsresult-status_code VALUE 0,
      c_valid_chars    TYPE string VALUE '()[]{}=,*?\/%+#!":;,_|'.

    CLASS-DATA:
      gs_config TYPE t_config.

    DATA:
      gs_state TYPE t_state.

    CLASS-METHODS:
      read_config RAISING zcx_bc_table_content.

    METHODS:
      call_sms_service
        RAISING
          cx_ai_system_fault
          zcx_bc_sms_send,

      format_message RAISING zcx_bc_sms_send,

      format_recipients
        RAISING
          zcx_bc_phone_number
          zcx_bc_sms_send,

      remove_duplicate_recipients.

ENDCLASS.



CLASS zcl_bc_sms_mobildev IMPLEMENTATION.

  METHOD call_sms_service.

    NEW zbc_mobildevco_apisoap(
        logical_port_name = gs_config-port
      )->smsmulti_senders(
        EXPORTING input  = VALUE #(
          username     = gs_config-user
          company_code = gs_config-clnt
          password     = gs_config-pass
          originator   = gs_config-orig
          sms_info_array = VALUE #(
            smsinfo = VALUE #(
              FOR _gsmno IN gs_state-recipients (
                gsmno   = _gsmno
                message = gs_state-message
              )
            )
          )
        )
        IMPORTING output = DATA(ls_output)
      ).

    IF ls_output-smsmulti_senders_result-status_code NE c_status_success.
      RAISE EXCEPTION TYPE zcx_bc_sms_send
        EXPORTING
          textid     = zcx_bc_sms_send=>sms_vendor_returned_error
          error_text = |{ CONV string( ls_output-smsmulti_senders_result-status_code ) } { ls_output-smsmulti_senders_result-status_text }|.
    ENDIF.

  ENDMETHOD.

  METHOD format_message.

    " Formatla """"""""""""""""""""""""""""""""""""""""""""""""""""""
    " Aşağıdaki formatlama kuralları,
    " https://mobildev.com/kutuphane.asp?sid=82 adresinden alındı.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    zcl_bc_text_toolkit=>replace_turkish_characters( CHANGING cv_text = gs_state-message ).

    zcl_bc_text_toolkit=>remove_non_alphanum_chars(
      EXPORTING iv_valid_chars = c_valid_chars
      CHANGING   cv_text = gs_state-message
    ).

    " Karakter sınırı """""""""""""""""""""""""""""""""""""""""""""""

    IF strlen( gs_state-message ) GT c_char_limit.
      IF gs_state-options-tolerate_text_truncate EQ abap_true.
        gs_state-message = gs_state-message+0(c_char_limit).
      ELSE.
        RAISE EXCEPTION TYPE zcx_bc_sms_send
          EXPORTING
            textid = zcx_bc_sms_send=>body_too_long.
      ENDIF.
    ENDIF.

    " Metin kalmadıysa hata """""""""""""""""""""""""""""""""""""""""

    IF gs_state-message IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_sms_send
        EXPORTING
          textid = zcx_bc_sms_send=>empty_body.
    ENDIF.

  ENDMETHOD.

  METHOD format_recipients.

    " Hazırlık """"""""""""""""""""""""""""""""""""""""""""""""""""""

    DELETE gs_state-recipients WHERE table_line IS INITIAL.
    remove_duplicate_recipients( ).

    " Formatlama """"""""""""""""""""""""""""""""""""""""""""""""""""
    " Aşağıdaki kurallar, https://mobildev.com/kutuphane.asp?sid=82
    " adresinden alınmıştır. Çağırılacak servis içerisinde, sadece
    " Türkiye numaralarına SMS gönderilebilmektedir. Yurtdışı
    " SMS için başka servis de var, ama işbu sınıf ilk yazıldığında
    " sadece Türkiye'ye SMS gönderileceğinden yurtdışı işine
    " girmedik.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT gs_state-recipients ASSIGNING FIELD-SYMBOL(<lv_recipient>).

      TRY.

          CONDENSE <lv_recipient>.


          IF ( <lv_recipient>+0(3) EQ '+90' OR
               <lv_recipient>+0(3) EQ '090'
             ) AND strlen( <lv_recipient> ) EQ 13.

            SHIFT <lv_recipient> LEFT BY 3 PLACES.
            CONTINUE.

          ELSEIF <lv_recipient>+0(2) EQ '90' AND
                 strlen( <lv_recipient> ) EQ 12.
            SHIFT <lv_recipient> LEFT BY 2 PLACES.
            CONTINUE.
          ELSEIF <lv_recipient>+0(4) EQ '0090' AND
                 strlen( <lv_recipient> ) EQ 14.
            SHIFT <lv_recipient> LEFT BY 4 PLACES.
            CONTINUE.
          ELSEIF <lv_recipient>+0(2) EQ '05' AND
                 strlen( <lv_recipient> ) EQ 11.
            SHIFT <lv_recipient> LEFT BY 1 PLACES.
            CONTINUE.
          ELSEIF <lv_recipient>+0(1) EQ '5' AND
                 strlen( <lv_recipient> ) EQ 10.
            CONTINUE.
          ELSE.

            RAISE EXCEPTION TYPE zcx_bc_phone_number
              EXPORTING
                textid       = zcx_bc_phone_number=>unsupported_format
                phone_number = <lv_recipient>.

          ENDIF.

        CATCH cx_root INTO DATA(lo_diaper).
          IF gs_state-options-tolerate_phone_number_error EQ abap_true.
            DELETE gs_state-recipients.
            CONTINUE.
          ELSE.
            RAISE EXCEPTION TYPE zcx_bc_phone_number
              EXPORTING
                previous     = lo_diaper
                phone_number = <lv_recipient>.
          ENDIF.
      ENDTRY.

    ENDLOOP.

    " Final """""""""""""""""""""""""""""""""""""""""""""""""""""""""

    remove_duplicate_recipients( ).

    IF gs_state-recipients IS INITIAL AND
       gs_state-options-tolerate_no_recipient EQ abap_false.

      RAISE EXCEPTION TYPE zcx_bc_sms_send
        EXPORTING
          textid = zcx_bc_sms_send=>no_recipient.

    ENDIF.

  ENDMETHOD.

  METHOD read_config.

    CHECK gs_config IS INITIAL.

    DATA(lo_par) = NEW zcl_bc_par_master(
      it_name_rng = VALUE #( (
        sign   = zcl_bc_ddic_toolkit=>c_sign_i
        option = zcl_bc_ddic_toolkit=>c_option_cp
        low    = c_parval-pattern
      ) )
    ).

    gs_config-clnt = lo_par->get_val( c_parval-client ).
    gs_config-orig = lo_par->get_val( c_parval-originator ).
    gs_config-pass = lo_par->get_val( c_parval-password ).
    gs_config-port = lo_par->get_val( c_parval-port ).
    gs_config-user = lo_par->get_val( c_parval-user ).

  ENDMETHOD.

  METHOD remove_duplicate_recipients.
    SORT gs_state-recipients.
    DELETE ADJACENT DUPLICATES FROM gs_state-recipients.
  ENDMETHOD.

  METHOD zif_bc_sms_vendor~send_sms.

    TRY.

        CLEAR gs_state.
        gs_state-message = iv_message.
        gs_state-recipients = it_recipients.

        read_config( ).
        format_recipients( ).
        format_message( ).
        call_sms_service( ).

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_sms_send
          EXPORTING
            textid   = zcx_bc_sms_send=>vendor_class_error
            previous = lo_diaper
            clsname  = CONV #( cl_abap_classdescr=>get_class_name( me ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.