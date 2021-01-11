CLASS zcl_bc_http_connection_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF check_input_dict,
             urls TYPE ssc_t_doc_search_url,
             rfcs TYPE rfcdest_tab,
           END OF check_input_dict.

    TYPES check_output_list TYPE STANDARD TABLE OF zbcs_http_check_output WITH EMPTY KEY.

    METHODS check_connections
      IMPORTING input         TYPE check_input_dict
      RETURNING VALUE(output) TYPE check_output_list.

    METHODS send_errors_via_email
      IMPORTING !distribution_lists TYPE zcl_bc_mail_facade=>tt_dlist
      RAISING   zcx_bc_mail_send.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF state_dict,
             output TYPE check_output_list,
           END OF state_dict.

    DATA state TYPE state_dict.

    METHODS check_url IMPORTING !url TYPE zbcs_http_check_output-url.
    METHODS check_rfc_destination IMPORTING !rfc_destination TYPE zbcs_http_check_output-rfc_destination.

    METHODS check_http_client
      IMPORTING
        !client TYPE REF TO if_http_client
        !key    TYPE zbcs_http_check_output_key.

    METHODS check_sysubrc RAISING zcx_bc_http.

    METHODS connection_error
      IMPORTING !key   TYPE zbcs_http_check_output_key
                !error TYPE REF TO cx_root.

    METHODS connection_success IMPORTING !key TYPE zbcs_http_check_output_key.

    METHODS key_to_uri
      IMPORTING !key       TYPE zbcs_http_check_output_key
      RETURNING VALUE(uri) TYPE string.
ENDCLASS.



CLASS zcl_bc_http_connection_check IMPLEMENTATION.
  METHOD check_connections.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ana yordam: İletilen Link'lere bağlanmayı dener,
    " bağlanamıyorsa hata üretir
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->state = VALUE #( ).

    LOOP AT input-urls ASSIGNING FIELD-SYMBOL(<url>).
      check_url( <url> ).
    ENDLOOP.

    LOOP AT input-rfcs ASSIGNING FIELD-SYMBOL(<rfc>).
      check_rfc_destination( <rfc> ).
    ENDLOOP.

    output = me->state-output.
  ENDMETHOD.


  METHOD check_url.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " URL bağlantı testi
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(key) = VALUE zbcs_http_check_output_key( url = url ).

        cl_http_client=>create_by_url(
          EXPORTING  url                = CONV #( key-url )
          IMPORTING  client             = DATA(client)
          EXCEPTIONS argument_not_found = 1
                     plugin_not_active  = 2
                     internal_error     = 3
                     OTHERS             = 4 ).

        check_sysubrc( ).

        check_http_client( key    = key
                           client = client ).

      CATCH cx_root INTO DATA(diaper).
        connection_error( key   = key
                          error = diaper ).
    ENDTRY.
  ENDMETHOD.


  METHOD check_rfc_destination.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " RFC Destination bağlantı testi
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(key) = VALUE zbcs_http_check_output_key( rfc_destination = rfc_destination ).

        cl_http_client=>create_by_destination(
          EXPORTING destination               = rfc_destination
          IMPORTING  client                   = DATA(client)
          EXCEPTIONS argument_not_found       = 1
                     destination_not_found    = 2
                     destination_no_authority = 3
                     plugin_not_active        = 4
                     internal_error           = 5
                     OTHERS                   = 6 ).

        check_sysubrc( ).

        check_http_client( key    = key
                           client = client ).

      CATCH cx_root INTO DATA(diaper).
        connection_error( key   = key
                          error = diaper ).
    ENDTRY.
  ENDMETHOD.


  METHOD check_http_client.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " URL veya RFC Destination üzerinden oluşmuş HTTP Client ile
    " bağlantı testi. Gerçek test burada yapılıyor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        client->request->set_method( if_http_request=>co_request_method_get ).

        client->send( EXCEPTIONS http_communication_failure = 1
                                 http_invalid_state         = 2
                                 http_processing_failed     = 3
                                 http_invalid_timeout       = 4
                                 OTHERS                     = 5 ).
        check_sysubrc( ).

        client->receive( EXCEPTIONS http_communication_failure = 1
                                    http_invalid_state         = 2
                                    http_processing_failed     = 3
                                    OTHERS                     = 4 ).
        check_sysubrc( ).
        connection_success( key ).

      CATCH cx_root INTO DATA(diaper).
        connection_error( key   = key
                          error = diaper ).
    ENDTRY.
  ENDMETHOD.


  METHOD check_sysubrc.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " SY-SUBRC kontrolü, 0 değilse hata
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK sy-subrc <> 0.

    RAISE EXCEPTION TYPE zcx_bc_http
      EXPORTING
        textid = zcx_bc_http=>connection_error.
  ENDMETHOD.


  METHOD connection_error.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Bağlantı hatası olduğunda, bunu hafızaya alır.
    " Hafızadaki bu hatalar; ileride ALV ile ekranda gösterilebilir
    " veya E-Posta olarak atılabilir.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(output) = CORRESPONDING zbcs_http_check_output( key ).
    output-success = abap_false.
    output-error_message = error->get_text( ).
    APPEND output TO me->state-output.
  ENDMETHOD.


  METHOD connection_success.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Bağlantı başarılı ise, bunu hafızaya alır.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(output) = CORRESPONDING zbcs_http_check_output( key ).
    output-success = abap_true.
    APPEND output TO me->state-output.
  ENDMETHOD.


  METHOD send_errors_via_email.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Talep üzerine, bağlantı hatalarını E-Posta ile ilgili dağıtım
    " listesine gönderir.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(body) = VALUE bcsy_text(
        FOR _output IN me->state-output
        WHERE ( success = abap_false )
        ( line = |{ key_to_uri( CORRESPONDING #( _output ) ) }| &&
                 | - { _output-error_message }| ) ).

    IF body IS INITIAL.
      RETURN.
    ENDIF.

    zcl_bc_mail_facade=>send_email(
        it_dlist   = distribution_lists
        iv_subject = |{ sy-sysid } { TEXT-946 }|
        it_body    = body ).
  ENDMETHOD.


  METHOD key_to_uri.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Hata mailinde kullanmak üzere, URL veya RFC Destination
    " değerini ID olarak döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    uri = COND #( WHEN key-rfc_destination IS NOT INITIAL
                  THEN key-rfc_destination
                  ELSE key-url ).
  ENDMETHOD.
ENDCLASS.