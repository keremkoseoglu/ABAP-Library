CLASS zcl_sd_invoice_edoc_analyser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF analysis_result_dict,
             fit_doc_found TYPE abap_bool,
             dispatched    TYPE abap_bool,
             cancelled     TYPE abap_bool,
             invoice_id    TYPE /fite/inv_1_de038,
             invoice_uuid  TYPE /fite/arc_1_de011,
             system_id     TYPE /fite/arc_1_de249,
           END OF analysis_result_dict.

    METHODS constructor IMPORTING !vbeln_vf TYPE vbeln_vf.
    METHODS analyse_earc RETURNING VALUE(result) TYPE analysis_result_dict.
    METHODS analyse_einv RETURNING VALUE(result) TYPE analysis_result_dict.
    METHODS analyse_e_   RETURNING VALUE(result) TYPE analysis_result_dict.
    METHODS is_earc_dispatched RETURNING VALUE(result) TYPE abap_bool.
    METHODS is_einv_dispatched RETURNING VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES system_id_range TYPE RANGE OF /fite/arc_1_t004-system_id.
    TYPES einv_status_set TYPE HASHED TABLE OF /fite/inv_1_de002 WITH UNIQUE KEY primary_key COMPONENTS table_line.
    TYPES msg_text_range TYPE RANGE OF /fite/arc_1_t001-msg_text.

    TYPES: BEGIN OF earc_status_dict,
             status       TYPE zsdt_earc_distat-status,
             msg_text_rng TYPE msg_text_range,
           END OF earc_status_dict,

           earc_status_list TYPE STANDARD TABLE OF earc_status_dict WITH EMPTY KEY.

    CLASS-DATA: dispatched_earc_statuses      TYPE earc_status_list,
                dispatched_earc_statuses_read TYPE abap_bool.

    CLASS-DATA: dispatched_einv_statuses      TYPE einv_status_set,
                dispatched_einv_statuses_read TYPE abap_bool.

    CLASS-METHODS read_dispatched_earc_statuses.
    CLASS-METHODS read_dispatched_einv_statuses.

    CONSTANTS: BEGIN OF inv_module,
                 sd TYPE /fite/arc_1_t001-inv_module VALUE 'SD',
               END OF inv_module.

    DATA vbeln_vf TYPE vbeln_vf.
ENDCLASS.



CLASS zcl_sd_invoice_edoc_analyser IMPLEMENTATION.
  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Nesneye ilk erişim
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->vbeln_vf = vbeln_vf.
  ENDMETHOD.


  METHOD analyse_earc.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Arşiv analizi
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA system_id_rng TYPE system_id_range.

    " Hazırlık """"""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT @zcl_bc_ddic_toolkit=>c_sign_i    AS sign,
           @zcl_bc_ddic_toolkit=>c_option_eq AS option,
           _a4~system_id AS low
           FROM vbrk
                INNER JOIN /fite/arc_1_t004 AS _a4 ON _a4~bukrs = vbrk~bukrs
           WHERE vbrk~vbeln = @me->vbeln_vf
           INTO CORRESPONDING FIELDS OF TABLE @system_id_rng ##too_many_itab_fields.

    IF system_id_rng IS INITIAL.
      RETURN.
    ENDIF.

    " E-Arşiv gönderildi mi? """"""""""""""""""""""""""""""""""""""""
    SELECT invoice_id, invoice_uuid, status, msg_text, system_id
           FROM /fite/arc_1_t001
           WHERE system_id      IN @system_id_rng AND
                 invoice_number = @me->vbeln_vf   AND
                 inv_module     = @me->inv_module-sd
           ORDER BY cre_date DESCENDING,
                    cre_time DESCENDING
           INTO TABLE @DATA(statu)
           UP TO 1 ROWS.

    IF statu IS INITIAL.
      RETURN.
    ENDIF.

    read_dispatched_earc_statuses( ).
    ASSIGN statu[ 1 ] TO FIELD-SYMBOL(<statu>).
    DATA(dispatched) = abap_false.

    LOOP AT me->dispatched_earc_statuses ASSIGNING FIELD-SYMBOL(<des>)
                                         WHERE status = <statu>-status.
      CHECK <statu>-msg_text IN <des>-msg_text_rng.
      dispatched = abap_true.
      EXIT.
    ENDLOOP.

    " E-Arşiv iptal edildi mi? """"""""""""""""""""""""""""""""""""""
    SELECT SINGLE mandt FROM /fite/arc_1_t024
           WHERE system_id      IN @system_id_rng AND
                 invoice_number =  @me->vbeln_vf AND
                 cancel_flag    =  @abap_true
           INTO @sy-mandt ##WRITE_OK.                   "#EC CI_NOORDER

    DATA(cancelled) = xsdbool( sy-subrc = 0 ).

    " Sonucu döndür """""""""""""""""""""""""""""""""""""""""""""""""
    result = VALUE #( fit_doc_found = abap_true
                      invoice_id    = <statu>-invoice_id
                      invoice_uuid  = <statu>-invoice_uuid
                      cancelled     = cancelled
                      dispatched    = dispatched
                      system_id     = <statu>-system_id ).
  ENDMETHOD.


  METHOD analyse_einv.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Fatura analizi
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA system_id_rng TYPE system_id_range.

    SELECT @zcl_bc_ddic_toolkit=>c_sign_i    AS sign,
           @zcl_bc_ddic_toolkit=>c_option_eq AS option,
           _a4~system_id AS low
           FROM vbrk
                INNER JOIN /fite/inv_1_t004 AS _a4 ON _a4~bukrs = vbrk~bukrs
           WHERE vbrk~vbeln = @me->vbeln_vf
           INTO CORRESPONDING FIELDS OF TABLE @system_id_rng ##too_many_itab_fields.

    IF system_id_rng IS INITIAL.
      RETURN.
    ENDIF.

    SELECT invoice_id, invoice_uuid, status, system_id
           FROM /fite/inv_1_t001
           WHERE system_id      IN @system_id_rng AND
                 invoice_number =  @me->vbeln_vf AND
                 inv_module     =  @me->inv_module-sd
           ORDER BY cre_date DESCENDING,
                    cre_time DESCENDING
           INTO TABLE @DATA(statu)
           UP TO 1 ROWS.

    IF statu IS INITIAL.
      RETURN.
    ENDIF.

    read_dispatched_einv_statuses( ).
    ASSIGN statu[ 1 ] TO FIELD-SYMBOL(<ls_statu>).

    ##FIXME.
    " Bir ara,
    " E-Fatura'nın iptal olup olmadığını anlayıp, aşağıdaki yapıya
    " CANCELLED diye eklememiz gerekiyor.

    result = VALUE #( fit_doc_found = abap_true
                      invoice_id    = <ls_statu>-invoice_id
                      invoice_uuid  = <ls_statu>-invoice_uuid
                      system_id     = <ls_Statu>-system_id
                      dispatched    = xsdbool( line_exists( me->dispatched_einv_statuses[
                                                            KEY primary_key COMPONENTS
                                                            table_line = <ls_statu>-status ] ) ) ).
  ENDMETHOD.


  METHOD analyse_e_.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Arşiv veya E-Fatura analizi (hangisi varsa)
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    result = analyse_earc( ).

    IF result-fit_doc_found = abap_true.
      RETURN.
    ENDIF.

    result = analyse_einv( ).
  ENDMETHOD.


  METHOD is_earc_dispatched.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Arşiv gönderildi mi
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    result = analyse_earc( )-dispatched.
  ENDMETHOD.


  METHOD is_einv_dispatched.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Fatura gönderildi mi
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    result = analyse_einv( )-dispatched.
  ENDMETHOD.


  METHOD read_dispatched_earc_statuses.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Gönderilmiş E-Arşiv faturalarına ait statüleri belirler
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK zcl_sd_invoice_edoc_analyser=>dispatched_earc_statuses_read = abap_false.
    SELECT * FROM zsdt_earc_distat INTO TABLE @DATA(distats). "#EC CI_NOWHERE

    LOOP AT distats INTO DATA(_distat)
                    GROUP BY ( status = _distat-status )
                    ASSIGNING FIELD-SYMBOL(<status>).

      DATA(earc_status) =
        VALUE earc_status_dict(
          status       = <status>-status
          msg_text_rng = VALUE #( FOR _pattern IN GROUP <status>
                                  ( sign   = ycl_addict_toolkit=>sign-include
                                    option = COND #( WHEN _pattern-msg_text_pattern CS '*'
                                                     THEN ycl_addict_toolkit=>option-cp
                                                     ELSE ycl_addict_toolkit=>option-eq )
                                    low    = _pattern-msg_text_pattern ) ) ).

      DELETE earc_status-msg_text_rng WHERE low IS INITIAL.
      APPEND earc_status TO zcl_sd_invoice_edoc_analyser=>dispatched_earc_statuses.
    ENDLOOP.

    zcl_sd_invoice_edoc_analyser=>dispatched_earc_statuses_read = abap_true.
  ENDMETHOD.


  METHOD read_dispatched_einv_statuses.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Gönderilmiş E-Faturalara ait statüleri belirler
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK zcl_sd_invoice_edoc_analyser=>dispatched_einv_statuses_read = abap_false.
    SELECT status FROM zsdt_einv_distat INTO TABLE @zcl_sd_invoice_edoc_analyser=>dispatched_einv_statuses. "#EC CI_NOWHERE
    zcl_sd_invoice_edoc_analyser=>dispatched_einv_statuses_read = abap_true.
  ENDMETHOD.
ENDCLASS.