CLASS zcl_sd_settlement_mgmt_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING wbeln         TYPE wbeln_ag
      RETURNING VALUE(result) TYPE REF TO zcl_sd_settlement_mgmt_doc
      RAISING   zcx_sd_settlement_mgmt_doc.

    CLASS-METHODS get_instance_await
      IMPORTING wbeln         TYPE wbeln_ag
                max_wait_secs TYPE i DEFAULT 300
      RETURNING VALUE(result) TYPE REF TO zcl_sd_settlement_mgmt_doc
      RAISING   zcx_sd_settlement_mgmt_doc.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
            wbeln TYPE wbeln_ag,
            obj TYPE REF TO zcl_sd_settlement_mgmt_doc,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict WITH UNIQUE KEY primary_key COMPONENTS wbeln.

    DATA wbeln TYPE wbeln_ag.

    CLASS-DATA multitons TYPE multiton_set.

    CLASS-METHODS does_doc_exist
      IMPORTING wbeln         TYPE wbeln_ag
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS ensure_doc_exists
      IMPORTING wbeln TYPE wbeln_ag
      RAISING   zcx_sd_settlement_mgmt_doc.

    METHODS constructor
      IMPORTING wbeln TYPE wbeln_ag
      RAISING   zcx_sd_settlement_mgmt_doc.
ENDCLASS.


CLASS zcl_sd_settlement_mgmt_doc IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_sd_settlement_mgmt_doc=>multitons[ KEY primary_key COMPONENTS wbeln = wbeln ] ).
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( wbeln = wbeln
                        obj   = NEW #( wbeln ) )
               INTO TABLE zcl_sd_settlement_mgmt_doc=>multitons REFERENCE INTO mt.
    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_instance_await.
    DATA(waited_secs) = 0.

    WHILE waited_secs <= max_wait_secs.
      IF does_doc_exist( wbeln ).
        result = get_instance( wbeln ).
        RETURN.
      ENDIF.

      WAIT UP TO 1 SECONDS.
      waited_secs += 1.
    ENDWHILE.

    RAISE EXCEPTION NEW zcx_sd_settlement_mgmt_doc( textid = zcx_sd_settlement_mgmt_doc=>doc_await_expire
                                                    wbeln  = wbeln ).
  ENDMETHOD.

  METHOD does_doc_exist.
    SELECT SINGLE FROM wbrk FIELDS @abap_true
         WHERE  wbeln = @wbeln
         INTO   @result.
  ENDMETHOD.

  METHOD ensure_doc_exists.
    CHECK NOT does_doc_exist( wbeln ).

    RAISE EXCEPTION NEW zcx_sd_settlement_mgmt_doc( textid = zcx_sd_settlement_mgmt_doc=>doc_not_found
                                                    wbeln  = wbeln ).
  ENDMETHOD.

  METHOD constructor.
    ensure_doc_exists( wbeln ).
    me->wbeln = wbeln.
  ENDMETHOD.
ENDCLASS.