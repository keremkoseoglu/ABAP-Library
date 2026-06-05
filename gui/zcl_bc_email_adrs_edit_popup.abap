CLASS zcl_bc_email_adrs_edit_popup DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor IMPORTING email_adrs TYPE REF TO zcl_bc_email_address_set.

    METHODS show        RAISING   RESUMABLE(ycx_addict_email_address).

  PRIVATE SECTION.
    DATA email_adrs TYPE REF TO zcl_bc_email_address_set.
ENDCLASS.


CLASS zcl_bc_email_adrs_edit_popup IMPLEMENTATION.
  METHOD constructor.
    me->email_adrs = email_adrs.
  ENDMETHOD.

  METHOD show.
    DATA(popup_texts) = me->email_adrs->conv_to_txlines( ).
    DATA(popup_texts_before) = popup_texts.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING im_title = TEXT-tit
      CHANGING  ch_text  = popup_texts.

    CHECK popup_texts <> popup_texts_before.

    TRY.
        me->email_adrs->set_rows( VALUE #( FOR _pt IN popup_texts
                                           WHERE ( table_line IS NOT INITIAL )
                                           ( _pt ) ) ).

      CATCH BEFORE UNWIND ycx_addict_email_address INTO DATA(adr_error).
        RAISE RESUMABLE EXCEPTION adr_error.
        RESUME.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.