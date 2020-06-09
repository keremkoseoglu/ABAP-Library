CLASS zcl_fi_document_type DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  class-methods GET_ltext
    importing
      !IV_SPRAS type SPRAS default sy-langu
      !IV_BLART type BLART
    returning
      value(Rv_ltext) type T003T-ltext .

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      tt_t003t TYPE HASHED TABLE OF t003t WITH UNIQUE KEY primary_key COMPONENTS spras blart .

    class-data GT_T003T type TT_T003T .

ENDCLASS.



CLASS zcl_fi_document_type IMPLEMENTATION.

  METHOD get_ltext.
    IF gt_t003t[] IS INITIAL.
      SELECT * INTO TABLE gt_t003t FROM t003t.
      IF sy-subrc ne 0.
        INSERT INITIAL LINE INTO TABLE gt_t003t.
      ENDIF.
    ENDIF.

    ASSIGN gt_t003t[ KEY primary_key COMPONENTS
      spras = iv_spras
      blart = iv_blart
    ] TO FIELD-SYMBOL(<ls_t003t>).

    check sy-subrc eq 0.
    rv_ltext = <ls_t003t>-ltext.

  ENDMETHOD.

ENDCLASS.