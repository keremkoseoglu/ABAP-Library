CLASS zcl_sd_distribution_channel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    class-methods GET_vtext
      importing
        !IV_SPRAS type SPRAS default sy-langu
        !IV_VTWEG type VTWEG
      returning
        value(Rv_vtext) type tvtwt-vtext .

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      tt_tvtwt TYPE HASHED TABLE OF tvtwt WITH UNIQUE KEY primary_key COMPONENTS spras vtweg .

    class-data:
      GT_TVTWT type TT_TVTWT .

ENDCLASS.



CLASS zcl_sd_distribution_channel IMPLEMENTATION.

  METHOD get_vtext.

    IF gt_tvtwt[] IS INITIAL.
      SELECT * INTO TABLE gt_tvtwt FROM tvtwt.
      IF sy-subrc ne 0.
        INSERT INITIAL LINE INTO TABLE gt_tvtwt.
      ENDIF.
    ENDIF.

    ASSIGN gt_tvtwt[ KEY primary_key COMPONENTS
      spras = iv_spras
      vtweg = iv_vtweg
    ] TO FIELD-SYMBOL(<ls_tvtwt>).

    check sy-subrc eq 0.

    rv_vtext = <ls_tvtwt>-vtext.

  ENDMETHOD.

ENDCLASS.