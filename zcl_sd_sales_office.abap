CLASS zcl_sd_sales_office DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    class-methods get_bezei
      importing
        !IV_SPRAS type SPRAS default sy-langu
        !IV_VKBUR type VKBUR
      returning
        value(Rv_bezei) type TVKBT-bezei.

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      tt_tvkbt TYPE HASHED TABLE OF tvkbt WITH UNIQUE KEY primary_key COMPONENTS spras vkbur .

    class-data GT_TVKBT type TT_TVKBT .

ENDCLASS.



CLASS ZCL_SD_SALES_OFFICE IMPLEMENTATION.


  METHOD get_bezei.

    IF gt_tvkbt[] IS INITIAL.
      SELECT * INTO TABLE gt_tvkbt FROM tvkbt.
      IF sy-subrc <> 0.
        INSERT INITIAL LINE INTO TABLE gt_tvkbt.
      ENDIF.
    ENDIF.

    ASSIGN gt_tvkbt[ KEY primary_key COMPONENTS
      spras = iv_spras
      vkbur = iv_vkbur
    ] TO FIELD-SYMBOL(<ls_tvkbt>).

    check sy-subrc eq 0.

    rv_bezei = <ls_tvkbt>-bezei.

  ENDMETHOD.
ENDCLASS.