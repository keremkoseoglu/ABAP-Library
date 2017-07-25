CLASS zcl_sd_division DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      get_division_txt
        IMPORTING
          !iv_spart       TYPE tspat-spart
          !iv_spras       TYPE tspat-spras DEFAULT sy-langu
        RETURNING
          VALUE(rv_vtext) TYPE tspat-vtext.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES tt_tspat TYPE HASHED TABLE OF tspat WITH UNIQUE KEY primary_key COMPONENTS spras spart.

    CLASS-DATA gt_tspat TYPE tt_tspat.

ENDCLASS.



CLASS zcl_sd_division IMPLEMENTATION.

  METHOD get_division_txt.

    IF gt_tspat IS INITIAL.
      SELECT * INTO TABLE gt_tspat FROM tspat.
    ENDIF.

    TRY.
        rv_vtext = gt_tspat[ KEY primary_key COMPONENTS
          spras = iv_spras
          spart = iv_spart
        ]-vtext.

      CATCH cx_sy_itab_line_not_found ##no_Handler .
    ENDTRY.

  ENDMETHOD.

ENDCLASS.