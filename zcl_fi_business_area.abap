CLASS zcl_fi_business_area DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    class-methods:
      GET_TGSBT
        importing
          !IV_GSBER type GSBER
          !IV_SPRAS type SPRAS default sy-langu
        returning
          value(RS_TGSBT) type TGSBT .

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      tt_tgsbt TYPE HASHED TABLE OF tgsbt WITH UNIQUE KEY primary_key COMPONENTS spras gsber .

    class-data GT_TGSBT type TT_TGSBT .

ENDCLASS.



CLASS zcl_fi_business_area IMPLEMENTATION.

  METHOD get_tgsbt.

    IF gt_tgsbt[] IS INITIAL.
      SELECT * INTO TABLE gt_tgsbt FROM tgsbt.
      IF sy-subrc <> 0.
        INSERT INITIAL LINE INTO TABLE gt_tgsbt.
      ENDIF.
    ENDIF.

    ASSIGN gt_tgsbt[ KEY primary_key COMPONENTS
                     spras = iv_spras
                     gsber = iv_gsber ] TO FIELD-SYMBOL(<ls_tgsbt>).
    IF sy-subrc = 0.
      rs_tgsbt = <ls_tgsbt>.
    ENDIF.
  ENDMETHOD.

ENDCLASS.