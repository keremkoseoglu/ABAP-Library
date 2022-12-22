CLASS zcl_sd_delivery_note DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      convert_weight_for_output
        CHANGING
          !cv_brgew TYPE lips-brgew
          !cv_ntgew TYPE lips-ntgew
          !cv_gewei TYPE lips-gewei.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_meins,
        kg TYPE meins VALUE 'KG',
      END OF c_meins.

ENDCLASS.



CLASS zcl_sd_delivery_note IMPLEMENTATION.

  METHOD convert_weight_for_output.

    DATA:
      lv_brgew TYPE lips-brgew,
      lv_ntgew TYPE lips-ntgew.

    CHECK
      cv_gewei IS NOT INITIAL AND
      cv_gewei NE c_meins-kg.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = cv_brgew
        unit_in              = cv_gewei
        unit_out             = c_meins-kg
      IMPORTING
        output               = lv_brgew
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10
        ##FM_SUBRC_OK.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = cv_ntgew
        unit_in              = cv_gewei
        unit_out             = c_meins-kg
      IMPORTING
        output               = lv_ntgew
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10
        ##FM_SUBRC_OK.

    cv_brgew = lv_brgew.
    cv_ntgew = lv_ntgew.
    cv_gewei = c_meins-kg.

  ENDMETHOD.

ENDCLASS.