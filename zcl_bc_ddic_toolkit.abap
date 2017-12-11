class ZCL_BC_DDIC_TOOLKIT definition
  public
  final
  create public .

public section.

  types TT_TABFLD type ZCL_BC_ABAP_TABLE=>TT_TABFLD .

  constants C_FIELDNAME_HIGH type FIELDNAME value 'HIGH' ##NO_TEXT.
  constants C_FIELDNAME_LOW type FIELDNAME value 'LOW' ##NO_TEXT.
  constants C_FIELDNAME_OPTION type FIELDNAME value 'OPTION' ##NO_TEXT.
  constants C_FIELDNAME_SIGN type FIELDNAME value 'SIGN' ##NO_TEXT.
  constants C_OPTION_BT type DDOPTION value 'BT' ##NO_TEXT.
  constants C_OPTION_CP type DDOPTION value 'CP' ##NO_TEXT.
  constants C_OPTION_EQ type DDOPTION value 'EQ' ##NO_TEXT.
  constants C_OPTION_GE type DDOPTION value 'GE' ##NO_TEXT.
  constants C_OPTION_LE type DDOPTION value 'LE' ##NO_TEXT.
  constants C_ROLLNAME_OPTION type ROLLNAME value 'DDOPTION' ##NO_TEXT.
  constants C_ROLLNAME_SIGN type ROLLNAME value 'DDSIGN' ##NO_TEXT.
  constants C_SIGN_E type DDSIGN value 'E' ##NO_TEXT.
  constants C_SIGN_I type DDSIGN value 'I' ##NO_TEXT.

  class-methods ALPHA_INPUT
    importing
      !IV_INPUT type ANY
    exporting
      !EV_OUTPUT type ANY .
  class-methods ALPHA_INPUT_CHANGE
    changing
      value(C_FIELD) type ANY .
  class-methods ALPHA_OUTPUT
    importing
      !IV_INPUT type ANY
    exporting
      value(EV_OUTPUT) type ANY .

    CLASS-METHODS char_to_dec2
      IMPORTING
        !iv_char      TYPE char20
      RETURNING
        VALUE(rv_dec) TYPE dmbtr .

  class-methods CHAR_TO_DEC3
    importing
      !IV_CHAR type CHAR20
    returning
      value(RV_DEC) type MENGE_D .

    CLASS-METHODS conv_10digitdate_to_8digitdate
      IMPORTING
        !iv_date       TYPE any
      RETURNING
        VALUE(rv_date) TYPE datum .

  class-methods CUNIT_INPUT
    importing
      !IV_MEINS type MEINS
    returning
      value(RV_MEINS) type MEINS .
  class-methods CUNIT_OUTPUT
    importing
      !IV_MEINS type MEINS
    returning
      value(RV_MEINS) type MEINS .
  class-methods DECIMAL_TO_CHAR_FOR_VBAP_FIELD
    importing
      !IV_DECIMAL type ZSDD_VBAP_MENGE
    changing
      !CV_CHAR type ANY .
  class-methods ENSURE_ALL_WA_FIELDS_FULL
    importing
      !IR_WA type ref to DATA
      !IV_STRUCTURE type TABNAME
      !IV_WA_VARIABLE_NAME type STRING optional
      !IV_FORGIVE_MISSING_STR_FLD type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_BC_FIELD_VALUE
      ZCX_BC_METHOD_PARAMETER
      ZCX_BC_VARIABLE .
  class-methods FLOAT_TO_CHAR
    importing
      !IV_FLOAT type ATFLV
    returning
      value(RV_CHAR) type ATNAM .
  class-methods GET_ROLLNAME_PAIRS
    importing
      !IV_TABNAME1 type TABNAME
      !IV_TABNAME2 type TABNAME
    returning
      value(RT_RET) type ZBCTT_ROLLNAME_PAIR
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods GET_TABLE_FIELDS
    importing
      !IV_TABNAME type TABNAME
    returning
      value(RT_FLD) type TT_TABFLD
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods MATNR_INPUT
    importing
      !IV_MATNR type CLIKE
    returning
      value(RV_MATNR) type MATNR .
  class-methods MATNR_OUTPUT
    importing
      !IV_MATNR type MATNR
    returning
      value(RV_MATNR) type MATNR .
  class-methods VBELN_INPUT
    importing
      !IV_VBELN type CLIKE
    returning
      value(RV_VBELN) type VBELN_VL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DDIC_TOOLKIT',
      c_meth_eawff TYPE seocpdname VALUE 'ENSURE_ALL_WA_FIELDS_FULL'.

ENDCLASS.



CLASS ZCL_BC_DDIC_TOOLKIT IMPLEMENTATION.


  METHOD alpha_input.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_output.

  ENDMETHOD.


  METHOD alpha_input_change.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_field
      IMPORTING
        output = c_field.

  ENDMETHOD.


  METHOD alpha_output.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_output.

  ENDMETHOD.

  METHOD char_to_dec2.
    DATA: lv_char TYPE char20.
    TRY.
        lv_char = iv_char.
        SEARCH lv_char FOR ','.
        IF sy-subrc = 0.
          REPLACE '.' WITH '' INTO lv_char.
          CONDENSE lv_char NO-GAPS.
        ELSE.
          REPLACE '.' WITH ',' INTO lv_char.
        ENDIF.
        CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
          EXPORTING
            i_char         = lv_char
          IMPORTING
*           E_FLOAT        =
            e_packed       = rv_dec
          EXCEPTIONS
            invalid_number = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
*   Implement suitable error handling here
        ENDIF.
      CATCH cx_root ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

  METHOD char_to_dec3.

    CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
      EXPORTING
        i_char         = iv_char
      IMPORTING
*       E_FLOAT        =
        e_packed       = rv_dec
      EXCEPTIONS
        invalid_number = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.

  METHOD conv_10digitdate_to_8digitdate.

    DATA : BEGIN OF ls_date,
             day(2),
             month(2),
             year(4),
           END OF ls_date.

    SPLIT iv_date AT '.' INTO ls_date-day ls_date-month ls_date-year.

    IF ls_date-day   IS NOT INITIAL AND
       ls_date-month IS NOT INITIAL AND
       ls_date-year  IS NOT INITIAL.

      alpha_input(
        EXPORTING iv_input  = ls_date-month
        IMPORTING ev_output = ls_date-month
      ).

      alpha_input(
        EXPORTING iv_input  = ls_date-day
        IMPORTING ev_output = ls_date-day
      ).

      rv_date = |{ ls_date-year }{ ls_date-month }{ ls_date-day }| .

    ELSE.

      rv_date = iv_date.
    ENDIF.

  ENDMETHOD.

  METHOD cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = iv_meins
*       LANGUAGE       = SY-LANGU
      IMPORTING
        output         = rv_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD cunit_output.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = iv_meins
*       LANGUAGE       = SY-LANGU
      IMPORTING
*       LONG_TEXT      =
        output         = rv_meins
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      rv_meins = iv_meins.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD decimal_to_char_for_vbap_field.
    DATA : lv_char TYPE char50.
    TRY .
        lv_char = iv_decimal.
        CONDENSE lv_char.
        cv_char = lv_char.
      CATCH cx_sy_conversion_no_number ##NO_HANDLER.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD ensure_all_wa_fields_full.

    " ______________________________
    " Parametre kontrolleri

    IF ir_wa IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_value_initial
          class_name  = c_clsname_me
          method_name = c_meth_eawff
          param_name  = 'IR_WA'.
    ENDIF.

    IF iv_structure IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_value_initial
          class_name  = c_clsname_me
          method_name = c_meth_eawff
          param_name  = 'IV_STRUCTURE'.
    ENDIF.

    " ______________________________
    " DDIC bilgilerini al

    TRY.
        DATA(lo_tab) = zcl_bc_abap_table=>get_instance( iv_structure ).
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING
            textid      = zcx_bc_method_parameter=>param_value_invalid
            previous    = lo_diaper
            class_name  = c_clsname_me
            method_name = c_meth_eawff
            param_name  = 'IV_STRUCTURE'.
    ENDTRY.

    " ______________________________
    " Alanların doluluk kontrolü

    ASSIGN ir_wa->* TO FIELD-SYMBOL(<ls_wa>).

    LOOP AT lo_tab->get_fields( ) ASSIGNING FIELD-SYMBOL(<ls_fld>).

      ASSIGN COMPONENT <ls_fld>-fieldname OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<lv_wa_val>).
      IF sy-subrc NE 0.

        IF iv_forgive_missing_str_fld EQ abap_true.
          CONTINUE.
        ELSE.

          RAISE EXCEPTION TYPE zcx_bc_method_parameter
            EXPORTING
              textid       = zcx_bc_method_parameter=>param_pair_inconsistent
              previous     = NEW zcx_bc_work_area_structure(
                  textid    = zcx_bc_work_area_structure=>field_missing
                  workarea  = CONV #( iv_wa_variable_name )
                  fieldname = <ls_fld>-fieldname
              )
              class_name   = c_clsname_me
              method_name  = c_meth_eawff
              param_name   = 'IR_WA'
              param_name_2 = 'IV_STRUCTURE'.
        ENDIF.

      ENDIF.

      CHECK <lv_wa_val> IS INITIAL.

      RAISE EXCEPTION TYPE zcx_bc_field_value
        EXPORTING
          textid    = zcx_bc_field_value=>missing_value
          fieldname = |{ <ls_fld>-fieldname }|
          ddtext    = zcl_bc_data_element=>get_text_safe( <ls_fld>-rollname ).

    ENDLOOP.

  ENDMETHOD.


  METHOD float_to_char.
    DATA : lv_dec TYPE dec10_2.


    CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
      EXPORTING
        if_float  = iv_float
*       IF_SIGNIFICANT_PLACES       = 15
      IMPORTING
        ef_packed = lv_dec
      EXCEPTIONS
        overflow  = 1.
    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                     DISPLAY LIKE 'E'.
    ENDIF.

    WRITE lv_dec TO rv_char.
    CONDENSE rv_char.

  ENDMETHOD.


  METHOD get_rollname_pairs.

    rt_ret = zcl_bc_abap_table=>get_rollname_pairs(
      iv_tabname1 = iv_tabname1
      iv_tabname2 = iv_tabname2
    ).

  ENDMETHOD.


  METHOD get_table_fields.
    rt_fld = CORRESPONDING #( zcl_bc_abap_table=>get_instance( iv_tabname )->get_fields( ) ).
  ENDMETHOD.


  METHOD matnr_input.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = iv_matnr
      IMPORTING
        output       = rv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2
        ##FM_SUBRC_OK.

  ENDMETHOD.


  METHOD matnr_output.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = iv_matnr
      IMPORTING
        output = rv_matnr.

  ENDMETHOD.


  METHOD VBELN_INPUT.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input        = iv_vbeln
      IMPORTING
        output       = rv_vbeln
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2
        ##FM_SUBRC_OK.

  ENDMETHOD.
ENDCLASS.