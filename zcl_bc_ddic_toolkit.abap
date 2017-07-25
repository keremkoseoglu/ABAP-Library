CLASS zcl_bc_ddic_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_tabfld TYPE zcl_bc_abap_table=>tt_tabfld.

    CONSTANTS:
      c_fieldname_high   TYPE fieldname VALUE 'HIGH',
      c_fieldname_low    TYPE fieldname VALUE 'LOW',
      c_fieldname_option TYPE fieldname VALUE 'OPTION',
      c_fieldname_sign   TYPE fieldname VALUE 'SIGN',

      c_option_bt        TYPE ddoption  VALUE 'BT',
      c_option_cp        TYPE ddoption  VALUE 'CP',
      c_option_eq        TYPE ddoption  VALUE 'EQ',
      c_option_ge        TYPE ddoption  VALUE 'GE',
      c_option_le        TYPE ddoption  VALUE 'LE',

      c_rollname_option  TYPE rollname  VALUE 'DDOPTION',
      c_rollname_sign    TYPE rollname  VALUE 'DDSIGN',

      c_sign_e           TYPE ddsign    VALUE 'E',
      c_sign_i           TYPE ddsign    VALUE 'I'.


    CLASS-METHODS ensure_all_wa_fields_full
      IMPORTING
        !ir_wa                      TYPE REF TO data
        !iv_structure               TYPE tabname
        !iv_wa_variable_name        TYPE string OPTIONAL
        !iv_forgive_missing_str_fld TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_bc_field_value
        zcx_bc_method_parameter
        zcx_bc_variable.

    CLASS-METHODS get_rollname_pairs
      IMPORTING
        !iv_tabname1  TYPE tabname
        !iv_tabname2  TYPE tabname
      RETURNING
        VALUE(rt_ret) TYPE zbctt_rollname_pair
      RAISING
        zcx_bc_table_content .

    CLASS-METHODS get_table_fields
      IMPORTING
        !iv_tabname   TYPE tabname
      RETURNING
        VALUE(rt_fld) TYPE tt_tabfld
      RAISING
        zcx_bc_table_content .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DDIC_TOOLKIT',
      c_meth_eawff TYPE seocpdname VALUE 'ENSURE_ALL_WA_FIELDS_FULL'.

ENDCLASS.



CLASS zcl_bc_ddic_toolkit IMPLEMENTATION.

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


  METHOD get_rollname_pairs.

    rt_ret = zcl_bc_abap_table=>get_rollname_pairs(
      iv_tabname1 = iv_tabname1
      iv_tabname2 = iv_tabname2
    ).

  ENDMETHOD.


  METHOD get_table_fields.
    rt_fld = CORRESPONDING #( zcl_bc_abap_table=>get_instance( iv_tabname )->get_fields( ) ).
  ENDMETHOD.

ENDCLASS.