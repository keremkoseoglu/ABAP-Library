CLASS zcl_bc_itab_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_itab_has_fields
      IMPORTING
        !iv_tabname TYPE tabname OPTIONAL
        !ir_tab     TYPE REF TO data
        !it_fld     TYPE tc_field_tab
      RAISING
        zcx_bc_data_format .

    CLASS-METHODS does_itab_have_any_value
      IMPORTING
        !ir_data_tab  TYPE REF TO data
        !ir_value_tab TYPE REF TO data
        !iv_fnam      TYPE fieldname
      RETURNING
        VALUE(rv_has) TYPE abap_bool .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_itab_toolkit IMPLEMENTATION.

  METHOD check_itab_has_fields.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    CHECK it_fld[] IS NOT INITIAL.

    ASSIGN ir_tab->* TO <lt_tab>.
    CHECK <lt_tab> IS ASSIGNED.

    READ TABLE <lt_tab> INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_tab>).
    CHECK sy-subrc EQ 0.

    LOOP AT it_fld ASSIGNING FIELD-SYMBOL(<lv_fld>).
      ASSIGN COMPONENT <lv_fld> OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_tab>).
      CHECK NOT ( sy-subrc EQ 0 AND <lv_tab> IS ASSIGNED ).

      RAISE EXCEPTION TYPE zcx_bc_data_format
        EXPORTING
          fieldname = <lv_fld>
          tabname   = iv_tabname.
    ENDLOOP.

  ENDMETHOD.

  METHOD does_itab_have_any_value.

    FIELD-SYMBOLS:
        <lt_data>  TYPE STANDARD TABLE,
        <lt_value> TYPE STANDARD TABLE.

    ASSIGN ir_data_tab->* TO <lt_data>.
    CHECK <lt_data> IS ASSIGNED AND <lt_data>[] IS NOT INITIAL.

    ASSIGN ir_value_tab->* TO <lt_value>.
    CHECK <lt_value> IS ASSIGNED AND <lt_value>[] IS NOT INITIAL.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).

      ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_data>).
      CHECK <lv_data> IS ASSIGNED.

      LOOP AT <lt_value> ASSIGNING FIELD-SYMBOL(<ls_value>).
        CHECK <lv_data> EQ <ls_value>.
        rv_has = abap_true.
        RETURN.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.