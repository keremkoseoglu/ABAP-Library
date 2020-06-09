CLASS zcl_bc_text_search_rng_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance_by_rollname
      IMPORTING
        !iv_rollname  TYPE rollname
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_text_search_rng_builder
      RAISING
        zcx_bc_table_content.


    CLASS-METHODS get_instance_by_tab_fld
      IMPORTING
        !iv_tabname   TYPE tabname
        !iv_fldname   TYPE fieldname
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_text_search_rng_builder
      RAISING
        zcx_bc_table_content.

    METHODS get_range
      IMPORTING
        !iv_search_text TYPE clike
      EXPORTING
        er_range        TYPE REF TO data
      RAISING
        zcx_bc_method_parameter
        zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_char_nbsp       TYPE char1      VALUE ` `,
               c_char_plus       TYPE char1      VALUE '+',
               c_char_star       TYPE char1      VALUE '*',
               c_clsname_me      TYPE seoclsname VALUE 'ZCL_BC_TEXT_SEARCH_RNG_BUILDER',
               c_meth_gibtf      TYPE seocpdname VALUE 'GET_INSTANCE_BY_TAB_FLD',
               c_param_gibtf_fld TYPE seocpdname VALUE 'IV_FLDNAME',
               c_tabname_dd03l   TYPE tabname    VALUE 'DD03L'.

    DATA gv_rollname TYPE rollname.

    METHODS constructor
      IMPORTING
        !iv_rollname TYPE rollname
      RAISING
        zcx_bc_table_content.

ENDCLASS.



CLASS zcl_bc_text_search_rng_builder IMPLEMENTATION.


  METHOD constructor.

    DATA(lo_dtel) = CAST zcl_bc_data_element(
      zcl_bc_multiton=>get_obj(
        iv_clsname  = zcl_bc_data_element=>c_clsname_me
        iv_objectid = CONV #( iv_rollname )
      )
    ).

    gv_rollname = iv_rollname.
  ENDMETHOD.


  METHOD get_instance_by_rollname.
    ro_obj = NEW #( iv_rollname ).
  ENDMETHOD.


  METHOD get_instance_by_tab_fld.
    ro_obj = get_instance_by_rollname(
      zcl_bc_abap_table=>get_instance( iv_tabname )->get_rollname_of_field( iv_fldname )
    ).
  ENDMETHOD.


  METHOD get_range.

    FIELD-SYMBOLS: <lt_rng>    TYPE STANDARD TABLE,
                   <lv_option> TYPE ddoption,
                   <lv_sign>   TYPE ddsign.

    CLEAR er_range.

    DATA(lo_dyn_itab) = zcl_bc_dynamic_itab=>get_instance_as_range( gv_rollname ).
    er_range = lo_dyn_itab->get_itab_ref( ).

    ASSIGN er_range->* TO <lt_rng>.
    APPEND INITIAL LINE TO <lt_rng> ASSIGNING FIELD-SYMBOL(<ls_rng>).

    ASSIGN COMPONENT zcl_bc_ddic_toolkit=>c_fieldname_sign OF STRUCTURE <ls_rng> TO <lv_sign>.
    <lv_sign> = zcl_bc_ddic_toolkit=>c_sign_i.

    ASSIGN COMPONENT zcl_bc_ddic_toolkit=>c_fieldname_option OF STRUCTURE <ls_rng> TO <lv_option>.
    <lv_option> = zcl_bc_ddic_toolkit=>c_option_cp.

    ASSIGN COMPONENT zcl_bc_ddic_toolkit=>c_fieldname_low OF STRUCTURE <ls_rng> TO FIELD-SYMBOL(<lv_low>).
    <lv_low> = |{ iv_search_text }|.

    TRANSLATE <lv_low> TO UPPER CASE.
    REPLACE ALL OCCURRENCES OF: ` ` IN <lv_low> WITH c_char_star,
                                'ı' IN <lv_low> WITH c_char_plus,
                                'I' IN <lv_low> WITH c_char_plus,
                                'ğ' IN <lv_low> WITH c_char_plus,
                                'Ğ' IN <lv_low> WITH c_char_plus,
                                'ü' IN <lv_low> WITH c_char_plus,
                                'Ü' IN <lv_low> WITH c_char_plus,
                                'ş' IN <lv_low> WITH c_char_plus,
                                'Ş' IN <lv_low> WITH c_char_plus,
                                'i' IN <lv_low> WITH c_char_plus,
                                'İ' IN <lv_low> WITH c_char_plus,
                                'ö' IN <lv_low> WITH c_char_plus,
                                'Ö' IN <lv_low> WITH c_char_plus,
                                'ç' IN <lv_low> WITH c_char_plus,
                                'Ç' IN <lv_low> WITH c_char_plus.

    SHIFT <lv_low> : RIGHT DELETING TRAILING c_char_star,
                     LEFT  DELETING LEADING  c_char_star,
                     LEFT  DELETING LEADING  space.

    APPEND <ls_rng> TO <lt_rng> ASSIGNING FIELD-SYMBOL(<ls_rng_new>).
    ASSIGN COMPONENT zcl_bc_ddic_toolkit=>c_fieldname_low OF STRUCTURE <ls_rng_new> TO FIELD-SYMBOL(<lv_low_new>).
    <lv_low_new> = |{ c_char_star }{ <lv_low> }{ c_char_star }|.

    APPEND <ls_rng> TO <lt_rng> ASSIGNING <ls_rng_new>.
    ASSIGN COMPONENT zcl_bc_ddic_toolkit=>c_fieldname_low OF STRUCTURE <ls_rng_new> TO <lv_low_new>.
    <lv_low_new> = |{ c_char_star }{ <lv_low> }|.

    APPEND <ls_rng> TO <lt_rng> ASSIGNING <ls_rng_new>.
    ASSIGN COMPONENT zcl_bc_ddic_toolkit=>c_fieldname_low OF STRUCTURE <ls_rng_new> TO <lv_low_new>.
    <lv_low_new> = |{ <lv_low> }{ c_char_star }|.

  ENDMETHOD.
ENDCLASS.