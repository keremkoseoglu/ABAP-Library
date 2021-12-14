CLASS zcl_bc_data_element DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_bc_multiton.

    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DATA_ELEMENT'.

    DATA gs_def TYPE dd04l READ-ONLY.
    DATA gs_txt TYPE dd04t READ-ONLY.
    DATA core   TYPE REF TO ycl_addict_data_element READ-ONLY.

    CLASS-METHODS get_instance_from_addict
      IMPORTING !core         TYPE REF TO ycl_addict_data_element
      RETURNING VALUE(output) TYPE REF TO zcl_bc_data_element
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_shortest_text_safe
      IMPORTING !iv_rollname   TYPE rollname
      RETURNING VALUE(rv_text) TYPE ddtext.

    CLASS-METHODS get_text_safe
      IMPORTING !iv_rollname   TYPE rollname
      RETURNING VALUE(rv_text) TYPE ddtext.

    METHODS:
      constructor
        IMPORTING !iv_rollname TYPE rollname
        RAISING   zcx_bc_table_content,

      get_domain
        RETURNING VALUE(ro_domain) TYPE REF TO zcl_bc_abap_domain
        RAISING   zcx_bc_table_content,

      get_shortest_text
        IMPORTING !iv_worst_case_rollname TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rv_text)          TYPE ddtext,

      get_text
        IMPORTING !iv_worst_case_rollname TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rv_text)          TYPE ddtext,

      validate_value
        IMPORTING
          !iv_value TYPE val_single
        RAISING
          zcx_bc_domain
          zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_multiton,
             rollname TYPE rollname,
             obj      TYPE REF TO zcl_bc_data_element,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton
                       WITH UNIQUE KEY primary_key COMPONENTS rollname.

    CLASS-DATA gt_multiton TYPE tt_multiton.
ENDCLASS.



CLASS zcl_bc_data_element IMPLEMENTATION.
  METHOD get_instance_from_addict.
    CHECK core IS NOT INITIAL.

    output = CAST zcl_bc_data_element(
        zcl_bc_multiton=>get_obj(
          iv_clsname  = zcl_bc_data_element=>c_clsname_me
          iv_objectid = CONV #( core->def-rollname ) ) ).

    output->core = core.
  ENDMETHOD.


  METHOD constructor.
    TRY.
        me->core = ycl_addict_data_element=>get_instance( iv_rollname ).
      CATCH ycx_addict_table_content INTO DATA(tc).
        zcx_bc_table_content=>raise_from_addict( tc ).
    ENDTRY.

    me->gs_Def = me->core->def.
    me->gs_txt = me->core->txt.
  ENDMETHOD.


  METHOD get_domain.
    ro_domain = zcl_bc_abap_domain=>get_instance( gs_def-domname ).
  ENDMETHOD.


  METHOD get_shortest_text.
    rv_text = me->core->get_shortest_text( worst_case_rollname = iv_worst_case_rollname ).
  ENDMETHOD.


  METHOD get_text.
    rv_text = me->core->get_text( worst_case_rollname = iv_worst_case_rollname ).
  ENDMETHOD.


  METHOD get_shortest_text_safe.
    rv_text = ycl_Addict_data_element=>get_shortest_text_safe( iv_rollname ).
  ENDMETHOD.


  METHOD get_text_safe.
    rv_text = ycl_addict_data_element=>get_text_safe( iv_rollname ).
  ENDMETHOD.


  METHOD validate_value.
    get_domain( )->validate_value( iv_value ).
  ENDMETHOD.


  METHOD zif_bc_multiton~get_instance.
    TRY.
        ro_obj ?= NEW zcl_bc_data_element( CONV #( iv_objectid ) ).
      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE cx_sy_create_object_error
          EXPORTING
            textid    = cx_sy_create_object_error=>cx_sy_create_object_error
            previous  = lo_diaper
            classname = CONV #( c_clsname_me ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.