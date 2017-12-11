CLASS zcl_bc_dynamic_itab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPE-POOLS slis.

    TYPES: BEGIN OF t_fld,
             fnam TYPE fieldname,
             dtel TYPE rollname,
           END OF t_fld,

           tt_fld      TYPE STANDARD TABLE OF t_fld WITH DEFAULT KEY,
           tt_fnam_rng TYPE RANGE OF fieldname.

    DATA gt_fld TYPE tt_fld READ-ONLY.

    CLASS-METHODS create_range
      IMPORTING
        !im_v_field TYPE fieldname
      EXPORTING
        !ex_r_wa    TYPE REF TO data
        !ex_r_tab   TYPE REF TO data .

    CLASS-METHODS get_instance_as_range
      IMPORTING
        !iv_rollname  TYPE rollname
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_dynamic_itab
      RAISING
        zcx_bc_method_parameter
        zcx_bc_table_content.

    CLASS-METHODS get_instance_with_tabname
      IMPORTING
        !iv_tabname   TYPE tabname
        !it_fnam_rng  TYPE tt_fnam_rng OPTIONAL
        !it_extra_fld type tt_fld optional
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_dynamic_itab
      RAISING
        zcx_bc_method_parameter.

    METHODS constructor
      IMPORTING
        !it_fld TYPE tt_fld
      RAISING
        zcx_bc_method_parameter.

    METHODS get_alv_fcat
      RETURNING
        VALUE(rt_fcat) TYPE slis_t_fieldcat_alv.

    METHODS get_itab_ref
      RETURNING
        VALUE(rr_ref) TYPE REF TO data.

    METHODS get_wa_ref
      RETURNING
        VALUE(rr_ref) TYPE REF TO data.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DYNAMIC_ITAB'.

    DATA: gt_comp TYPE cl_abap_structdescr=>component_table,
          gt_fcat TYPE slis_t_fieldcat_alv,
          gr_tref TYPE REF TO data,
          gr_wref TYPE REF TO data.

    METHODS build_comp.

    METHODS validate_fld
      IMPORTING
        !it_fld TYPE tt_fld
      RAISING
        zcx_bc_table_content.

ENDCLASS.



CLASS ZCL_BC_DYNAMIC_ITAB IMPLEMENTATION.


  METHOD build_comp.

    DATA:
      lo_element TYPE REF TO cl_abap_elemdescr,
      lo_tab     TYPE REF TO cl_abap_tabledescr.

    CHECK gt_comp[] IS INITIAL.

    LOOP AT gt_fld ASSIGNING FIELD-SYMBOL(<ls_fld>).

      TRY.

          lo_element ?=  cl_abap_elemdescr=>describe_by_name( <ls_fld>-dtel ).
          APPEND VALUE #( name = <ls_fld>-fnam type = lo_element ) TO gt_comp.
          CONTINUE.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      TRY.
          lo_tab ?= cl_abap_tabledescr=>describe_by_name( <ls_fld>-dtel ).
          APPEND VALUE #( name = <ls_fld>-fnam type = lo_tab ) TO gt_comp.
          CONTINUE.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    TRY.
        validate_fld( it_fld ).
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING
            textid      = zcx_bc_method_parameter=>param_value_invalid
            previous    = lo_cx_root
            class_name  = c_clsname_me
            method_name = 'CONSTRUCTOR'
            param_name  = 'IT_FLD'.
    ENDTRY.

    gt_fld[] = it_fld[].
  ENDMETHOD.

  METHOD create_range.

    DATA:
      lr_structdescr TYPE REF TO cl_abap_structdescr,
      lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lr_datadescr   TYPE REF TO cl_abap_datadescr,
      lt_components  TYPE abap_component_tab,
      ls_component   TYPE LINE OF abap_component_tab.
*    lr_wa             type ref to data,
*    lr_tab            type ref to data.

* determine components of structure -> lt_components
    MOVE 'SIGN' TO ls_component-name.
    ls_component-type = cl_abap_elemdescr=>get_c( p_length = 1 ).
    INSERT ls_component INTO TABLE lt_components.

    MOVE 'OPTION' TO ls_component-name.
    ls_component-type = cl_abap_elemdescr=>get_c( p_length = 2 ).
    INSERT ls_component INTO TABLE lt_components.

    MOVE 'LOW' TO ls_component-name.
    ls_component-type ?= cl_abap_elemdescr=>describe_by_name( 'MATNR' ).
    INSERT ls_component INTO TABLE lt_components.

    MOVE 'HIGH' TO ls_component-name.
    ls_component-type ?= cl_abap_elemdescr=>describe_by_name( 'MATNR' ).
    INSERT ls_component INTO TABLE lt_components.

* get structure descriptor -> lr_STRUCTDESCR
    lr_structdescr = cl_abap_structdescr=>create( lt_components ).

* create work area of structure lr_STRUCTDESCR -> lr_WA
    CREATE DATA ex_r_wa TYPE HANDLE lr_structdescr.
*  assign lr_wa->* to <fs_range>.

    lr_datadescr = lr_structdescr.
    lr_tabledescr = cl_abap_tabledescr=>create( lr_datadescr ).

* Create dynmaic internal table
    CREATE DATA ex_r_tab TYPE HANDLE lr_tabledescr.
*  assign lr_tab->* to <fs_range_tab>.

  ENDMETHOD.

  METHOD get_alv_fcat.

    IF gt_fcat[] IS INITIAL.

      LOOP AT gt_fld ASSIGNING FIELD-SYMBOL(<ls_fld>).

        TRY.
            DATA(lo_dtel) = zcl_bc_data_element=>get_instance( <ls_fld>-dtel ).
            DATA(lo_doma) = lo_dtel->get_domain( ).
          CATCH cx_root .
            CONTINUE.
        ENDTRY.

        APPEND VALUE #(
          fieldname = <ls_fld>-fnam
          intlen    = lo_doma->gs_def-leng
          rollname  = <ls_fld>-dtel
          lowercase = lo_doma->gs_def-lowercase
        ) TO gt_fcat.

      ENDLOOP.

    ENDIF.

    rt_fcat[] = gt_fcat[].

  ENDMETHOD.


  METHOD get_instance_as_range.

    zcl_bc_data_element=>get_instance( iv_rollname ).

    ro_obj = NEW #( VALUE #( ( fnam = zcl_bc_ddic_toolkit=>c_fieldname_sign   dtel = zcl_bc_ddic_toolkit=>c_rollname_sign )
                             ( fnam = zcl_bc_ddic_toolkit=>c_fieldname_option dtel = zcl_bc_ddic_toolkit=>c_rollname_option )
                             ( fnam = zcl_bc_ddic_toolkit=>c_fieldname_low    dtel = iv_rollname )
                             ( fnam = zcl_bc_ddic_toolkit=>c_fieldname_high   dtel = iv_rollname ) ) ).

  ENDMETHOD.


  METHOD get_instance_with_tabname.

    DATA lt_fld TYPE tt_fld.

    SELECT fieldname AS fnam
           rollname  AS dtel
           INTO CORRESPONDING FIELDS OF TABLE lt_fld
           FROM dd03l
           WHERE tabname   EQ iv_tabname
             AND fieldname IN it_fnam_rng
             AND fieldname NE '.INCLUDE'
           ORDER BY position.

    append lines of it_extra_fld to lt_fld.

    ro_obj = NEW #( lt_fld ).

  ENDMETHOD.


  METHOD get_itab_ref.

    IF gr_tref IS INITIAL.

      build_comp( ).

      DATA(lo_tab) = cl_abap_tabledescr=>create( p_line_type  = cl_abap_structdescr=>create( gt_comp )
                                                 p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                 p_unique     = abap_false ).

      CREATE DATA gr_tref TYPE HANDLE lo_tab.

    ENDIF.

    rr_ref = gr_tref.

  ENDMETHOD.


  METHOD get_wa_ref.

    IF gr_wref IS INITIAL.

      build_comp( ).
      DATA(lo_str) = cl_abap_structdescr=>create( gt_comp ).

      CREATE DATA gr_wref TYPE HANDLE lo_str.

    ENDIF.

    rr_ref = gr_wref.

  ENDMETHOD.


  METHOD validate_fld.

    DATA lt_fld TYPE tt_fld.

*   Her alan bir kez geçebilir
    lt_fld[] = it_fld[].
    SORT lt_fld BY fnam.
    DELETE ADJACENT DUPLICATES FROM lt_fld COMPARING fnam.

    IF lines( lt_fld ) NE lines( it_fld ).
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid    = zcx_bc_table_content=>column_values_duplicate
          tabname   = 'IT_FLD'
          fieldname = 'FNAM'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.