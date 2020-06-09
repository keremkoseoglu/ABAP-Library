CLASS zcl_bc_dol_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    constants: c_fnam_trkorr  type fieldname value 'TRKORR',
               c_tabname_dlwr type tabname   value 'ZBCS_DOL_LIST_WITH_REQ'.

    TYPES: BEGIN OF t_gekod,
             gekod TYPE zbcd_jira_gekod,
           END OF t_gekod,

           tt_dol_list    TYPE STANDARD TABLE OF zbcs_dol_list WITH DEFAULT KEY,
           tt_dol_list_wr type standard table of ZBCS_DOL_LIST_WITH_REQ with default key,
           tt_gekod       TYPE STANDARD TABLE OF t_gekod WITH DEFAULT KEY,
           tt_trkorr_rng  TYPE RANGE OF trkorr,

           BEGIN OF t_param,
             gekod      TYPE tt_gekod,
             trkorr_rng TYPE tt_trkorr_rng,
           END OF t_param.

    class-methods:

      get_dol_obj
        IMPORTING !iv_object TYPE trobjtype
        EXPORTING !eo_dol    TYPE REF TO zif_bc_dol_obj
        RAISING   zcx_bc_table_content.

    METHODS get_list
      IMPORTING
        !is_param      TYPE t_param
      RETURNING
        VALUE(rt_list) TYPE tt_dol_list.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_dol_obj,
             object TYPE trobjtype,
             obj    TYPE REF TO zif_bc_dol_obj,
             cx     TYPE REF TO zcx_bc_table_content,
           END OF t_dol_obj,

           BEGIN OF t_trkorr,
             trkorr TYPE trkorr,
           END OF t_trkorr,

           tt_dol_obj     TYPE HASHED TABLE OF t_dol_obj WITH UNIQUE KEY primary_key COMPONENTS object,
           tt_trkorr      TYPE STANDARD TABLE OF t_trkorr WITH DEFAULT KEY.

    CONSTANTS: c_clsname_obj_pfx  TYPE seoclsname VALUE 'ZCL_BC_DOL_OBJ_',
               c_option_eq        type ddoption value 'EQ',
               c_option_cp        TYPE ddoption VALUE 'CP',
               c_sign_i           TYPE ddsign   VALUE 'I',
               c_tabname_seoclass TYPE tabname VALUE 'SEOCLASS'.

    class-data: gt_dol_obj TYPE tt_dol_obj.

    DATA: gt_list    TYPE tt_dol_list,
          gt_trkorr  TYPE tt_trkorr,
          gs_param   TYPE t_param.

    METHODS build_request_list.
    METHODS read_request_contents.

ENDCLASS.



CLASS ZCL_BC_DOL_MODEL IMPLEMENTATION.


  METHOD build_request_list.

    gt_trkorr = Corresponding #( zcl_bc_transport_request=>get_request_list(
      is_param = value #(
        s_as4text = value #( for ls_gekod in gs_param-gekod (
          option = c_option_cp
          sign   = c_sign_i
          low    = |{ ls_gekod-gekod }*|
        ) )
        p_srch_strkorr = abap_True
      )
    ) ).

  ENDMETHOD.


  METHOD get_dol_obj.

    DATA: lr_obj     TYPE REF TO object,
          lv_clsname TYPE seoclsname.

    READ TABLE gt_dol_obj ASSIGNING FIELD-SYMBOL(<ls_dol_obj>)
               WITH TABLE KEY primary_key
               COMPONENTS object = iv_object.

    IF sy-subrc NE 0.

      DATA(ls_dol_obj) = VALUE t_dol_obj( object = iv_object ).

      lv_clsname = |{ c_clsname_obj_pfx }{ iv_object }|.

      SELECT SINGLE clsname INTO @DATA(lv_clsname_dummy)
             FROM seoclass
             WHERE clsname EQ @lv_clsname.

      IF sy-subrc EQ 0.
        CREATE OBJECT lr_obj TYPE (lv_clsname).
        ls_dol_obj-obj ?= lr_obj.
      ELSE.
        ls_dol_obj-cx = NEW #( objectid = CONV #( lv_clsname )
                               tabname  = c_tabname_seoclass
                               textid   = zcx_bc_table_content=>entry_missing ).
      ENDIF.

      INSERT ls_dol_obj INTO TABLE gt_dol_obj ASSIGNING <ls_dol_obj>.

    ENDIF.

    IF <ls_dol_obj>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_dol_obj>-cx.
    ENDIF.

    eo_dol = <ls_dol_obj>-obj.

  ENDMETHOD.


  METHOD get_list.

    gs_param = is_param.
    CLEAR gt_list[].

    build_request_list( ).
    CHECK gt_trkorr[] IS NOT INITIAL.

    read_request_contents( ).
    CHECK gt_list[] IS NOT INITIAL.

    rt_list[] = gt_list[].

  ENDMETHOD.


  METHOD read_request_contents.

    CHECK gt_trkorr[] IS NOT INITIAL.

    zcl_Bc_transport_Request=>get_request_objects(
      exporting
        it_trkorr_rng = value #( for ls_trkorr in gt_trkorr (
                          option = c_option_eq
                          sign   = c_sign_i
                          low    = ls_Trkorr-trkorr
                        ) )
      importing
        et_list = gt_list
    ).

  ENDMETHOD.
ENDCLASS.