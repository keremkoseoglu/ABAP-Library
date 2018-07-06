CLASS zcl_bc_table_field DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      go_data_element TYPE REF TO zcl_bc_data_element READ-ONLY,
      gs_def          TYPE dd03l READ-ONLY.

    CLASS-METHODS:

      get_instance
        IMPORTING
          !iv_tab       TYPE tabname
          !iv_fld       TYPE fieldname
        RETURNING
          VALUE(ro_obj) TYPE REF TO zcl_bc_table_field
        RAISING
          zcx_bc_table_content,

      get_instance_by_fullname
        IMPORTING
          !iv_fullname  TYPE clike
        RETURNING
          VALUE(ro_obj) TYPE REF TO zcl_bc_table_field
        RAISING
          zcx_bc_method_parameter
          zcx_bc_table_content.

    methods:

      get_text
        returning value(rv_text) type ddtext.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        tab TYPE tabname,
        fld TYPE fieldname,
        obj TYPE REF TO zcl_bc_table_field,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS tab fld.

    CONSTANTS:
      c_clsname_me         TYPE seoclsname VALUE 'ZCL_BC_TABLE_FIELD',
      c_fullname_separator TYPE char1 VALUE '-',
      c_meth_gibf          TYPE seocpdname VALUE 'GET_INSTANCE_BY_FULLNAME',
      c_param_fullname     TYPE seocpdname VALUE 'IV_FULLNAME',
      c_tabname_main       TYPE tabname VALUE 'DD03L'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

ENDCLASS.



CLASS ZCL_BC_TABLE_FIELD IMPLEMENTATION.


  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS
        tab = iv_tab
        fld = iv_fld
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton(
        tab = iv_tab
        fld = iv_fld
      ).

      CREATE OBJECT ls_multiton-obj.

      SELECT SINGLE *
        INTO @ls_multiton-obj->gs_def
        FROM dd03l
        WHERE
          tabname   EQ @ls_multiton-tab AND
          fieldname EQ @ls_multiton-fld
        ##WARN_OK.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = |{ ls_multiton-tab }-{ ls_multiton-fld }|
            tabname  = c_tabname_main.
      ENDIF.

      IF ls_multiton-obj->gs_def-rollname IS NOT INITIAL.

        ls_multiton-obj->go_data_element = cast zcl_bc_data_element(
          zcl_bc_multiton=>get_obj(
            iv_clsname  = zcl_bc_data_element=>c_clsname_me
            iv_objectid = conv #( ls_multiton-obj->gs_def-rollname )
          )
        ).

      ENDIF.

      INSERT ls_multiton
        INTO TABLE gt_multiton
        ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_instance_by_fullname.

    DATA:
      lv_fld TYPE fieldname,
      lv_tab TYPE tabname.

    SPLIT iv_fullname
      AT c_fullname_separator
      INTO lv_tab lv_fld.

    IF lv_tab IS INITIAL OR
       lv_fld IS INITIAL.

      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_value_invalid
          class_name  = c_clsname_me
          method_name = c_meth_gibf
          param_name  = c_param_fullname.

    ENDIF.

    ro_obj = get_instance(
      iv_tab = lv_tab
      iv_fld = lv_fld
    ).

  ENDMETHOD.


  method get_text.

    if go_data_element is not initial.
      rv_text = go_Data_element->get_text( iv_worst_case_rollname = ABAP_false ).
    endif.

    check rv_text is initial.

    rv_text = |{ gs_def-tabname }{ c_fullname_separator }{ gs_def-fieldname }|.

  endmethod.
ENDCLASS.