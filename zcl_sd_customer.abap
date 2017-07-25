CLASS zcl_sd_customer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CONSTANTS c_fnam_kunnr TYPE fieldname VALUE 'KUNNR'.

    DATA gs_def TYPE kna1 READ-ONLY.

    CLASS-METHODS:
      cache_name1
        IMPORTING
          !ir_tab  TYPE REF TO data
          !iv_fnam TYPE fieldname DEFAULT c_fnam_kunnr
        RAISING
          zcx_bc_class_method,

      get_instance
        IMPORTING !iv_kunnr     TYPE kunnr
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_customer
        RAISING   zcx_bc_table_content,

      get_kna1
        IMPORTING !iv_kunnr      TYPE kunnr
        RETURNING VALUE(rs_kna1) TYPE kna1 ,

      GET_KNVV
        importing
          !IV_KUNNR type KNVV-KUNNR
          !IV_VKORG type KNVV-VKORG
          !IV_VTWEG type KNVV-VTWEG
          !IV_SPART type KNVV-SPART
        returning
          value(RS_KNVV) type KNVV,

      get_name1
        IMPORTING
          !iv_kunnr       TYPE kunnr
        RETURNING
          VALUE(rv_name1) TYPE name1_gp.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_knvv
        TYPE HASHED TABLE OF knvv
        WITH UNIQUE KEY primary_key COMPONENTS kunnr vkorg vtweg spart,

      BEGIN OF t_multiton,
        kunnr TYPE kna1-kunnr,
        obj   TYPE REF TO zcl_sd_customer,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS kunnr,

      BEGIN OF t_name1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
      END OF t_name1,

      tt_name1 TYPE HASHED TABLE OF t_name1 WITH UNIQUE KEY primary_key COMPONENTS kunnr.

    CONSTANTS:
      c_clsname_me       TYPE seoclsname VALUE 'ZCL_SD_CUSTOMER',
      c_meth_cache_wgbez TYPE seocpdname VALUE 'CACHE_NAME1',
      c_tabname_def      TYPE tabname    VALUE 'KNA1'.

    CLASS-DATA:
      gt_kna1     TYPE HASHED TABLE OF kna1 WITH UNIQUE KEY primary_key COMPONENTS kunnr,
      gt_knvv     type tt_knvv,
      gt_multiton TYPE tt_multiton,
      gt_name1    TYPE tt_name1.

ENDCLASS.



CLASS ZCL_SD_CUSTOMER IMPLEMENTATION.


  METHOD cache_name1.

    DATA:
      lt_kunnr_rng TYPE range_kunnr_tab.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_kunnr> TYPE any.

    TRY.

        CHECK ir_tab IS NOT INITIAL.
        ASSIGN ir_tab->* TO <lt_tab>.
        CHECK <lt_tab> IS ASSIGNED.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_kunnr>.

          CHECK <lv_kunnr> IS ASSIGNED AND
                <lv_kunnr> IS NOT INITIAL AND
                ( NOT line_exists( gt_name1[ KEY primary_key COMPONENTS kunnr = <lv_kunnr> ] ) ).

          COLLECT VALUE range_kunnr_wa(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = <lv_kunnr>
          ) INTO lt_kunnr_rng.

        ENDLOOP.

        CHECK lt_kunnr_rng IS NOT INITIAL.

        SELECT kunnr, name1 APPENDING CORRESPONDING FIELDS OF TABLE @gt_name1
          FROM kna1
          WHERE kunnr IN @lt_kunnr_rng.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_wgbez.

    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS kunnr = iv_kunnr ] TO FIELD-SYMBOL(<ls_multiton>).
    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( kunnr = iv_kunnr ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE * INTO ls_multiton-obj->gs_def FROM kna1 WHERE kunnr EQ ls_multiton-kunnr.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-kunnr )
            tabname  = c_tabname_def.
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_kna1.

    DATA ls_kna1 TYPE kna1.

    ASSIGN gt_kna1[ KEY primary_key COMPONENTS kunnr = iv_kunnr ] TO FIELD-SYMBOL(<ls_kna1>).
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM kna1 INTO ls_kna1 WHERE kunnr = iv_kunnr.
      ls_kna1-kunnr = iv_kunnr.
      INSERT ls_kna1 INTO TABLE gt_kna1 ASSIGNING <ls_kna1>.
    ENDIF.

    rs_kna1 = <ls_kna1>.

  ENDMETHOD.


  METHOD get_knvv.

    DATA ls_knvv TYPE knvv.

    ASSIGN gt_knvv[
      KEY primary_key
      COMPONENTS
        kunnr = iv_kunnr
        vkorg = iv_vkorg
        vtweg = iv_vtweg
        spart = iv_spart
    ] TO FIELD-SYMBOL(<ls_knvv>).

    IF sy-subrc ne 0.
      CLEAR ls_knvv.
      SELECT SINGLE * FROM knvv INTO ls_knvv
        WHERE kunnr = iv_kunnr
          AND vkorg = iv_vkorg
          AND vtweg = iv_vtweg
          AND spart = iv_spart.
*
      ls_knvv-kunnr = iv_kunnr.
      ls_knvv-vkorg = iv_vkorg.
      ls_knvv-vtweg = iv_vtweg.
      ls_knvv-spart = iv_spart.
      INSERT ls_knvv INTO TABLE gt_knvv ASSIGNING <ls_knvv>.
    ENDIF.

    rs_knvv = <ls_knvv>.
  ENDMETHOD.


  METHOD get_name1.

    DATA ls_name1 TYPE t_name1.

    ASSIGN gt_name1[ KEY primary_key COMPONENTS
      kunnr = iv_kunnr
    ] TO FIELD-SYMBOL(<ls_name1>).

    IF sy-subrc NE 0.
      SELECT SINGLE kunnr name1
        INTO CORRESPONDING FIELDS OF ls_name1
        FROM kna1
        WHERE kunnr EQ iv_kunnr.

      ls_name1-kunnr = iv_kunnr.
      INSERT ls_name1 INTO TABLE gt_name1 ASSIGNING <ls_name1>.
    ENDIF.

    rv_name1 = <ls_name1>-name1.

  ENDMETHOD.
ENDCLASS.