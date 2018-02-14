CLASS zcl_mm_vendor DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CONSTANTS:
      c_fnam_company TYPE fieldname VALUE 'BUKRS',
      c_fnam_vendor  TYPE fieldname VALUE 'LIFNR'.

    DATA gs_def TYPE lfa1.

    METHODS:
      get_adrc RETURNING VALUE(rs_adrc) TYPE adrc,

      get_company_code_data
        IMPORTING !iv_bukrs    TYPE bukrs
        RETURNING VALUE(rs_cc) TYPE lfb1
        RAISING   zcx_bc_table_content.

    CLASS-METHODS:
      cache_itab_with_adrc_data
        IMPORTING
          !ir_itab         TYPE REF TO data
          !iv_fnam_vendor  TYPE fieldname DEFAULT c_fnam_vendor
          !iv_fnam_company TYPE fieldname DEFAULT c_fnam_company
        RAISING
          zcx_bc_method_parameter,

      cache_itab_with_comp_code_data
        IMPORTING
          !ir_itab         TYPE REF TO data
          !iv_fnam_vendor  TYPE fieldname DEFAULT c_fnam_vendor
          !iv_fnam_company TYPE fieldname DEFAULT c_fnam_company
        RAISING
          zcx_bc_method_parameter,

      get_default_acc_grp_vals
        IMPORTING
          !iv_bukrs       TYPE bukrs
          !iv_ktokk       TYPE ktokk
        RETURNING
          VALUE(rs_def)   TYPE ZFIT_XK_DZAHLS
        RAISING
          zcx_bc_table_content,

      get_instance
        IMPORTING !iv_lifnr     TYPE lifnr
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_vendor
        RAISING   zcx_bc_table_content,

      get_lfa1
        IMPORTING !iv_lifnr      TYPE lifnr
        RETURNING VALUE(rs_lfa1) TYPE lfa1 .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_company_code_data,
        bukrs TYPE bukrs,
        lfb1  TYPE lfb1,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_company_code_data,

      tt_company_code_data TYPE HASHED TABLE OF t_company_code_data
        WITH UNIQUE KEY primary_key COMPONENTS bukrs,

      tt_def_acc_grp_Val
        TYPE HASHED TABLE OF zfit_xk_dzahls
        WITH UNIQUE KEY primary_key COMPONENTS bukrs ktokk,

      BEGIN OF t_lazy_flg,
        adrc TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        adrc TYPE adrc,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_lfa1_key,
        lifnr TYPE lfa1-lifnr,
      END OF t_lfa1_key,

      tt_lfa1_key
        TYPE STANDARD TABLE OF t_lfa1_key
        WITH DEFAULT KEY,

      BEGIN OF t_lfb1_key,
        bukrs TYPE lfb1-bukrs,
        lifnr TYPE lfb1-lifnr,
      END OF t_lfb1_key,

      tt_lfb1_key
        TYPE STANDARD TABLE OF t_lfb1_key
        WITH DEFAULT KEY,

      BEGIN OF t_mt,
        lifnr TYPE lifnr,
        obj   TYPE REF TO zcl_mm_vendor,
      END OF t_mt,

      tt_mt TYPE HASHED TABLE OF t_mt WITH UNIQUE KEY primary_key COMPONENTS lifnr.

    CONSTANTS:
      c_clsname_me            TYPE seoclsname VALUE 'ZCL_MM_VENDOR',
      c_meth_ciwccd           TYPE seocpdname VALUE 'CACHE_ITAB_WITH_COMP_CODE_DATA',
      c_tabname_company_data  TYPE tabname    VALUE 'LFB1',
      c_tabname_def           TYPE tabname    VALUE 'LFA1',
      c_tabname_def_pay_block TYPE tabname    VALUE 'ZFIT_XK_DZAHLS'.

    DATA:
      gs_lazy              TYPE t_lazy,
      gt_company_code_data TYPE tt_company_code_data.

    CLASS-DATA:
      gt_def_acc_grp_val TYPE tt_def_acc_Grp_Val,
      gt_lfa1            TYPE HASHED TABLE OF lfa1 WITH UNIQUE KEY primary_key COMPONENTS lifnr,
      gt_mt              TYPE tt_mt.

    CLASS-METHODS:
      create_and_cache_multiton IMPORTING !it_lfa1_key TYPE tt_lfa1_key.

    METHODS:
      constructor
        IMPORTING
          !iv_lifnr TYPE lifnr OPTIONAL
          !is_lfa1  TYPE lfa1 OPTIONAL
        RAISING
          zcx_bc_table_content.


ENDCLASS.



CLASS zcl_mm_vendor IMPLEMENTATION.


  METHOD cache_itab_with_adrc_data.

    DATA:
      lt_adrc_key TYPE tt_lfa1_key,
      lt_lfa1_key TYPE tt_lfa1_key,
      lv_append_a TYPE abap_bool,
      lv_append_b TYPE abap_bool.

    FIELD-SYMBOLS:
      <lt_itab>  TYPE ANY TABLE,
      <lv_lifnr> TYPE lfb1-lifnr.

    CHECK ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.
    CHECK <lt_itab> IS NOT INITIAL.

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).

      ASSIGN COMPONENT iv_fnam_vendor  OF STRUCTURE <ls_itab> TO <lv_lifnr>.

      IF <lv_lifnr> IS NOT ASSIGNED.

        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING
            textid      = zcx_bc_method_parameter=>param_value_invalid
            class_name  = c_clsname_me
            method_name = c_meth_ciwccd
            param_name  = |{ iv_fnam_vendor }|.

      ENDIF.

      CLEAR:
        lv_append_a,
        lv_append_b.

      ASSIGN gt_mt[
          lifnr = <lv_lifnr>
        ] TO FIELD-SYMBOL(<ls_mt>).

      IF sy-subrc EQ 0.
        lv_append_a = abap_false.
        lv_append_b = xsdbool( <ls_mt>-obj->gs_lazy-flg-adrc EQ abap_false ).
      ELSE.
        lv_append_a = abap_true.
        lv_append_b = abap_true.
      ENDIF.

      IF lv_append_a EQ abap_true.
        APPEND VALUE #( lifnr = <lv_lifnr> ) TO lt_lfa1_key.
      ENDIF.

      IF lv_append_b EQ abap_true.
        APPEND VALUE #( lifnr = <lv_lifnr> ) TO lt_adrc_key.
      ENDIF.

    ENDLOOP.

    SORT lt_lfa1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1_key.
    SORT lt_adrc_key.
    DELETE ADJACENT DUPLICATES FROM lt_adrc_key.

    create_and_cache_multiton( lt_lfa1_key ).

    IF lt_adrc_key IS NOT INITIAL.

      SELECT lfa1~lifnr, adrc~*
        FROM
          lfa1
          INNER JOIN adrc ON adrc~addrnumber EQ lfa1~adrnr
        FOR ALL ENTRIES IN @lt_adrc_key
        WHERE
          lfa1~lifnr     EQ @lt_adrc_key-lifnr AND
          adrc~date_from LE @sy-datum AND
          adrc~date_to   GE @sy-datum
        INTO TABLE @DATA(lt_adrc).

      LOOP AT lt_adrc ASSIGNING FIELD-SYMBOL(<ls_adrc>).

        ASSIGN gt_mt[
            KEY primary_key COMPONENTS
            lifnr = <ls_adrc>-lifnr
          ] TO <ls_mt>.

        CHECK sy-subrc EQ 0. " Paranoya

        <ls_mt>-obj->gs_lazy-flg-adrc = abap_true.
        <ls_mt>-obj->gs_lazy-val-adrc = CORRESPONDING #( <ls_adrc>-adrc ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD cache_itab_with_comp_code_data.

    DATA:
      lt_lfa1_key TYPE tt_lfa1_key,
      lt_lfb1_key TYPE tt_lfb1_key,
      lv_append_a TYPE abap_bool,
      lv_append_b TYPE abap_bool.

    FIELD-SYMBOLS:
      <lt_itab>  TYPE ANY TABLE,
      <lv_bukrs> TYPE lfb1-bukrs,
      <lv_lifnr> TYPE lfb1-lifnr.

    CHECK ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.
    CHECK <lt_itab> IS NOT INITIAL.

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).

      ASSIGN COMPONENT:
        iv_fnam_company OF STRUCTURE <ls_itab> TO <lv_bukrs>,
        iv_fnam_vendor  OF STRUCTURE <ls_itab> TO <lv_lifnr>.

      IF <lv_lifnr> IS NOT ASSIGNED OR
         <lv_bukrs> IS NOT ASSIGNED.

        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING
            textid      = zcx_bc_method_parameter=>param_value_invalid
            class_name  = c_clsname_me
            method_name = c_meth_ciwccd
            param_name  = |{ iv_fnam_company } { iv_fnam_vendor }|.

      ENDIF.

      CLEAR:
        lv_append_a,
        lv_append_b.

      ASSIGN gt_mt[
          lifnr = <lv_lifnr>
        ] TO FIELD-SYMBOL(<ls_mt>).

      IF sy-subrc EQ 0.
        lv_append_a = abap_false.

        lv_append_b = xsdbool(
          NOT line_exists(
            <ls_mt>-obj->gt_company_code_data[ bukrs = <lv_bukrs> ]
          )
        ).

      ELSE.
        lv_append_a = abap_true.
        lv_append_b = abap_true.
      ENDIF.

      IF lv_append_a EQ abap_true.
        APPEND VALUE #(
            lifnr = <lv_lifnr>
          ) TO lt_lfa1_key.
      ENDIF.

      IF lv_append_b EQ abap_true.
        APPEND VALUE t_lfb1_key(
            lifnr = <lv_lifnr>
            bukrs = <lv_bukrs>
          ) TO lt_lfb1_key.
      ENDIF.

    ENDLOOP.

    SORT lt_lfa1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1_key.
    SORT lt_lfb1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfb1_key.

    create_and_cache_multiton( lt_lfa1_key ).

    IF lt_lfb1_key IS NOT INITIAL.

      SELECT *
        FROM lfb1
        FOR ALL ENTRIES IN @lt_lfb1_key
        WHERE
          lifnr EQ @lt_lfb1_key-lifnr AND
          bukrs EQ @lt_lfb1_key-bukrs
        INTO TABLE @DATA(lt_lfb1).

      LOOP AT lt_lfb1 ASSIGNING FIELD-SYMBOL(<ls_lfb1>).

        ASSIGN gt_mt[
            KEY primary_key COMPONENTS
            lifnr = <ls_lfb1>-lifnr
          ] TO <ls_mt>.

        CHECK sy-subrc EQ 0.

        INSERT VALUE #(
            bukrs = <ls_lfb1>-bukrs
            lfb1  = <ls_lfb1>
          ) INTO TABLE <ls_mt>-obj->gt_company_code_data.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    IF is_lfa1 IS SUPPLIED.
      gs_def = is_lfa1.
    ELSE.

      SELECT SINGLE * INTO @gs_def
        FROM lfa1
        WHERE lifnr EQ @iv_lifnr.

      CHECK sy-subrc NE 0.

      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_lifnr )
          tabname  = c_tabname_def.

    ENDIF.

  ENDMETHOD.


  METHOD create_and_cache_multiton.

    CHECK it_lfa1_key IS NOT INITIAL.

    SELECT *
      FROM lfa1
      FOR ALL ENTRIES IN @it_lfa1_key
      WHERE lifnr EQ @it_lfa1_key-lifnr
      INTO TABLE @DATA(lt_lfa1).

    LOOP AT lt_lfa1 ASSIGNING FIELD-SYMBOL(<ls_lfa1>).

      TRY.
          INSERT VALUE #(
              lifnr = <ls_lfa1>-lifnr
              obj   = NEW #( is_lfa1 = <ls_lfa1> )
            ) INTO TABLE gt_mt.
        CATCH cx_root ##no_handler .
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_adrc.

    IF gs_lazy-flg-adrc EQ abap_false.

      SELECT SINGLE *
        FROM adrc
        WHERE
          addrnumber EQ @gs_def-adrnr AND
          date_from  LE @sy-datum AND
          date_to    GE @sy-datum
        INTO @gs_lazy-val-adrc
        ##WARN_OK.

      gs_lazy-flg-adrc = abap_true.
    ENDIF.

    rs_adrc = gs_lazy-val-adrc.

  ENDMETHOD.


  METHOD get_company_code_data.

    ASSIGN gt_company_code_data[
            KEY primary_key COMPONENTS
            bukrs = iv_bukrs
        ] TO FIELD-SYMBOL(<ls_cc>).

    IF sy-subrc NE 0.

      DATA(ls_cc) = VALUE t_company_code_data( bukrs = iv_bukrs ).

      SELECT SINGLE *
          FROM lfb1
          WHERE (
              lifnr EQ @gs_def-lifnr AND
              bukrs EQ @ls_cc-bukrs
          )
          INTO @ls_cc-lfb1.

      IF sy-subrc NE 0.

        ls_cc-cx = NEW #(
            textid    = zcx_bc_table_content=>entry_missing
            objectid  = |{ gs_def-lifnr } { ls_cc-bukrs }|
            tabname   = c_tabname_company_data
        ).

      ENDIF.

      INSERT ls_cc
          INTO TABLE gt_company_code_data
          ASSIGNING <ls_cc>.

    ENDIF.

    IF <ls_cc>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cc>-cx.
    ENDIF.

    rs_cc = <ls_cc>-lfb1.

  ENDMETHOD.

  METHOD get_default_acc_grp_vals.

    IF gt_def_acc_grp_val IS INITIAL.
      SELECT * FROM zfit_xk_dzahls INTO TABLE @gt_def_acc_grp_val.
      IF gt_def_acc_grp_val IS INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_def_acc_grp_val.
      ENDIF.
    ENDIF.

    TRY.
        rs_def = gt_def_acc_grp_val[
          KEY primary_key COMPONENTS
          bukrs = iv_bukrs
          ktokk = iv_ktokk
        ].
      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            previous = lo_silnf
            objectid = |{ iv_bukrs } { iv_ktokk }|
            tabname  = c_tabname_def_pay_block.

    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_mt[ KEY primary_key COMPONENTS lifnr = iv_lifnr ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      INSERT VALUE #(
          lifnr = iv_lifnr
          obj   = NEW #( iv_lifnr = iv_lifnr )
        ) INTO TABLE gt_mt ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.


  METHOD get_lfa1.

    DATA ls_lfa1 TYPE lfa1.

    ASSIGN gt_lfa1[ KEY primary_key COMPONENTS lifnr = iv_lifnr ] TO FIELD-SYMBOL(<ls_lfa1>).
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = iv_lifnr.
      ls_lfa1-lifnr = iv_lifnr.
      INSERT ls_lfa1 INTO TABLE gt_lfa1 ASSIGNING <ls_lfa1>.
    ENDIF.

    rs_lfa1 = <ls_lfa1>.

  ENDMETHOD.
ENDCLASS.