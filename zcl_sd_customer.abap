CLASS zcl_sd_customer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CONSTANTS c_fnam_kunnr TYPE fieldname VALUE 'KUNNR'.

    TYPES:
      tt_kunnr
        TYPE STANDARD TABLE OF kunnr
        WITH DEFAULT KEY,

      tt_parob
        TYPE STANDARD TABLE OF zsdt_cus_parob
        WITH DEFAULT KEY.

    DATA gs_def TYPE kna1 READ-ONLY.

    CLASS-METHODS:
      cache_kna1
        IMPORTING
          !ir_tab  TYPE REF TO data
          !iv_fnam TYPE fieldname DEFAULT c_fnam_kunnr
        RAISING
          zcx_bc_class_method,

      cache_name1
        IMPORTING
          !ir_tab  TYPE REF TO data
          !iv_fnam TYPE fieldname DEFAULT c_fnam_kunnr
        RAISING
          zcx_bc_class_method,

      ensure_obl_part_funcs_filled
        IMPORTING
          !it_vkorg TYPE zcl_sd_sales_org=>tt_vkorg
          !it_parob TYPE tt_parob
        RAISING
          zcx_sd_parob,

      get_def_land_sd_values
        IMPORTING !iv_land1     TYPE land1
        RETURNING VALUE(rs_val) TYPE zfit_xd_def_lsd_fld
        RAISING   zcx_bc_table_content,

      get_def_tax_codes
        IMPORTING !iv_ktokd     TYPE ktokd
        RETURNING VALUE(rs_val) TYPE zfit_xd_def_stc_fld
        RAISING   zcx_bc_table_content,

      get_hq_branch_codes
        IMPORTING
          !iv_need_hq     TYPE abap_bool DEFAULT abap_true
          !iv_need_br     TYPE abap_bool DEFAULT abap_true
          !it_bukrs       TYPE tpmy_range_bukrs OPTIONAL
          !it_knrze       TYPE range_kunnr_tab OPTIONAL
        RETURNING
          VALUE(rt_kunnr) TYPE tt_kunnr,

      get_instance
        IMPORTING !iv_kunnr     TYPE kunnr
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_customer
        RAISING   zcx_sd_customer_def,

      get_kna1
        IMPORTING !iv_kunnr      TYPE kunnr
        RETURNING VALUE(rs_kna1) TYPE kna1 ,

      get_knvv
        IMPORTING
          !iv_kunnr      TYPE knvv-kunnr
          !iv_vkorg      TYPE knvv-vkorg
          !iv_vtweg      TYPE knvv-vtweg
          !iv_spart      TYPE knvv-spart
        RETURNING
          VALUE(rs_knvv) TYPE knvv,

      get_name1
        IMPORTING
          !iv_kunnr       TYPE kunnr
        RETURNING
          VALUE(rv_name1) TYPE name1_gp,

      set_head_customer_if_found
        IMPORTING !iv_bukrs TYPE bukrs
        CHANGING  !cv_kunnr TYPE kunnr.


    METHODS:
      get_head_customer
        IMPORTING !iv_bukrs      TYPE bukrs
        RETURNING VALUE(ro_head) TYPE REF TO zcl_sd_customer
        RAISING   zcx_sd_customer_hq_def,

      get_transport_zone
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_transport_zone
        RAISING   zcx_sd_tzone.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_def_stc
        TYPE HASHED TABLE OF zfit_xd_def_stc
        WITH UNIQUE KEY primary_key COMPONENTS ktokd,

      tt_land1_rng TYPE RANGE OF land1,

      BEGIN OF t_def_land_sd_val,
        land1_rng TYPE tt_land1_rng,
        val       TYPE zfit_xd_def_lsd_fld,
      END OF t_def_land_sd_val,

      tt_def_land_sd_val
        TYPE STANDARD TABLE OF t_def_land_sd_val
        WITH DEFAULT KEY,

      BEGIN OF t_def_lsv_cache,
        land1 TYPE land1,
        val   TYPE zfit_xd_def_lsd_fld,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_def_lsv_cache,

      tt_def_lsv_cache
        TYPE HASHED TABLE OF t_def_lsv_cache
        WITH UNIQUE KEY primary_key COMPONENTS land1,

      BEGIN OF t_clazy_flg,
        def_land_sd_val TYPE abap_bool,
        parob           TYPE abap_bool,
      END OF t_clazy_flg,

      BEGIN OF t_clazy_val,
        def_land_sd_val TYPE tt_def_land_sd_val,
        parob           TYPE tt_parob,
      END OF t_clazy_val,

      BEGIN OF t_clazy,
        flg TYPE t_clazy_flg,
        val TYPE t_clazy_val,
      END OF t_clazy,

      BEGIN OF t_hq,
        bukrs TYPE bukrs,
        knrze TYPE knrze,
        obj   TYPE REF TO zcl_sd_customer,
        cx    TYPE REF TO zcx_sd_customer_hq_def,
      END OF t_hq,

      tt_hq
        TYPE HASHED TABLE OF t_hq
        WITH UNIQUE KEY primary_key COMPONENTS bukrs,

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
      c_tabname_def_stc  TYPE tabname    VALUE 'ZFIT_XD_DEF_STC',
      c_tabname_lsd      TYPE tabname    VALUE 'ZFIT_XD_DEF_LSD'.

    CLASS-DATA:
      gs_clazy     TYPE t_clazy,
      gt_def_stc   TYPE tt_def_stc,
      gt_hq        TYPE tt_hq,
      gt_kna1      TYPE HASHED TABLE OF kna1 WITH UNIQUE KEY primary_key COMPONENTS kunnr,
      gt_knvv      TYPE tt_knvv,
      gt_lsv_cache TYPE tt_def_lsv_cache,
      gt_multiton  TYPE tt_multiton,
      gt_name1     TYPE tt_name1.

    CLASS-METHODS:
      read_parob_lazy.
ENDCLASS.



CLASS zcl_sd_customer IMPLEMENTATION.

  METHOD cache_kna1.

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
                ( NOT line_exists( gt_kna1[ KEY primary_key COMPONENTS kunnr = <lv_kunnr> ] ) ).

          COLLECT VALUE range_kunnr_wa(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = <lv_kunnr>
          ) INTO lt_kunnr_rng.

        ENDLOOP.

        CHECK lt_kunnr_rng IS NOT INITIAL.

        SELECT *
          FROM kna1
          WHERE kunnr IN @lt_kunnr_rng
          APPENDING CORRESPONDING FIELDS OF TABLE @gt_kna1.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_wgbez.

    ENDTRY.

  ENDMETHOD.

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


  METHOD ensure_obl_part_funcs_filled.

    CHECK (
        it_vkorg IS NOT INITIAL AND
        it_parob IS NOT INITIAL
    ).

    read_parob_lazy( ).
    CHECK gs_clazy-val-parob IS NOT INITIAL.

    DATA(lt_vkorg_rng) = VALUE zcl_sd_sales_org=>tt_vkorg_rng(
        FOR GROUPS gr1 OF ls_vkorg IN it_vkorg
        GROUP BY ls_vkorg-vkorg
        (
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = gr1
        )
    ).

    DATA(lt_parob_bin) = it_parob.
    SORT lt_parob_bin BY vkorg parvw. " Binary Search

    LOOP AT gs_clazy-val-parob
        ASSIGNING FIELD-SYMBOL(<ls_parob>)
        WHERE vkorg IN lt_vkorg_rng.

      READ TABLE lt_parob_bin
          TRANSPORTING NO FIELDS
          WITH KEY
              vkorg = <ls_parob>-vkorg
              parvw = <ls_parob>-parvw
          BINARY SEARCH.

      CHECK sy-subrc NE 0.

      RAISE EXCEPTION TYPE zcx_sd_parob
        EXPORTING
          textid = zcx_sd_parob=>par_func_obl_in_sales_org
          vkorg  = <ls_parob>-vkorg
          parvw  = <ls_parob>-parvw.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_head_customer.

    IF gt_hq IS INITIAL.
      SELECT *
        FROM zfit_mrk_sube
        WHERE kunnr EQ @gs_def-kunnr
        INTO CORRESPONDING FIELDS OF TABLE @gt_hq
        ##TOO_MANY_ITAB_FIELDS.                         "#EC CI_NOFIRST

      IF gt_hq IS INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_hq.
      ENDIF.
    ENDIF.

    ASSIGN gt_hq[
        KEY primary_key COMPONENTS
        bukrs = iv_bukrs
      ] TO FIELD-SYMBOL(<ls_hq>).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_sd_customer_hq_def
        EXPORTING
          kunnr = gs_def-kunnr
          bukrs = iv_bukrs.
    ENDIF.

    IF <ls_hq>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_hq>-cx.
    ENDIF.

    IF <ls_hq>-obj IS INITIAL.

      TRY.
          <ls_hq>-obj = get_instance( <ls_hq>-knrze ).
        CATCH zcx_sd_customer_def INTO DATA(lo_cd).

          <ls_hq>-cx = NEW zcx_sd_customer_hq_def(
              textid   = zcx_sd_customer_hq_def=>hq_value_invalid
              previous = lo_cd
              kunnr    = gs_def-kunnr
              bukrs    = <ls_hq>-bukrs
              knrze    = <ls_hq>-knrze
          ).

          RAISE EXCEPTION <ls_hq>-cx.

      ENDTRY.

    ENDIF.

    ro_head = <ls_hq>-obj.

  ENDMETHOD.

  METHOD get_def_land_sd_values.

    ASSIGN gt_lsv_cache[
        KEY primary_key COMPONENTS
        land1 = iv_land1
      ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_def_lsv_cache( land1 = iv_land1 ).

      IF gs_clazy-flg-def_land_sd_val EQ abap_false.

        SELECT ddsign, land1 FROM zfit_xd_def_lsd INTO TABLE @DATA(lt_db). "#EC CI_NOWHERE

        gs_clazy-val-def_land_sd_val = VALUE #(
          FOR ls_db IN lt_db (
            val = CORRESPONDING #( ls_db )
            land1_rng = VALUE #( (
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = ls_db-ddsign
              low    = ls_db-land1
            ) )
          )
        ).

        gs_clazy-flg-def_land_sd_val = abap_true.
      ENDIF.

      DATA(lv_found) = abap_false.

      LOOP AT gs_clazy-val-def_land_sd_val ASSIGNING FIELD-SYMBOL(<ls_db>).
        CHECK ls_cache-land1 IN <ls_db>-land1_rng.
        ls_cache-val = <ls_db>-val.
        lv_found = abap_true.
      ENDLOOP.

      IF lv_found IS INITIAL.
        ls_cache-cx = NEW #(
          textid    = zcx_bc_table_content=>entry_missing
          objectid  = CONV #( ls_cache-land1 )
          tabname   = c_tabname_lsd
        ).
      ENDIF.

      INSERT ls_cache
        INTO TABLE gt_lsv_cache
        ASSIGNING <ls_cache>.

    ENDIF.

    IF <ls_cache>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-cx.
    ENDIF.

    rs_val = <ls_cache>-val.

  ENDMETHOD.

  METHOD get_def_tax_codes.

    IF gt_def_stc IS INITIAL.
      SELECT * FROM zfit_xd_def_stc INTO TABLE @gt_def_stc. "#EC CI_NOWHERE
      IF gt_def_stc IS INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_def_stc.
      ENDIF.
    ENDIF.

    TRY.
        rs_val = CORRESPONDING #(
          gt_def_stc[
            KEY primary_key COMPONENTS
            ktokd = iv_ktokd
          ]
        ).

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            previous = lo_silnf
            objectid = CONV #( iv_ktokd )
            tabname  = c_tabname_def_stc.

    ENDTRY.

  ENDMETHOD.

  METHOD get_hq_branch_codes.

    SELECT *
      FROM zfit_mrk_sube
      WHERE bukrs IN @it_bukrs
      INTO TABLE @DATA(lt_db).

    DELETE lt_db WHERE NOT ( knrze IN it_knrze ). " Aralık geniş olabileceği için sorguya koymadım

    LOOP AT lt_db ASSIGNING FIELD-SYMBOL(<ls_db>).

      IF iv_need_br EQ abap_true.
        APPEND <ls_db>-kunnr TO rt_kunnr.
      ENDIF.

      IF iv_need_hq EQ abap_true.
        APPEND <ls_db>-knrze TO rt_kunnr.
      ENDIF.

    ENDLOOP.

    SORT rt_kunnr.
    DELETE ADJACENT DUPLICATES FROM rt_kunnr.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS kunnr = iv_kunnr ] TO FIELD-SYMBOL(<ls_multiton>).
    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( kunnr = iv_kunnr ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE * INTO ls_multiton-obj->gs_def FROM kna1 WHERE kunnr EQ ls_multiton-kunnr.

      IF sy-subrc NE 0.

        RAISE EXCEPTION TYPE zcx_sd_customer_def
          EXPORTING
            kunnr = ls_multiton-kunnr.

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

    DATA:
      ls_knvv  TYPE knvv,
      lv_kunnr TYPE kunnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_kunnr
      IMPORTING
        output = lv_kunnr.

    ASSIGN gt_knvv[
      KEY primary_key
      COMPONENTS
        kunnr = lv_kunnr
        vkorg = iv_vkorg
        vtweg = iv_vtweg
        spart = iv_spart
    ] TO FIELD-SYMBOL(<ls_knvv>).

    IF sy-subrc NE 0.
      CLEAR ls_knvv.
      SELECT SINGLE * FROM knvv INTO ls_knvv
        WHERE kunnr = lv_kunnr
          AND vkorg = iv_vkorg
          AND vtweg = iv_vtweg
          AND spart = iv_spart.
*
      ls_knvv-kunnr = lv_kunnr.
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

  METHOD get_transport_zone.

    ro_obj = zcl_sd_transport_zone=>get_instance(
      iv_land1 = gs_def-land1
      iv_zone1 = gs_def-lzone
    ).

  ENDMETHOD.

  METHOD read_parob_lazy.
    CHECK gs_clazy-flg-parob IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gs_clazy-val-parob FROM zsdt_cus_parob. "#EC CI_NOWHERE
    gs_clazy-flg-parob = abap_true.
  ENDMETHOD.


  METHOD set_head_customer_if_found.

    CHECK cv_kunnr IS NOT INITIAL.

    TRY.
        DATA(lv_knrze) = get_instance( cv_kunnr )->get_head_customer( iv_bukrs )->gs_def-kunnr.
        cv_kunnr = lv_knrze.
      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.