CLASS zcl_mm_material DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_marm TYPE STANDARD TABLE OF marm WITH DEFAULT KEY.

    CONSTANTS:
      c_aland_def    TYPE aland     VALUE 'TR',

      c_fnam_lgort   TYPE fieldname VALUE 'LGORT',
      c_fnam_maktx   TYPE fieldname VALUE 'MAKTX',
      c_fnam_matnr   TYPE fieldname VALUE 'MATNR',
      c_fnam_perkz   TYPE fieldname VALUE 'PERKZ',
      c_fnam_vkorg   TYPE fieldname VALUE 'VKORG',
      c_fnam_vtweg   TYPE fieldname VALUE 'VTWEG',
      c_fnam_werks   TYPE fieldname VALUE 'WERKS',

      c_perkz_day    TYPE perkz     VALUE 'D',
      c_perkz_init   TYPE perkz     VALUE space,
      c_perkz_month  TYPE perkz     VALUE 'M',
      c_perkz_week   TYPE perkz     VALUE 'W',
      c_perkz_year   TYPE perkz     VALUE 'P',

      c_tabname_marc TYPE tabname   VALUE 'MARC',
      c_tabname_mch1 type tabname   value 'MCH1'.

    DATA gs_def TYPE mara READ-ONLY.

    CLASS-METHODS:
      cache_maktx
        IMPORTING
          !ir_tab             TYPE REF TO data
          !iv_fnam            TYPE fieldname DEFAULT c_fnam_matnr
          !iv_spras           TYPE sylangu DEFAULT sy-langu
          !iv_conv_exit_input TYPE abap_bool DEFAULT abap_false
        RAISING
          zcx_bc_class_method,

      cache_mara
        IMPORTING
          !ir_tab  TYPE REF TO data
          !iv_fnam TYPE fieldname DEFAULT c_fnam_matnr
        RAISING
          zcx_bc_class_method,

      cache_marc
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_fnam_werks TYPE fieldname DEFAULT c_fnam_werks
        RAISING
          zcx_bc_class_method,

      cache_mard
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_fnam_werks TYPE fieldname DEFAULT c_fnam_werks
          !iv_fnam_lgort TYPE fieldname DEFAULT c_fnam_lgort
        RAISING
          zcx_bc_class_method,

      cache_marc_with_fixed_plant
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_werks      TYPE werks_d
        RAISING
          zcx_bc_class_method,

      cache_mlan
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_aland      TYPE aland DEFAULT c_aland_def
        RAISING
          zcx_bc_class_method,

      cache_mvke
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_fnam_vkorg TYPE fieldname DEFAULT c_fnam_vkorg
          !iv_fnam_vtweg TYPE fieldname DEFAULT c_fnam_vtweg
        RAISING
          zcx_bc_class_method,

      cache_mvke_with_fixed_dstchn
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_vkorg      TYPE vkorg
          !iv_vtweg      TYPE vtweg
        RAISING
          zcx_bc_class_method,

      ensure_mard_existence_bulk
        IMPORTING
          !ir_tab        TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_fnam_werks TYPE fieldname DEFAULT c_fnam_werks
          !iv_fnam_lgort TYPE fieldname DEFAULT c_fnam_lgort
          !iv_cache_mard TYPE abap_bool DEFAULT abap_true
          !io_log        TYPE REF TO zcl_bc_applog_facade OPTIONAL
        RAISING
          zcx_bc_class_method
          zcx_mm_material_stg_loc_def,

      fill_itab_with_maktx
        IMPORTING
          !ir_itab       TYPE REF TO data
          !iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
          !iv_fnam_maktx TYPE fieldname DEFAULT c_fnam_maktx,

      get_maktx
        IMPORTING
          !iv_matnr       TYPE matnr
          !iv_spras       TYPE spras DEFAULT sy-langu
        RETURNING
          VALUE(rv_maktx) TYPE maktx,

      get_mara
        IMPORTING !iv_matnr      TYPE matnr
        RETURNING VALUE(rs_mara) TYPE mara,

      get_mlan
        IMPORTING
          !iv_matnr      TYPE matnr
          !iv_aland      TYPE aland DEFAULT c_aland_def
        RETURNING
          VALUE(rs_mlan) TYPE mlan,

      get_marm
        IMPORTING !iv_matnr      TYPE matnr
        RETURNING VALUE(rt_marm) TYPE tt_marm,

      get_marc
        IMPORTING
          !iv_matnr      TYPE matnr
          !iv_werks      TYPE werks_d
        RETURNING
          VALUE(rs_marc) TYPE marc,

      get_mvke
        IMPORTING
          !iv_matnr      TYPE matnr
          !iv_vkorg      TYPE vkorg
          !iv_vtweg      TYPE vtweg
          !iv_must_exist TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rs_mvke) TYPE mvke
        RAISING
          zcx_sd_zm_material,

      get_instance
        IMPORTING !iv_matnr     TYPE matnr
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_material
        RAISING   zcx_bc_table_content,

      get_tax_rate
        IMPORTING
          !iv_matnr TYPE matnr
          !iv_aland TYPE aland DEFAULT c_aland_def
        EXPORTING
          !ev_kdv   TYPE clike
          !ev_otv   TYPE clike.

    METHODS:
      attach_gos_url
        IMPORTING
          !is_url TYPE zcl_bc_gos_toolkit=>t_url
        RAISING
          zcx_bc_gos_url_attach,

      ensure_batch_existence
        IMPORTING !iv_charg TYPE mch1-charg
        RAISING   cx_no_entry_in_table,

      ensure_material_def_in_plant
        IMPORTING !iv_werks TYPE werks_d
        RAISING   zcx_mm_material_plant,

      ensure_not_blocked_for_order
        IMPORTING
          !iv_vkorg TYPE vkorg
          !iv_vtweg TYPE vtweg
        RAISING
          zcx_sd_zm_material,

      get_gos_url_list
        RETURNING VALUE(rt_list) TYPE zcl_bc_gos_toolkit=>tt_url
        RAISING   zcx_bc_gos_doc_content,

      is_blocked_for_order
        IMPORTING
          !iv_vkorg         TYPE vkorg OPTIONAL
          !iv_vtweg         TYPE vtweg OPTIONAL
        RETURNING
          VALUE(rv_blocked) TYPE abap_bool
        RAISING
          zcx_sd_zm_material,

      is_material_defined_in_plant
        IMPORTING !iv_werks         TYPE werks_d
        RETURNING VALUE(rv_defined) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_marc_subrc,
        matnr TYPE marc-matnr,
        werks TYPE marc-werks,
        subrc TYPE sysubrc,
      END OF t_marc_subrc,

      tt_marc_subrc
        TYPE HASHED TABLE OF t_marc_subrc
        WITH UNIQUE KEY primary_key COMPONENTS matnr werks,

      BEGIN OF t_mard,
        matnr TYPE mard-matnr,
        werks TYPE mard-werks,
        lgort TYPE mard-lgort,
      END OF t_mard,

      tt_mard
        TYPE HASHED TABLE OF t_mard
        WITH UNIQUE KEY primary_key COMPONENTS matnr werks lgort,

      BEGIN OF t_marm_cache,
        matnr TYPE marm-matnr,
        marm  TYPE tt_marm,
      END OF t_marm_cache,

      tt_marm_cache
        TYPE HASHED TABLE OF t_marm_cache
        WITH UNIQUE KEY primary_key COMPONENTS matnr,

      tt_mlan_cache
        TYPE HASHED TABLE OF mlan
        WITH UNIQUE KEY primary_key COMPONENTS matnr aland,

      BEGIN OF t_multiton,
        matnr TYPE matnr,
        obj   TYPE REF TO zcl_mm_material,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS matnr,

      tt_mvke     TYPE HASHED TABLE OF mvke WITH UNIQUE KEY primary_key COMPONENTS matnr vkorg vtweg,

      BEGIN OF t_block_cache,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
        tvms  TYPE tvms,
      END OF t_block_cache,

      tt_block_cache
        TYPE HASHED TABLE OF t_block_cache
        WITH UNIQUE KEY primary_key COMPONENTS vkorg vtweg,

      BEGIN OF t_batch_existence_cache,
        charg TYPE mch1-charg,
        cx    TYPE REF TO cx_no_entry_in_table,
      END OF t_batch_existence_cache,

      tt_batch_existence_cache TYPE HASHED TABLE OF t_batch_existence_cache WITH UNIQUE KEY primary_key COMPONENTS charg.


    CONSTANTS:
      c_clsname_me          TYPE seoclsname          VALUE 'ZCL_MM_MATERIAL',
      c_fedia_blocked       TYPE spvbc               VALUE 'B',
      c_gos_classname       TYPE bapibds01-classname VALUE 'BUS1001006',
      c_meth_cache_mara     TYPE seocpdname          VALUE 'CACHE_MARA',
      c_meth_cache_marc     TYPE seocpdname          VALUE 'CACHE_MARC',
      c_meth_cache_marc_wfp TYPE seocpdname          VALUE 'CACHE_MARC_WITH_FIXED_PLANT',
      c_meth_cache_mard     TYPE seocpdname          VALUE 'CACHE_MARD',
      c_meth_cache_mlan     TYPE seocpdname          VALUE 'CACHE_MLAN',
      c_meth_cache_wgbez    TYPE seocpdname          VALUE 'CACHE_MAKTX',
      c_tabname_mara        TYPE tabname             VALUE 'MARA',
      c_tabname_mard        TYPE tabname             VALUE 'MARD'.

    DATA:
      gt_batch_existence_cache TYPE tt_batch_existence_cache,
      gt_block_cache           TYPE tt_block_cache.

    CLASS-DATA:
      gt_makt
        TYPE HASHED TABLE OF makt
        WITH UNIQUE KEY primary_key COMPONENTS spras matnr,

      gt_mara
        TYPE HASHED TABLE OF mara
        WITH UNIQUE KEY primary_key COMPONENTS matnr,

      gt_marc
        TYPE HASHED TABLE OF marc
        WITH UNIQUE KEY primary_key COMPONENTS matnr werks,

      gt_marc_subrc TYPE tt_marc_subrc,
      gt_mard       TYPE tt_mard,
      gt_marm       TYPE tt_marm_cache,
      gt_mlan       TYPE tt_mlan_cache,
      gt_mvke       TYPE tt_mvke,
      gt_multiton   TYPE tt_multiton.

    METHODS:
      get_block_status
        IMPORTING
          !iv_vkorg        TYPE vkorg
          !iv_vtweg        TYPE vtweg
        RETURNING
          VALUE(rr_status) TYPE REF TO t_block_cache
        RAISING
          zcx_sd_zm_material.

ENDCLASS.



CLASS zcl_mm_material IMPLEMENTATION.

  METHOD attach_gos_url.

    zcl_bc_gos_toolkit=>attach_url(
      is_key      = VALUE #(
        classname = c_gos_classname
        objkey    = gs_def-matnr
      )
      is_url      = is_url
    ).

  ENDMETHOD.

  METHOD cache_maktx.

    DATA:
      lt_matnr_rng TYPE range_t_matnr,
      lv_matnr     TYPE matnr.

    FIELD-SYMBOLS:
      <lt_tab>       TYPE ANY TABLE,
      <lv_matnr_raw> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_matnr_raw>.

          CHECK <lv_matnr_raw> IS ASSIGNED AND
                <lv_matnr_raw> IS NOT INITIAL.

          IF iv_conv_exit_input EQ abap_true.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <lv_matnr_raw>
              IMPORTING
                output       = lv_matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            CHECK sy-subrc EQ 0.
          ELSE.
            lv_matnr = <lv_matnr_raw>.
          ENDIF.

          CHECK NOT line_exists( gt_makt[ KEY primary_key COMPONENTS spras = iv_spras matnr = lv_matnr ] ).

          COLLECT VALUE range_s_matnr(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = lv_matnr
          ) INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.

          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_makt
            FROM makt
            WHERE spras EQ @iv_spras AND
                  matnr IN @lt_matnr_rng.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_wgbez.

    ENDTRY.

  ENDMETHOD.


  METHOD cache_mara.

    DATA:
      lt_matnr_rng TYPE range_t_matnr.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matnr> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_matnr>.

          CHECK <lv_matnr> IS ASSIGNED AND
                <lv_matnr> IS NOT INITIAL AND
                ( NOT line_exists( gt_mara[ KEY primary_key COMPONENTS matnr = <lv_matnr> ] ) ).

          COLLECT VALUE range_s_matnr(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = <lv_matnr>
          ) INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_mara
            FROM mara
            WHERE matnr IN @lt_matnr_rng.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_mara.

    ENDTRY.

  ENDMETHOD.


  METHOD cache_marc.

    DATA:
      lt_matnr_rng TYPE range_t_matnr,
      lt_werks_rng TYPE range_t_werks.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matnr> TYPE matnr,
      <lv_werks> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT:
            iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>,
            iv_fnam_werks OF STRUCTURE <ls_tab> TO <lv_werks>.

          CHECK <lv_matnr> IS ASSIGNED AND
                <lv_matnr> IS NOT INITIAL AND
                <lv_werks> IS ASSIGNED AND
                <lv_werks> IS NOT INITIAL AND
                ( NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <lv_matnr> werks = <lv_werks> ] ) ).

          COLLECT VALUE:
            range_s_matnr(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_matnr>
            ) INTO lt_matnr_rng,

            range_s_werks(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_werks>
            ) INTO lt_werks_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL AND
              lt_werks_rng IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(lt_marc)
            FROM marc
            WHERE matnr IN @lt_matnr_rng AND
                  werks IN @lt_werks_rng.
        ENDIF.

        LOOP AT lt_marc ASSIGNING FIELD-SYMBOL(<ls_marc>).
          CHECK NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <ls_marc>-matnr werks = <ls_marc>-werks ] ).
          INSERT <ls_marc> INTO TABLE gt_marc.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_marc.

    ENDTRY.

  ENDMETHOD.

  METHOD cache_marc_with_fixed_plant.

    DATA:
      lt_matnr_rng TYPE range_t_matnr.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matnr> TYPE matnr.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>.

          CHECK <lv_matnr> IS ASSIGNED AND
                <lv_matnr> IS NOT INITIAL AND
                ( NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <lv_matnr> werks = iv_werks ] ) ).

          COLLECT VALUE range_s_matnr(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = <lv_matnr>
          ) INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(lt_marc)
            FROM marc
            WHERE matnr IN @lt_matnr_rng AND
                  werks EQ @iv_werks.
        ENDIF.

        LOOP AT lt_marc ASSIGNING FIELD-SYMBOL(<ls_marc>).
          CHECK NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <ls_marc>-matnr werks = <ls_marc>-werks ] ).
          INSERT <ls_marc> INTO TABLE gt_marc.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_marc_wfp.

    ENDTRY.

  ENDMETHOD.

  METHOD cache_mard.

    DATA:
      lt_lgort_rng TYPE ranges_lgort_tt,
      lt_matnr_rng TYPE range_t_matnr,
      lt_werks_rng TYPE range_t_werks.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_lgort> TYPE any,
      <lv_matnr> TYPE matnr,
      <lv_werks> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.


        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT:
            iv_fnam_lgort OF STRUCTURE <ls_tab> TO <lv_lgort>,
            iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>,
            iv_fnam_werks OF STRUCTURE <ls_tab> TO <lv_werks>.

          CHECK
            <lv_lgort> IS ASSIGNED AND
            <lv_lgort> IS NOT INITIAL AND
            <lv_matnr> IS ASSIGNED AND
            <lv_matnr> IS NOT INITIAL AND
            <lv_werks> IS ASSIGNED AND
            <lv_werks> IS NOT INITIAL AND
            ( NOT line_exists(
                gt_mard[
                  KEY primary_key COMPONENTS
                  matnr = <lv_matnr>
                  werks = <lv_werks>
                  lgort = <lv_lgort>
                ]
              )
            ).

          COLLECT VALUE:
            range_lgort_s(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_lgort>
            ) INTO lt_lgort_rng,

            range_s_matnr(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_matnr>
            ) INTO lt_matnr_rng,

            range_s_werks(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_werks>
            ) INTO lt_werks_rng.

        ENDLOOP.

        IF
          lt_lgort_rng IS NOT INITIAL AND
          lt_matnr_rng IS NOT INITIAL AND
          lt_werks_rng IS NOT INITIAL.

          SELECT matnr, werks, lgort
            FROM mard
            WHERE
              matnr IN @lt_matnr_rng AND
              werks IN @lt_werks_rng AND
              lgort IN @lt_lgort_rng AND
              (
                lvorm EQ @space OR
                lvorm IS NULL
              )
            INTO TABLE @DATA(lt_mard).
        ENDIF.

        LOOP AT lt_mard ASSIGNING FIELD-SYMBOL(<ls_mard>).
          CHECK NOT line_exists(
            gt_mard[
              KEY primary_key COMPONENTS
              matnr = <ls_mard>-matnr
              werks = <ls_mard>-werks
              lgort = <ls_mard>-lgort
            ]
          ).

          INSERT <ls_mard> INTO TABLE gt_mard.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_mard.

    ENDTRY.

  ENDMETHOD.

  METHOD cache_mlan.

    DATA:
      lt_matnr_rng TYPE range_t_matnr.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matnr> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>.

          CHECK <lv_matnr> IS ASSIGNED AND
                <lv_matnr> IS NOT INITIAL AND
                ( NOT line_exists( gt_mlan[ KEY primary_key COMPONENTS matnr = <lv_matnr> aland = iv_aland ] ) ).

          COLLECT VALUE range_s_matnr(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = <lv_matnr>
          ) INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_mlan
            FROM mlan
            WHERE
              matnr IN @lt_matnr_rng AND
              aland EQ @iv_aland.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_mlan.

    ENDTRY.

  ENDMETHOD.

  METHOD cache_mvke.

    DATA:
      lt_matnr_rng TYPE range_t_matnr,
      lt_vkorg_rng TYPE shp_vkorg_range_t,
      lt_vtweg_rng TYPE shp_vtweg_range_t.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matnr> TYPE matnr,
      <lv_vkorg> TYPE any,
      <lv_vtweg> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT:
            iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>,
            iv_fnam_vkorg OF STRUCTURE <ls_tab> TO <lv_vkorg>,
            iv_fnam_vtweg OF STRUCTURE <ls_tab> TO <lv_vtweg>.

          CHECK <lv_matnr> IS ASSIGNED AND
                <lv_matnr> IS NOT INITIAL AND
                <lv_vkorg> IS ASSIGNED AND
                <lv_vkorg> IS NOT INITIAL AND
                <lv_vtweg> IS ASSIGNED AND
                <lv_vtweg> IS NOT INITIAL AND
                ( NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <lv_matnr> vkorg = <lv_vkorg> vtweg = <lv_vtweg> ] ) ).

          COLLECT VALUE:
            range_s_matnr(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_matnr>
            ) INTO lt_matnr_rng,

            shp_vkorg_range(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_vkorg>
            ) INTO lt_vkorg_rng,

            shp_vtweg_range(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_vtweg>
            ) INTO lt_vtweg_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL AND
           lt_vkorg_rng IS NOT INITIAL AND
           lt_vtweg_rng IS NOT INITIAL.

          SELECT * INTO TABLE @DATA(lt_mvke)
            FROM mvke
            WHERE matnr IN @lt_matnr_rng AND
                  vkorg IN @lt_vkorg_rng AND
                  vtweg IN @lt_vtweg_rng.


          LOOP AT lt_mvke ASSIGNING FIELD-SYMBOL(<ls_mvke>).
            CHECK NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <ls_mvke>-matnr vkorg = <ls_mvke>-vkorg vtweg = <ls_mvke>-vtweg ] ).
            INSERT <ls_mvke> INTO TABLE gt_mvke.
          ENDLOOP.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_marc.

    ENDTRY.

  ENDMETHOD.

  METHOD cache_mvke_with_fixed_dstchn.

    DATA:
      lt_matnr_rng TYPE range_t_matnr.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matnr> TYPE clike.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>.

          CHECK
            <lv_matnr> IS ASSIGNED AND
            <lv_matnr> IS NOT INITIAL AND
            (
              NOT line_exists(
                gt_mvke[
                  KEY primary_key COMPONENTS
                  matnr = <lv_matnr>
                  vkorg = iv_vkorg
                  vtweg = iv_vtweg
                ]
              )
            ).

          COLLECT VALUE range_s_matnr(
              option = zcl_bc_ddic_toolkit=>c_option_eq
              sign   = zcl_bc_ddic_toolkit=>c_sign_i
              low    = <lv_matnr>
            ) INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS INITIAL.
          RETURN.
        ENDIF.

        SELECT * INTO TABLE @DATA(lt_mvke)
          FROM mvke
          WHERE matnr IN @lt_matnr_rng AND
                vkorg EQ @iv_vkorg AND
                vtweg EQ @iv_vtweg.

        LOOP AT lt_mvke ASSIGNING FIELD-SYMBOL(<ls_mvke>).
          CHECK NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <ls_mvke>-matnr vkorg = <ls_mvke>-vkorg vtweg = <ls_mvke>-vtweg ] ).
          INSERT <ls_mvke> INTO TABLE gt_mvke.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_marc.

    ENDTRY.

  ENDMETHOD.

  METHOD ensure_mard_existence_bulk.

    FIELD-SYMBOLS <lt_tab> TYPE ANY TABLE.

    CHECK ir_tab IS NOT INITIAL.

    ASSERT
      iv_fnam_matnr IS NOT INITIAL AND
      iv_fnam_werks IS NOT INITIAL AND
      iv_fnam_lgort IS NOT INITIAL.


    ASSIGN ir_tab->* TO <lt_tab>.
    IF <lt_tab> IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_cache_mard EQ abap_true.
      cache_mard(
        ir_tab        = ir_tab
        iv_fnam_matnr = iv_fnam_matnr
        iv_fnam_werks = iv_fnam_werks
        iv_fnam_lgort = iv_fnam_lgort
      ).
    ENDIF.

    LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

      ASSIGN COMPONENT:
        iv_fnam_matnr OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_matnr>),
        iv_fnam_werks OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_werks>),
        iv_fnam_lgort OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_lgort>).

      ASSERT
        <lv_matnr> IS ASSIGNED AND
        <lv_werks> IS ASSIGNED AND
        <lv_lgort> IS ASSIGNED.

      CHECK
        <lv_matnr> IS NOT INITIAL AND
        <lv_werks> IS NOT INITIAL AND
        <lv_lgort> IS NOT INITIAL.

      CHECK NOT line_exists(
        gt_mard[
          KEY primary_key COMPONENTS
          matnr = <lv_matnr>
          werks = <lv_werks>
          lgort = <lv_lgort>
        ]
      ).

      DATA(lo_cx) = NEW zcx_mm_material_stg_loc_def(
        matnr    = <lv_matnr>
        werks    = <lv_werks>
        lgort    = <lv_lgort>
        previous = NEW zcx_bc_table_content(
          textid   = zcx_bc_table_content=>entry_missing
          objectid = |{ <lv_matnr> } { <lv_werks> } { <lv_lgort> }|
          tabname  = c_tabname_mard
        )
      ).

      IF io_log IS NOT INITIAL.
        io_log->add_exception( lo_cx ).
      ENDIF.

      RAISE RESUMABLE EXCEPTION lo_cx.

    ENDLOOP.

  ENDMETHOD.

  METHOD fill_itab_with_maktx.

    DATA lt_matnr TYPE matnr_tty.
    FIELD-SYMBOLS <lt_itab> TYPE STANDARD TABLE.

    " Hazırlık """"""""""""""""""""""""""""""""""""""""""""""""""""""

    ASSERT ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.
    IF <lt_itab> IS INITIAL.
      RETURN.
    ENDIF.

    " Malzeme metinlerini çek """""""""""""""""""""""""""""""""""""""

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).
      ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_matnr>).
      ASSERT sy-subrc EQ 0.
      CHECK <lv_matnr> IS NOT INITIAL.
      APPEND <lv_matnr> TO lt_matnr.
    ENDLOOP.

    IF lt_matnr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT matnr, spras, maktx
      FROM makt
      FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr EQ @lt_matnr-table_line
      INTO TABLE @DATA(lt_makt).

    IF lt_makt IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_makt BY matnr spras. " Binary Search

    " Geri yaz """"""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT <lt_itab> ASSIGNING <ls_itab>.

      ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_itab> TO <lv_matnr>.
      CHECK <lv_matnr> IS NOT INITIAL.
      ASSIGN COMPONENT iv_fnam_maktx OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_maktx>).
      ASSERT sy-subrc EQ 0.

      READ TABLE lt_makt
        ASSIGNING FIELD-SYMBOL(<ls_makt>)
        WITH KEY
          matnr = <lv_matnr>
          spras = sy-langu
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lv_maktx> = <ls_makt>-maktx.
        CONTINUE.
      ENDIF.

      READ TABLE lt_makt
        ASSIGNING <ls_makt>
        WITH KEY matnr = <lv_matnr>
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lv_maktx> = <ls_makt>-maktx.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_block_status.

    ASSIGN gt_block_cache[
        KEY primary_key COMPONENTS
        vkorg = iv_vkorg
        vtweg = iv_vtweg
      ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_block_cache(
        vkorg = iv_vkorg
        vtweg = iv_vtweg
      ).

      DATA(lv_vmsta) = COND vmsta(
        WHEN ls_cache-vkorg IS NOT INITIAL AND
             ls_cache-vtweg IS NOT INITIAL
        THEN get_mvke(
               iv_matnr      = gs_def-matnr
               iv_vkorg      = ls_cache-vkorg
               iv_vtweg      = ls_cache-vtweg
               iv_must_exist = abap_true
             )-vmsta
         ELSE gs_def-mstav
      ).

      IF lv_vmsta IS NOT INITIAL.
        SELECT SINGLE *
          FROM tvms
          WHERE vmsta EQ @lv_vmsta
          INTO @ls_cache-tvms.
      ENDIF.

      INSERT ls_cache
        INTO TABLE gt_block_cache
        ASSIGNING <ls_cache>.

    ENDIF.

    rr_status = REF #( <ls_cache> ).

  ENDMETHOD.

  METHOD get_gos_url_list.

    rt_list = zcl_bc_gos_toolkit=>get_url_list(
      is_key      = VALUE #(
        classname = c_gos_classname
        objkey    = gs_def-matnr
      )
    ).

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS matnr = iv_matnr ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( matnr = iv_matnr ).
      ls_multiton-obj = NEW #( ).

      SELECT SINGLE * INTO @ls_multiton-obj->gs_def
        FROM mara
        WHERE matnr EQ @ls_multiton-matnr.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-matnr )
            tabname  = c_tabname_mara.
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.


  METHOD get_maktx.

    DATA ls_makt TYPE makt.

    ASSIGN gt_makt[ KEY primary_key COMPONENTS
      spras = iv_spras
      matnr = iv_matnr
    ] TO FIELD-SYMBOL(<ls_makt>).

    IF sy-subrc NE 0.
      SELECT SINGLE * FROM makt INTO ls_makt
        WHERE spras EQ iv_spras
          AND matnr EQ iv_matnr.
      ls_makt-spras = iv_spras.
      ls_makt-matnr = iv_matnr.
      INSERT ls_makt INTO TABLE gt_makt ASSIGNING <ls_makt>.
    ENDIF.

    rv_maktx = <ls_makt>-maktx.

  ENDMETHOD.


  METHOD get_mara.

    DATA ls_mara TYPE mara.

    ASSIGN gt_mara[ KEY primary_key COMPONENTS
      matnr = iv_matnr
    ] TO FIELD-SYMBOL(<ls_mara>).

    IF sy-subrc NE 0.
      SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr EQ iv_matnr.
      ls_mara-matnr = iv_matnr.
      INSERT ls_mara INTO TABLE gt_mara ASSIGNING <ls_mara>.
    ENDIF.

    rs_mara = <ls_mara>.

  ENDMETHOD.


  METHOD get_marc.

    DATA ls_marc TYPE marc.

    ASSIGN gt_marc[ KEY primary_key COMPONENTS
      matnr = iv_matnr
      werks = iv_werks
    ] TO FIELD-SYMBOL(<ls_marc>).

    IF sy-subrc NE 0.

      SELECT SINGLE * FROM marc INTO ls_marc
        WHERE matnr EQ iv_matnr
          AND werks EQ iv_werks.

      DATA(ls_marc_subrc) = VALUE t_marc_subrc(
        matnr = iv_matnr
        werks = iv_werks
        subrc = sy-subrc
      ).

      INSERT ls_marc_subrc INTO TABLE gt_marc_subrc.

      ls_marc-matnr = iv_matnr.
      ls_marc-werks = iv_werks.
      INSERT ls_marc INTO TABLE gt_marc ASSIGNING <ls_marc>.

    ENDIF.

    rs_marc = <ls_marc>.

  ENDMETHOD.

  METHOD get_marm.

    ASSIGN gt_marm[
        KEY primary_key COMPONENTS
        matnr = iv_matnr
      ] TO FIELD-SYMBOL(<ls_marm>).

    IF sy-subrc NE 0.

      DATA(ls_marm) = VALUE t_marm_cache( matnr = iv_matnr ).

      SELECT *
        FROM marm
        WHERE matnr EQ @ls_marm-matnr
        INTO TABLE @ls_marm-marm.

      INSERT ls_marm
        INTO TABLE gt_marm
        ASSIGNING <ls_marm>.

    ENDIF.

    rt_marm = <ls_marm>-marm.

  ENDMETHOD.

  METHOD get_mlan.

    DATA ls_mlan TYPE mlan.

    ASSIGN gt_mlan[ KEY primary_key COMPONENTS
        matnr = iv_matnr
        aland = iv_aland
      ] TO FIELD-SYMBOL(<ls_mlan>).

    IF sy-subrc NE 0.
      SELECT SINGLE * FROM mlan
        WHERE
          matnr EQ @iv_matnr AND
          aland EQ @iv_aland
        INTO @ls_mlan.
      ls_mlan-matnr = iv_matnr.
      ls_mlan-aland = iv_aland.
      INSERT ls_mlan INTO TABLE gt_mlan ASSIGNING <ls_mlan>.
    ENDIF.

    rs_mlan = <ls_mlan>.

  ENDMETHOD.

  METHOD get_mvke.

    DATA ls_mvke TYPE mvke.

    ASSIGN gt_mvke[ KEY primary_key COMPONENTS
      matnr = iv_matnr
      vkorg = iv_vkorg
      vtweg = iv_vtweg
    ] TO FIELD-SYMBOL(<ls_mvke>).

    IF sy-subrc NE 0.
      CLEAR ls_mvke.
      SELECT SINGLE * FROM mvke INTO ls_mvke
        WHERE matnr = iv_matnr
          AND vkorg = iv_vkorg
          AND vtweg = iv_vtweg.

      IF sy-subrc NE 0 AND
         iv_must_exist EQ abap_true.

        RAISE EXCEPTION TYPE zcx_sd_zm_material
          EXPORTING
            matnr  = iv_matnr
            textid = zcx_sd_zm_material=>undefined_for_dist_chan
            vkorg  = iv_vkorg
            vtweg  = iv_vtweg.

      ENDIF.

      ls_mvke-matnr = iv_matnr.
      ls_mvke-vkorg = iv_vkorg.
      ls_mvke-vtweg = iv_vtweg.
      INSERT ls_mvke INTO TABLE gt_mvke ASSIGNING <ls_mvke>.
    ENDIF.

    rs_mvke = <ls_mvke>.

  ENDMETHOD.

  METHOD get_tax_rate.

    CLEAR: ev_kdv, ev_otv.

    DATA(ls_mlan) = zcl_mm_material=>get_mlan(
      iv_matnr = iv_matnr
      iv_aland = iv_aland
    ).

    ev_kdv = SWITCH #(
      ls_mlan-taxm1
      WHEN 1 THEN '1'
      WHEN 2 THEN '8'
      WHEN 3 THEN '18'
    ).

    ev_otv = SWITCH #(
      ls_mlan-taxm2
      WHEN 0 THEN '0'
      WHEN 1 THEN '20'
    ).

  ENDMETHOD.

  METHOD ensure_batch_existence.

    ASSIGN gt_batch_existence_cache[ KEY primary_key COMPONENTS charg = iv_charg
                                   ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.
      DATA(ls_cache) = VALUE t_batch_existence_cache( charg = iv_charg ).

      SELECT SINGLE mandt FROM mch1
             WHERE matnr EQ @gs_def-matnr AND
                   charg EQ @ls_cache-charg AND
                   lvorm EQ @abap_false
             INTO @sy-mandt ##write_ok .

      IF sy-subrc NE 0.
        ls_cache-cx = NEW #( table_name = conv #( c_tabname_mch1 )
                             entry_name = |{ gs_def-matnr } { ls_cache-charg }| ).
      ENDIF.

      INSERT ls_cache INTO TABLE gt_batch_existence_cache ASSIGNING <ls_cache>.
    ENDIF.

    IF <ls_cache>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-cx.
    ENDIF.

  ENDMETHOD.

  METHOD ensure_material_def_in_plant.

    CHECK is_material_defined_in_plant( iv_werks ) EQ abap_false.

    RAISE EXCEPTION TYPE zcx_mm_material_plant
      EXPORTING
        matnr    = gs_def-matnr
        werks    = iv_werks
        previous = NEW zcx_bc_table_content(
          textid   = zcx_bc_table_content=>entry_missing
          objectid = |{ gs_def-matnr } { iv_werks }|
          tabname  = c_tabname_marc
        ).

  ENDMETHOD.

  METHOD ensure_not_blocked_for_order.

    CHECK is_blocked_for_order(
        iv_vkorg = iv_vkorg
        iv_vtweg = iv_vtweg
      ) EQ abap_true.

    RAISE EXCEPTION TYPE zcx_sd_zm_material
      EXPORTING
        textid = zcx_sd_zm_material=>blocked_for_dist_chan
        matnr  = gs_def-matnr
        vkorg  = iv_vkorg
        vtweg  = iv_vtweg.

  ENDMETHOD.

  METHOD is_blocked_for_order.

    DATA(lr_block_status) = get_block_status(
      iv_vkorg = iv_vkorg
      iv_vtweg = iv_vtweg
    ).

    rv_blocked = xsdbool( lr_block_status->tvms-spvbc EQ c_fedia_blocked ).

  ENDMETHOD.

  METHOD is_material_defined_in_plant.

    get_marc(
      iv_matnr = gs_def-matnr
      iv_werks = iv_werks
    ).

    rv_defined = COND #(
      WHEN NOT line_exists(
          gt_marc_subrc[
            KEY primary_key COMPONENTS
            matnr = gs_def-matnr
            werks = iv_werks
          ]
        )
      THEN abap_false

      WHEN gt_marc_subrc[
            KEY primary_key COMPONENTS
            matnr = gs_def-matnr
            werks = iv_werks
        ]-subrc EQ 0
      THEN abap_true

      ELSE abap_false

    ).

  ENDMETHOD.

ENDCLASS.