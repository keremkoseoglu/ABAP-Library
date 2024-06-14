CLASS zcl_mm_material DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: tt_marm TYPE STANDARD TABLE OF marm WITH DEFAULT KEY,

           BEGIN OF t_valuation_via_plant,
             werks TYPE t001w-werks,
             hrkft TYPE mbew-hrkft,
           END OF t_valuation_via_plant,

           BEGIN OF t_warehouse,
             lgnum TYPE mlgn-lgnum,
             lgbkz TYPE mlgn-lgbkz,
             ltkze TYPE mlgn-ltkze,
             ltkza TYPE mlgn-ltkza,
           END OF t_warehouse.

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
      c_tabname_mch1 TYPE tabname   VALUE 'MCH1'. "#EC CI_USAGE_OK[2370131]

    DATA gs_def TYPE mara READ-ONLY.

    CLASS-METHODS:
      cache_maktx
        IMPORTING ir_tab             TYPE REF TO data
                  iv_fnam            TYPE fieldname DEFAULT c_fnam_matnr
                  iv_spras           TYPE sylangu   DEFAULT sy-langu
                  iv_conv_exit_input TYPE abap_bool DEFAULT abap_false
        RAISING   zcx_bc_class_method,

      cache_mara
        IMPORTING ir_tab  TYPE REF TO data
                  iv_fnam TYPE fieldname DEFAULT c_fnam_matnr
        RAISING   zcx_bc_class_method,

      cache_marc
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_fnam_werks TYPE fieldname DEFAULT c_fnam_werks
        RAISING   zcx_bc_class_method,

      cache_mard
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_fnam_werks TYPE fieldname DEFAULT c_fnam_werks
                  iv_fnam_lgort TYPE fieldname DEFAULT c_fnam_lgort
        RAISING   zcx_bc_class_method,

      cache_marc_with_fixed_plant
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_werks      TYPE werks_d
        RAISING   zcx_bc_class_method,

      cache_mlan
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_aland      TYPE aland     DEFAULT c_aland_def
        RAISING   zcx_bc_class_method,

      cache_mvke
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_fnam_vkorg TYPE fieldname DEFAULT c_fnam_vkorg
                  iv_fnam_vtweg TYPE fieldname DEFAULT c_fnam_vtweg
        RAISING   zcx_bc_class_method,

      cache_mvke_with_fixed_dstchn
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_vkorg      TYPE vkorg
                  iv_vtweg      TYPE vtweg
        RAISING   zcx_bc_class_method,

      cache_mvke_with_materials
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
        RAISING   zcx_bc_class_method,

      conversion_exit_output
        IMPORTING iv_matnr        TYPE clike
        RETURNING VALUE(rv_matnr) TYPE matnr,

      ensure_mard_existence_bulk
        IMPORTING ir_tab        TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname                   DEFAULT c_fnam_matnr
                  iv_fnam_werks TYPE fieldname                   DEFAULT c_fnam_werks
                  iv_fnam_lgort TYPE fieldname                   DEFAULT c_fnam_lgort
                  iv_cache_mard TYPE abap_bool                   DEFAULT abap_true
                  io_log        TYPE REF TO zcl_bc_applog_facade OPTIONAL
        RAISING   zcx_bc_class_method
                  RESUMABLE(zcx_mm_material_stg_loc_def),

      fill_itab_with_maktx
        IMPORTING ir_itab       TYPE REF TO data
                  iv_fnam_matnr TYPE fieldname DEFAULT c_fnam_matnr
                  iv_fnam_maktx TYPE fieldname DEFAULT c_fnam_maktx,

      get_maktx
        IMPORTING iv_matnr        TYPE matnr
                  iv_spras        TYPE spras DEFAULT sy-langu
        RETURNING VALUE(rv_maktx) TYPE maktx,

      get_mara
        IMPORTING iv_matnr       TYPE matnr
        RETURNING VALUE(rs_mara) TYPE mara,

      get_mbew
        IMPORTING iv_matnr       TYPE mbew-matnr
                  iv_bwkey       TYPE mbew-bwkey
        RETURNING VALUE(rs_mbew) TYPE mbew,

      get_mlan
        IMPORTING iv_matnr       TYPE matnr
                  iv_aland       TYPE aland DEFAULT c_aland_def
        RETURNING VALUE(rs_mlan) TYPE mlan,

      get_marm
        IMPORTING iv_matnr       TYPE matnr
        RETURNING VALUE(rt_marm) TYPE tt_marm,

      get_marc
        IMPORTING iv_matnr       TYPE matnr
                  iv_werks       TYPE werks_d
        RETURNING VALUE(rs_marc) TYPE marc,

      get_mvke
        IMPORTING iv_matnr       TYPE matnr
                  iv_vkorg       TYPE vkorg
                  iv_vtweg       TYPE vtweg
                  iv_must_exist  TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rs_mvke) TYPE mvke
        RAISING   zcx_sd_zm_material,

      get_instance
        IMPORTING iv_matnr      TYPE matnr
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_material
        RAISING   zcx_mm_material.

    CLASS-METHODS conversion_exit_input
      IMPORTING matnr         TYPE clike
      RETURNING VALUE(result) TYPE matnr
      RAISING   zcx_mm_material.

    METHODS:
      attach_gos_url
        IMPORTING is_url TYPE zcl_bc_gos_toolkit=>t_url
        RAISING   zcx_bc_gos_url_attach,

      ensure_batch_existence
        IMPORTING iv_charg TYPE mch1-charg
        RAISING   cx_no_entry_in_table,

      ensure_material_def_in_plant
        IMPORTING iv_werks TYPE werks_d
        RAISING   zcx_mm_material_plant,

      ensure_not_blocked_for_order
        IMPORTING iv_vkorg TYPE vkorg
                  iv_vtweg TYPE vtweg
        RAISING   zcx_sd_zm_material,

      get_gos_url_list
        RETURNING VALUE(rt_list) TYPE zcl_bc_gos_toolkit=>tt_url
        RAISING   zcx_bc_gos_doc_content,

      get_sales_uom
        IMPORTING iv_vkorg        TYPE vkorg OPTIONAL
                  iv_vtweg        TYPE vtweg OPTIONAL
        RETURNING VALUE(rv_vrkme) TYPE vrkme,

      get_valuation_data_via_plant
        IMPORTING iv_werks            TYPE werks_d
        RETURNING VALUE(rs_valuation) TYPE t_valuation_via_plant
        RAISING   cx_no_entry_in_table,

      get_warehouse_data
        IMPORTING iv_lgnum            TYPE mlgn-lgnum
        RETURNING VALUE(rs_warehouse) TYPE t_warehouse
        RAISING   cx_no_entry_in_table,

      is_blocked_for_order
        IMPORTING iv_vkorg          TYPE vkorg OPTIONAL
                  iv_vtweg          TYPE vtweg OPTIONAL
        RETURNING VALUE(rv_blocked) TYPE abap_bool
        RAISING   zcx_sd_zm_material,

      is_defined_in_any_sales_org
        IMPORTING it_vkorg          TYPE tdt_vkorg
        RETURNING VALUE(rv_defined) TYPE abap_bool,

      is_defined_in_any_warehouse RETURNING VALUE(rv_defined) TYPE abap_bool,

      is_material_defined_in_plant
        IMPORTING iv_werks          TYPE werks_d
        RETURNING VALUE(rv_defined) TYPE abap_bool.

    METHODS convert_quantity
      IMPORTING from_uom      TYPE mara-meins
                to_uom        TYPE mara-meins
                !quantity     TYPE mengv13
      RETURNING VALUE(result) TYPE mengv13
      RAISING   zcx_mm_material_unit_conv.

    METHODS convert_quantity_to_base_uom
      IMPORTING from_uom      TYPE mara-meins
                !quantity     TYPE mengv13
      RETURNING VALUE(result) TYPE mengv13
      RAISING   zcx_mm_material_unit_conv.

    METHODS conv_quan_to_base_uom_on_per
      IMPORTING from_menge      TYPE rke2_vvmi0
                from_meins      TYPE meins
                jahrper         TYPE jahrper
      EXPORTING converted_menge TYPE rke2_vvmi2
                converted_meins TYPE ekpo-meins
      RAISING   zcx_mm_material_unit_conv.

    METHODS conv_quan_fr_base_uom_on_per
      IMPORTING from_menge      TYPE rke2_vvmi0
                to_meins        TYPE meins
                jahrper         TYPE jahrper
      EXPORTING converted_menge TYPE rke2_vvmi2
                base_uom        TYPE meins
      RAISING   zcx_mm_material_unit_conv.

    METHODS get_net_weight_on_period
      IMPORTING jahrper  TYPE jahrper
                to_gewei TYPE gewei OPTIONAL
      EXPORTING ntgew    TYPE float
                gewei    TYPE mara-gewei
      RAISING   zcx_mm_material_unit_conv.

    METHODS get_net_weight_on_date
      IMPORTING !datum   TYPE datum
                to_gewei TYPE gewei OPTIONAL
      EXPORTING ntgew    TYPE float
                gewei    TYPE mara-gewei
      RAISING   zcx_mm_material_unit_conv.

    METHODS get_tax_rate
      IMPORTING iv_aland TYPE aland DEFAULT c_aland_def
      EXPORTING ev_kdv   TYPE clike
                ev_otv   TYPE clike.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_marc_subrc,
        matnr TYPE marc-matnr,
        werks TYPE marc-werks,
        subrc TYPE sysubrc,
      END OF t_marc_subrc.
    TYPES tt_marc_subrc TYPE HASHED TABLE OF t_marc_subrc
              WITH UNIQUE KEY primary_key COMPONENTS matnr werks.
    TYPES:
      BEGIN OF t_mard,
        matnr TYPE mard-matnr,
        werks TYPE mard-werks,
        lgort TYPE mard-lgort,
      END OF t_mard.
    TYPES tt_mard TYPE HASHED TABLE OF t_mard
              WITH UNIQUE KEY primary_key COMPONENTS matnr werks lgort.
    TYPES:
      BEGIN OF t_marm_cache,
        matnr TYPE marm-matnr,
        marm  TYPE tt_marm,
      END OF t_marm_cache.
    TYPES tt_marm_cache TYPE HASHED TABLE OF t_marm_cache
              WITH UNIQUE KEY primary_key COMPONENTS matnr.
    TYPES tt_mlan_cache TYPE HASHED TABLE OF mlan
              WITH UNIQUE KEY primary_key COMPONENTS matnr aland.
    TYPES:
      BEGIN OF t_multiton,
        matnr TYPE matnr,
        obj   TYPE REF TO zcl_mm_material,
      END OF t_multiton.
    TYPES tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS matnr.
    TYPES tt_mvke     TYPE HASHED TABLE OF mvke WITH UNIQUE KEY primary_key COMPONENTS matnr vkorg vtweg.
    TYPES:
      BEGIN OF t_block_cache,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
        tvms  TYPE tvms,
      END OF t_block_cache.
    TYPES tt_block_cache TYPE HASHED TABLE OF t_block_cache
              WITH UNIQUE KEY primary_key COMPONENTS vkorg vtweg.
    TYPES:
      BEGIN OF t_batch_existence_cache,
        charg TYPE mch1-charg,
        cx    TYPE REF TO cx_no_entry_in_table,
      END OF t_batch_existence_cache.
    TYPES tt_batch_existence_cache TYPE HASHED TABLE OF t_batch_existence_cache WITH UNIQUE KEY primary_key COMPONENTS charg.
    TYPES tt_warehouse             TYPE HASHED TABLE OF t_warehouse WITH UNIQUE KEY primary_key COMPONENTS lgnum.
    TYPES tt_valuation_via_plant   TYPE HASHED TABLE OF t_valuation_via_plant
                                   WITH UNIQUE KEY primary_key COMPONENTS werks.

    TYPES: BEGIN OF t_tax_rate_cache,
             aland TYPE mlan-aland,
             kdv   TYPE char2,
             otv   TYPE char2,
           END OF t_tax_rate_cache,

           tt_tax_rate_cache TYPE HASHED TABLE OF t_tax_rate_cache
                             WITH UNIQUE KEY primary_key COMPONENTS aland.

    CONSTANTS:
      BEGIN OF c_tabname,
        master    TYPE tabname VALUE 'MARA',
        stock     TYPE tabname VALUE 'MARD',
        valuation TYPE tabname VALUE 'MBEW',
        warehouse TYPE tabname VALUE 'MLGN',
      END OF c_tabname.
    CONSTANTS c_clsname_me          TYPE seoclsname          VALUE 'ZCL_MM_MATERIAL' ##NO_TEXT.
    CONSTANTS c_fedia_blocked       TYPE spvbc               VALUE 'B' ##NO_TEXT.
    CONSTANTS c_gos_classname       TYPE bapibds01-classname VALUE 'BUS1001006' ##NO_TEXT.
    CONSTANTS c_meth_cache_mara     TYPE seocpdname          VALUE 'CACHE_MARA' ##NO_TEXT.
    CONSTANTS c_meth_cache_marc     TYPE seocpdname          VALUE 'CACHE_MARC' ##NO_TEXT.
    CONSTANTS c_meth_cache_marc_wfp TYPE seocpdname          VALUE 'CACHE_MARC_WITH_FIXED_PLANT' ##NO_TEXT.
    CONSTANTS c_meth_cache_mard     TYPE seocpdname          VALUE 'CACHE_MARD' ##NO_TEXT.
    CONSTANTS c_meth_cache_mlan     TYPE seocpdname          VALUE 'CACHE_MLAN' ##NO_TEXT.
    CONSTANTS c_meth_cache_wgbez    TYPE seocpdname          VALUE 'CACHE_MAKTX' ##NO_TEXT.

    DATA gt_batch_existence_cache TYPE tt_batch_existence_cache.
    DATA gt_block_cache           TYPE tt_block_cache.
    DATA gt_valuation_via_plant   TYPE tt_valuation_via_plant.
    DATA gt_warehouse             TYPE tt_warehouse.
    DATA gv_warehouse_read        TYPE abap_bool.
    DATA gt_tax_rates             TYPE tt_tax_rate_cache.
    DATA go_uom_hist_set          TYPE REF TO zif_mm_mat_uom_hist_set.

    CLASS-DATA gt_makt       TYPE HASHED TABLE OF makt
                   WITH UNIQUE KEY primary_key COMPONENTS spras matnr.
    CLASS-DATA gt_mara       TYPE HASHED TABLE OF mara
                   WITH UNIQUE KEY primary_key COMPONENTS matnr.
    CLASS-DATA gt_marc       TYPE HASHED TABLE OF marc
                   WITH UNIQUE KEY primary_key COMPONENTS matnr werks.
    CLASS-DATA gt_mbew       TYPE HASHED TABLE OF mbew
                         WITH UNIQUE KEY primary_key COMPONENTS matnr bwkey.
    CLASS-DATA gt_marc_subrc TYPE tt_marc_subrc.
    CLASS-DATA gt_mard       TYPE tt_mard.
    CLASS-DATA gt_marm       TYPE tt_marm_cache.
    CLASS-DATA gt_mlan       TYPE tt_mlan_cache.
    CLASS-DATA gt_mvke       TYPE tt_mvke.
    CLASS-DATA gt_multiton   TYPE tt_multiton.

    METHODS get_block_status
      IMPORTING iv_vkorg         TYPE vkorg
                iv_vtweg         TYPE vtweg
      RETURNING VALUE(rr_status) TYPE REF TO t_block_cache
      RAISING   zcx_sd_zm_material.

    METHODS read_warehouse_lazy.
ENDCLASS.


CLASS zcl_mm_material IMPLEMENTATION.
  METHOD attach_gos_url.
    zcl_bc_gos_toolkit=>attach_url( is_key = VALUE #( classname = c_gos_classname
                                                      objkey    = gs_def-matnr )
                                    is_url = is_url ).
  ENDMETHOD.

  METHOD cache_maktx.
    DATA:
      lv_matnr     TYPE matnr,
      lt_matnr_rng TYPE range_t_matnr.

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

          CHECK     <lv_matnr_raw> IS ASSIGNED
                AND <lv_matnr_raw> IS NOT INITIAL.

          IF iv_conv_exit_input = abap_true.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING  input        = <lv_matnr_raw>
              IMPORTING  output       = lv_matnr
              EXCEPTIONS length_error = 1
                         OTHERS       = 2.

            CHECK sy-subrc = 0.
          ELSE.
            lv_matnr                      "#EC CI_FLDEXT_OK[0002215424]
= <lv_matnr_raw>.
          ENDIF.

          CHECK NOT line_exists( gt_makt[ KEY primary_key COMPONENTS spras = iv_spras matnr = lv_matnr ] ).

          COLLECT VALUE range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = lv_matnr )
                  INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.

          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_makt
                 FROM makt
                 WHERE spras  = @iv_spras
                   AND matnr IN @lt_matnr_rng.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_wgbez ).

    ENDTRY.
  ENDMETHOD.

  METHOD cache_mara.
    DATA lt_matnr_rng TYPE range_t_matnr.

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

          CHECK           <lv_matnr> IS ASSIGNED
                AND       <lv_matnr> IS NOT INITIAL
                AND ( NOT line_exists( gt_mara[ KEY primary_key COMPONENTS matnr = <lv_matnr> ] ) ).

          COLLECT VALUE range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_matnr> )
                  INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_mara
                 FROM mara
                 WHERE matnr IN @lt_matnr_rng.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_mara ).

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

          CHECK           <lv_matnr> IS ASSIGNED
                AND       <lv_matnr> IS NOT INITIAL
                AND       <lv_werks> IS ASSIGNED
                AND       <lv_werks> IS NOT INITIAL
                AND ( NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <lv_matnr> werks = <lv_werks> ] ) ).

          COLLECT VALUE:
            range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                           sign   = zcl_bc_ddic_toolkit=>c_sign_i
                           low    = <lv_matnr> )
                  INTO lt_matnr_rng,

                  range_s_werks( option = zcl_bc_ddic_toolkit=>c_option_eq
                                 sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                 low    = <lv_werks> )
                  INTO lt_werks_rng.

        ENDLOOP.

        IF     lt_matnr_rng IS NOT INITIAL
           AND lt_werks_rng IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(lt_marc)
                 FROM marc
                 WHERE matnr IN @lt_matnr_rng
                   AND werks IN @lt_werks_rng.
        ENDIF.

        LOOP AT lt_marc ASSIGNING FIELD-SYMBOL(<ls_marc>).
          CHECK NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <ls_marc>-matnr werks = <ls_marc>-werks ] ).
          INSERT <ls_marc> INTO TABLE gt_marc.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_marc ).

    ENDTRY.
  ENDMETHOD.

  METHOD cache_marc_with_fixed_plant.
    DATA lt_matnr_rng TYPE range_t_matnr.

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

          CHECK           <lv_matnr> IS ASSIGNED
                AND       <lv_matnr> IS NOT INITIAL
                AND ( NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <lv_matnr> werks = iv_werks ] ) ).

          COLLECT VALUE range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_matnr> )
                  INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(lt_marc)
                 FROM marc
                 WHERE matnr IN @lt_matnr_rng
                   AND werks  = @iv_werks.
        ENDIF.

        LOOP AT lt_marc ASSIGNING FIELD-SYMBOL(<ls_marc>).
          CHECK NOT line_exists( gt_marc[ KEY primary_key COMPONENTS matnr = <ls_marc>-matnr werks = <ls_marc>-werks ] ).
          INSERT <ls_marc> INTO TABLE gt_marc.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_marc_wfp ).

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
                          <lv_lgort> IS ASSIGNED
                AND       <lv_lgort> IS NOT INITIAL
                AND       <lv_matnr> IS ASSIGNED
                AND       <lv_matnr> IS NOT INITIAL
                AND       <lv_werks> IS ASSIGNED
                AND       <lv_werks> IS NOT INITIAL
                AND ( NOT line_exists( gt_mard[
                                                         KEY primary_key
                                                         COMPONENTS matnr = <lv_matnr>
                                                                    werks = <lv_werks>
                                                                    lgort = <lv_lgort> ] ) ).

          COLLECT VALUE:
            range_lgort_s( option = zcl_bc_ddic_toolkit=>c_option_eq
                           sign   = zcl_bc_ddic_toolkit=>c_sign_i
                           low    = <lv_lgort> )
                  INTO lt_lgort_rng,

                  range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                 sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                 low    = <lv_matnr> )
                  INTO lt_matnr_rng,

                  range_s_werks( option = zcl_bc_ddic_toolkit=>c_option_eq
                                 sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                 low    = <lv_werks> )
                  INTO lt_werks_rng.

        ENDLOOP.

        IF
               lt_lgort_rng IS NOT INITIAL
           AND lt_matnr_rng IS NOT INITIAL
           AND lt_werks_rng IS NOT INITIAL.

          SELECT matnr, werks, lgort FROM mard
                 WHERE matnr IN @lt_matnr_rng
                   AND werks IN @lt_werks_rng
                   AND lgort IN @lt_lgort_rng
                   AND (
                         lvorm = @space
                 OR lvorm IS NULL )
                 INTO TABLE @DATA(lt_mard).
        ENDIF.

        LOOP AT lt_mard ASSIGNING FIELD-SYMBOL(<ls_mard>).
          CHECK NOT line_exists( gt_mard[
                                                   KEY primary_key
                                                   COMPONENTS matnr = <ls_mard>-matnr
                                                              werks = <ls_mard>-werks
                                                              lgort = <ls_mard>-lgort ] ).

          INSERT <ls_mard> INTO TABLE gt_mard.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_mard ).

    ENDTRY.
  ENDMETHOD.

  METHOD cache_mlan.
    DATA lt_matnr_rng TYPE range_t_matnr.

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

          CHECK           <lv_matnr> IS ASSIGNED
                AND       <lv_matnr> IS NOT INITIAL
                AND ( NOT line_exists( gt_mlan[ KEY primary_key COMPONENTS matnr = <lv_matnr> aland = iv_aland ] ) ).

          COLLECT VALUE range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_matnr> )
                  INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS NOT INITIAL.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_mlan
                 FROM mlan
                 WHERE matnr IN @lt_matnr_rng
                   AND aland  = @iv_aland.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_mlan ).

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

          CHECK           <lv_matnr> IS ASSIGNED
                AND       <lv_matnr> IS NOT INITIAL
                AND       <lv_vkorg> IS ASSIGNED
                AND       <lv_vkorg> IS NOT INITIAL
                AND       <lv_vtweg> IS ASSIGNED
                AND       <lv_vtweg> IS NOT INITIAL
                AND ( NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <lv_matnr>
                                                                           vkorg = <lv_vkorg>
                                                                           vtweg = <lv_vtweg> ] ) ).

          COLLECT VALUE:
            range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                           sign   = zcl_bc_ddic_toolkit=>c_sign_i
                           low    = <lv_matnr> )
                  INTO lt_matnr_rng,

                  shp_vkorg_range( option = zcl_bc_ddic_toolkit=>c_option_eq
                                   sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                   low    = <lv_vkorg> )
                  INTO lt_vkorg_rng,

                  shp_vtweg_range( option = zcl_bc_ddic_toolkit=>c_option_eq
                                   sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                   low    = <lv_vtweg> )
                  INTO lt_vtweg_rng.

        ENDLOOP.

        IF     lt_matnr_rng IS NOT INITIAL
           AND lt_vkorg_rng IS NOT INITIAL
           AND lt_vtweg_rng IS NOT INITIAL.

          SELECT * INTO TABLE @DATA(lt_mvke)
                 FROM mvke
                 WHERE matnr IN @lt_matnr_rng
                   AND vkorg IN @lt_vkorg_rng
                   AND vtweg IN @lt_vtweg_rng.

          LOOP AT lt_mvke ASSIGNING FIELD-SYMBOL(<ls_mvke>).
            CHECK NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <ls_mvke>-matnr
                                                                       vkorg = <ls_mvke>-vkorg
                                                                       vtweg = <ls_mvke>-vtweg ] ).
            INSERT <ls_mvke> INTO TABLE gt_mvke.
          ENDLOOP.
        ENDIF.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_marc ).

    ENDTRY.
  ENDMETHOD.

  METHOD cache_mvke_with_fixed_dstchn.
    DATA lt_matnr_rng TYPE range_t_matnr.

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
                      <lv_matnr> IS ASSIGNED
                AND   <lv_matnr> IS NOT INITIAL
                AND (
                NOT line_exists( gt_mvke[
                                                   KEY primary_key
                                                   COMPONENTS matnr = <lv_matnr>
                                                              vkorg = iv_vkorg
                                                              vtweg = iv_vtweg ] ) ).

          COLLECT VALUE range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_matnr> )
                  INTO lt_matnr_rng.

        ENDLOOP.

        IF lt_matnr_rng IS INITIAL.
          RETURN.
        ENDIF.

        SELECT * INTO TABLE @DATA(lt_mvke)
               FROM mvke
               WHERE matnr IN @lt_matnr_rng
                 AND vkorg  = @iv_vkorg
                 AND vtweg  = @iv_vtweg.

        LOOP AT lt_mvke ASSIGNING FIELD-SYMBOL(<ls_mvke>).
          CHECK NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <ls_mvke>-matnr
                                                                     vkorg = <ls_mvke>-vkorg
                                                                     vtweg = <ls_mvke>-vtweg ] ).
          INSERT <ls_mvke> INTO TABLE gt_mvke.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_marc ).

    ENDTRY.
  ENDMETHOD.

  METHOD cache_mvke_with_materials.
    DATA lt_matnr_rng TYPE range_t_matnr.

    FIELD-SYMBOLS: <lt_tab>   TYPE ANY TABLE,
                   <lv_matnr> TYPE clike.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).
          ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_tab> TO <lv_matnr>.

          COLLECT VALUE range_s_matnr( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_matnr> )
                  INTO lt_matnr_rng.
        ENDLOOP.

        IF lt_matnr_rng IS INITIAL.
          RETURN.
        ENDIF.

        SELECT * FROM mvke
               WHERE matnr IN @lt_matnr_rng
               INTO TABLE @DATA(lt_mvke).

        LOOP AT lt_mvke ASSIGNING FIELD-SYMBOL(<ls_mvke>).
          CHECK NOT line_exists( gt_mvke[ KEY primary_key COMPONENTS matnr = <ls_mvke>-matnr
                                                                     vkorg = <ls_mvke>-vkorg
                                                                     vtweg = <ls_mvke>-vtweg ] ).

          INSERT <ls_mvke> INTO TABLE gt_mvke.
        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_marc ).

    ENDTRY.
  ENDMETHOD.

  METHOD conversion_exit_output.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING input  = iv_matnr
      IMPORTING output = rv_matnr.
  ENDMETHOD.

  METHOD ensure_mard_existence_bulk.
    FIELD-SYMBOLS <lt_tab> TYPE ANY TABLE.

    CHECK ir_tab IS NOT INITIAL.

    ASSERT     iv_fnam_matnr IS NOT INITIAL
           AND iv_fnam_werks IS NOT INITIAL
           AND iv_fnam_lgort IS NOT INITIAL.

    ASSIGN ir_tab->* TO <lt_tab>.
    IF <lt_tab> IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_cache_mard = abap_true.
      cache_mard( ir_tab        = ir_tab
                  iv_fnam_matnr = iv_fnam_matnr
                  iv_fnam_werks = iv_fnam_werks
                  iv_fnam_lgort = iv_fnam_lgort ).
    ENDIF.

    LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

      ASSIGN COMPONENT: iv_fnam_matnr OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_matnr>),
                        iv_fnam_werks OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_werks>),
                        iv_fnam_lgort OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_lgort>).

      ASSERT     <lv_matnr> IS ASSIGNED
             AND <lv_werks> IS ASSIGNED
             AND <lv_lgort> IS ASSIGNED.

      CHECK     <lv_matnr> IS NOT INITIAL
            AND <lv_werks> IS NOT INITIAL
            AND <lv_lgort> IS NOT INITIAL.

      CHECK NOT line_exists( gt_mard[ KEY primary_key
                                      COMPONENTS matnr = <lv_matnr>
                                                 werks = <lv_werks>
                                                 lgort = <lv_lgort> ] ).

      DATA(lo_cx) = NEW zcx_mm_material_stg_loc_def( matnr    = <lv_matnr>
                                                     werks    = <lv_werks>
                                                     lgort    = <lv_lgort>
                                                     previous = NEW zcx_bc_table_content(
                                                         textid   = zcx_bc_table_content=>entry_missing
                                                         objectid = |{ <lv_matnr> } { <lv_werks> } { <lv_lgort> }|
                                                         tabname  = c_tabname-stock ) ).

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
      ASSERT sy-subrc = 0.
      CHECK <lv_matnr> IS NOT INITIAL.
      APPEND <lv_matnr>                   "#EC CI_FLDEXT_OK[0002215424]
             TO lt_matnr.
    ENDLOOP.

    IF lt_matnr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT matnr, spras, maktx FROM makt
           FOR ALL ENTRIES IN @lt_matnr
           WHERE matnr = @lt_matnr-table_line
           INTO TABLE @DATA(lt_makt).

    IF lt_makt IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_makt BY matnr
                    spras. " Binary Search

    " Geri yaz """"""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT <lt_itab> ASSIGNING <ls_itab>.

      ASSIGN COMPONENT iv_fnam_matnr OF STRUCTURE <ls_itab> TO <lv_matnr>.
      CHECK <lv_matnr> IS NOT INITIAL.
      ASSIGN COMPONENT iv_fnam_maktx OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_maktx>).
      ASSERT sy-subrc = 0.

      READ TABLE lt_makt
           ASSIGNING FIELD-SYMBOL(<ls_makt>)
           WITH KEY matnr = <lv_matnr>
                    spras = sy-langu
           BINARY SEARCH.
      IF sy-subrc = 0.
        <lv_maktx> = <ls_makt>-maktx.
        CONTINUE.
      ENDIF.

      READ TABLE lt_makt
           ASSIGNING <ls_makt>
           WITH KEY matnr = <lv_matnr>
           BINARY SEARCH.
      IF sy-subrc = 0.
        <lv_maktx> = <ls_makt>-maktx.
        CONTINUE.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_block_status.
    ASSIGN gt_block_cache[ KEY primary_key COMPONENTS vkorg = iv_vkorg
                                                      vtweg = iv_vtweg ]
           TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc <> 0.

      DATA(ls_cache) = VALUE t_block_cache( vkorg = iv_vkorg
                                            vtweg = iv_vtweg ).

      DATA(lv_vmsta) = COND vmsta(
        WHEN ls_cache-vkorg IS NOT INITIAL
         AND ls_cache-vtweg IS NOT INITIAL
        THEN get_mvke( iv_matnr      = gs_def-matnr
                       iv_vkorg      = ls_cache-vkorg
                       iv_vtweg      = ls_cache-vtweg
                       iv_must_exist = abap_true
             )-vmsta
        ELSE gs_def-mstav ).

      IF lv_vmsta IS NOT INITIAL.
        SELECT SINGLE * FROM tvms
               WHERE vmsta = @lv_vmsta
               INTO @ls_cache-tvms.
      ENDIF.

      INSERT ls_cache
             INTO TABLE gt_block_cache
             ASSIGNING <ls_cache>.

    ENDIF.

    rr_status = REF #( <ls_cache> ).
  ENDMETHOD.

  METHOD get_gos_url_list.
    rt_list = zcl_bc_gos_toolkit=>get_url_list( is_key = VALUE #( classname = c_gos_classname
                                                                  objkey    = gs_def-matnr ) ).
  ENDMETHOD.

  METHOD get_sales_uom.
    IF iv_vkorg IS NOT INITIAL AND iv_vtweg IS NOT INITIAL.
      TRY.
          DATA(ls_mvke) = get_mvke( iv_matnr      = gs_def-matnr
                                    iv_vkorg      = iv_vkorg
                                    iv_vtweg      = iv_vtweg
                                    iv_must_exist = abap_false ).
        CATCH cx_root.
          ASSERT 1 = 0.
      ENDTRY.
    ENDIF.

    rv_vrkme = COND #( WHEN ls_mvke-vrkme IS NOT INITIAL
                       THEN ls_mvke-vrkme
                       ELSE gs_def-meins ).
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS matnr = iv_matnr ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( matnr = iv_matnr ).
      ls_multiton-obj = NEW #( ).

      SELECT SINGLE * FROM mara
             WHERE matnr = @ls_multiton-matnr
             INTO @ls_multiton-obj->gs_def.

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_mm_material( textid = zcx_mm_material=>undefined
                                             matnr  = ls_multiton-matnr ).
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_mt>.
    ENDIF.

    ro_obj = <ls_mt>-obj.
  ENDMETHOD.

  METHOD get_maktx.
    DATA ls_makt TYPE makt.

    ASSIGN gt_makt[ KEY primary_key COMPONENTS spras = iv_spras
                                               matnr = iv_matnr ]
           TO FIELD-SYMBOL(<ls_makt>).

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM makt
             INTO ls_makt
             WHERE spras = iv_spras
               AND matnr = iv_matnr.
      ls_makt-spras = iv_spras.
      ls_makt-matnr = iv_matnr.
      INSERT ls_makt INTO TABLE gt_makt ASSIGNING <ls_makt>.
    ENDIF.

    rv_maktx = <ls_makt>-maktx.
  ENDMETHOD.

  METHOD get_mara.
    DATA ls_mara TYPE mara.

    ASSIGN gt_mara[ KEY primary_key COMPONENTS matnr = iv_matnr ]
           TO FIELD-SYMBOL(<ls_mara>).

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr = iv_matnr.
      ls_mara-matnr = iv_matnr.
      INSERT ls_mara INTO TABLE gt_mara ASSIGNING <ls_mara>.
    ENDIF.

    rs_mara = <ls_mara>.
  ENDMETHOD.

  METHOD get_marc.
    DATA ls_marc TYPE marc.

    ASSIGN gt_marc[ KEY primary_key COMPONENTS matnr = iv_matnr
                                               werks = iv_werks ]
           TO FIELD-SYMBOL(<ls_marc>).

    IF sy-subrc <> 0.

      SELECT SINGLE * FROM marc
             INTO ls_marc
             WHERE matnr = iv_matnr
               AND werks = iv_werks.

      DATA(ls_marc_subrc) = VALUE t_marc_subrc( matnr = iv_matnr
                                                werks = iv_werks
                                                subrc = sy-subrc ).

      INSERT ls_marc_subrc INTO TABLE gt_marc_subrc.

      ls_marc-matnr = iv_matnr.
      ls_marc-werks = iv_werks.
      INSERT ls_marc INTO TABLE gt_marc ASSIGNING <ls_marc>.

    ENDIF.

    rs_marc = <ls_marc>.
  ENDMETHOD.

  METHOD get_marm.
    ASSIGN gt_marm[ KEY primary_key COMPONENTS matnr = iv_matnr ]
           TO FIELD-SYMBOL(<ls_marm>).

    IF sy-subrc <> 0.

      DATA(ls_marm) = VALUE t_marm_cache( matnr = iv_matnr ).

      SELECT * FROM marm
             WHERE matnr = @ls_marm-matnr
             INTO TABLE @ls_marm-marm.

      INSERT ls_marm
             INTO TABLE gt_marm
             ASSIGNING <ls_marm>.

    ENDIF.

    rt_marm = <ls_marm>-marm.
  ENDMETHOD.

  METHOD get_mbew.
    DATA ls_mbew TYPE mbew.

    ASSIGN gt_mbew[ KEY primary_key COMPONENTS matnr = iv_matnr
                                               bwkey = iv_bwkey ]
           TO FIELD-SYMBOL(<ls_mbew>).

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM mbew
             INTO ls_mbew
             WHERE matnr = iv_matnr
               AND bwkey = iv_bwkey. "#EC CI_NOORDER
      ls_mbew-matnr = iv_matnr.
      ls_mbew-bwkey = iv_bwkey.
      INSERT ls_mbew INTO TABLE gt_mbew ASSIGNING <ls_mbew>.
    ENDIF.

    rs_mbew = <ls_mbew>.
  ENDMETHOD.

  METHOD get_mlan.
    DATA ls_mlan TYPE mlan.

    ASSIGN gt_mlan[ KEY primary_key COMPONENTS matnr = iv_matnr
                                               aland = iv_aland ]
           TO FIELD-SYMBOL(<ls_mlan>).

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM mlan
             WHERE matnr = @iv_matnr
               AND aland = @iv_aland
             INTO @ls_mlan.
      ls_mlan-matnr = iv_matnr.
      ls_mlan-aland = iv_aland.
      INSERT ls_mlan INTO TABLE gt_mlan ASSIGNING <ls_mlan>.
    ENDIF.

    rs_mlan = <ls_mlan>.
  ENDMETHOD.

  METHOD get_mvke.
    DATA ls_mvke TYPE mvke.

    ASSIGN gt_mvke[ KEY primary_key COMPONENTS matnr = iv_matnr
                                               vkorg = iv_vkorg
                                               vtweg = iv_vtweg ]
           TO FIELD-SYMBOL(<ls_mvke>).

    IF sy-subrc <> 0.
      CLEAR ls_mvke.

      SELECT SINGLE * FROM mvke
             WHERE matnr = @iv_matnr
               AND vkorg = @iv_vkorg
               AND vtweg = @iv_vtweg
             INTO @ls_mvke.

      IF     sy-subrc      <> 0
         AND iv_must_exist  = abap_true.

        RAISE EXCEPTION NEW zcx_sd_zm_material( matnr  = iv_matnr
                                                textid = zcx_sd_zm_material=>undefined_for_dist_chan
                                                vkorg  = iv_vkorg
                                                vtweg  = iv_vtweg ).

      ENDIF.

      ls_mvke-matnr = iv_matnr.
      ls_mvke-vkorg = iv_vkorg.
      ls_mvke-vtweg = iv_vtweg.
      INSERT ls_mvke INTO TABLE gt_mvke ASSIGNING <ls_mvke>.
    ENDIF.

    rs_mvke = <ls_mvke>.
  ENDMETHOD.

  METHOD get_tax_rate.
    CLEAR: ev_kdv,
           ev_otv.

    TRY.
        DATA(lr_tax_rate) = REF #( gt_tax_rates[ KEY primary_key COMPONENTS aland = iv_aland ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(ls_new_tax_rate) = VALUE t_tax_rate_cache( aland = iv_aland ).

        DATA(ls_mlan) = get_mlan( iv_matnr = gs_def-matnr
                                  iv_aland = iv_aland ).

        TRY.
            ls_new_tax_rate-kdv = zcl_sd_tax_class=>get_instance( ls_mlan-taxm1 )->get_tax_rate_text( ).
          CATCH cx_root.
            ls_new_tax_rate-kdv = '0'.
        ENDTRY.

        SELECT SINGLE FROM zi_sd_tr_sct_rate
               FIELDS vat_rate_txt
               WHERE taxm2 = @ls_mlan-taxm2
                 AND land1 = @iv_aland
               INTO @ls_new_tax_rate-otv.

        IF ls_new_tax_rate-otv = space.
          ls_new_tax_rate-otv = '0'.
        ENDIF.

        INSERT ls_new_tax_rate INTO TABLE gt_tax_rates REFERENCE INTO lr_tax_rate.
    ENDTRY.

    ev_kdv = lr_tax_rate->kdv.
    ev_otv = lr_tax_rate->otv.
  ENDMETHOD.

  METHOD get_valuation_data_via_plant.
    ASSIGN gt_valuation_via_plant[ KEY primary_key COMPONENTS werks = iv_werks ]
           TO FIELD-SYMBOL(<ls_vp>).

    IF sy-subrc <> 0.

      DATA(ls_vp) = VALUE t_valuation_via_plant( werks = iv_werks ).

      TRY.
          DATA(lv_bwkey) = zcl_mm_plant=>get_instance( iv_werks )->gs_def-bwkey.
        CATCH cx_root INTO DATA(lo_diaper).
          RAISE EXCEPTION NEW cx_no_entry_in_table( previous   = lo_diaper
                                                    table_name = CONV #( zcl_mm_plant=>c_tabname_def )
                                                    entry_name = CONV #( iv_werks ) ).
      ENDTRY.

      SELECT SINGLE hrkft FROM mbew
             WHERE matnr = @gs_def-matnr
               AND bwkey = @lv_bwkey
             INTO CORRESPONDING FIELDS OF @ls_vp.       "#EC CI_NOORDER

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW cx_no_entry_in_table( previous   = lo_diaper
                                                  table_name = CONV #( c_tabname-valuation )
                                                  entry_name = |{ gs_def-matnr } { lv_bwkey }| ).
      ENDIF.

      INSERT ls_vp INTO TABLE gt_valuation_via_plant ASSIGNING <ls_vp>.

    ENDIF.

    rs_valuation = <ls_vp>.
  ENDMETHOD.

  METHOD get_warehouse_data.
    read_warehouse_lazy( ).

    TRY.
        rs_warehouse = CORRESPONDING #( gt_warehouse[ KEY primary_key COMPONENTS lgnum = iv_lgnum ] ).
      CATCH cx_sy_itab_line_not_found INTO DATA(lo_cx_itab). " TODO: variable is assigned but never used (ABAP cleaner)
        RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( c_tabname-warehouse )
                                                  entry_name = |{ gs_def-matnr } { iv_lgnum }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD ensure_batch_existence.
    ASSIGN gt_batch_existence_cache[ KEY primary_key COMPONENTS charg = iv_charg ]
           TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc <> 0.
      DATA(ls_cache) = VALUE t_batch_existence_cache( charg = iv_charg ).

      SELECT SINGLE mandt FROM mch1
             WHERE matnr = @gs_def-matnr
               AND charg = @ls_cache-charg
               AND lvorm = @abap_false
             INTO @sy-mandt ##WRITE_OK.

      IF sy-subrc <> 0.
        ls_cache-cx = NEW #( table_name = CONV #( c_tabname_mch1 )
                             entry_name = |{ gs_def-matnr } { ls_cache-charg }| ).
      ENDIF.

      INSERT ls_cache INTO TABLE gt_batch_existence_cache ASSIGNING <ls_cache>.
    ENDIF.

    IF <ls_cache>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-cx.
    ENDIF.
  ENDMETHOD.

  METHOD ensure_material_def_in_plant.
    CHECK is_material_defined_in_plant( iv_werks ) = abap_false.

    RAISE EXCEPTION NEW zcx_mm_material_plant( matnr    = gs_def-matnr
                                               werks    = iv_werks
                                               previous = NEW zcx_bc_table_content(
                                                                  textid   = zcx_bc_table_content=>entry_missing
                                                                  objectid = |{ gs_def-matnr } { iv_werks }|
                                                                  tabname  = c_tabname_marc ) ).
  ENDMETHOD.

  METHOD ensure_not_blocked_for_order.
    CHECK is_blocked_for_order( iv_vkorg = iv_vkorg
                                iv_vtweg = iv_vtweg )
          = abap_true.

    RAISE EXCEPTION NEW zcx_sd_zm_material( textid = zcx_sd_zm_material=>blocked_for_dist_chan
                                            matnr  = gs_def-matnr
                                            vkorg  = iv_vkorg
                                            vtweg  = iv_vtweg ).
  ENDMETHOD.

  METHOD is_blocked_for_order.
    DATA(lr_block_status) = get_block_status( iv_vkorg = iv_vkorg
                                              iv_vtweg = iv_vtweg ).

    rv_blocked = xsdbool( lr_block_status->tvms-spvbc = c_fedia_blocked ).
  ENDMETHOD.

  METHOD is_defined_in_any_sales_org.
    CHECK it_vkorg IS NOT INITIAL.

    LOOP AT it_vkorg ASSIGNING FIELD-SYMBOL(<lv_vkorg>).

      TRY.
          DATA(lt_distribution_channels) = zcl_sd_sales_org=>get_instance( <lv_vkorg> )->get_distribution_channels( ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      LOOP AT lt_distribution_channels ASSIGNING FIELD-SYMBOL(<lv_vtweg>).
        TRY.
            get_mvke( iv_matnr      = gs_def-matnr
                      iv_vkorg      = <lv_vkorg>
                      iv_vtweg      = <lv_vtweg>
                      iv_must_exist = abap_true ).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.

        rv_defined = abap_true.
        RETURN.                                          "#EC CI_NOORDER
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_defined_in_any_warehouse.
    read_warehouse_lazy( ).
    rv_defined = xsdbool( gt_warehouse IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_material_defined_in_plant.
    get_marc( iv_matnr = gs_def-matnr
              iv_werks = iv_werks ).

    rv_defined =
      COND #( WHEN NOT line_exists( gt_marc_subrc[
                                                    KEY primary_key
                                                    COMPONENTS matnr = gs_def-matnr
                                                               werks = iv_werks ] ) THEN abap_false

              WHEN gt_marc_subrc[ KEY primary_key COMPONENTS matnr = gs_def-matnr
                                                             werks = iv_werks
                                ]-subrc = 0                                         THEN abap_true

              ELSE                                                                       abap_false ).
  ENDMETHOD.

  METHOD convert_quantity.
    TRY.
        CHECK quantity IS NOT INITIAL. " 0 ise, 0 dönsün

        IF from_uom = to_uom.
          result = quantity.
          RETURN.
        ENDIF.

        DATA(i_menge) = CONV ekpo-menge( abs( quantity ) ).
        DATA(e_menge) = CONV ekpo-menge( 0 ).

        ##FM_SUBRC_OK
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING  i_matnr              = gs_def-matnr
                     i_in_me              = from_uom
                     i_out_me             = to_uom
                     i_menge              = i_menge
          IMPORTING  e_menge              = e_menge
          EXCEPTIONS error_in_application = 1
                     error                = 2
                     OTHERS               = 3.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'MD_CONVERT_MATERIAL_UNIT' ).

        IF quantity < 0.
          e_menge *= -1.
        ENDIF.

        result = e_menge.

      CATCH cx_root INTO DATA(conv_error).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( previous = conv_error
                                                       matnr    = gs_def-matnr
                                                       src_uom  = from_uom
                                                       tar_uom  = to_uom ).
    ENDTRY.
  ENDMETHOD.

  METHOD convert_quantity_to_base_uom.
    result = convert_quantity( from_uom = from_uom
                               to_uom   = gs_def-meins
                               quantity = quantity ).
  ENDMETHOD.

  METHOD conv_quan_to_base_uom_on_per.
    CLEAR: converted_menge,
           converted_meins.

    IF go_uom_hist_set IS INITIAL.
      go_uom_hist_set = CAST #( NEW zcl_mm_mat_uom_hist_set( VALUE #( ( gs_def-matnr ) ) ) ).
    ENDIF.

    go_uom_hist_set->convert_to_base_uom_on_period( EXPORTING matnr           = gs_def-matnr
                                                              from_menge      = from_menge
                                                              from_meins      = from_meins
                                                              jahrper         = jahrper
                                                    IMPORTING converted_menge = converted_menge
                                                              converted_meins = converted_meins ).
  ENDMETHOD.

  METHOD conv_quan_fr_base_uom_on_per.
    CLEAR: converted_menge,
           base_uom.

    IF go_uom_hist_set IS INITIAL.
      go_uom_hist_set = CAST #( NEW zcl_mm_mat_uom_hist_set( VALUE #( ( gs_def-matnr ) ) ) ).
    ENDIF.

    go_uom_hist_set->convert_fr_base_uom_on_period( EXPORTING matnr           = gs_def-matnr
                                                              from_menge      = from_menge
                                                              to_meins        = to_meins
                                                              jahrper         = jahrper
                                                    IMPORTING converted_menge = converted_menge
                                                              base_uom        = base_uom ).
  ENDMETHOD.

  METHOD get_net_weight_on_period.
    CLEAR: ntgew,
           gewei.

    IF go_uom_hist_set IS INITIAL.
      go_uom_hist_set = CAST #( NEW zcl_mm_mat_uom_hist_set( VALUE #( ( gs_def-matnr ) ) ) ).
    ENDIF.

    go_uom_hist_set->get_net_weight_on_period( EXPORTING matnr    = gs_def-matnr
                                                         jahrper  = jahrper
                                                         to_gewei = to_gewei
                                               IMPORTING ntgew    = ntgew
                                                         gewei    = gewei ).
  ENDMETHOD.

  METHOD get_net_weight_on_date.
    CLEAR: ntgew,
           gewei.

    IF go_uom_hist_set IS INITIAL.
      go_uom_hist_set = CAST #( NEW zcl_mm_mat_uom_hist_set( VALUE #( ( gs_def-matnr ) ) ) ).
    ENDIF.

    go_uom_hist_set->get_net_weight_on_date( EXPORTING matnr    = gs_def-matnr
                                                       datum    = datum
                                                       to_gewei = to_gewei
                                             IMPORTING ntgew    = ntgew
                                                       gewei    = gewei ).
  ENDMETHOD.

  METHOD read_warehouse_lazy.
    CHECK gv_warehouse_read = abap_false.

    SELECT lgnum, lgbkz, ltkze, ltkza
           FROM mlgn
           WHERE matnr = @gs_def-matnr
             AND lvorm = @abap_false
           INTO CORRESPONDING FIELDS OF TABLE @gt_warehouse.

    gv_warehouse_read = abap_true.
  ENDMETHOD.

  METHOD conversion_exit_input.
    TRY.
        ##FM_SUBRC_OK
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING  input        = matnr
          IMPORTING  output       = result
          EXCEPTIONS length_error = 1
                     OTHERS       = 2.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'CONVERSION_EXIT_MATN1_INPUT' ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_mm_material( textid   = zcx_mm_material=>invalid_matnr
                                             previous = diaper
                                             matnr    = matnr ). "#EC CI_FLDEXT_OK[0002215424]
    ENDTRY.
  ENDMETHOD.
ENDCLASS.