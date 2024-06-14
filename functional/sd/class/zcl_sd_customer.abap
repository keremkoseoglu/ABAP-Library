CLASS zcl_sd_customer DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES tt_kunnr TYPE STANDARD TABLE OF kunnr WITH DEFAULT KEY.
    TYPES tt_parob TYPE STANDARD TABLE OF zsdt_cus_parob WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF c_fnam,
                 kunnr TYPE fieldname VALUE 'KUNNR',
                 name1 TYPE fieldname VALUE 'NAME1',
                 name3 TYPE fieldname VALUE 'NAME3',
               END OF c_fnam.

    CONSTANTS: BEGIN OF c_fatura_kapsami,
                 e_arsiv  TYPE zfid_fatura_kapsami_v VALUE 'EA',
                 e_fatura TYPE zfid_fatura_kapsami_v VALUE 'EF',
                 kagit    TYPE zfid_fatura_kapsami_v VALUE 'KA',
               END OF c_fatura_kapsami.

    CONSTANTS: BEGIN OF c_purpose,
                 mutabakat TYPE zsdd_invoice_read_purpose VALUE 'MUTABAKAT',
               END OF c_purpose.

    DATA gs_def TYPE kna1 READ-ONLY.

    CLASS-METHODS cache_kna1
      IMPORTING ir_tab  TYPE REF TO data
                iv_fnam TYPE fieldname DEFAULT c_fnam-kunnr
      RAISING   zcx_bc_class_method.

    CLASS-METHODS cache_name1
      IMPORTING ir_tab  TYPE REF TO data
                iv_fnam TYPE fieldname DEFAULT c_fnam-kunnr
      RAISING   zcx_bc_class_method.

    CLASS-METHODS conv_exit_kunnr_input
      IMPORTING iv_kunnr        TYPE kunnr
      RETURNING VALUE(rv_kunnr) TYPE kunnr.

    CLASS-METHODS ensure_obl_part_funcs_filled
      IMPORTING it_vkorg TYPE zcl_sd_sales_org=>tt_vkorg
                it_parob TYPE tt_parob
      RAISING   zcx_sd_parob.

    CLASS-METHODS get_def_land_sd_values
      IMPORTING iv_land1      TYPE land1
      RETURNING VALUE(rs_val) TYPE zfit_xd_def_lsd_fld
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_def_tax_codes
      IMPORTING iv_ktokd      TYPE ktokd
      RETURNING VALUE(rs_val) TYPE zfit_xd_def_stc_fld
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_hq_branch_codes
      IMPORTING iv_need_hq      TYPE abap_bool        DEFAULT abap_true
                iv_need_br      TYPE abap_bool        DEFAULT abap_true
                it_bukrs        TYPE tpmy_range_bukrs OPTIONAL
                it_knrze        TYPE trty_kunnr_range OPTIONAL
      RETURNING VALUE(rt_kunnr) TYPE tt_kunnr.

    CLASS-METHODS get_instance
      IMPORTING iv_kunnr      TYPE kunnr
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_customer
      RAISING   zcx_sd_customer_def.

    CLASS-METHODS get_invoice_scope_by_address
      IMPORTING iv_adrnr        TYPE adrc-addrnumber
                iv_purpose      TYPE zsdd_invoice_read_purpose OPTIONAL
      RETURNING VALUE(rv_scope) TYPE zfid_fatura_kapsami_v
      RAISING   zcx_sd_doc_inv_scope.

    CLASS-METHODS get_invoice_scope_by_delivery
      IMPORTING iv_vbeln        TYPE vbeln_vl
                iv_purpose      TYPE zsdd_invoice_read_purpose OPTIONAL
      RETURNING VALUE(rv_scope) TYPE zfid_fatura_kapsami_v
      RAISING   zcx_sd_doc_inv_scope.

    CLASS-METHODS get_invoice_scope_by_partner
      IMPORTING iv_vbeln        TYPE vbeln
                iv_purpose      TYPE zsdd_invoice_read_purpose OPTIONAL
      RETURNING VALUE(rv_scope) TYPE zfid_fatura_kapsami_v
      RAISING   zcx_sd_doc_inv_scope.

    CLASS-METHODS get_invoice_scope_by_tax
      IMPORTING iv_stcd2        TYPE stcd2
                iv_purpose      TYPE zsdd_invoice_read_purpose OPTIONAL
      RETURNING VALUE(rv_scope) TYPE zfid_fatura_kapsami_v.

    CLASS-METHODS get_kna1
      IMPORTING iv_kunnr       TYPE kunnr
      RETURNING VALUE(rs_kna1) TYPE kna1.

    CLASS-METHODS get_knvv
      IMPORTING iv_kunnr       TYPE knvv-kunnr
                iv_vkorg       TYPE knvv-vkorg
                iv_vtweg       TYPE knvv-vtweg
                iv_spart       TYPE knvv-spart
      RETURNING VALUE(rs_knvv) TYPE knvv.

    CLASS-METHODS get_name1
      IMPORTING iv_kunnr        TYPE kunnr
      RETURNING VALUE(rv_name1) TYPE name1_gp.

    CLASS-METHODS put_name1_into_itab
      IMPORTING ir_itab        TYPE REF TO data
                iv_kunnr_field TYPE fieldname
                iv_name1_field TYPE fieldname.

    CLASS-METHODS set_head_customer_if_found
      IMPORTING iv_bukrs TYPE bukrs
      CHANGING  cv_kunnr TYPE kunnr.

    METHODS ensure_defined_in_dist_channel
      IMPORTING iv_vkorg TYPE vkorg
                iv_vtweg TYPE vtweg
      RAISING   zcx_sd_customer_def.

    METHODS get_head_customer
      IMPORTING iv_bukrs       TYPE bukrs
      RETURNING VALUE(ro_head) TYPE REF TO zcl_sd_customer
      RAISING   zcx_sd_customer_hq_def.

    METHODS get_invoice_scope
      RETURNING VALUE(rv_scope) TYPE zfid_fatura_kapsami_v.

    METHODS get_transport_zone
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_transport_zone
      RAISING   zcx_sd_tzone.

    METHODS is_employer
      RETURNING VALUE(rv_employer) TYPE abap_bool.

    METHODS get_land
      RETURNING VALUE(output) TYPE REF TO zcl_bc_land
      RAISING   cx_no_entry_in_table.

    METHODS is_eu_member
      RETURNING VALUE(output) TYPE abap_bool
      RAISING   cx_no_entry_in_table.

    METHODS get_company_payment_term
      IMPORTING bukrs         TYPE bukrs
      RETURNING VALUE(result) TYPE t052-ztag1.

  PRIVATE SECTION.
    TYPES tt_def_stc   TYPE HASHED TABLE OF zfit_xd_def_stc WITH UNIQUE KEY primary_key COMPONENTS ktokd.

    TYPES tt_land1_rng TYPE RANGE OF land1.

    TYPES: BEGIN OF t_def_land_sd_val,
             land1_rng TYPE tt_land1_rng,
             val       TYPE zfit_xd_def_lsd_fld,
           END OF t_def_land_sd_val,

           tt_def_land_sd_val TYPE STANDARD TABLE OF t_def_land_sd_val WITH DEFAULT KEY.

    TYPES: BEGIN OF t_def_lsv_cache,
             land1 TYPE land1,
             val   TYPE zfit_xd_def_lsd_fld,
             cx    TYPE REF TO zcx_bc_table_content,
           END OF t_def_lsv_cache,

           tt_def_lsv_cache TYPE HASHED TABLE OF t_def_lsv_cache
                             WITH UNIQUE KEY primary_key COMPONENTS land1.

    TYPES: BEGIN OF t_clazy_flg,
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
           END OF t_clazy.

    TYPES: BEGIN OF t_hq,
             bukrs TYPE bukrs,
             knrze TYPE knrze,
             obj   TYPE REF TO zcl_sd_customer,
             cx    TYPE REF TO zcx_sd_customer_hq_def,
           END OF t_hq,

           tt_hq TYPE HASHED TABLE OF t_hq
                 WITH UNIQUE KEY primary_key COMPONENTS bukrs.

    TYPES tt_knvv TYPE HASHED TABLE OF knvv
                  WITH UNIQUE KEY primary_key COMPONENTS kunnr vkorg vtweg spart.

    TYPES: BEGIN OF t_multiton,
             kunnr TYPE kna1-kunnr,
             obj   TYPE REF TO zcl_sd_customer,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS kunnr.

    TYPES: BEGIN OF t_name1,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
           END OF t_name1,

           tt_name1 TYPE HASHED TABLE OF t_name1 WITH UNIQUE KEY primary_key COMPONENTS kunnr.

    TYPES: BEGIN OF t_fatura_kapsami_cache,
             stcd2          TYPE stcd2,
             purpose        TYPE zsdd_invoice_read_purpose,
             fatura_kapsami TYPE zfid_fatura_kapsami_v,
           END OF t_fatura_kapsami_cache,

           tt_fatura_kapsami_cache TYPE HASHED TABLE OF t_fatura_kapsami_cache
                                   WITH UNIQUE KEY primary_key COMPONENTS stcd2 purpose.

    TYPES tt_fatura_kapsami TYPE STANDARD TABLE OF zfid_fatura_kapsami_v WITH DEFAULT KEY.

    TYPES: BEGIN OF t_dist_chan_cache,
             vkorg TYPE vkorg,
             vtweg TYPE vtweg,
             error TYPE REF TO zcx_sd_customer_def,
           END OF t_dist_chan_cache,

           tt_dist_chan_cache TYPE HASHED TABLE OF t_dist_chan_cache
                              WITH UNIQUE KEY primary_key COMPONENTS vkorg vtweg.

    TYPES: BEGIN OF t_company_payment_term,
             bukrs TYPE bukrs,
             ztag1 TYPE t052-ztag1,
           END OF t_company_payment_term,

           tt_company_payment_term TYPE HASHED TABLE OF t_company_payment_term
                                   WITH UNIQUE KEY primary_key COMPONENTS bukrs.

    CONSTANTS: BEGIN OF c_parvw,
                 payer TYPE parvw VALUE 'RG',
               END OF c_parvw,

               BEGIN OF c_ktokd,
                 employer TYPE ktokd VALUE 'PERS',
               END OF c_ktokd,

               BEGIN OF c_tabname,
                 adrc    TYPE tabname VALUE 'ADRC',
                 def_stc TYPE tabname VALUE 'ZFIT_XD_DEF_STC',
                 knvv    TYPE tabname VALUE 'KNVV',
                 lsd     TYPE tabname VALUE 'ZFIT_XD_DEF_LSD',
                 vbpa    TYPE tabname VALUE 'VBPA',
               END OF c_tabname,

               c_clsname_me       TYPE seoclsname VALUE 'ZCL_SD_CUSTOMER',
               c_meth_cache_wgbez TYPE seocpdname VALUE 'CACHE_NAME1'.

    CLASS-DATA gs_clazy                TYPE t_clazy.
    CLASS-DATA gt_def_stc              TYPE tt_def_stc.
    CLASS-DATA gt_fatura_kapsami_cache TYPE tt_fatura_kapsami_cache.
    CLASS-DATA gt_hq                   TYPE tt_hq.
    CLASS-DATA gt_kna1                 TYPE HASHED TABLE OF kna1 WITH UNIQUE KEY primary_key COMPONENTS kunnr.
    CLASS-DATA gt_knvv                 TYPE tt_knvv.
    CLASS-DATA gt_lsv_cache            TYPE tt_def_lsv_cache.
    CLASS-DATA gt_multiton             TYPE tt_multiton.
    CLASS-DATA gt_name1                TYPE tt_name1.

    DATA: gt_dist_chan_cache     TYPE tt_dist_chan_cache,
          gt_comp_pay_term_cache TYPE tt_company_payment_term.

    CLASS-METHODS read_parob_lazy.
ENDCLASS.


CLASS zcl_sd_customer IMPLEMENTATION.
  METHOD get_invoice_scope.
    rv_scope = get_invoice_scope_by_tax( gs_def-stcd2 ).
  ENDMETHOD.

  METHOD cache_name1.
    DATA lt_kunnr_rng TYPE trty_kunnr_range.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_kunnr> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_kunnr>.

          CHECK           <lv_kunnr> IS ASSIGNED
                AND       <lv_kunnr> IS NOT INITIAL
                AND ( NOT line_exists( gt_name1[ KEY primary_key COMPONENTS kunnr = <lv_kunnr> ] ) ).

          COLLECT VALUE vdkunnr_range( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_kunnr> )
                  INTO lt_kunnr_rng.

        ENDLOOP.

        IF lt_kunnr_rng IS INITIAL.
          RETURN.
        ENDIF.

        SELECT kunnr, name1 APPENDING CORRESPONDING FIELDS OF TABLE @gt_name1
               FROM kna1
               WHERE kunnr IN @lt_kunnr_rng.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_wgbez ).

    ENDTRY.
  ENDMETHOD.

  METHOD get_invoice_scope_by_delivery.
    TRY.

        DATA(lv_delivery_customer_code) = zcl_sd_delivery=>get_instance( iv_vbeln )->gs_head-kunnr.
        DATA(lo_delivery_customer) = zcl_sd_customer=>get_instance( lv_delivery_customer_code ).

        CASE lo_delivery_customer->is_employer( ).
          WHEN abap_true.  " PERS durumu """"""""""""""""""""""""""""""""""""""""""""""""""""""

            DATA(lt_scope) = VALUE tt_fatura_kapsami( FOR _vbeln_va IN zcl_sd_delivery=>get_instance( iv_vbeln )->get_orders( )
                                                      ( get_invoice_scope_by_partner( _vbeln_va ) ) ).

            SORT lt_scope.
            DELETE ADJACENT DUPLICATES FROM lt_scope.

            CASE lines( lt_scope ).
              WHEN 0.
                RAISE EXCEPTION NEW zcx_sd_doc_inv_scope( textid = zcx_sd_doc_inv_scope=>no_scope_for_delivery_order
                                                          vbeln  = iv_vbeln ).
              WHEN 1.
                rv_scope = lt_scope[ 1 ].
              WHEN OTHERS.
                RAISE EXCEPTION NEW zcx_sd_doc_inv_scope( textid = zcx_sd_doc_inv_scope=>multiple_scope_for_dlv_order
                                                          vbeln  = iv_vbeln ).
            ENDCASE.

          WHEN abap_false. " PERS olmayan durum """""""""""""""""""""""""""""""""""""""""""""""

            rv_scope = lo_delivery_customer->get_invoice_scope( ).

        ENDCASE.

      CATCH zcx_sd_doc_inv_scope INTO DATA(lo_cx_scope).
        RAISE EXCEPTION lo_cx_scope.

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION NEW zcx_sd_doc_inv_scope( previous = lo_diaper
                                                  vbeln    = iv_vbeln ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_eu_member.
    output = get_land( )->def-xegld.
  ENDMETHOD.

  METHOD ensure_obl_part_funcs_filled.
    CHECK     it_vkorg IS NOT INITIAL
          AND it_parob IS NOT INITIAL.

    read_parob_lazy( ).
    IF gs_clazy-val-parob IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_vkorg_rng) = VALUE zcl_sd_sales_org=>tt_vkorg_rng( FOR GROUPS gr1 OF ls_vkorg IN it_vkorg
                                                               GROUP BY ls_vkorg-vkorg
                                                               ( option = zcl_bc_ddic_toolkit=>c_option_eq
                                                                 sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                                                 low    = gr1 ) ).

    DATA(lt_parob_bin) = it_parob.
    SORT lt_parob_bin BY vkorg
                         parvw. " Binary Search

    LOOP AT gs_clazy-val-parob
         ASSIGNING FIELD-SYMBOL(<ls_parob>)
         WHERE vkorg IN lt_vkorg_rng.

      READ TABLE lt_parob_bin
           TRANSPORTING NO FIELDS
           WITH KEY vkorg = <ls_parob>-vkorg
                    parvw = <ls_parob>-parvw
           BINARY SEARCH.

      CHECK sy-subrc <> 0.

      RAISE EXCEPTION NEW zcx_sd_parob( textid = zcx_sd_parob=>par_func_obl_in_sales_org
                                        vkorg  = <ls_parob>-vkorg
                                        parvw  = <ls_parob>-parvw ).

    ENDLOOP.
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
                            KEY primary_key
                            COMPONENTS ktokd = iv_ktokd ] ).

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

        RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                  previous = lo_silnf
                                                  objectid = CONV #( iv_ktokd )
                                                  tabname  = c_tabname-def_stc ).

    ENDTRY.
  ENDMETHOD.

  METHOD cache_kna1.
    DATA lt_kunnr_rng TYPE trty_kunnr_range.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_kunnr> TYPE any.

    CHECK ir_tab IS NOT INITIAL.

    TRY.

        ASSIGN ir_tab->* TO <lt_tab>.
        IF <lt_tab> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_kunnr>.

          CHECK           <lv_kunnr> IS ASSIGNED
                AND       <lv_kunnr> IS NOT INITIAL
                AND ( NOT line_exists( gt_kna1[ KEY primary_key COMPONENTS kunnr = <lv_kunnr> ] ) ).

          COLLECT VALUE vdkunnr_range( option = zcl_bc_ddic_toolkit=>c_option_eq
                                       sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                       low    = <lv_kunnr> )
                  INTO lt_kunnr_rng.

        ENDLOOP.

        IF lt_kunnr_rng IS INITIAL.
          RETURN.
        ENDIF.

        SELECT * FROM kna1
               WHERE kunnr IN @lt_kunnr_rng
               APPENDING CORRESPONDING FIELDS OF TABLE @gt_kna1.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_wgbez ).

    ENDTRY.
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

  METHOD get_transport_zone.
    ro_obj = zcl_sd_transport_zone=>get_instance( iv_land1 = gs_def-land1
                                                  iv_zone1 = gs_def-lzone ).
  ENDMETHOD.

  METHOD get_instance.
    DATA(lv_kunnr) = zcl_sd_customer=>conv_exit_kunnr_input( iv_kunnr ).

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS kunnr = lv_kunnr ] TO FIELD-SYMBOL(<ls_multiton>).
    IF sy-subrc <> 0.

      DATA(ls_multiton) = VALUE t_multiton( kunnr = lv_kunnr ).

      ls_multiton-obj = NEW #( ).

      SELECT SINGLE * INTO ls_multiton-obj->gs_def FROM kna1 WHERE kunnr = ls_multiton-kunnr.

      IF sy-subrc <> 0.

        RAISE EXCEPTION NEW zcx_sd_customer_def( kunnr = ls_multiton-kunnr ).

      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD get_invoice_scope_by_tax.
    DATA:
      lv_earc  TYPE char1,
      lv_einv  TYPE char1,
      lv_paper TYPE char1.

    ASSIGN gt_fatura_kapsami_cache[ KEY primary_key COMPONENTS stcd2   = iv_stcd2
                                                               purpose = iv_purpose ]
           TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc <> 0.

      DATA(ls_cache) = VALUE t_fatura_kapsami_cache( stcd2   = iv_stcd2
                                                     purpose = iv_purpose ).

      " E-Fatura / kağıt """""""""""""""""""""""""""""""""""""""""""

      DATA(lv_taxno) = CONV /fite/inv_1_de017( ls_cache-stcd2 ).

      CALL FUNCTION '/FITE/INV_1_F021'
        EXPORTING i_tax_number     = lv_taxno
                  i_inv_date       = sy-datum
        IMPORTING e_einv_partn_ind = lv_einv
                  e_paper_printout = lv_paper.

      ls_cache-fatura_kapsami = COND #(
        WHEN lv_einv = abap_true  THEN c_fatura_kapsami-e_fatura
        WHEN lv_paper = abap_true THEN c_fatura_kapsami-kagit ).

      " E-Arşiv """"""""""""""""""""""""""""""""""""""""""""""""""""

      IF iv_purpose <> zcl_sd_customer=>c_purpose-mutabakat AND ls_cache-fatura_kapsami <> c_fatura_kapsami-e_fatura.

        DATA(lv_taxnu) = CONV /fite/arc_1_de017( ls_cache-stcd2 ).

        CALL FUNCTION '/FITE/ARC_1_F002'
          EXPORTING i_tax_number     = lv_taxnu
                    i_inv_date       = sy-datum
          IMPORTING e_arch_partn_ind = lv_earc.

        ls_cache-fatura_kapsami = COND #(
          WHEN lv_earc = abap_true
          THEN c_fatura_kapsami-e_arsiv
          ELSE ls_cache-fatura_kapsami ).

      ENDIF.

      " Bu kadar """"""""""""""""""""""""""""""""""""""""""""""""""""

      INSERT ls_cache
             INTO TABLE gt_fatura_kapsami_cache
             ASSIGNING <ls_cache>.

    ENDIF.

    rv_scope = <ls_cache>-fatura_kapsami.
  ENDMETHOD.

  METHOD put_name1_into_itab.
    FIELD-SYMBOLS <lt_itab> TYPE ANY TABLE.

    ASSERT ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.
    IF <lt_itab> IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cache_name1( ir_tab  = ir_itab
                     iv_fnam = iv_kunnr_field ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).
      ASSIGN COMPONENT: iv_kunnr_field OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_kunnr>),
                        iv_name1_field OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_name1>).

      ASSERT <lv_kunnr> IS ASSIGNED AND <lv_name1> IS ASSIGNED.

      <lv_name1> = get_name1( <lv_kunnr> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_invoice_scope_by_partner.
    DATA lv_initial_posnr TYPE vbpa-posnr.

    TRY.

        SELECT SINGLE adrnr FROM vbpa
               WHERE vbeln = @iv_vbeln
                 AND posnr = @lv_initial_posnr
                 AND parvw = @c_parvw-payer
               INTO @DATA(lv_adrnr).

        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( c_tabname-vbpa )
                                                    entry_name = |{ iv_vbeln } { c_parvw-payer }| ).
        ENDIF.

        rv_scope = get_invoice_scope_by_address( iv_adrnr   = lv_adrnr
                                                 iv_purpose = iv_purpose ).

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION NEW zcx_sd_doc_inv_scope( previous = lo_diaper
                                                  vbeln    = iv_vbeln ).

    ENDTRY.
  ENDMETHOD.

  METHOD ensure_defined_in_dist_channel.
    ASSIGN gt_dist_chan_cache[ KEY primary_key COMPONENTS vkorg = iv_vkorg
                                                          vtweg = iv_vtweg ]
           TO FIELD-SYMBOL(<ls_cache>).
    IF sy-subrc <> 0.
      DATA(ls_cache) = VALUE t_dist_chan_cache( vkorg = iv_vkorg
                                                vtweg = iv_vtweg ).

      ##WRITE_OK
      SELECT SINGLE FROM knvv                           "#EC CI_NOORDER
             FIELDS loevm
             WHERE kunnr = @gs_def-kunnr
               AND vkorg = @ls_cache-vkorg
               AND vtweg = @ls_cache-vtweg
             INTO @DATA(loevm).

      ls_cache-error = COND #( WHEN sy-subrc <> 0 THEN
                                 NEW #( textid    = zcx_sd_customer_def=>undefined_in_sd_area
                                        kunnr     = gs_def-kunnr
                                        cust_keys = |{ ls_cache-vkorg } { ls_cache-vtweg }| )

                               WHEN sy-subrc = 0 AND loevm = abap_true THEN
                                 NEW #( textid    = zcx_sd_customer_def=>deleted_in_sd_area
                                        kunnr     = gs_def-kunnr
                                        cust_keys = |{ ls_cache-vkorg } { ls_cache-vtweg }| ) ).

      INSERT ls_cache INTO TABLE gt_dist_chan_cache ASSIGNING <ls_cache>.
    ENDIF.

    IF <ls_cache>-error IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-error.
    ENDIF.
  ENDMETHOD.

  METHOD conv_exit_kunnr_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = iv_kunnr
      IMPORTING output = rv_kunnr.
  ENDMETHOD.

  METHOD get_kna1.
    DATA ls_kna1 TYPE kna1.

    ASSIGN gt_kna1[ KEY primary_key COMPONENTS kunnr = iv_kunnr ] TO FIELD-SYMBOL(<ls_kna1>).
    IF sy-subrc <> 0.
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
      EXPORTING input  = iv_kunnr
      IMPORTING output = lv_kunnr.

    ASSIGN gt_knvv[ KEY primary_key
      COMPONENTS kunnr = lv_kunnr
                 vkorg = iv_vkorg
                 vtweg = iv_vtweg
                 spart = iv_spart ]
           TO FIELD-SYMBOL(<ls_knvv>).

    IF sy-subrc <> 0.
      CLEAR ls_knvv.
      SELECT SINGLE * FROM knvv
             INTO ls_knvv
             WHERE kunnr = lv_kunnr
               AND vkorg = iv_vkorg
               AND vtweg = iv_vtweg
               AND spart = iv_spart.
      "
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

    ASSIGN gt_name1[ KEY primary_key COMPONENTS kunnr = iv_kunnr ]
           TO FIELD-SYMBOL(<ls_name1>).

    IF sy-subrc <> 0.
      SELECT SINGLE kunnr name1 INTO CORRESPONDING FIELDS OF ls_name1
             FROM kna1
             WHERE kunnr = iv_kunnr.

      ls_name1-kunnr = iv_kunnr.
      INSERT ls_name1 INTO TABLE gt_name1 ASSIGNING <ls_name1>.
    ENDIF.

    rv_name1 = <ls_name1>-name1.
  ENDMETHOD.

  METHOD get_def_land_sd_values.
    ASSIGN gt_lsv_cache[ KEY primary_key COMPONENTS land1 = iv_land1 ]
           TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc <> 0.

      DATA(ls_cache) = VALUE t_def_lsv_cache( land1 = iv_land1 ).

      IF gs_clazy-flg-def_land_sd_val = abap_false.

        SELECT ddsign, land1 FROM zfit_xd_def_lsd INTO TABLE @DATA(lt_db). "#EC CI_NOWHERE

        gs_clazy-val-def_land_sd_val = VALUE #( FOR ls_db IN lt_db
                                                ( val       = CORRESPONDING #( ls_db )
                                                  land1_rng = VALUE #( ( option = zcl_bc_ddic_toolkit=>c_option_eq
                                                                         sign   = ls_db-ddsign
                                                                         low    = ls_db-land1 ) ) ) ).

        gs_clazy-flg-def_land_sd_val = abap_true.
      ENDIF.

      DATA(lv_found) = abap_false.

      LOOP AT gs_clazy-val-def_land_sd_val ASSIGNING FIELD-SYMBOL(<ls_db>).
        CHECK ls_cache-land1 IN <ls_db>-land1_rng.
        ls_cache-val = <ls_db>-val.
        lv_found = abap_true.
      ENDLOOP.

      IF lv_found IS INITIAL.
        ls_cache-cx = NEW #( textid   = zcx_bc_table_content=>entry_missing
                             objectid = CONV #( ls_cache-land1 )
                             tabname  = c_tabname-lsd ).
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

  METHOD get_land.
    output = zcl_bc_land=>get_instance( VALUE #( land1 = me->gs_def-land1 ) ).
  ENDMETHOD.

  METHOD read_parob_lazy.
    CHECK gs_clazy-flg-parob IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gs_clazy-val-parob FROM zsdt_cus_parob. "#EC CI_NOWHERE
    gs_clazy-flg-parob = abap_true.
  ENDMETHOD.

  METHOD get_company_payment_term.
    ASSIGN gt_comp_pay_term_cache[ KEY primary_key COMPONENTS bukrs = bukrs ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc <> 0.
      SELECT SINGLE
             FROM knb1
                  LEFT OUTER JOIN t052
                    ON t052~zterm = knb1~zterm
             FIELDS t052~ztag1
             WHERE knb1~bukrs = @bukrs
               AND knb1~kunnr = @gs_def-kunnr
             INTO @DATA(ztag1).

      INSERT VALUE #( bukrs = bukrs
                      ztag1 = ztag1 )
             INTO TABLE gt_comp_pay_term_cache
             ASSIGNING <ls_cache>.
    ENDIF.

    result = <ls_cache>-ztag1.
  ENDMETHOD.

  METHOD get_hq_branch_codes.
    SELECT * FROM zfit_mrk_sube
           WHERE bukrs IN @it_bukrs
           INTO TABLE @DATA(lt_db).

    DELETE lt_db WHERE NOT ( knrze IN it_knrze ). " Aralık geniş olabileceği için sorguya koymadım

    LOOP AT lt_db ASSIGNING FIELD-SYMBOL(<ls_db>).

      IF iv_need_br = abap_true.
        APPEND <ls_db>-kunnr TO rt_kunnr.
      ENDIF.

      IF iv_need_hq = abap_true.
        APPEND <ls_db>-knrze TO rt_kunnr.
      ENDIF.

    ENDLOOP.

    SORT rt_kunnr.
    DELETE ADJACENT DUPLICATES FROM rt_kunnr.
  ENDMETHOD.

  METHOD get_head_customer.
    IF gt_hq IS INITIAL.
      SELECT * FROM zfit_mrk_sube
             WHERE kunnr = @gs_def-kunnr
             INTO CORRESPONDING FIELDS OF TABLE @gt_hq
        ##TOO_MANY_ITAB_FIELDS.                         "#EC CI_NOFIRST

      IF gt_hq IS INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_hq.
      ENDIF.
    ENDIF.

    ASSIGN gt_hq[ KEY primary_key COMPONENTS bukrs = iv_bukrs ]
           TO FIELD-SYMBOL(<ls_hq>).

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_sd_customer_hq_def( kunnr = gs_def-kunnr
                                                  bukrs = iv_bukrs ).
    ENDIF.

    IF <ls_hq>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_hq>-cx.
    ENDIF.

    IF <ls_hq>-obj IS INITIAL.

      TRY.
          <ls_hq>-obj = get_instance( <ls_hq>-knrze ).
        CATCH zcx_sd_customer_def INTO DATA(lo_cd).

          <ls_hq>-cx = NEW zcx_sd_customer_hq_def( textid   = zcx_sd_customer_hq_def=>hq_value_invalid
                                                   previous = lo_cd
                                                   kunnr    = gs_def-kunnr
                                                   bukrs    = <ls_hq>-bukrs
                                                   knrze    = <ls_hq>-knrze ).

          RAISE EXCEPTION <ls_hq>-cx.

      ENDTRY.

    ENDIF.

    ro_head = <ls_hq>-obj.
  ENDMETHOD.

  METHOD get_invoice_scope_by_address.
    DATA lt_split TYPE STANDARD TABLE OF ad_name3 WITH DEFAULT KEY.

    TRY.

        SELECT SINGLE name3 FROM adrc
               WHERE addrnumber  = @iv_adrnr
                 AND date_from  <= @sy-datum
                 AND date_to    >= @sy-datum
               INTO @DATA(lv_name3).                         "#EC CI_NOORDER

        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( c_tabname-adrc )
                                                    entry_name = |{ iv_adrnr } { sy-datum }| ).
        ENDIF.

        SPLIT lv_name3 AT ':' INTO TABLE lt_split.

        IF lines( lt_split ) = 2.
          rv_scope = get_invoice_scope_by_tax( iv_stcd2   = CONV #( lt_split[ 2 ] )
                                               iv_purpose = iv_purpose ).
        ELSE.
          rv_scope = c_fatura_kapsami-e_arsiv.
        ENDIF.

      CATCH zcx_sd_doc_inv_scope INTO DATA(lo_cx_scope).
        RAISE EXCEPTION lo_cx_scope.

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION NEW zcx_sd_doc_inv_scope( textid   = zcx_sd_doc_inv_scope=>cant_determine_for_address
                                                  previous = lo_diaper
                                                  adrnr    = iv_adrnr ).

    ENDTRY.
  ENDMETHOD.

  METHOD is_employer.
    rv_employer = xsdbool( gs_def-ktokd = c_ktokd-employer ).
  ENDMETHOD.
ENDCLASS.