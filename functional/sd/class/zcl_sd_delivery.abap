CLASS zcl_sd_delivery DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_head,
        vbeln     TYPE likp-vbeln,
        kunnr     TYPE likp-kunnr,
        customer  TYPE REF TO zcl_sd_customer,
        btgew     TYPE likp-btgew,
        gewei     TYPE likp-gewei,
        vstel     TYPE likp-vstel,
        vbtyp     TYPE likp-vbtyp,
        gbstk     TYPE v_vbuk_s4-gbstk,
        wbstk     TYPE v_vbuk_s4-wbstk,
        wadat_ist TYPE likp-wadat_ist,
        kostk     TYPE v_vbuk_s4-kostk,
        kunag     TYPE likp-kunag,
        lfart     TYPE likp-lfart,
        pkstk     TYPE v_vbuk_s4-pkstk,
        vkorg     TYPE likp-vkorg,
        erdat     TYPE likp-erdat,
      END OF t_head,

      BEGIN OF t_item,
        posnr       TYPE lips-posnr,
        lgort       TYPE lips-lgort,
        werks       TYPE lips-werks,
        storage_loc TYPE REF TO zcl_sd_storage_location,
        stloc_cx    TYPE REF TO zcx_sd_stloc_def,
        vgbel       TYPE lips-vgbel,
        vgpos       TYPE lips-vgpos,
        vgtyp       TYPE lips-vgtyp,
        matnr       TYPE lips-matnr,
        vrkme       TYPE lips-vrkme,
        lfimg       TYPE lips-lfimg,
        meins       TYPE lips-meins,
        pstyv       TYPE lips-pstyv,
        uecha       TYPE lips-uecha,
        lgmng       TYPE lips-lgmng,
        charg       TYPE lips-charg,
        xchpf       TYPE lips-xchpf,
        uepos       TYPE lips-uepos,
        bwtar       TYPE lips-bwtar,
        brgew       TYPE lips-brgew,
        ntgew       TYPE lips-ntgew,
        gewei       TYPE lips-gewei,
        vtweg       TYPE lips-vtweg,
      END OF t_item,

      tt_item TYPE STANDARD TABLE OF t_item WITH DEFAULT KEY,

      BEGIN OF t_weight,
        btgew TYPE likp-btgew,
        gewei TYPE likp-gewei,
      END OF t_weight,

      tt_vbeln TYPE STANDARD TABLE OF vbeln WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF pstyv,
                 cargo       TYPE pstyv VALUE 'ZKRG',
                 sub         TYPE pstyv VALUE 'ZSSP',
                 sub_batch   TYPE pstyv VALUE 'ZBEP',
                 stock_batch TYPE pstyv VALUE 'ZSNP',
               END OF pstyv.

    CONSTANTS: BEGIN OF statv,
                 not_relevant         TYPE statv VALUE space,
                 not_yet_processed    TYPE statv VALUE 'A',
                 partially_processed  TYPE statv VALUE 'B',
                 completely_processed TYPE statv VALUE 'C',
               END OF statv.

    CONSTANTS: BEGIN OF table,
                 delivery_item TYPE tabname VALUE 'LIPS',
               END OF table.

    CONSTANTS: BEGIN OF vbtyp,
                 delivery              TYPE vbtypl VALUE 'J',
                 shipping_notification TYPE vbtypl VALUE '7',
                 mat_doc               TYPE vbtypl VALUE 'R',
               END OF vbtyp.

    CONSTANTS: BEGIN OF wbstk,
                 complete TYPE v_vbuk_s4-wbstk VALUE 'C',
               END OF wbstk.

    CONSTANTS: BEGIN OF parvw,
                 sold_to TYPE vbpa-parvw VALUE 'AG',
               END OF parvw.

    DATA gs_head TYPE t_head.

    CLASS-METHODS:
      get_instance
        IMPORTING iv_vbeln         TYPE likp-vbeln
                  iv_bypass_buffer TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(ro_obj)    TYPE REF TO zcl_sd_delivery
        RAISING   zcx_sd_delivery_def,

      get_total_weight_of_dlvs_wo_hu
        IMPORTING it_vbeln        TYPE tt_vbeln
                  iv_gewei        TYPE likp-gewei
        RETURNING VALUE(rv_btgew) TYPE likp-btgew
        RAISING   zcx_bc_function_subrc
                  zcx_sd_delivery_def,

      get_total_weight_of_deliveries
        IMPORTING it_vbeln        TYPE tt_vbeln
                  iv_gewei        TYPE likp-gewei
        RETURNING VALUE(rv_btgew) TYPE likp-btgew
        RAISING   zcx_bc_function_subrc
                  zcx_sd_delivery_def.

    CLASS-METHODS does_delivery_exist
      IMPORTING iv_vbeln         TYPE likp-vbeln
      RETURNING VALUE(rv_exists) TYPE abap_bool.

    CLASS-METHODS does_delivery_item_exist
      IMPORTING vbeln         TYPE lips-vbeln
                posnr         TYPE lips-posnr
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS:
      attach_gos_doc
        IMPORTING iv_filename    TYPE clike
                  iv_description TYPE clike
                  iv_hex_string  TYPE xstring
        RAISING   zcx_bc_gos_doc_attach,

      del_gos_doc
        IMPORTING is_docid TYPE so_entryid
        RAISING   zcx_bc_gos_doc_delete,

      get_gos_docs
        RETURNING VALUE(rt_doc) TYPE zcl_bc_gos_toolkit=>tt_doc_content
        RAISING   zcx_bc_gos_doc_content,

      get_items
        IMPORTING bypass_buffer  TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rt_item) TYPE tt_item,

      get_orders RETURNING VALUE(rt_vbeln) TYPE tt_vbeln,

      get_total_weight_without_hu
        RETURNING VALUE(rs_weight) TYPE t_weight
        RAISING   zcx_sd_delivery_def,

      get_total_weight
        RETURNING VALUE(rs_weight) TYPE t_weight
        RAISING   zcx_sd_delivery_def,

      has_effective_mat_doc
        IMPORTING bypass_buffer TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_multi_cargo
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_sd_customer_def.

    METHODS has_multiple_orders       RETURNING VALUE(result) TYPE abap_bool.
    METHODS has_multi_dlv_order       RETURNING VALUE(result) TYPE abap_bool.
    METHODS ensure_single_order_dlv   RAISING   zcx_sd_delivery_def.

    METHODS get_singular_dist_channel RETURNING VALUE(result) TYPE vtweg.

    METHODS get_partner_country
      IMPORTING parvw         TYPE vbpa-parvw
      RETURNING VALUE(result) TYPE land1
      RAISING   zcx_sd_delivery_def.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_lazy_flg,
        item               TYPE abap_bool,
        orders             TYPE abap_bool,
        total_weight_wo_hu TYPE abap_bool,
        total_weight       TYPE abap_bool,
        eff_mat_doc        TYPE abap_bool,
      END OF t_lazy_flg.
    TYPES:
      BEGIN OF t_lazy_val,
        item               TYPE tt_item,
        orders             TYPE tt_vbeln,
        total_weight_wo_hu TYPE t_weight,
        total_weight       TYPE t_weight,
        eff_mat_doc        TYPE abap_bool,
      END OF t_lazy_val.
    TYPES:
      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy.
    TYPES:
      BEGIN OF t_multiton,
        vbeln TYPE likp-vbeln,
        cx    TYPE REF TO zcx_sd_delivery_def,
        obj   TYPE REF TO zcl_sd_delivery,
      END OF t_multiton.
    TYPES tt_multiton TYPE HASHED TABLE OF t_multiton
              WITH UNIQUE KEY primary_key COMPONENTS vbeln.

    TYPES vtweg_list  TYPE STANDARD TABLE OF vtweg WITH KEY table_line.

    TYPES: BEGIN OF t_partner_country,
             parvw TYPE vbpa-parvw,
             land1 TYPE land1,
           END OF t_partner_country,

           tt_partner_country TYPE HASHED TABLE OF t_partner_country WITH UNIQUE KEY primary_key COMPONENTS parvw.

    CONSTANTS:
      BEGIN OF c_pkstk,
        completed TYPE pkstk VALUE 'C',
      END OF c_pkstk.
    CONSTANTS:
      BEGIN OF c_vpobj,
        delivery TYPE vpobj VALUE '01',
      END OF c_vpobj.
    CONSTANTS c_gos_classname TYPE bapibds01-classname VALUE 'LIKP' ##NO_TEXT.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA: gs_lazy            TYPE t_lazy,
          gt_partner_country TYPE tt_partner_country.

    METHODS constructor
      IMPORTING iv_vbeln TYPE likp-vbeln
      RAISING   zcx_sd_delivery_def.
ENDCLASS.


CLASS zcl_sd_delivery IMPLEMENTATION.
  METHOD attach_gos_doc.
    zcl_bc_gos_toolkit=>attach_doc( iv_filename    = iv_filename
                                    iv_description = iv_description
                                    iv_hex_string  = iv_hex_string
                                    is_key         = VALUE #( classname = c_gos_classname
                                                              objkey    = gs_head-vbeln ) ).
  ENDMETHOD.

  METHOD constructor.
    TRY.
        SELECT SINGLE likp~vbeln, likp~kunnr, likp~btgew, likp~gewei, likp~vstel, likp~vbtyp, likp~wadat_ist,
                      likp~kunag, likp~lfart, likp~vkorg, likp~erdat, vbuk~gbstk, vbuk~wbstk, vbuk~kostk, vbuk~pkstk
               FROM likp
                    LEFT JOIN v_vbuk_s4 AS vbuk
                      ON vbuk~vbeln = likp~vbeln
               WHERE likp~vbeln = @iv_vbeln
               INTO CORRESPONDING FIELDS OF @gs_head.   "#EC CI_NOORDER

        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zcx_sd_delivery_def( vbeln = iv_vbeln ).
        ENDIF.

        gs_head-customer = COND #( WHEN gs_head-vbtyp <> me->vbtyp-shipping_notification " VOL-12786
                                   THEN zcl_sd_customer=>get_instance( gs_head-kunnr ) ).

      CATCH zcx_sd_delivery_def INTO DATA(lo_def).
        RAISE EXCEPTION lo_def.
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION NEW zcx_sd_delivery_def( textid   = zcx_sd_delivery_def=>invalid_data
                                                 previous = lo_diaper
                                                 vbeln    = iv_vbeln ).
    ENDTRY.
  ENDMETHOD.

  METHOD del_gos_doc.
    zcl_bc_gos_toolkit=>delete_doc( is_folder_id = VALUE #( objtp = is_docid+0(3)
                                                            objyr = is_docid+3(2)
                                                            objno = is_docid+5(12) )
                                    is_object_id = VALUE #( objtp = is_docid+17(3)
                                                            objyr = is_docid+20(2)
                                                            objno = is_docid+22(12) )
                                    is_key       = VALUE #( classname = c_gos_classname
                                                            objkey    = gs_head-vbeln ) ).
  ENDMETHOD.

  METHOD has_effective_mat_doc.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ters kaydı alınmamış malzeme belgesi var mı?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF bypass_buffer = abap_true.
      CLEAR: gs_lazy-flg-eff_mat_doc,
             gs_lazy-val-eff_mat_doc.
    ENDIF.

    IF gs_lazy-flg-eff_mat_doc IS INITIAL.
      DO 1 TIMES.
        SELECT DISTINCT vbeln, mjahr FROM vbfa
               WHERE vbelv   = @gs_head-vbeln
                 AND vbtyp_v = @gs_head-vbtyp
                 AND vbtyp_n = @me->vbtyp-mat_doc
               INTO TABLE @DATA(mat_docs).

        DATA(mkpf_keys) = CORRESPONDING zcl_mm_material_document=>mat_doc_key_list( mat_docs MAPPING mblnr = vbeln ).
        CHECK mkpf_keys IS NOT INITIAL.

        SELECT mblnr FROM zmmv_effective_mat_doc
               FOR ALL ENTRIES IN @mkpf_keys
               WHERE mblnr = @mkpf_keys-mblnr
                 AND mjahr = @mkpf_keys-mjahr
               INTO TABLE @DATA(effective_mat_docs).

        gs_lazy-val-eff_mat_doc = xsdbool( effective_mat_docs IS NOT INITIAL ).
      ENDDO.

      gs_lazy-flg-eff_mat_doc = abap_true.
    ENDIF.

    result = gs_lazy-val-eff_mat_doc.
  ENDMETHOD.

  METHOD get_gos_docs.
    rt_doc = zcl_bc_gos_toolkit=>get_doc_content( VALUE #( classname = c_gos_classname
                                                           objkey    = gs_head-vbeln ) ).
  ENDMETHOD.

  METHOD get_instance.
    IF iv_bypass_buffer = abap_true.
      ro_obj = NEW #( iv_vbeln ).
      RETURN.
    ENDIF.

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS vbeln = iv_vbeln ]
           TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      DATA(ls_mt) = VALUE t_multiton( vbeln = iv_vbeln ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-vbeln ).
        CATCH zcx_sd_delivery_def INTO ls_mt-cx ##NO_HANDLER.
      ENDTRY.

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.
    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.
  ENDMETHOD.

  METHOD get_items.
    IF bypass_buffer = abap_true.
      CLEAR: gs_lazy-flg-item,
             gs_lazy-val-item.
    ENDIF.

    IF gs_lazy-flg-item = abap_false.
      SELECT posnr, lgort, werks, vgbel, vgpos, vgtyp, matnr, vrkme, lfimg, meins, pstyv, uecha, lgmng, charg, xchpf,
             uepos, bwtar, brgew, ntgew, gewei, vtweg
             FROM lips
             WHERE vbeln = @gs_head-vbeln
             INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-item ##TOO_MANY_ITAB_FIELDS.

      LOOP AT gs_lazy-val-item ASSIGNING FIELD-SYMBOL(<ls_item>).
        TRY.
            <ls_item>-storage_loc = zcl_sd_storage_location=>get_instance( iv_werks = <ls_item>-werks
                                                                           iv_lgort = <ls_item>-lgort ).

          CATCH zcx_sd_stloc_def INTO <ls_item>-stloc_cx ##NO_HANDLER.
        ENDTRY.
      ENDLOOP.

      gs_lazy-flg-item = abap_true.
    ENDIF.

    rt_item = gs_lazy-val-item.
  ENDMETHOD.

  METHOD does_delivery_exist.
    SELECT SINGLE mandt FROM likp
           WHERE vbeln = @iv_vbeln
           INTO @sy-mandt ##WRITE_OK.

    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD does_delivery_item_exist.
    SELECT SINGLE FROM lips
           FIELDS @abap_true
           WHERE vbeln = @vbeln
             AND posnr = @posnr
           INTO @result.
  ENDMETHOD.

  METHOD get_total_weight_of_dlvs_wo_hu.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VEKP hariç toplam teslimat ağırlığı
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT it_vbeln ASSIGNING FIELD-SYMBOL(<lv_vbeln>).
      DATA(ls_weight)               = zcl_sd_delivery=>get_instance( <lv_vbeln> )->get_total_weight_without_hu( ).
      DATA(lv_dlv_weight_converted) = CONV likp-btgew( 0 ).

      ##FM_SUBRC_OK
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING  input                = ls_weight-btgew
                   unit_in              = ls_weight-gewei
                   unit_out             = iv_gewei
        IMPORTING  output               = lv_dlv_weight_converted
        EXCEPTIONS conversion_not_found = 1
                   division_by_zero     = 2
                   input_invalid        = 3
                   output_invalid       = 4
                   overflow             = 5
                   type_invalid         = 6
                   units_missing        = 7
                   unit_in_not_found    = 8
                   unit_out_not_found   = 9
                   OTHERS               = 10.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).
      rv_btgew += lv_dlv_weight_converted.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_total_weight_of_deliveries.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VEKP dahil toplam teslimat ağırlığı
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT it_vbeln ASSIGNING FIELD-SYMBOL(<lv_vbeln>).
      DATA(ls_weight)               = zcl_sd_delivery=>get_instance( <lv_vbeln> )->get_total_weight( ).
      DATA(lv_dlv_weight_converted) = CONV likp-btgew( 0 ).

      ##FM_SUBRC_OK
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING  input                = ls_weight-btgew
                   unit_in              = ls_weight-gewei
                   unit_out             = iv_gewei
        IMPORTING  output               = lv_dlv_weight_converted
        EXCEPTIONS conversion_not_found = 1
                   division_by_zero     = 2
                   input_invalid        = 3
                   output_invalid       = 4
                   overflow             = 5
                   type_invalid         = 6
                   units_missing        = 7
                   unit_in_not_found    = 8
                   unit_out_not_found   = 9
                   OTHERS               = 10.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).
      rv_btgew += lv_dlv_weight_converted.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_multi_cargo.
    DATA(cargo_kunnr) = COND kunnr( WHEN gs_head-kunag IS NOT INITIAL
                                    THEN gs_head-kunag
                                    ELSE gs_head-kunnr ).

    result = CAST zif_sd_ckkrg_customer_set( zcl_sd_ckkrg_all_customers=>get_instance( ) )->has_customer( cargo_kunnr ).
  ENDMETHOD.

  METHOD get_orders.
    IF gs_lazy-flg-orders IS INITIAL.

      gs_lazy-val-orders = VALUE #( FOR GROUPS _vgbel OF _item IN get_items( )
                                    GROUP BY _item-vgbel
                                    ( _vgbel ) ).

      SORT gs_lazy-val-orders.
      DELETE ADJACENT DUPLICATES FROM gs_lazy-val-orders.
      DELETE gs_lazy-val-orders WHERE table_line IS INITIAL.

      gs_lazy-flg-orders = abap_true.
    ENDIF.

    rt_vbeln = gs_lazy-val-orders.
  ENDMETHOD.

  METHOD get_total_weight_without_hu.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VEKP olmadan toplam ağırlık hesaplar
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        IF gs_lazy-flg-total_weight_wo_hu IS INITIAL.
          DATA(plt_kostl) = zcl_sd_plt_mdref_kostl=>get_instance( ).
          gs_lazy-val-total_weight_wo_hu-gewei = gs_head-gewei.

          DATA(lipss) = get_items( ).

          LOOP AT lipss ASSIGNING FIELD-SYMBOL(<lips>)
               WHERE    brgew IS NOT INITIAL
                     OR ntgew IS NOT INITIAL.

            CHECK NOT plt_kostl->is_mat_plt_bklas( matnr = <lips>-matnr
                                                   werks = <lips>-werks ).

            DATA(lips_weight) = SWITCH #( gs_head-pkstk
                                          WHEN 'C'
                                          THEN <lips>-brgew
                                          ELSE <lips>-ntgew ).

            CASE <lips>-gewei.
              WHEN gs_lazy-val-total_weight_wo_hu-gewei.
                DATA(converted_lips_weight) = lips_weight.

              WHEN OTHERS.
                CLEAR converted_lips_weight.

                ##FM_SUBRC_OK
                CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
                  EXPORTING  input                = lips_weight
                             unit_in              = <lips>-gewei
                             unit_out             = gs_lazy-val-total_weight_wo_hu-gewei
                  IMPORTING  output               = converted_lips_weight
                  EXCEPTIONS conversion_not_found = 1
                             division_by_zero     = 2
                             input_invalid        = 3
                             output_invalid       = 4
                             overflow             = 5
                             type_invalid         = 6
                             units_missing        = 7
                             unit_in_not_found    = 8
                             unit_out_not_found   = 9
                             OTHERS               = 10.

                zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).
            ENDCASE.

            gs_lazy-val-total_weight_wo_hu-btgew +=
                                                 converted_lips_weight.
          ENDLOOP.

          gs_lazy-flg-total_weight_wo_hu = abap_true.
        ENDIF.

        rs_weight = gs_lazy-val-total_weight_wo_hu.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_sd_delivery_def( textid   = zcx_sd_delivery_def=>cant_calc_weight
                                                 previous = diaper
                                                 vbeln    = gs_head-vbeln ).
    ENDTRY.
  ENDMETHOD.

  METHOD has_multiple_orders.
    DATA(orders) = get_orders( ).
    result = xsdbool( lines( orders ) > 1 ).
  ENDMETHOD.

  METHOD has_multi_dlv_order.
    SELECT DISTINCT vbfa~vbeln
           FROM lips
                INNER JOIN vbfa
                  ON  vbfa~vbelv   = lips~vgbel
                  AND vbfa~vbtyp_n = 'J'
           WHERE lips~vbeln = @gs_head-vbeln
           INTO TABLE @DATA(vbelns).

    result = xsdbool( lines( vbelns ) > 1 ).
  ENDMETHOD.

  METHOD ensure_single_order_dlv.
    CHECK    has_multiple_orders( )
          OR has_multi_dlv_order( ).

    RAISE EXCEPTION NEW zcx_sd_delivery_def( textid = zcx_sd_delivery_def=>multi_order_delivery ).
  ENDMETHOD.

  METHOD get_singular_dist_channel.
    DATA(items) = get_items( ).

    DATA(vtwegs) = VALUE vtweg_list( FOR GROUPS _vtweg OF _item IN items
                                     WHERE ( vtweg IS NOT INITIAL )
                                     GROUP BY _item-vtweg
                                     ( _vtweg ) ).

    CASE lines( vtwegs ).
      WHEN 0.
        RETURN.
      WHEN 1.
        result = vtwegs[ 1 ].
      WHEN OTHERS.
        ##FIXME. " Burada düzgün bir exception
        ASSERT 1 = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD get_partner_country.
    DATA empty_posnr TYPE vbpa-posnr.

    TRY.
        DATA(lr_pc) = REF #( gt_partner_country[ KEY primary_key
                                                 parvw = parvw ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(ls_new_pc) = VALUE t_partner_country( parvw = parvw ).

        SELECT SINGLE
               FROM vbpa
                    INNER JOIN adrc
                      ON adrc~addrnumber = vbpa~adrnr
               FIELDS adrc~country
               WHERE vbpa~vbeln      = @gs_head-vbeln
                 AND vbpa~posnr      = @empty_posnr
                 AND vbpa~parvw      = @ls_new_pc-parvw
                 AND adrc~date_from <= @sy-datum
                 AND adrc~nation     = @space
                 AND adrc~date_to   >= @sy-datum
               INTO @ls_new_pc-land1.

        IF ls_new_pc-land1 IS INITIAL.
          RAISE EXCEPTION NEW zcx_sd_delivery_def( textid = zcx_sd_delivery_def=>no_country_for_partner
                                                   vbeln  = gs_head-vbeln
                                                   parvw  = ls_new_pc-parvw ).
        ENDIF.

        INSERT ls_new_pc INTO TABLE gt_partner_country REFERENCE INTO lr_pc.
    ENDTRY.

    result = lr_pc->land1.
  ENDMETHOD.

  METHOD get_total_weight.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VEKP dahil toplam ağırlık hesaplar
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        IF gs_lazy-flg-total_weight IS INITIAL.

          " LIPS-BRGEW toplamı """"""""""""""""""""""""""""""""""""""""""
          DATA(plt_kostl) = zcl_sd_plt_mdref_kostl=>get_instance( ).
          gs_lazy-val-total_weight-gewei = gs_head-gewei.

          DATA(lipss) = get_items( ).

          LOOP AT lipss ASSIGNING FIELD-SYMBOL(<lips>)
               WHERE brgew IS NOT INITIAL.

            CHECK NOT plt_kostl->is_mat_plt_bklas( matnr = <lips>-matnr
                                                   werks = <lips>-werks ).

            CASE <lips>-gewei.
              WHEN gs_lazy-val-total_weight-gewei.
                DATA(lips_brgew) = <lips>-brgew.
              WHEN OTHERS.
                CLEAR lips_brgew.

                CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
                  EXPORTING  input                = <lips>-brgew
                             unit_in              = <lips>-gewei
                             unit_out             = gs_lazy-val-total_weight-gewei
                  IMPORTING  output               = lips_brgew
                  EXCEPTIONS conversion_not_found = 1
                             division_by_zero     = 2
                             input_invalid        = 3
                             output_invalid       = 4
                             overflow             = 5
                             type_invalid         = 6
                             units_missing        = 7
                             unit_in_not_found    = 8
                             unit_out_not_found   = 9
                             OTHERS               = 10
                  ##FM_SUBRC_OK.

                zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).
            ENDCASE.

            gs_lazy-val-total_weight-btgew +=
                                           lips_brgew.
          ENDLOOP.

          " VEKP toplamı """"""""""""""""""""""""""""""""""""""""""""""""
          DATA(lv_vpobjkey) = CONV vpobjkey( gs_head-vbeln ).

          SELECT tarag, gewei FROM vekp
                 WHERE vpobj    = @c_vpobj-delivery
                   AND vpobjkey = @lv_vpobjkey
                 INTO TABLE @DATA(lt_vekp).

          LOOP AT lt_vekp ASSIGNING FIELD-SYMBOL(<ls_vekp>).
            IF <ls_vekp>-gewei = gs_lazy-val-total_weight-gewei.
              gs_lazy-val-total_weight-btgew += <ls_vekp>-tarag.
            ELSE.
              DATA(lv_tarag_in_likp_gewei) = CONV vekp-tarag( 0 ).

              ##FM_SUBRC_OK
              CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
                EXPORTING  input                = <ls_vekp>-tarag
                           unit_in              = <ls_vekp>-gewei
                           unit_out             = gs_lazy-val-total_weight-gewei
                IMPORTING  output               = lv_tarag_in_likp_gewei
                EXCEPTIONS conversion_not_found = 1
                           division_by_zero     = 2
                           input_invalid        = 3
                           output_invalid       = 4
                           overflow             = 5
                           type_invalid         = 6
                           units_missing        = 7
                           unit_in_not_found    = 8
                           unit_out_not_found   = 9
                           OTHERS               = 10.

              zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).
              gs_lazy-val-total_weight-btgew += lv_tarag_in_likp_gewei.
            ENDIF.
          ENDLOOP.

          gs_lazy-flg-total_weight = abap_true.
        ENDIF.

        rs_weight = gs_lazy-val-total_weight.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_sd_delivery_def( textid   = zcx_sd_delivery_def=>cant_calc_weight
                                                 previous = diaper
                                                 vbeln    = gs_head-vbeln ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.