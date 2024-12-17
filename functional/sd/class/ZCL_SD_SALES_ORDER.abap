CLASS zcl_sd_sales_order DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_vbak,
        vkorg    TYPE vbak-vkorg,
        vtweg    TYPE vbak-vtweg,
        spart    TYPE vbak-spart,
        kunnr    TYPE vbak-kunnr,
        faksk    TYPE vbak-faksk,
        waerk    TYPE vbak-waerk,
        kvgr2    TYPE vbak-kvgr2,
        bukrs_vf TYPE vbak-bukrs_vf,
      END OF t_vbak.
    TYPES:
      BEGIN OF t_ord_dlv_map,
        vbeln_va TYPE vbeln_va,
        posnr_va TYPE posnr_va,
        vbeln_vl TYPE vbeln_vl,
        posnr_vl TYPE posnr_vl,
      END OF t_ord_dlv_map.
    TYPES tt_ord_dlv_map TYPE STANDARD TABLE OF t_ord_dlv_map WITH DEFAULT KEY.
    TYPES:
      BEGIN OF t_ord_inv_map,
        vbeln_va TYPE vbeln_va,
        posnr_va TYPE posnr_va,
        vbeln_vl TYPE vbeln_vl,
        posnr_vl TYPE posnr_vl,
        vbeln_vf TYPE vbeln_vf,
        posnr_vf TYPE posnr_vf,
        fkart    TYPE fkart,
        kunrg    TYPE vbrk-kunrg,
      END OF t_ord_inv_map.
    TYPES tt_ord_inv_map TYPE STANDARD TABLE OF t_ord_inv_map WITH DEFAULT KEY.
    TYPES:
      BEGIN OF t_partner,
        kunnr TYPE vbpa-kunnr,
        parvw TYPE vbpa-parvw,
        adrnr TYPE vbpa-adrnr,
        name1 TYPE adrc-name1,
        name2 TYPE adrc-name2,
        name3 TYPE adrc-name3,
        name4 TYPE adrc-name4,
      END OF t_partner.
    TYPES:
      BEGIN OF t_item_partner,
        posnr TYPE vbpa-posnr,
        parvw TYPE vbpa-parvw,
        kunnr TYPE vbpa-kunnr,
      END OF t_item_partner.
    TYPES:
      BEGIN OF t_vbkd,
        ihrez_e TYPE vbkd-ihrez_e,
      END OF t_vbkd.

    DATA gv_vbeln TYPE vbeln_va READ-ONLY.

    CONSTANTS: BEGIN OF c_ktgrm,
                 end_product     TYPE ktgrm VALUE '01', " Mamul
                 commercial_good TYPE ktgrm VALUE '03', " Ticari mal
               END OF c_ktgrm.

    CONSTANTS: BEGIN OF c_material_procedure,
                 sanal  TYPE char1 VALUE 'S',
                 ticari TYPE char1 VALUE 'T',
               END OF c_material_procedure.

    CONSTANTS: BEGIN OF c_parvw,
                 payer   TYPE parvw VALUE 'RG',
                 sold_to TYPE parvw VALUE 'AG',
               END OF c_parvw.

    CONSTANTS c_default_max_wait TYPE i VALUE 60.

    CLASS-METHODS get_instance
      IMPORTING iv_vbeln         TYPE vbeln_va
                iv_wait          TYPE i         OPTIONAL
                iv_bypass_buffer TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ro_obj)    TYPE REF TO zcl_sd_sales_order
      RAISING   zcx_sd_order.

    CLASS-METHODS get_material_procedure
      IMPORTING iv_matnr            TYPE vbap-matnr
                iv_upmat            TYPE vbap-upmat
                iv_vkorg            TYPE vbak-vkorg
                iv_vtweg            TYPE vbak-vtweg
      RETURNING VALUE(rv_procedure) TYPE char1
      RAISING   zcx_sd_zm_material.

    CLASS-METHODS wait_until_order_created
      IMPORTING iv_vbeln    TYPE vbeln_va
                iv_max_wait TYPE i DEFAULT c_default_max_wait
      RAISING   zcx_sd_order.

    CLASS-METHODS wait_until_order_item_created
      IMPORTING iv_vbeln TYPE vbeln_va
                iv_posnr TYPE posnr_va
      RAISING   zcx_sd_order.

    CLASS-METHODS wait_until_order_lockable
      IMPORTING iv_vbeln    TYPE vbeln_va
                iv_max_wait TYPE i DEFAULT c_default_max_wait
      RAISING   zcx_bc_lock.

    CLASS-METHODS exists_in_curr_month_for_mat
      IMPORTING matnr         TYPE matnr
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS get_mats_in_curr_month
      IMPORTING matnrs        TYPE table_matnr
      RETURNING VALUE(result) TYPE table_matnr.

    METHODS are_all_items_rejected
      RETURNING VALUE(rv_all_rejected) TYPE abap_bool.

    METHODS ensure_item_exists
      IMPORTING iv_posnr_va TYPE vbap-posnr
      RAISING   cx_no_entry_in_table.

    METHODS ensure_material_exists
      IMPORTING iv_matnr TYPE vbap-matnr
      RAISING   zcx_sd_order.

    METHODS get_deliveries
      RETURNING VALUE(rt_map) TYPE tt_ord_dlv_map.

    METHODS get_header
      RETURNING VALUE(rs_vbak) TYPE t_vbak.

    METHODS get_invoices
      RETURNING VALUE(rt_map) TYPE tt_ord_inv_map.

    METHODS get_partner
      IMPORTING iv_parvw          TYPE parvw
      RETURNING VALUE(rs_partner) TYPE t_partner
      RAISING   zcx_bc_table_content.

    METHODS get_item_partner
      IMPORTING iv_parvw          TYPE parvw
                iv_posnr          TYPE vbpa-posnr
      RETURNING VALUE(rs_partner) TYPE t_item_partner
      RAISING   zcx_bc_table_content.

    METHODS get_sole_item_number
      RETURNING VALUE(rv_posnr) TYPE posnr_va
      RAISING   zcx_bc_table_content.

    METHODS has_product
      RETURNING VALUE(rv_has) TYPE abap_bool.

    METHODS is_invoiced
      RETURNING VALUE(rv_invoiced) TYPE abap_bool.

    METHODS is_scope_fat_ayristi
      RETURNING VALUE(rv_tek) TYPE abap_bool.

    METHODS get_header_business_data
      RETURNING VALUE(result) TYPE t_vbkd.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_multiton,
        vbeln TYPE vbeln_va,
        obj   TYPE REF TO zcl_sd_sales_order,
        cx    TYPE REF TO zcx_sd_order,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS vbeln,

      BEGIN OF t_vbap,
        posnr TYPE vbap-posnr,
        abgru TYPE vbap-abgru,
        ktgrm TYPE vbap-ktgrm,
        uepos TYPE vbap-uepos,
        upmat TYPE vbap-upmat,
        matnr TYPE vbap-matnr,
      END OF t_vbap,

      tt_vbap         TYPE STANDARD TABLE OF t_vbap WITH DEFAULT KEY,

      tt_partner      TYPE HASHED TABLE OF t_partner
        WITH UNIQUE KEY primary_key COMPONENTS parvw,

      tt_item_partner TYPE HASHED TABLE OF t_item_partner
        WITH UNIQUE KEY primary_key COMPONENTS posnr parvw,

      BEGIN OF t_lazy_flg,
        dlv           TYPE abap_bool,
        has_product   TYPE abap_bool,
        inv           TYPE abap_bool,
        partners      TYPE abap_bool,
        item_partners TYPE abap_bool,
        sfa           TYPE abap_bool,
        vbak          TYPE abap_bool,
        vbap          TYPE abap_bool,
        header_vbkd   TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        dlv           TYPE tt_ord_dlv_map,
        has_product   TYPE abap_bool,
        inv           TYPE tt_ord_inv_map,
        partners      TYPE tt_partner,
        item_partners TYPE tt_item_partner,
        sfa           TYPE abap_bool,
        vbak          TYPE t_vbak,
        vbap          TYPE tt_vbap,
        header_vbkd   TYPE t_vbkd,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      tt_fkart_rng TYPE RANGE OF fkart,

      BEGIN OF t_material_procedure_cache,
        matnr     TYPE vbap-matnr,
        vkorg     TYPE vbak-vkorg,
        vtweg     TYPE vbak-vtweg,
        procedure TYPE char1,
      END OF t_material_procedure_cache,

      tt_material_procedure_cache TYPE HASHED TABLE OF t_material_procedure_cache
                                  WITH UNIQUE KEY primary_key COMPONENTS matnr vkorg vtweg.

    TYPES: BEGIN OF t_matnr_bool,
             matnr     TYPE matnr,
             existence TYPE abap_bool,
           END OF t_matnr_bool,

           tt_matnr_bool TYPE HASHED TABLE OF t_matnr_bool WITH UNIQUE KEY primary_key COMPONENTS matnr.

    CONSTANTS: BEGIN OF c_mtpos,
                 set TYPE mtpos VALUE 'ZLUM',
               END OF c_mtpos,

               BEGIN OF c_tabname,
                 head    TYPE tabname VALUE 'VBAK',
                 item    TYPE tabname VALUE 'VBAP',
                 partner TYPE tabname VALUE 'VBPA',
               END OF c_tabname,

               BEGIN OF c_vbtyp,
                 dlv TYPE vbtypl VALUE 'J',
                 inv TYPE vbtypl VALUE 'M',
               END OF c_vbtyp.

    CLASS-DATA: gt_material_procedure_cache TYPE tt_material_procedure_cache,
                gt_multiton                 TYPE tt_multiton,
                gt_cur_mon_mat_exist_cache  TYPE tt_matnr_bool.

    DATA gs_lazy TYPE t_lazy.

    METHODS:
      read_partners_lazy,
      read_item_partners_lazy,
      read_vbak_lazy,
      read_vbap_lazy.
ENDCLASS.


CLASS zcl_sd_sales_order IMPLEMENTATION.
  METHOD are_all_items_rejected.
    read_vbap_lazy( ).

    LOOP AT gs_lazy-val-vbap TRANSPORTING NO FIELDS
         WHERE     uepos IS INITIAL
               AND abgru IS INITIAL.
      RETURN.
    ENDLOOP.

    rv_all_rejected = abap_true.
  ENDMETHOD.

  METHOD ensure_item_exists.
    read_vbap_lazy( ).

    IF NOT line_exists( gs_lazy-val-vbap[ posnr = iv_posnr_va ] ).
      RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( c_tabname-item )
                                                entry_name = CONV #( iv_posnr_va ) ).
    ENDIF.
  ENDMETHOD.

  METHOD ensure_material_exists.
    read_vbap_lazy( ).

    IF NOT line_exists( gs_lazy-val-vbap[ matnr = iv_matnr ] ).
      RAISE EXCEPTION NEW zcx_sd_order( textid = zcx_sd_order=>material_not_in_order
                                        vbeln  = gv_vbeln
                                        matnr  = iv_matnr ).
    ENDIF.
  ENDMETHOD.

  METHOD get_deliveries.
    IF gs_lazy-flg-dlv IS INITIAL.
      SELECT vbelv AS vbeln_va,
             posnv AS posnr_va,
             vbeln AS vbeln_vl,
             posnn AS posnr_vl
             FROM vbfa
             WHERE vbelv   = @gv_vbeln
               AND vbtyp_n = @c_vbtyp-dlv
             INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-dlv.

      gs_lazy-flg-dlv = abap_true.
    ENDIF.

    rt_map = gs_lazy-val-dlv.
  ENDMETHOD.

  METHOD get_header.
    read_vbak_lazy( ).
    rs_vbak = gs_lazy-val-vbak.
  ENDMETHOD.

  METHOD get_instance.
    IF iv_bypass_buffer = abap_true.
      DELETE gt_multiton WHERE vbeln = iv_vbeln.
    ENDIF.

    ASSIGN gt_multiton[ KEY         primary_key
                        COMPONENTS vbeln = iv_vbeln ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( vbeln = iv_vbeln ).

      ls_multiton-obj = NEW #( ).
      DATA(lv_waited_sec) = 0.

      DO.
        SELECT SINGLE vbeln FROM vbak
               WHERE vbeln = @ls_multiton-vbeln
               INTO @ls_multiton-obj->gv_vbeln.

        IF sy-subrc = 0.
          EXIT.
        ELSE.
          lv_waited_sec += 1.

          IF lv_waited_sec > iv_wait.
            ls_multiton-cx = NEW #( textid = zcx_sd_order=>order_doesnt_exist
                                    vbeln  = ls_multiton-vbeln ).
            EXIT.
          ENDIF.

          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    IF <ls_multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_multiton>-cx.
    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD get_invoices.
    IF gs_lazy-flg-inv IS INITIAL.
      DATA(lt_dlv) = get_deliveries( ).

      IF lt_dlv IS NOT INITIAL.

        SELECT vbfa~vbelv, vbfa~posnv, vbfa~vbeln, vbfa~posnn, vbrk~fkart, vbrk~kunrg
               FROM vbfa
                    INNER JOIN vbrk
                      ON vbrk~vbeln = vbfa~vbeln
               FOR ALL ENTRIES IN @lt_dlv
               WHERE vbelv   = @lt_dlv-vbeln_vl
                 AND posnv   = @lt_dlv-posnr_vl
                 AND vbtyp_n = @c_vbtyp-inv
                 AND draft   = @space
               INTO TABLE @DATA(lt_vbfa).   "#EC CI_DB_OPERATION_OK[2768887]

        LOOP AT lt_dlv ASSIGNING FIELD-SYMBOL(<ls_dlv>).

          LOOP AT lt_vbfa
               ASSIGNING FIELD-SYMBOL(<ls_vbfa>)
               WHERE
                         vbelv = <ls_dlv>-vbeln_vl
                     AND posnv = <ls_dlv>-posnr_vl.

            APPEND VALUE #( vbeln_va = <ls_dlv>-vbeln_va
                            posnr_va = <ls_dlv>-posnr_va
                            vbeln_vl = <ls_dlv>-vbeln_vl
                            posnr_vl = <ls_dlv>-posnr_vl
                            vbeln_vf = <ls_vbfa>-vbeln
                            posnr_vf = <ls_vbfa>-posnn
                            fkart    = <ls_vbfa>-fkart
                            kunrg    = <ls_vbfa>-kunrg )
                   TO gs_lazy-val-inv.

          ENDLOOP.
        ENDLOOP.

      ENDIF.

      gs_lazy-flg-inv = abap_true.
    ENDIF.

    rt_map = gs_lazy-val-inv.
  ENDMETHOD.

  METHOD wait_until_order_item_created.
    wait_until_order_created( iv_vbeln ).

    DO c_default_max_wait TIMES.
      SELECT SINGLE mandt FROM vbap
             WHERE vbeln = @iv_vbeln
               AND posnr = @iv_posnr
             INTO @sy-mandt ##WRITE_OK.

      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    RAISE EXCEPTION NEW zcx_sd_order( textid = zcx_sd_order=>order_item_not_found
                                      vbeln  = iv_vbeln
                                      posnr  = iv_posnr ).
  ENDMETHOD.

  METHOD wait_until_order_lockable.
    DO iv_max_wait TIMES.
      CALL FUNCTION 'ENQUEUE_EVVBAKE'
        EXPORTING  vbeln          = iv_vbeln
        EXCEPTIONS foreign_lock   = 1
                   system_failure = 2
                   OTHERS         = 3.

      CASE sy-subrc.
        WHEN 0.
          CALL FUNCTION 'DEQUEUE_EVVBAKE'
            EXPORTING vbeln = iv_vbeln.

          RETURN.
        WHEN OTHERS.
          WAIT UP TO 1 SECONDS.
      ENDCASE.
    ENDDO.

    RAISE EXCEPTION NEW zcx_bc_lock( textid   = zcx_bc_lock=>locked_for_too_long
                                     bname    = CONV #( sy-msgv1 )
                                     objectid = CONV #( iv_vbeln ) ).
  ENDMETHOD.

  METHOD exists_in_curr_month_for_mat.
    TRY.
        result = gt_cur_mon_mat_exist_cache[ KEY primary_key COMPONENTS matnr = matnr ]-existence.

      CATCH cx_sy_itab_line_not_found.
        DATA(lt_order_existence) = get_mats_in_curr_month( VALUE #( ( matnr ) ) ).
        result                   = xsdbool( lt_order_existence IS NOT INITIAL ).

        INSERT VALUE #( matnr     = matnr
                        existence = result )
               INTO TABLE gt_cur_mon_mat_exist_cache.
    ENDTRY.
  ENDMETHOD.

  METHOD get_mats_in_curr_month.
    DATA(lt_unique_mats) = VALUE table_matnr( FOR GROUPS _grp OF _mat IN matnrs
                                              WHERE ( table_line IS NOT INITIAL )
                                              GROUP BY _mat
                                              ( _grp ) ).

    CHECK lt_unique_mats IS NOT INITIAL.
    DATA(lt_readable_mats) = VALUE table_matnr( ).

    LOOP AT lt_unique_mats REFERENCE INTO DATA(lr_unique_mat).
      CHECK NOT line_exists( gt_cur_mon_mat_exist_cache[ KEY primary_key COMPONENTS matnr = lr_unique_mat->* ] ).
      APPEND lr_unique_mat->* TO lt_readable_mats.
      CONTINUE.
    ENDLOOP.

    IF lt_readable_mats IS NOT INITIAL.
      DATA(lv_start_of_month) = CONV erdat( |{ sy-datum+0(6) }01| ).

      ##ITAB_KEY_IN_SELECT
      SELECT FROM vbak
                  INNER JOIN vbap
                    ON vbap~vbeln = vbak~vbeln
                  INNER JOIN @lt_readable_mats AS _mat
                    ON _mat~table_line = vbap~matnr
             FIELDS DISTINCT vbap~matnr,
                             @abap_true AS existence
             WHERE vbak~erdat >= @lv_start_of_month
               AND vbak~erdat <= @sy-datum
             APPENDING CORRESPONDING FIELDS OF TABLE @gt_cur_mon_mat_exist_cache.
    ENDIF.

    LOOP AT lt_unique_mats REFERENCE INTO lr_unique_mat.
      TRY.
          DATA(lr_cache) = REF #( gt_cur_mon_mat_exist_cache[ KEY primary_key COMPONENTS matnr = lr_unique_mat->* ] ).
          CHECK lr_cache->existence = abap_true.
          APPEND lr_unique_mat->* TO result.

        CATCH cx_sy_itab_line_not_found.
          INSERT VALUE #( matnr     = lr_unique_mat->*
                          existence = abap_false )
                 INTO TABLE gt_cur_mon_mat_exist_cache.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_partner.
    read_partners_lazy( ).

    ASSIGN gs_lazy-val-partners[ KEY primary_key COMPONENTS parvw = iv_parvw ]
           TO FIELD-SYMBOL(<ls_partner>).

    IF sy-subrc = 0.
      rs_partner = <ls_partner>.
    ELSE.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                objectid = |{ gv_vbeln } { iv_parvw }|
                                                tabname  = c_tabname-partner ).
    ENDIF.
  ENDMETHOD.

  METHOD get_sole_item_number.
    read_vbap_lazy( ).

    CASE lines( gs_lazy-val-vbap ).
      WHEN 0.
        RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                  objectid = CONV #( gv_vbeln )
                                                  tabname  = c_tabname-item ).

      WHEN 1.
        rv_posnr = gs_lazy-val-vbap[ 1 ]-posnr.

      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>multiple_entries_for_key
                                                  objectid = CONV #( gv_vbeln )
                                                  tabname  = c_tabname-item ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_material_procedure.
    DATA(lv_procedure_matnr) = COND matnr( WHEN iv_upmat IS NOT INITIAL
                                           THEN iv_upmat
                                           ELSE iv_matnr ).

    ASSIGN gt_material_procedure_cache[ KEY primary_key COMPONENTS matnr = lv_procedure_matnr
                                                                   vkorg = iv_vkorg
                                                                   vtweg = iv_vtweg ]
           TO FIELD-SYMBOL(<ls_cache>).
    IF sy-subrc <> 0.

      DATA(ls_cache) = VALUE t_material_procedure_cache( matnr = lv_procedure_matnr
                                                         vkorg = iv_vkorg
                                                         vtweg = iv_vtweg ).

      DATA(lv_mtpos) = zcl_mm_material=>get_mvke( iv_matnr      = lv_procedure_matnr
                                                  iv_vkorg      = iv_vkorg
                                                  iv_vtweg      = iv_vtweg
                                                  iv_must_exist = abap_true )-mtpos.

      ls_cache-procedure = SWITCH #( lv_mtpos
                                     WHEN c_mtpos-set
                                     THEN c_material_procedure-sanal
                                     ELSE c_material_procedure-ticari ).

      INSERT ls_cache INTO TABLE gt_material_procedure_cache ASSIGNING <ls_cache>.
    ENDIF.

    rv_procedure = <ls_cache>-procedure.
  ENDMETHOD.

  METHOD is_invoiced.
    rv_invoiced = xsdbool( get_invoices( ) IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_scope_fat_ayristi.
    IF gs_lazy-flg-sfa IS INITIAL.
      DATA(lt_inv) = get_invoices( ).

      IF lt_inv IS NOT INITIAL.
        SELECT mandt FROM zsdt_fat_ayristi
               FOR ALL ENTRIES IN @lt_inv
               WHERE fkart = @lt_inv-fkart
                 AND kunnr = @lt_inv-kunrg
               INTO TABLE @DATA(lt_dummy).

        gs_lazy-val-sfa = xsdbool( lt_dummy IS NOT INITIAL ).
      ENDIF.

      gs_lazy-flg-sfa = abap_true.
    ENDIF.

    rv_tek = gs_lazy-val-sfa.
  ENDMETHOD.

  METHOD get_header_business_data.
    IF me->gs_lazy-flg-header_vbkd IS INITIAL.
      SELECT SINGLE * FROM vbkd
             WHERE vbeln = @me->gv_vbeln
               AND posnr = '000000'
             INTO CORRESPONDING FIELDS OF
             @me->gs_lazy-val-header_vbkd.

      me->gs_lazy-flg-header_vbkd = abap_true.
    ENDIF.

    result = me->gs_lazy-val-header_vbkd.
  ENDMETHOD.

  METHOD get_item_partner.
    read_item_partners_lazy( ).

    ASSIGN gs_lazy-val-item_partners[ KEY primary_key COMPONENTS posnr = iv_posnr
                                                                 parvw = iv_parvw ]
           TO FIELD-SYMBOL(<ls_partner>).

    IF sy-subrc = 0.
      rs_partner = <ls_partner>.
    ELSE.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                objectid = |{ gv_vbeln } { iv_posnr } { iv_parvw }|
                                                tabname  = c_tabname-partner ).
    ENDIF.
  ENDMETHOD.

  METHOD read_partners_lazy.
    DATA lv_posnr_initial TYPE vbpa-posnr.

    CHECK gs_lazy-flg-partners = abap_false.

    SELECT DISTINCT vbpa~kunnr, vbpa~parvw, vbpa~adrnr, adrc~name1, adrc~name2, adrc~name3, adrc~name4
           FROM vbpa
                INNER JOIN adrc
                  ON adrc~addrnumber = vbpa~adrnr
           WHERE vbpa~vbeln      = @gv_vbeln
             AND vbpa~posnr      = @lv_posnr_initial
             AND adrc~date_from <= @sy-datum
             AND adrc~date_to   >= @sy-datum
           INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-partners.

    gs_lazy-flg-partners = abap_true.
  ENDMETHOD.

  METHOD read_vbak_lazy.
    CHECK gs_lazy-flg-vbak = abap_false.

    SELECT SINGLE vkorg, vtweg, spart, kunnr, faksk, waerk, kvgr2, bukrs_vf
           INTO CORRESPONDING FIELDS OF @gs_lazy-val-vbak
           FROM vbak
           WHERE vbeln = @gv_vbeln.

    ASSERT sy-subrc = 0.
    gs_lazy-flg-vbak = abap_true.
  ENDMETHOD.

  METHOD read_vbap_lazy.
    CHECK gs_lazy-flg-vbap = abap_false.

    SELECT posnr, abgru, ktgrm, uepos, upmat, matnr
           FROM vbap
           WHERE vbeln = @gv_vbeln
           INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-vbap.

    ASSERT sy-subrc = 0.

    gs_lazy-flg-vbap = abap_true.
  ENDMETHOD.

  METHOD wait_until_order_created.
    DO iv_max_wait TIMES.
      SELECT SINGLE mandt FROM vbak
             WHERE vbeln = @iv_vbeln
             INTO @sy-mandt ##WRITE_OK.

      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    RAISE EXCEPTION NEW zcx_sd_order( textid = zcx_sd_order=>order_not_found
                                      vbeln  = iv_vbeln ).
  ENDMETHOD.

  METHOD read_item_partners_lazy.
    DATA empty_posnr TYPE posnr.

    CHECK gs_lazy-flg-item_partners = abap_false.

    SELECT DISTINCT vbpa~posnr, vbpa~parvw, vbpa~kunnr
           FROM vbpa
           WHERE vbpa~vbeln  = @gv_vbeln
             AND vbpa~posnr <> @empty_posnr
           INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-item_partners.

    gs_lazy-flg-item_partners = abap_true.
  ENDMETHOD.

  METHOD has_product.
    IF gs_lazy-flg-has_product IS INITIAL.
      read_vbap_lazy( ).

      gs_lazy-val-has_product = xsdbool(    line_exists( gs_lazy-val-vbap[ ktgrm = c_ktgrm-end_product ] )
                                         OR line_exists( gs_lazy-val-vbap[ ktgrm = c_ktgrm-commercial_good ] ) ).
      gs_lazy-flg-has_product = abap_true.
    ENDIF.

    rv_has = gs_lazy-val-has_product.
  ENDMETHOD.
ENDCLASS.