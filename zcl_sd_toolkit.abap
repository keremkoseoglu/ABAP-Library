class ZCL_SD_TOOLKIT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_order,
        vbeln TYPE vbak-vbeln,
        vbtyp TYPE vbak-vbtyp,
      END OF ty_order .
  types:
    tt_order TYPE SORTED TABLE OF ty_order
                       WITH NON-UNIQUE KEY vbeln .
  types:
    tt_vbeln TYPE STANDARD TABLE OF vbak-vbeln WITH DEFAULT KEY .
  types T_UYS_ALAN type ZSDT_UYS_ALAN .
  types:
    tt_uys_alan TYPE HASHED TABLE OF t_uys_alan WITH UNIQUE KEY primary_key COMPONENTS auart vkorg vtweg field .
  types:
    tt_ex_werks TYPE HASHED TABLE OF zsdt_ex_werks WITH UNIQUE KEY primary_key COMPONENTS werks spart .
  types:
    BEGIN OF t_matnr,
        matnr TYPE matnr,
      END OF t_matnr .
  types:
    tt_matnr TYPE STANDARD TABLE OF t_matnr WITH DEFAULT KEY .
  types:
    tt_pstyv_rng TYPE RANGE OF vbap-pstyv .
  types:
    BEGIN OF t_quan_wlm,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        matnr TYPE matnr,
        omeng TYPE omeng,
      END OF t_quan_wlm .
  types:
    tt_quan_wlm
            TYPE HASHED TABLE OF t_quan_wlm
            WITH UNIQUE KEY primary_key COMPONENTS werks lgort matnr .

  data CO_LOG type ref to ZCL_BC_APPLOG_FACADE .
  class-data GV_EXTNUMBER type BALNREXT .
  class-data GT_UYS_ALAN type TT_UYS_ALAN .
  class-data GT_EX_WERKS type TT_EX_WERKS .

  class-methods CHECK_KNA1_EXISTENCE
    importing
      !IV_KUNNR type KUNNR
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods CHECK_KNB1_EXISTENCE
    importing
      !IV_BUKRS type KNB1-BUKRS
      !IV_KUNNR type KNB1-KUNNR
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods CHECK_KNMT_EXISTENCE
    importing
      !IV_VKORG type VKORG
      !IV_VTWEG type VTWEG
      !IV_KUNNR type KUNNR
      !IV_MATNR type MATNR
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods CLOSE_SALES_ORDER
    importing
      !IV_VBELN type VBELN_VA
      !IV_POSNR type POSNR_VA
      !IV_REASON_REJECT type CHAR2
    exporting
      !ET_RETURN type BAPIRET2_TAB
    raising
      ZCX_SD_GENEL .
  class-methods COMMIT .
  class-methods CREATE_SD_LOG_OBJECT
    importing
      !IV_EXTNUMBER type CHAR16
    returning
      value(CO_LOG) type ref to ZCL_BC_APPLOG_FACADE .
  class-methods DISPLAY_LOG
    importing
      !T_LOG_HANDLE type BAL_T_LOGH .

  class-methods GET_FIRST_ORDER_OF_BSTKD
    importing
      !IV_BSTKD type VBKD-BSTKD
    returning
      value(RV_VBELN) type VBAK-VBELN
    raising
      ZCX_BC_TABLE_CONTENT .

  class-methods GET_OPEN_DELIVERY_QUAN
    importing
      !IV_MATNR type MATNR
      !IT_WERKS type CFB_T_WERKS_RANGE
    exporting
      !EV_QUAN type OMENG
      !ET_WLM type TT_QUAN_WLM .
  class-methods GET_ORDERS_BY_BSTNK
    importing
      !IV_BSTNK type VBAK-BSTNK
    returning
      value(RT_VBELN) type TT_VBELN .
  class-methods ROLLBACK .
  class-methods GET_SET_KALEM_PSTYV_RNG
    returning
      value(RT_PSTYV_RNG) type TT_PSTYV_RNG .
  class-methods GET_STLOC_STOCK
    importing
      !IT_MATNR type TT_MATNR
      !IT_WERKS type CFB_T_WERKS_RANGE
    returning
      value(RT_STOCK) type ZSDTT_BEKLEYEN_SIPARIS_02 .
  class-methods VA02_ADD_FIELD_IS_REQUIRED
    importing
      !IV_AUART type AUART
      !IV_VKORG type VKORG
      !IV_VTWEG type VTWEG
      !IV_FIELD_NAME type ZSDD_REQUIRED_FIELDS
    exporting
      value(EV_NOT_FOUND) type ABAP_BOOL
    returning
      value(RV_RETURN) type ABAP_BOOL .
  class-methods CHECK_DATE_OVERLAP
    importing
      !IV_BEGDA1 type DATS
      !IV_ENDDA1 type DATS
      !IV_BEGDA2 type DATS
      !IV_ENDDA2 type DATS
    returning
      value(RV_OVERLAP) type CHAR1 .
  class-methods ALPHA_OUTPUT
    importing
      !IV_INPUT type ANY
    returning
      value(RV_OUTPUT) type CHAR10 .

  class-methods CIFT_KALEM_SIPARIS_UY
    importing
      !IV_WERKS type WERKS_D
      !IV_SPART type SPART
    returning
      value(RV_RC) type SUBRC .
  class-methods CHECK_DELIVERY
    importing
      !IV_VBELN type VBELN_VA
      !IV_POSNR type POSNR_VA
    returning
      value(RV_SUBRC) type SUBRC .
  class-methods GET_OPEN_BILLING_QUAN
    importing
      value(IT_VBELN) type ZSDTT_VBELN
    exporting
      value(ET_OPEN_ITEM) type ZSDTT_ACIK_FATURA_MIK
      !ET_RETURN type BAPIRETTAB .
  class-methods CLOSE_SALES_ORDER_ALL
    importing
      !IV_VBELN type VBELN_VA
      !IV_REASON_REJECT type CHAR2
    exporting
      !ET_RETURN type BAPIRET2_TAB
    raising
      ZCX_SD_GENEL .
  class-methods MUSTERI_HF_IPTAL
    importing
      value(IS_VBRK) type VBRK optional
      value(IS_VBAK) type VBAK optional
      value(IT_VBRP) type VBRPVB_T optional
      value(IT_FKART) type J_3RS_SO_INVOICE_SD optional
      value(IV_AUART) type AUART optional
      value(IV_MUKERRERLIK_KONTROL) type ABAP_BOOL optional
    exporting
      value(EV_VBELN_VA) type VBELN_VA
    returning
      value(RV_SUBRC) type SUBRC .
  class-methods UPDATE_CREDIT_CONTROL
    importing
      !IS_KNKK type KNKK
    returning
      value(RT_MESSAGES) type TAB_BDCMSGCOLL .
  class-methods IS_PSTYV_BEDELSIZ
    importing
      !IV_PSTYV type PSTYV
    returning
      value(RV_BEDELSIZ) type ABAP_BOOL .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_kna1_mt,
        kunnr TYPE kunnr,
        subrc TYPE sysubrc,
      END OF t_kna1_mt .
    TYPES:
      tt_kna1_mt
          TYPE HASHED TABLE OF t_kna1_mt
          WITH UNIQUE KEY primary_key COMPONENTS kunnr .
    TYPES:
      BEGIN OF t_kunnr,
        kunnr TYPE kunnr,
      END OF t_kunnr .
    TYPES:
      tt_kunnr
          TYPE HASHED TABLE OF t_kunnr
          WITH UNIQUE KEY primary_key COMPONENTS kunnr .
    TYPES:
      BEGIN OF t_company_clients,
        bukrs TYPE knb1-bukrs,
        kunnr TYPE tt_kunnr,
      END OF t_company_clients .
    TYPES:
      tt_company_clients
          TYPE HASHED TABLE OF t_company_clients
          WITH UNIQUE KEY primary_key COMPONENTS bukrs .
    TYPES:
      BEGIN OF t_odtab,
        matnr TYPE matnr,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        charg TYPE charg_d,
        vbmna TYPE omeng,        "offene Anfragemenge
        vbmnb TYPE omeng,        "offene Angebotsmenge
        vbmnc TYPE omeng,        "offene Auftragsmenge
        vbmne TYPE omeng,        "offene Lieferplanmenge
        vbmng TYPE omeng,        "offene Kontraktmenge
        vbmni TYPE omeng,        "offene Menge kostenloser Liefer.
        vbmnj TYPE omeng,        "offene Liefermenge
        vrkme TYPE vrkme,        "VerkaufsME     ALRK014884 SW
        wmeng TYPE wmeng,        "Erfmg im Beleg ALRK014884 SW
      END OF t_odtab .
    TYPES:
      tt_odtab TYPE STANDARD TABLE OF t_odtab WITH DEFAULT KEY .

    TYPES tt_pstyv_hash TYPE HASHED TABLE OF pstyv WITH UNIQUE KEY primary_key COMPONENTS table_line.

    CONSTANTS:
      c_pstyv_dummy      TYPE tvpt-pstyv VALUE '_DMY'.

    CLASS-DATA gt_company_clients TYPE tt_company_clients .
    CLASS-DATA gt_kna1_mt TYPE tt_kna1_mt .
    CLASS-DATA gt_set_kalem_pstyv_rng TYPE tt_pstyv_rng.
    CLASS-DATA gt_bedelsiz_pstyv TYPE tt_pstyv_hash.
ENDCLASS.



CLASS ZCL_SD_TOOLKIT IMPLEMENTATION.


  METHOD alpha_output.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = rv_output.

  ENDMETHOD.

  METHOD check_date_overlap.
    CLEAR : rv_overlap.

    CALL FUNCTION 'TTE_CHK_DTRNG_DATERANGE'
      EXPORTING
        datefrom           = iv_begda1
        dateto             = iv_endda1
        checkdatefrom      = iv_begda2
        checkdateto        = iv_endda2
      EXCEPTIONS
        date_range_overlap = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      rv_overlap = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_delivery.
    SELECT SINGLE mandt INTO sy-mandt
  FROM vbfa WHERE vbelv = iv_vbeln
              AND posnv = iv_posnr
              AND vbtyp_n = 'J'
              AND ( vbtyp_v = 'C' OR vbtyp_v = 'I' ) ##WRITE_OK ##WARN_OK.
    rv_subrc = sy-subrc .

  ENDMETHOD.


  METHOD check_kna1_existence.

    ASSIGN gt_kna1_mt[ KEY primary_key COMPONENTS kunnr = iv_kunnr ] TO FIELD-SYMBOL(<ls_mt>).
    IF sy-subrc NE 0.
      INSERT VALUE #( kunnr = iv_kunnr ) INTO TABLE gt_kna1_mt ASSIGNING <ls_mt>.
      SELECT SINGLE mandt INTO @DATA(lv_mandt) FROM kna1 WHERE kunnr EQ @<ls_mt>-kunnr.
      <ls_mt>-subrc = sy-subrc.
    ENDIF.

    CHECK <ls_mt>-subrc NE 0.

    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        textid   = zcx_bc_table_content=>entry_missing
        objectid = CONV #( iv_kunnr )
        tabname  = 'KNA1'.

  ENDMETHOD.


  METHOD check_knb1_existence.

    ASSIGN gt_company_clients[
      KEY primary_key
      COMPONENTS bukrs = iv_bukrs
    ] TO FIELD-SYMBOL(<ls_cc>).

    IF sy-subrc NE 0.

      INSERT VALUE #( bukrs = iv_bukrs )
        INTO TABLE gt_company_clients
        ASSIGNING <ls_cc>.

      SELECT kunnr
        INTO CORRESPONDING FIELDS OF TABLE @<ls_cc>-kunnr
        FROM knb1
        WHERE bukrs EQ @<ls_cc>-bukrs.

    ENDIF.

    CHECK NOT line_exists( <ls_cc>-kunnr[
      KEY primary_key
      COMPONENTS kunnr = iv_kunnr
    ] ).

    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        textid   = zcx_bc_table_content=>entry_missing
        objectid = CONV #( |{ iv_bukrs } { iv_kunnr } | )
        tabname  = 'KNB1'.

  ENDMETHOD.


  METHOD check_knmt_existence.

    SELECT SINGLE mandt INTO sy-mandt ##write_ok
           FROM knmt
           WHERE vkorg EQ iv_vkorg
             AND vtweg EQ iv_vtweg
             AND kunnr EQ iv_kunnr
             AND matnr EQ iv_matnr.

    CHECK sy-subrc NE 0.

    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        objectid = |{ iv_vkorg } - { iv_vtweg } - { iv_kunnr } - { iv_matnr } |
        tabname  = 'KNMT'
        textid   = zcx_bc_table_content=>entry_missing.

  ENDMETHOD.


  METHOD cift_kalem_siparis_uy.

    IF gt_ex_werks[] IS INITIAL.
      SELECT * FROM zsdt_ex_werks INTO TABLE gt_ex_werks.
      IF sy-subrc <> 0.
        INSERT INITIAL LINE INTO TABLE gt_ex_werks.
      ENDIF.
    ENDIF.
    ASSIGN gt_ex_werks[ KEY primary_key COMPONENTS
                        werks = iv_werks
                        spart = iv_spart ] TO FIELD-SYMBOL(<ls_ex_werks>).
    IF sy-subrc <> 0.
      rv_rc = 4.
      RETURN.
    ENDIF.
    rv_rc = 0.
  ENDMETHOD.


  METHOD close_sales_order.

    DATA : BEGIN OF bapi,
             doc      TYPE bapivbeln-vbeln,
             s_oh_inx TYPE bapisdh1x,
             t_oi_in  TYPE TABLE OF bapisditm,
             s_oi_in  TYPE bapisditm,
             t_oi_inx TYPE TABLE OF bapisditmx,
             s_oi_inx TYPE bapisditmx,
             t_return TYPE bapiret2_tab,
           END OF bapi.

    TRY.

        bapi-doc                  = iv_vbeln.
        bapi-s_oh_inx-updateflag  = 'U'.

        bapi-s_oi_in-itm_number   = iv_posnr.
        bapi-s_oi_in-reason_rej   = iv_reason_reject.
        APPEND bapi-s_oi_in TO bapi-t_oi_in.

        bapi-s_oi_inx-itm_number  = iv_posnr.
        bapi-s_oi_inx-updateflag  = 'U'.
        bapi-s_oi_inx-reason_rej  = 'X'.
        APPEND bapi-s_oi_inx TO bapi-t_oi_inx.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = bapi-doc
            order_header_inx = bapi-s_oh_inx
          TABLES
            order_item_in    = bapi-t_oi_in
            order_item_inx   = bapi-t_oi_inx
            return           = bapi-t_return.

        et_return = bapi-t_return.

        LOOP AT bapi-t_return ASSIGNING FIELD-SYMBOL(<fs_return>)
          WHERE type CA 'AEX'.EXIT.
        ENDLOOP.

        IF sy-subrc = 0.
          rollback( ).
          RAISE EXCEPTION TYPE zcx_sd_genel
            EXPORTING
              textid = zcx_sd_genel=>bapi_error.
        ELSE.
          commit( ).
        ENDIF.

      CATCH zcx_sd_genel INTO DATA(lo_cx_sd).
        RAISE EXCEPTION lo_cx_sd.

      CATCH cx_root INTO DATA(lo_cx_root)  ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_sd_genel
          EXPORTING
            previous = lo_cx_root
            textid   = zcx_sd_genel=>sip_close_error.

    ENDTRY.

  ENDMETHOD.


  METHOD close_sales_order_all.

    DATA : BEGIN OF bapi,
             doc      TYPE bapivbeln-vbeln,
             s_oh_inx TYPE bapisdh1x,
             t_oi_in  TYPE TABLE OF bapisditm,
             s_oi_in  TYPE bapisditm,
             t_oi_inx TYPE TABLE OF bapisditmx,
             s_oi_inx TYPE bapisditmx,
             t_return TYPE bapiret2_tab,
           END OF bapi.

    SELECT vbeln,posnr INTO TABLE @DATA(lt_vbap)
      FROM vbap
      WHERE vbeln EQ @iv_vbeln.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    TRY.

        bapi-doc                  = iv_vbeln.
        bapi-s_oh_inx-updateflag  = 'U'.

        LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<ls_vbap>).
          bapi-s_oi_in-itm_number   = <ls_vbap>-posnr.
          bapi-s_oi_in-reason_rej   = iv_reason_reject.
          APPEND bapi-s_oi_in TO bapi-t_oi_in.

          bapi-s_oi_inx-itm_number  = <ls_vbap>-posnr.
          bapi-s_oi_inx-updateflag  = 'U'.
          bapi-s_oi_inx-reason_rej  = 'X'.
          APPEND bapi-s_oi_inx TO bapi-t_oi_inx.
        ENDLOOP.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = bapi-doc
            order_header_inx = bapi-s_oh_inx
          TABLES
            order_item_in    = bapi-t_oi_in
            order_item_inx   = bapi-t_oi_inx
            return           = bapi-t_return.

        et_return = bapi-t_return.

        LOOP AT bapi-t_return ASSIGNING FIELD-SYMBOL(<fs_return>)
          WHERE type CA 'AEX'.EXIT.
        ENDLOOP.

        IF sy-subrc = 0.
          rollback( ).
          RAISE EXCEPTION TYPE zcx_sd_genel
            EXPORTING
              textid = zcx_sd_genel=>bapi_error.
        ELSE.
          commit( ).
        ENDIF.

      CATCH zcx_sd_genel INTO DATA(lo_cx_sd).
        RAISE EXCEPTION lo_cx_sd.

      CATCH cx_root INTO DATA(lo_cx_root)  ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_sd_genel
          EXPORTING
            previous = lo_cx_root
            textid   = zcx_sd_genel=>sip_close_error.

    ENDTRY.

  ENDMETHOD.


  METHOD commit.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.


  METHOD create_sd_log_object.

    gv_extnumber = iv_extnumber.

    TRY.
        co_log = NEW zcl_bc_applog_facade( iv_object    = 'ZSD'
                                           iv_subobject = 'CHANGEOR'
                                           iv_extnumber = gv_extnumber ).
      CATCH cx_root  ##CATCH_ALL ##no_Handler.
    ENDTRY.

  ENDMETHOD.


  METHOD display_log.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY' ##FM_SUBRC_OK
      EXPORTING
        i_t_log_handle       = t_log_handle
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

  ENDMETHOD.

  method get_First_order_of_bstkd.

    SELECT SINGLE vbeln ##WARN_OK
           INTO rv_vbeln
           FROM vbkd
           WHERE bstkd_m EQ iv_bstkd. " BSTKD_M için index var

    CHECK sy-subrc NE 0.

    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        objectid = CONV #( iv_bstKD )
        tabname  = 'VBKD'
        textid   = zcx_bc_table_content=>entry_missing.

  endmethod.

  METHOD get_open_billing_quan.
    DATA : lt_vbeln  TYPE TABLE OF vbeln,
           lt_order  TYPE tt_order,
           lv_kwmeng TYPE vbap-kwmeng.

    LOOP AT it_vbeln ASSIGNING FIELD-SYMBOL(<ls_vbeln>).
      COLLECT <ls_vbeln>-vbeln INTO lt_vbeln.
    ENDLOOP.

    CHECK lt_vbeln IS NOT INITIAL.

    SELECT vbeln vbtyp INTO TABLE lt_order FROM vbak
           FOR ALL ENTRIES IN lt_vbeln
           WHERE vbeln EQ lt_vbeln-table_line.

    SELECT vbeln,posnr,kwmeng,umvkn,umvkz INTO TABLE @DATA(lt_vbap) FROM vbap
           FOR ALL ENTRIES IN @lt_vbeln
           WHERE vbeln EQ @lt_vbeln-table_line AND abgru EQ @space .
    SORT lt_vbap BY vbeln posnr.
* hatalı belgeleri tespit et
    LOOP AT it_vbeln ASSIGNING <ls_vbeln>.
      DATA(lv_tabix) = sy-tabix.
      READ TABLE lt_order ASSIGNING FIELD-SYMBOL(<ls_order>)
                 WITH TABLE KEY vbeln = <ls_vbeln>-vbeln.
      IF sy-subrc NE 0.
        APPEND VALUE #( id = 'LS'
                        number = '001'
                        type = 'E'
                        message_v1 = <ls_vbeln>-vbeln
                        message_v2 = TEXT-001 ) TO et_return.
        DELETE it_vbeln INDEX lv_tabix.
      ELSE.
        CASE <ls_order>-vbtyp.
          WHEN 'C'.
          WHEN OTHERS.
            APPEND VALUE #( id = 'LS'
                            number = '001'
                            type = 'E'
                            message_v1 = <ls_vbeln>-vbeln
                            message_v2 = TEXT-002
                            message_v3 = TEXT-003
                             ) TO et_return.
            DELETE it_vbeln INDEX lv_tabix.
        ENDCASE.
      ENDIF.
    ENDLOOP.

* kalanlar için açık fat miktarını bul
    CHECK it_vbeln IS NOT INITIAL.
    SELECT vbfa~vbelv,
           vbfa~posnv,
           vbfa~vbeln,
           vbfa~posnn,
           vbfa~vbtyp_n,
           vbap~kwmeng,
           vbfa~rfmng,
           vbap~umvkz,
           vbap~umvkn
           INTO TABLE @DATA(lt_vbfa)
           FROM vbfa
           INNER JOIN vbap
                   ON vbap~vbeln EQ vbfa~vbelv AND
                      vbap~posnr EQ vbfa~posnv AND
                      vbap~abgru EQ @space
           INNER JOIN vbrk
                   ON vbrk~vbeln EQ vbfa~vbeln AND
                      vbrk~fksto EQ @space AND
                      vbrk~sfakn EQ @space
           FOR ALL ENTRIES IN  @it_vbeln
           WHERE vbfa~vbelv EQ  @it_vbeln-vbeln AND
                 vbfa~posnv EQ  @it_vbeln-posnr AND
                 vbfa~vbtyp_n EQ 'M'.

    LOOP AT it_vbeln ASSIGNING <ls_vbeln>.
      APPEND INITIAL LINE TO et_open_item ASSIGNING FIELD-SYMBOL(<ls_open>).
      MOVE-CORRESPONDING <ls_vbeln> TO <ls_open>.
      CLEAR : lv_kwmeng,lv_tabix.
      LOOP AT lt_vbfa ASSIGNING FIELD-SYMBOL(<ls_vbfa>)
                      WHERE vbelv EQ <ls_vbeln>-vbeln AND
                            posnv EQ <ls_vbeln>-posnr.
        IF lv_tabix IS INITIAL.
          lv_kwmeng = <ls_vbfa>-kwmeng * <ls_vbfa>-umvkz / <ls_vbfa>-umvkn.
          lv_tabix = sy-tabix.
        ENDIF.
        SUBTRACT <ls_vbfa>-rfmng FROM lv_kwmeng.
      ENDLOOP.
      IF sy-subrc EQ 0.
        <ls_open>-open_tob = lv_kwmeng * <ls_vbfa>-umvkz / <ls_vbfa>-umvkn.
        <ls_open>-open_sob = lv_kwmeng.
      ELSE.
        READ TABLE lt_vbap ASSIGNING FIELD-SYMBOL(<ls_vbap>)
                   WITH KEY vbeln = <ls_vbeln>-vbeln
                            posnr = <ls_vbeln>-posnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_open>-open_tob = <ls_vbap>-kwmeng * <ls_vbap>-umvkz / <ls_vbap>-umvkn.
          <ls_open>-open_sob = <ls_vbap>-kwmeng .
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE et_open_item WHERE open_sob EQ 0.

  ENDMETHOD.


  METHOD get_open_delivery_quan.

    DATA lt_tab TYPE tt_odtab.

    CLEAR:
      ev_quan,
      et_wlm.

    CALL FUNCTION 'MB_SELECT_SD_SCHEDULED_STOCK'
      EXPORTING
        x_matnr = iv_matnr
      TABLES
        xtab4   = lt_tab
        xwerks  = it_werks.

    IF ev_quan IS REQUESTED.
      ev_quan = REDUCE #(
        INIT ret TYPE omeng
        FOR ls_tab IN lt_tab
        NEXT ret = ret + ls_tab-vbmnj
      ).
    ENDIF.

    IF et_wlm IS REQUESTED.

      LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
        COLLECT VALUE t_quan_wlm(
          werks = <ls_tab>-werks
          lgort = <ls_tab>-lgort
          matnr = <ls_tab>-matnr
          omeng = <ls_tab>-vbmnj
        ) INTO et_wlm.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_orders_by_bstnk.

    SELECT vbeln INTO TABLE rt_vbeln
           FROM vbak
           WHERE bstnk EQ iv_bstnk.

  ENDMETHOD.


  METHOD get_set_kalem_pstyv_rng.

    IF gt_set_kalem_pstyv_rng IS INITIAL.

      SELECT
          @zcl_bc_ddic_toolkit=>c_option_eq AS option,
          @zcl_bc_ddic_toolkit=>c_sign_i AS sign,
          pstyv AS low
        INTO CORRESPONDING FIELDS OF TABLE @gt_set_kalem_pstyv_rng
        FROM zsdt_ic_pstyv
        ##too_many_itab_fields.

      IF gt_set_kalem_pstyv_rng IS INITIAL.

        gt_set_kalem_pstyv_rng = VALUE #( (
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = c_pstyv_dummy
        ) ).

        SELECT SINGLE mandt INTO sy-mandt
            FROM tvpt
            WHERE pstyv IN gt_set_kalem_pstyv_rng
            ##WARN_OK ##WRITE_OK.

        ASSERT sy-subrc NE 0.

      ENDIF.

    ENDIF.

    rt_pstyv_rng = gt_set_kalem_pstyv_rng.

  ENDMETHOD.


  METHOD get_stloc_stock.

    CHECK it_matnr IS NOT INITIAL.

    SELECT mard~matnr werks lgort labst speme meins
      INTO CORRESPONDING FIELDS OF TABLE rt_stock
      FROM
        mard
        INNER JOIN mara ON mard~matnr = mara~matnr
      FOR ALL ENTRIES IN it_matnr
      WHERE
        mard~matnr EQ it_matnr-matnr AND
        mard~werks IN it_werks.

  ENDMETHOD.


  METHOD is_pstyv_bedelsiz.

    IF gt_bedelsiz_pstyv IS INITIAL.
      SELECT pstyv FROM zsdt_bedelsiz_kl INTO TABLE @gt_bedelsiz_pstyv.
    ENDIF.

    rv_bedelsiz = xsdbool(
      line_exists(
        gt_bedelsiz_pstyv[ KEY primary_key COMPONENTS table_line = iv_pstyv ]
      )
    ).

  ENDMETHOD.


  METHOD musteri_hf_iptal.

    rv_subrc = 4.

    IF iv_mukerrerlik_kontrol EQ abap_true.
      SELECT SINGLE vbeln INTO ev_vbeln_va FROM vbfa
         WHERE vbelv EQ is_vbrk-vbeln AND
               vbtyp_n EQ is_vbak-vbtyp ##WARN_OK.
      IF sy-subrc EQ 0.
        rv_subrc = 8.
        RETURN.
      ENDIF.
    ENDIF.

    IF it_vbrp IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbrp FROM vbrp
        WHERE vbeln EQ is_vbrk-vbeln ##TOO_MANY_ITAB_FIELDS.
    ENDIF.

    LOOP AT it_vbrp ASSIGNING FIELD-SYMBOL(<ls_vbrp>).

      IF is_vbak-auart IS INITIAL.
        SELECT SINGLE * INTO is_vbak FROM vbak
           WHERE vbeln EQ <ls_vbrp>-aubel.
      ENDIF.

      SELECT SINGLE mandt INTO sy-mandt FROM zsdt_3m_hf_03
                      WHERE spart = <ls_vbrp>-spart ##WRITE_OK.
      IF sy-subrc NE 0.
        SELECT SINGLE bwkey INTO @DATA(lv_bwkey) FROM t001w
                 WHERE  werks  EQ  @<ls_vbrp>-werks.
        SELECT SINGLE bukrs INTO @DATA(lv_bukrs) FROM t001k
               WHERE  bwkey  EQ @lv_bwkey.
        IF  lv_bukrs NE is_vbrk-bukrs AND
            is_vbrk-fkart IN it_fkart AND
            is_vbak-auart EQ iv_auart .
          rv_subrc = 0.
        ELSE.
          rv_subrc = 4.
          EXIT.
        ENDIF.
      ELSE.
        rv_subrc = 4.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD rollback.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ENDMETHOD.


  METHOD update_credit_control.
    DATA: lv_tcode  TYPE sytcode VALUE 'FD32',
          ls_option TYPE ctu_params.

    DATA: lv_klimk TYPE bdc_fval.
    DATA: lv_dbekr TYPE bdc_fval.
    DATA: lv_nxtrv TYPE bdc_fval.
    WRITE  is_knkk-klimk TO lv_klimk ##UOM_IN_MES.
    WRITE  is_knkk-dbekr TO lv_dbekr ##UOM_IN_MES.
    IF is_knkk-nxtrv IS NOT INITIAL.
      WRITE  is_knkk-nxtrv TO lv_nxtrv.
    ENDIF.
    CONDENSE: lv_klimk,
              lv_dbekr,
              lv_nxtrv.

    DATA(lo_bdc) = NEW zcl_bc_bdc( ).
    lo_bdc->add_scr( iv_prg = 'SAPMF02C' iv_dyn = '0100' ).
    lo_bdc->add_fld(: iv_nam = 'BDC_OKCODE' iv_val = '/00' ),
                      iv_nam = 'RF02L-KUNNR' iv_val = CONV #( is_knkk-kunnr ) ),
                      iv_nam = 'RF02L-KKBER' iv_val = CONV #( is_knkk-kkber ) ),
                      iv_nam = 'RF02L-D0210' iv_val = CONV #( abap_true ) ).

    lo_bdc->add_scr( iv_prg = 'SAPMF02C' iv_dyn = '0210' ).
    lo_bdc->add_fld(: iv_nam = 'BDC_OKCODE' iv_val = '=UPDA' ) ,
                      iv_nam = 'KNKK-KLIMK' iv_val = lv_klimk ),
                      iv_nam = 'KNKK-DBEKR' iv_val = lv_dbekr ) ,
                      iv_nam = 'KNKK-KNKLI' iv_val = CONV #( is_knkk-knkli ) ),
                      iv_nam = 'KNKK-NXTRV' iv_val = lv_nxtrv ) .

    CLEAR ls_option.
    ls_option-dismode   = 'N'.
    ls_option-updmode   = 'S'.
*
    lo_bdc->submit( EXPORTING
                      iv_tcode  = lv_tcode
                      is_option = ls_option
                    IMPORTING
                      et_msg    = rt_messages ).



  ENDMETHOD.


  METHOD va02_add_field_is_required.
    CLEAR : rv_return,ev_not_found.
    DATA ls_uys_alan TYPE zsdt_uys_alan.
    IF gt_uys_alan[] IS INITIAL.
      SELECT * FROM zsdt_uys_alan INTO TABLE gt_uys_alan.
      IF sy-subrc <> 0.
        CLEAR ls_uys_alan.
        ls_uys_alan-auart = iv_auart.
        ls_uys_alan-vkorg = iv_vkorg.
        ls_uys_alan-vtweg = iv_vtweg.
        INSERT ls_uys_alan INTO TABLE gt_uys_alan.
      ENDIF.
    ENDIF.
    ASSIGN gt_uys_alan[ KEY primary_key COMPONENTS
                        auart = iv_auart
                        vkorg = iv_vkorg
                        vtweg = iv_vtweg
                        field = iv_field_name ] TO FIELD-SYMBOL(<ls_uys_alan>).
    IF sy-subrc = 0.
      rv_return = <ls_uys_alan>-required.
*      ASSIGN COMPONENT IV_FIELD_NAME OF STRUCTURE <ls_uys_alan> TO FIELD-SYMBOL(<lv_field>).
*      IF sy-subrc = 0.
*        rv_return = <lv_field>.
*      ENDIF.
      "--------->> add by mehmet sertkaya 09.11.2016 09:24:35
    ELSE.
      ev_not_found = abap_true.
      "-----------------------------<<
    ENDIF.
  ENDMETHOD.
ENDCLASS.