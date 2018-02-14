class ZCL_FI_TOOLKIT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_hesap,
        sube   TYPE rfposxext-konto,
        merkez TYPE rfposxext-konto,
      END OF ty_hesap .
  types:
    tt_hesap TYPE STANDARD TABLE OF ty_hesap .
  types:
    BEGIN OF ty_bseg,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        buzei TYPE bseg-buzei,
        kunnr TYPE bseg-kunnr,
        lifnr TYPE bseg-lifnr,
        hkont TYPE bseg-hkont,
        koart TYPE bseg-koart,
        vbeln TYPE bseg-vbeln,
        vbel2 TYPE bseg-vbel2,
        posn2 TYPE bseg-posn2,
        gkont TYPE bseg-hkont,
        konto TYPE kunnr,
        anln1 TYPE bseg-anln1,
        anln2 TYPE bseg-anln2,
      END OF ty_bseg .
  types:
    BEGIN OF ty_konto,
      bukrs TYPE bukrs,
      konto TYPE konto,
      END OF ty_konto .
  types:
    BEGIN OF ty_devir_items,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        buzei TYPE buzei,
        bukrs TYPE bukrs,
        konto TYPE hkont,
        shkzg TYPE shkzg,
        dmshb TYPE dmbtr,
        dmbe2 TYPE dmbe2,
        dmbe3 TYPE dmbe3,
        umskz TYPE umskz,
        filkd TYPE filkd,
        wrbtr TYPE wrbtr,
        waers TYPE waers,
        gsber TYPE gsber,
      END OF ty_devir_items .
  types:
    BEGIN OF ty_devir,
        bukrs TYPE bukrs,
        konto TYPE hkont,
        shkzg TYPE shkzg,
        dmshb TYPE dmbtr,
        dmbe2 TYPE dmbe2,
        dmbe3 TYPE dmbe3,
        umskz TYPE umskz,
        filkd TYPE filkd,
        wrbtr TYPE wrbtr,
        waers TYPE waers,
        gsber TYPE gsber,
      END OF ty_devir .
  types:
    tt_devir TYPE STANDARD TABLE OF ty_devir .
  types:
    BEGIN OF t_doc_xblnr,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        xblnr TYPE bkpf-xblnr,
      END OF t_doc_xblnr .
  types:
    tt_doc_xblnr TYPE STANDARD TABLE OF t_doc_xblnr WITH DEFAULT KEY .

  constants C_KOART_KUNNR type KOART value 'D' ##NO_TEXT.
  constants C_KOART_LIFNR type KOART value 'K' ##NO_TEXT.
  constants C_BORC type SHKZG value 'S' ##NO_TEXT.
  constants C_ALACAK type SHKZG value 'H' ##NO_TEXT.
  constants C_MAL_HAREKETI type AWTYP value 'MKPF' ##NO_TEXT.
  constants C_SATINALMA_FATURASI type AWTYP value 'RMRP' ##NO_TEXT.
  constants C_SATIS_FATURASI type AWTYP value 'VBRK' ##NO_TEXT.

  class-methods CHECK_IBAN_DUPLICATE
    importing
      !IT_IBAN type ZFITT_IBAN_RNG
      !IV_GET_VENDOR type ABAP_BOOL default ABAP_TRUE
      !IV_GET_CLIENT type ABAP_BOOL default ABAP_TRUE
      !IT_LIFNR type ZQMTT_LIFNR optional
      !IT_KUNNR type RANGE_KUNNR_TAB optional
    raising
      ZCX_FI_IBAN .
  class-methods CONVERT_DATUM_TO_GDATU
    importing
      !IV_DATUM type DATUM
    returning
      value(RV_GDATU) type TCURR-GDATU .
  class-methods GET_IBAN_CODES
    importing
      !IT_IBAN type ZFITT_IBAN_RNG optional
      !IV_GET_VENDOR type ABAP_BOOL default ABAP_TRUE
      !IV_GET_CLIENT type ABAP_BOOL default ABAP_TRUE
      !IT_LIFNR type ZQMTT_LIFNR optional
      !IT_KUNNR type RANGE_KUNNR_TAB optional
    returning
      value(RT_TIBAN) type ZFITT_TIBAN .
  class-methods MODIFY_REPORT_FALGLL03_FLBXN
    importing
      !IR_DATA type ref to DATA .
  class-methods DEVIR_FBLXN
    importing
      !IT_HESAP type TT_HESAP
    exporting
      !ET_DEVIR type TT_DEVIR .
  class-methods DISPLAY_FI_DOC_IN_GUI
    importing
      !IV_BELNR type BKPF-BELNR
      !IV_BUKRS type BKPF-BUKRS
      !IV_GJAHR type BKPF-GJAHR .
  class-methods EKSTRE_FBLXN
    changing
      !CT_ITEMS type IT_RFPOSXEXT
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods GET_BKPF_XBLNR
    changing
      !CT_DOC type TT_DOC_XBLNR .
  class-methods GET_COMPANY_LONG_TEXT
    importing
      !IV_BUKRS type BUKRS
    returning
      value(RV_TEXT) type STRING
    raising
      ZCX_BC_TABLE_CONTENT .
  class-methods UPDATE_XBLNR
    importing
      !IT_XBLNR type TT_DOC_XBLNR
      !IV_COMMIT_EACH_DOC type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_BC_CLASS_METHOD .
  class-methods CLEAR_CUSTOMER_OPEN_ITEMS
    importing
      !IM_KUNNR type KUNNR
      !IM_BUKRS type BUKRS
      !IT_BELNR type RE_T_XCFR_BELNR .
  class-methods CLEAR_VENDOR_OPEN_ITEMS
    importing
      !IM_LIFNR type LIFNR
      !IM_BUKRS type BUKRS
      !IT_BELNR type RE_T_XCFR_BELNR .
  class-methods DETERMINE_DUE_DATE
    importing
      !IM_DOCUMENT type ZFIS_ACCDOCUMENT_KEY
    returning
      value(RE_NETDT) type NETDT .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF t_company_long_text,
        bukrs TYPE bukrs,
        text  TYPE string,
      END OF t_company_long_text .
  types:
    BEGIN OF t_dg_cache,
        datum TYPE datum,
        gdatu TYPE tcurr-gdatu,
      END OF t_dg_cache .
  types:
    tt_dg_cache TYPE HASHED TABLE OF t_dg_cache WITH UNIQUE KEY primary_key COMPONENTS datum .
  types:
    BEGIN OF t_kna1,
        kunnr TYPE kunnr,
        name1 TYPE name1_gp,
      END OF t_kna1 .
  types:
    tt_company_long_text TYPE HASHED TABLE OF t_company_long_text WITH UNIQUE KEY primary_key COMPONENTS bukrs .

  constants C_TABNAME_T001 type TABNAME value 'T001' ##NO_TEXT.
  class-data GT_COMPANY_LONG_TEXT type TT_COMPANY_LONG_TEXT .
  class-data GT_DG_CACHE type TT_DG_CACHE .
ENDCLASS.



CLASS ZCL_FI_TOOLKIT IMPLEMENTATION.


  METHOD check_iban_duplicate.

    DATA(lt_tiban) = get_iban_codes(
      it_iban       = it_iban
      iv_get_vendor = iv_get_vendor
      iv_get_client = iv_get_client
      it_lifnr      = it_lifnr
      it_kunnr      = it_kunnr
    ).

    CHECK lt_tiban IS NOT INITIAL.

    ASSIGN lt_tiban[ 1 ] TO FIELD-SYMBOL(<ls_tiban>).

    RAISE EXCEPTION TYPE zcx_fi_iban
      EXPORTING
        textid     = zcx_fi_iban=>already_used
        iban       = <ls_tiban>-iban
        party      = COND #( WHEN <ls_tiban>-kunnr IS NOT INITIAL THEN <ls_tiban>-kunnr
                             WHEN <ls_tiban>-lifnr IS NOT INITIAL THEN <ls_tiban>-lifnr
                           )
        party_type = COND #( WHEN <ls_tiban>-kunnr IS NOT INITIAL THEN TEXT-110
                             WHEN <ls_tiban>-lifnr IS NOT INITIAL THEN TEXT-111
                           ).

  ENDMETHOD.


  METHOD clear_customer_open_items.

    TRY.
        DATA(lo_bdc) = NEW zcl_bc_bdc( ).

        lo_bdc->add_scr(
          iv_prg = 'SAPMF05A'
          iv_dyn = '131'
        ).

        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '/00' ),
          iv_nam = 'RF05A-XNOPS'   iv_val = 'X' ),
          iv_nam = 'RF05A-XPOS1(03)'   iv_val = 'X' ),
          iv_nam = 'RF05A-AGKON' iv_val = CONV #( im_kunnr ) ),
          iv_nam = 'BKPF-BUKRS' iv_val = CONV #( im_bukrs ) ).

        LOOP AT it_belnr INTO DATA(ls_belnr).
          lo_bdc->add_scr(
            iv_prg = 'SAPMF05A'
            iv_dyn = '731'
          ).
          lo_bdc->add_fld(:
            iv_nam = 'BDC_OKCODE' iv_val = '/00' ),
            iv_nam = 'BDC_CURSOR'   iv_val = 'RF05A-SEL01(01)' ),
            iv_nam = 'RF05A-SEL01(01)'   iv_val = CONV #( ls_belnr ) ).
        ENDLOOP.
        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '=PA' ) .

        lo_bdc->add_scr(
          iv_prg = 'SAPDF05X'
          iv_dyn = '3100'
        ).

        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '=WAIT_USER' ) .

        lo_bdc->submit(
            iv_tcode  = 'F-32'
            is_option = VALUE #( dismode = zcl_bc_bdc=>c_dismode_error )
        ).


    ENDTRY.











  ENDMETHOD.


  method clear_vendor_open_items.

    try.
        data(lo_bdc) = new zcl_bc_bdc( ).

        lo_bdc->add_scr(
          iv_prg = 'SAPMF05A'
          iv_dyn = '131'
        ).

        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '/00' ),
          iv_nam = 'RF05A-XNOPS'   iv_val = 'X' ),
          iv_nam = 'RF05A-XPOS1(03)'   iv_val = 'X' ),
          iv_nam = 'RF05A-AGKON' iv_val = conv #( im_lifnr ) ),
          iv_nam = 'BKPF-BUKRS' iv_val = conv #( im_bukrs ) ).

        loop at it_belnr into data(ls_belnr).
        lo_bdc->add_scr(
          iv_prg = 'SAPMF05A'
          iv_dyn = '731'
        ).
          lo_bdc->add_fld(:
            iv_nam = 'BDC_OKCODE' iv_val = '/00' ),
            iv_nam = 'BDC_CURSOR'   iv_val = 'RF05A-SEL01(01)' ),
            iv_nam = 'RF05A-SEL01(01)'   iv_val = conv #( ls_belnr ) ).
        endloop.
        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '=PA' ) .

        lo_bdc->add_scr(
          iv_prg = 'SAPDF05X'
          iv_dyn = '3100'
        ).

        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '=WAIT_USER' ) .

        lo_bdc->submit(
            iv_tcode  = 'F-44'
            is_option = value #( dismode = zcl_bc_bdc=>c_dismode_error )
        ).


    endtry.











  endmethod.


  METHOD convert_datum_to_gdatu.

    DATA lv_datxt TYPE char10.

    ASSIGN gt_dg_cache[
        KEY primary_key
        COMPONENTS datum = iv_datum
    ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_dg_cache( datum = iv_datum ).

      WRITE iv_datum TO lv_datxt.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = lv_datxt
        IMPORTING
          output = ls_cache-gdatu.

      INSERT ls_cache INTO TABLE gt_dg_cache ASSIGNING <ls_cache>.

    ENDIF.

    rv_gdatu = <ls_cache>-gdatu.

  ENDMETHOD.


  method determine_due_date.
    data: i_faede type faede,
          e_faede type faede.
    select single * from bseg into @data(ls_bseg)
      where bukrs eq @im_document-bukrs
        and gjahr eq @im_document-gjahr
        and belnr eq @im_document-belnr
        and buzei eq @im_document-buzei.

    move-corresponding ls_bseg to i_faede ##ENH_OK.

    call function 'DETERMINE_DUE_DATE'
      exporting
        i_faede                    = i_faede
*       I_GL_FAEDE                 =
      importing
        e_faede                    = e_faede
      exceptions
        account_type_not_supported = 1
        others                     = 2.

    if sy-subrc <> 0  ##NEEDED.
* Implement suitable error handling here
    endif .

    re_netdt = e_faede-netdt.




  endmethod.


  method devir_fblxn.

    data : lv_keydt type sy-datum,
           lt_devir type table of ty_devir_items,
           ls_devir type ty_devir.

    field-symbols  : <lt_budat> type range_date_t,
                     <lt_saknr> type fagl_mm_t_range_saknr,
                     <lt_bukrs> type tpmy_range_bukrs,
                     <lv_odk>   type any,
                     <lv_apar>  type any.

    case sy-tcode.
      when 'FBL1N'.
        assign ('(RFITEMAP)SO_BUDAT[]') to <lt_budat>.
        check sy-subrc eq 0.
        assign ('(RFITEMAP)KD_BUKRS[]') to <lt_bukrs>.
        check sy-subrc eq 0.
        assign ('(RFITEMAP)X_SHBV') to <lv_odk>.
        check sy-subrc eq 0.
        assign ('(RFITEMAP)X_APAR') to <lv_apar>.
        check sy-subrc eq 0.
      when 'FBL3N'.
        assign ('(RFITEMGL)SO_BUDAT[]') to <lt_budat>.
        check sy-subrc eq 0.
        assign ('(RFITEMGL)SD_BUKRS[]') to <lt_bukrs>.
        check sy-subrc eq 0.
        assign ('(RFITEMGL)X_SHBV') to <lv_odk>.
        check sy-subrc eq 0.
      when 'FBL5N'.
        assign ('(RFITEMAR)SO_BUDAT[]') to <lt_budat>.
        check sy-subrc eq 0.
        assign ('(RFITEMAR)DD_BUKRS[]') to <lt_bukrs>.
        check sy-subrc eq 0.
        assign ('(RFITEMAR)X_SHBV') to <lv_odk>.
        check sy-subrc eq 0.
        assign ('(RFITEMAR)X_APAR') to <lv_apar>.
        check sy-subrc eq 0.
      when others.
        return.
    endcase.

    read table <lt_budat> index 1 assigning field-symbol(<ls_budat>).
    if sy-subrc eq 0.
      lv_keydt = <ls_budat>-low - 1.
    else.
      return.
    endif.

    clear :lt_devir,et_devir.

    case sy-tcode.
      when 'FBL1N'.

        check it_hesap is not initial.
        select
               belnr
               gjahr
               buzei
               bukrs
               lifnr
               shkzg
               dmbtr
               dmbe2
               dmbe3
               umskz
               filkd
               wrbtr
               waers
               into table lt_devir
               from bsik
               for all entries in it_hesap
               where bukrs in <lt_bukrs> and
                     budat le lv_keydt and
                     ( lifnr eq it_hesap-sube or lifnr eq it_hesap-merkez ).
        select
               belnr
               gjahr
               buzei
               bukrs
               lifnr
               shkzg
               dmbtr
               dmbe2
               dmbe3
               umskz
               filkd
               wrbtr
               waers
               appending table lt_devir
               from bsak
               for all entries in it_hesap
               where bukrs in <lt_bukrs> and
                     budat le lv_keydt and
                     augdt gt lv_keydt and
                     ( lifnr eq it_hesap-sube or lifnr eq it_hesap-merkez ).
        if <lv_apar> eq abap_true.
          select
                 belnr
                 gjahr
                 buzei
                 bukrs
                 kunnr
                 shkzg
                 dmbtr
                 dmbe2
                 dmbe3
                 umskz
                 filkd
                 wrbtr
                 waers
                 appending table lt_devir
                 from bsid
                 for all entries in it_hesap
                 where bukrs in <lt_bukrs> and
                       budat le lv_keydt and
                       ( kunnr eq it_hesap-sube or kunnr eq it_hesap-merkez ).
          select
                 belnr
                 gjahr
                 buzei
                 bukrs
                 kunnr
                 shkzg
                 dmbtr
                 dmbe2
                 dmbe3
                 umskz
                 filkd
                 wrbtr
                 waers
                 appending table lt_devir
                 from bsad
                for all entries in it_hesap
                 where bukrs in <lt_bukrs> and
                       budat le lv_keydt and
                       augdt gt lv_keydt and
                       ( kunnr eq it_hesap-sube or kunnr eq it_hesap-merkez ).
        endif.
      when 'FBL3N'.
        assign ('(RFITEMGL)SD_SAKNR[]') to <lt_saknr>.
        if sy-subrc eq 0.

          select
                 belnr
                 gjahr
                 buzei
                 bukrs
                 hkont as konto
                 shkzg
                 dmbtr as dmshb
                 dmbe2
                 dmbe3
*                 umskz
*                 filkd
                 wrbtr
                 waers
                 into corresponding fields of table lt_devir ##TOO_MANY_ITAB_FIELDS
                 from bsis
                 where bukrs in <lt_bukrs> and
                       budat le lv_keydt and
                       hkont in <lt_saknr>.
          select
                 belnr
                 gjahr
                 buzei
                 bukrs
                 hkont as konto
                 shkzg
                 dmbtr as dmshb
                 dmbe2
                 dmbe3
*                 umskz
*                 filkd
                 wrbtr
                 waers
                 appending corresponding fields of table lt_devir ##TOO_MANY_ITAB_FIELDS
                 from bsas
                 where bukrs in <lt_bukrs> and
                       budat le lv_keydt and
                       augdt gt lv_keydt and
                       hkont in <lt_saknr>.
        endif.
      when 'FBL5N'.

        check it_hesap is not initial.
        select
               belnr
               gjahr
               buzei
               bukrs
               kunnr
               shkzg
               dmbtr
               dmbe2
               dmbe3
               umskz
               filkd
               wrbtr
               waers
               gsber
               into table lt_devir
               from bsid
               for all entries in it_hesap
               where bukrs in <lt_bukrs> and
                     budat le lv_keydt and
                     ( kunnr eq it_hesap-sube or kunnr eq it_hesap-merkez ).
        select
               belnr
               gjahr
               buzei
               bukrs
               kunnr
               shkzg
               dmbtr
               dmbe2
               dmbe3
               umskz
               filkd
               wrbtr
               waers
               gsber
               appending table lt_devir
               from bsad
              for all entries in it_hesap
               where bukrs in <lt_bukrs> and
                     budat le lv_keydt and
                     augdt gt lv_keydt and
                     ( kunnr eq it_hesap-sube or kunnr eq it_hesap-merkez ).
        if <lv_apar> eq abap_true.
          select
                 belnr
                 gjahr
                 buzei
                 bukrs
                 lifnr
                 shkzg
                 dmbtr
                 dmbe2
                 dmbe3
                 umskz
                 filkd
                 wrbtr
                 waers
                 gsber
                 appending table lt_devir
                 from bsik
                 for all entries in it_hesap
                 where bukrs in <lt_bukrs> and
                       budat le lv_keydt and
                       ( lifnr eq it_hesap-sube or lifnr eq it_hesap-merkez ).
          select
                 belnr
                 gjahr
                 buzei
                 bukrs
                 lifnr
                 shkzg
                 dmbtr
                 dmbe2
                 dmbe3
                 umskz
                 filkd
                 wrbtr
                 waers
                 gsber
                 appending table lt_devir
                 from bsak
                 for all entries in it_hesap
                 where bukrs in <lt_bukrs> and
                       budat le lv_keydt and
                       augdt gt lv_keydt and
                       ( lifnr eq it_hesap-sube or lifnr eq it_hesap-merkez ).



        endif.
    endcase.

    if <lv_odk> is initial.
      delete lt_devir where umskz is not initial.
    endif.

    loop at it_hesap assigning field-symbol(<ls_hesap>)
            where merkez is not initial.
      delete lt_devir where konto eq <ls_hesap>-merkez and filkd ne <ls_hesap>-sube.
    endloop.

    loop at lt_devir assigning field-symbol(<ls_devir>).
      if <ls_devir>-shkzg eq c_alacak.
        multiply <ls_devir>-dmshb by -1.
        multiply <ls_devir>-dmbe2 by -1.
        multiply <ls_devir>-dmbe3 by -1.
        multiply <ls_devir>-wrbtr by -1.
      endif.
      clear <ls_devir>-shkzg.
      clear <ls_devir>-umskz.
*{  EDIT  Berrin Ulus 25.04.2016 15:05:25
* HAR-9421
      clear : <ls_devir>-filkd.
*}  EDIT  Berrin Ulus 25.04.2016 15:05:25
      clear ls_devir.
      move-corresponding <ls_devir> to ls_devir.
      collect ls_devir into et_devir.
    endloop.

  endmethod.


  METHOD display_fi_doc_in_gui.

    SET PARAMETER ID:
      'BLN' FIELD iv_belnr,
      'BUK' FIELD iv_bukrs,
      'GJR' FIELD iv_gjahr.

    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDMETHOD.


  method ekstre_fblxn.
    "--------->> written by mehmet sertkaya 18.12.2015 11:29:39
*--------------------------------------------------------------------*
* E K S T R E
*--------------------------------------------------------------------*

    check ct_items is not initial.
    case sy-tcode.
      when 'FBL1N'.
        assign ('(RFITEMAP)X_AISEL') to field-symbol(<lv_x_aisel>).
        assign ('(RFITEMAP)PA_VARI') to field-symbol(<lv_vari>).

      when 'FBL3N'.
        assign ('(RFITEMGL)X_AISEL') to <lv_x_aisel>.
        assign ('(RFITEMGL)PA_VARI') to <lv_vari>.

      when 'FBL5N'.
        assign ('(RFITEMAR)X_AISEL') to <lv_x_aisel>.
        assign ('(RFITEMAR)PA_VARI') to <lv_vari>.

      when others.
        return.

    endcase.

    if sy-subrc eq 0 and sy-tcode(3) eq 'FBL'.
      if <lv_vari> cs 'EKSTRE'.

        if <lv_x_aisel> ne abap_true.
          message text-003 type 'I'.
          return.
        endif.

        call function 'SAPGUI_PROGRESS_INDICATOR' ##FM_SUBRC_OK
          exporting
*           percentage =
            text   = text-002
          exceptions
            others = 1.


        "-->> changed by mehmet sertkaya 21.07.2016 13:52:32
        " HAR-10448 - Müşteri Ekstresinde XX belge ters kaydı
*          delete ct_items where blart eq 'XX' and gjahr ge '2016'.
        loop at ct_items assigning field-symbol(<ls_items>) where blart eq zcl_fi_document_type=>get_customer_clearing_doc_type( ) and gjahr ge '2016'.
          delete ct_items where belnr eq <ls_items>-zzstblg and gjahr eq <ls_items>-zzstjah.
          delete ct_items.
        endloop.
        "-----------------------------<


        sort ct_items by konto budat ascending.

        check  ct_items is not initial.
*--------------------------------------------------------------------*
* Devirleri bul
*--------------------------------------------------------------------*
        data : lt_devir        type table of ty_devir,
               lt_devir_sorted type sorted table of ty_devir
                               with non-unique key bukrs konto,
               lv_konto_temp   type hkont,
               lt_item_devir   type it_rfposxext,
               lt_item_sum_top type it_rfposxext,
               lt_item_sum     type it_rfposxext,
               ls_item_devir   type line of it_rfposxext,
               ls_item_sum     type line of it_rfposxext,
               ls_item_sum_    type line of it_rfposxext,
               lv_tabix        type sy-tabix,
               ls_devir        type ty_devir,
               lt_devir_merkez type sorted table of ty_devir with unique key bukrs konto ##NEEDED,
               ls_devir_merkez type ty_devir,
               lt_mkpf_key     type table of mkpf,
               lt_rbkp_key     type table of rbkp,
               lt_vbrk_key     type table of vbrk,
               lv_awkey        type awkey,
               lt_mseg         type sorted table of mseg with non-unique key mblnr mjahr,
               lt_rbkp         type sorted table of rbkp with non-unique key belnr gjahr,
               lt_vbrp         type sorted table of vbrp with non-unique key vbeln,
               lt_vbkd         type sorted table of vbkd with non-unique key vbeln,
               ls_hesap        type ty_hesap,
               lt_hesap        type tt_hesap,
               lt_t001         type sorted table of t001 with unique key bukrs ##NEEDED,
*               lv_konto        type konto,
               lt_konto        type table of ty_konto,
               ls_konto        type ty_konto,
               lc_green        type col_item value 'C51',
               lc_yellow       type col_item value 'C31'.

        loop at ct_items assigning <ls_items>.
          clear ls_hesap.
          ls_hesap-sube = <ls_items>-konto.
          collect ls_hesap into lt_hesap.
          ls_konto-bukrs = <ls_items>-bukrs.
          ls_konto-konto = <ls_items>-konto.
          collect ls_konto into lt_konto.
        endloop.
        assign  ('(SAPLFI_ITEMS)GB_CENTRAL_ITEMS') to field-symbol(<lv_merkez>).
        if sy-subrc eq 0.
          if <lv_merkez> eq abap_true.
            case sy-tcode.
              when 'FBL5N'.
                select * into table @data(lt_knb1) from knb1
                    for all entries in @lt_hesap
                    where kunnr eq @lt_hesap-sube and
                          knrze ne @space.
                sort lt_knb1 by bukrs kunnr.
                loop at lt_knb1 assigning field-symbol(<ls_knb1>).
                  ls_hesap-sube = <ls_knb1>-kunnr.
                  ls_hesap-merkez = <ls_knb1>-knrze.
                  collect ls_hesap into lt_hesap.
                endloop.

              when 'FBL1N'.

                select * into table @data(lt_lfb1) from lfb1
                    for all entries in @lt_hesap
                    where lifnr eq @lt_hesap-sube and
                          lnrze ne @space.
                sort lt_lfb1 by bukrs lifnr.
                loop at lt_lfb1 assigning field-symbol(<ls_lfb1>).
                  ls_hesap-sube   = <ls_lfb1>-lifnr.
                  ls_hesap-merkez = <ls_lfb1>-lnrze.
                  collect ls_hesap into lt_hesap.
                endloop.

            endcase.
          endif.
        endif.
        select * from t001  into table lt_t001.

        zcl_fi_toolkit=>devir_fblxn(
                    exporting it_hesap = lt_hesap
                    importing et_devir = lt_devir
                                             ).
        sort lt_devir by bukrs konto gsber.
        lt_devir_sorted = lt_devir.

*--------------------------------------------------------------------*
* Malzeme belgelerini al - sadece ters kayıt olanlar
*--------------------------------------------------------------------*
        loop at ct_items assigning <ls_items>
           where zzawtyp eq c_mal_hareketi or zzawtyp eq  c_satinalma_faturasi
               or  zzawtyp eq  c_satis_faturasi.
          if <ls_items>-zzawtyp eq c_mal_hareketi.
            append value #( mblnr = <ls_items>-zzawkey(10)
                            mjahr = <ls_items>-zzawkey+10(4)
                           ) to lt_mkpf_key.
          elseif <ls_items>-zzawtyp eq c_satinalma_faturasi.
            append value #( belnr = <ls_items>-zzawkey(10)
                            gjahr = <ls_items>-zzawkey+10(4)
                           ) to lt_rbkp_key.
          elseif <ls_items>-zzawtyp eq c_satis_faturasi.
            append value #( vbeln = <ls_items>-zzawkey(10)
                           ) to lt_vbrk_key.
          endif.
        endloop.

        sort lt_rbkp_key by belnr gjahr.
        sort lt_mkpf_key by mblnr mjahr.
        sort lt_vbrk_key by vbeln.
        delete adjacent duplicates from lt_mkpf_key comparing mblnr mjahr.
        delete adjacent duplicates from lt_rbkp_key comparing belnr gjahr.
        delete adjacent duplicates from lt_vbrk_key comparing vbeln.

        if lt_rbkp_key is not initial.
          select * from rbkp into table lt_rbkp
                   for all entries in lt_rbkp_key
                   where belnr eq lt_rbkp_key-belnr and
                         gjahr eq lt_rbkp_key-gjahr.
* ters kayıt olanların m.b. sini bul
        endif.
        if lt_mkpf_key is not initial.
          select * from mseg into table lt_mseg
                   for all entries in lt_mkpf_key
                   where mblnr eq lt_mkpf_key-mblnr and
                         mjahr eq lt_mkpf_key-mjahr.
* ters kayıt olanların m.b. sini bul
        endif.
        if lt_vbrk_key[] is not initial.
          select * from vbrp into table lt_vbrp
                   for all entries in lt_vbrk_key
                   where vbeln eq lt_vbrk_key-vbeln.
          delete adjacent duplicates from lt_vbrp comparing vbeln.
          if lt_vbrp[] is not initial.
            select * from vbkd into table lt_vbkd
                   for all entries in lt_vbrp
                   where vbeln eq lt_vbrp-aubel.
          endif.
* ters kayıt olanların m.b. sini bul
        endif.
*--------------------------------------------------------------------*
* Ek alanları güncelle
*--------------------------------------------------------------------*
        sort ct_items by konto budat ascending.
        loop at ct_items assigning <ls_items>.
          lv_tabix = sy-tabix.

*  test kayıt belgesi
          if <ls_items>-zzawtyp eq c_mal_hareketi.
            read table lt_mseg with table key mblnr = <ls_items>-zzawkey(10)
                                              mjahr = conv #( <ls_items>-zzawkey+10(4) )
                                              assigning field-symbol(<ls_mseg>).
            if sy-subrc eq 0.
* mb 'nin ters kaydınınn faturası
* bu case çok az olacağını için direk bkpf 'e gidildi.
              clear lv_awkey.
              if <ls_mseg>-smbln is  not initial.
                lv_awkey = |{ <ls_mseg>-smbln }{ <ls_mseg>-sjahr }|.
              else.
                read table lt_mseg with key smbln = <ls_mseg>-mblnr
                                            sjahr = <ls_mseg>-mjahr
                                            assigning <ls_mseg>.

                if sy-subrc eq 0.
                  lv_awkey = |{ <ls_mseg>-mblnr }{ <ls_mseg>-mjahr }|.
                endif.
              endif.

            endif.

          elseif <ls_items>-zzawtyp eq c_satinalma_faturasi.
            read table lt_rbkp with table key belnr = <ls_items>-zzawkey(10)
                                              gjahr = conv #( <ls_items>-zzawkey+10(4) )
                                              assigning field-symbol(<ls_rbkp>).
            if sy-subrc eq 0.
* mb 'nin ters kaydınınn faturası
* bu case çok az olacağını için direk bkpf 'e gidildi.
              clear lv_awkey.
              if <ls_rbkp>-stblg is  not initial.
                lv_awkey = |{ <ls_rbkp>-stblg }{ <ls_rbkp>-stjah }|.
              else.
                read table lt_rbkp with key stblg = <ls_rbkp>-belnr
                                            stjah = <ls_rbkp>-gjahr
                                            assigning <ls_rbkp>.

                if sy-subrc eq 0.
                  lv_awkey = |{ <ls_rbkp>-belnr }{ <ls_rbkp>-gjahr }|.
                endif.
              endif.

            endif.
          elseif <ls_items>-zzawtyp eq c_satis_faturasi.
            read table lt_vbrp with key vbeln = <ls_items>-zzawkey(10)
                                            assigning field-symbol(<ls_vbrp>).
            if sy-subrc eq 0.
              if <ls_vbrp>-vgtyp = 'J' or <ls_vbrp>-vgtyp = 'T'.
                <ls_items>-zzteslimat = <ls_vbrp>-vgbel.
              endif.
              read table lt_vbkd with key vbeln = <ls_vbrp>-aubel
                                           assigning field-symbol(<ls_vbkd>).
              if sy-subrc eq 0.
                <ls_items>-zzbstkd = <ls_vbkd>-bstkd.
              endif.
            endif.
          endif.

          if lv_awkey is  not initial.
            select single belnr into <ls_items>-zzstblg from bkpf
                          where awtyp = <ls_items>-zzawtyp and
                                awkey = lv_awkey
                          ##WARN_OK .
            if sy-subrc eq 0.
              <ls_items>-zzstjah = <ls_items>-gjahr.
            endif.
          endif.
****************************************************************************************************

          if lv_konto_temp is initial or
             lv_konto_temp ne <ls_items>-konto.
* Devir kalemini ekle
            clear : ls_devir,ls_devir_merkez.
            loop at lt_devir_sorted into ls_devir
              where bukrs = <ls_items>-bukrs
                and konto = <ls_items>-konto.

              if <lv_merkez> is assigned.
                if <lv_merkez> eq abap_true.
                  case sy-tcode.
                    when 'FBL1N'.
                      read table lt_lfb1 assigning <ls_lfb1>
                                         with key bukrs = <ls_items>-bukrs
                                                  lifnr = <ls_items>-konto binary search.
                      if sy-subrc eq 0.
                        loop at lt_devir_sorted assigning field-symbol(<lfs_devir_sorted_lnrze>)
                                                where bukrs = <ls_items>-bukrs
                                                  and konto = <ls_lfb1>-lnrze.
                          append <lfs_devir_sorted_lnrze> to lt_devir_merkez.
                        endloop.
                      endif.
                    when 'FBL5N'.
                      read table lt_knb1 assigning <ls_knb1>
                                         with key bukrs = <ls_items>-bukrs
                                                  kunnr = <ls_items>-konto binary search.
                      if sy-subrc eq 0.
                        loop at lt_devir_sorted assigning field-symbol(<lfs_devir_sorted_knrze>)
                                                where bukrs = <ls_items>-bukrs
                                                  and konto = <ls_knb1>-knrze.
                          append <lfs_devir_sorted_knrze> to lt_devir_merkez.
                        endloop.

                      endif.
                  endcase.
                endif.
              endif.

              clear ls_item_devir.
              if ls_devir-bukrs is initial.
                move-corresponding ls_devir_merkez to ls_item_devir  ##ENH_OK.
                ls_item_devir-wrshb =  ls_devir_merkez-wrbtr.
                ls_item_devir-waers =  ls_devir_merkez-waers.
              else.
                add ls_devir_merkez-dmshb to ls_devir-dmshb.
                add ls_devir_merkez-dmbe2 to ls_devir-dmbe2.
                add ls_devir_merkez-dmbe3 to ls_devir-dmbe3.
                add ls_devir_merkez-wrbtr to ls_devir-wrbtr.

                move-corresponding ls_devir to ls_item_devir  ##ENH_OK.
                ls_item_devir-wrshb =  ls_devir-wrbtr.
                ls_item_devir-waers =  ls_devir-waers.

              endif.

              ls_item_devir-zzname1_ku = <ls_items>-zzname1_ku.
              ls_item_devir-zzname1_li = <ls_items>-zzname1_li.
              ls_item_devir-konto = <ls_items>-konto.
              ls_item_devir-zuonr = text-dvg.
              ls_item_devir-u_bktxt =  ls_item_devir-sgtxt  = |{ text-004 }({ <ls_items>-konto })|.
              ls_item_devir-hwaer = <ls_items>-hwaer.
              ls_item_devir-hwae2 = <ls_items>-hwae2.
              ls_item_devir-hwae3 = <ls_items>-hwae3.
              if ls_item_devir-dmshb lt 0.
                ls_item_devir-zzalacak_upb = ls_item_devir-dmshb * -1.
                ls_item_devir-zzalacak_2pb = ls_item_devir-dmbe2 * -1.
                ls_item_devir-zzalacak_3pb = ls_item_devir-dmbe3 * -1.
              else.
                ls_item_devir-zzborc_upb = ls_item_devir-dmshb.
                ls_item_devir-zzborc_2pb = ls_item_devir-dmbe2.
                ls_item_devir-zzborc_3pb = ls_item_devir-dmbe3.
              endif.
              ls_item_devir-bukrs = <ls_items>-bukrs.
              ls_item_devir-konto = <ls_items>-konto.
              ls_item_devir-color = lc_green.

              collect ls_item_devir into lt_item_devir.

              clear ls_item_sum.
              ls_item_sum-wrshb = ls_item_devir-wrshb.
              ls_item_sum-waers = ls_item_devir-waers.
              ls_item_sum-dmshb = ls_item_devir-dmshb.
              ls_item_sum-hwaer = ls_item_devir-hwaer.
              ls_item_sum_-hwaer = ls_item_sum-hwaer.

              add ls_item_sum-dmshb to ls_item_sum_-dmshb.

              ls_item_sum-color = lc_yellow.
              collect ls_item_sum into lt_item_sum_top.
            endloop.
            if sy-subrc ne 0.
* devir yoksa boş devir yazısı yazalım.
              clear ls_item_devir.
              ls_item_devir-bukrs = <ls_items>-bukrs.
              ls_item_devir-konto = <ls_items>-konto.
              ls_item_devir-zuonr = text-dvg.
              ls_item_devir-color = lc_green.
              ls_item_devir-zzname1_ku = <ls_items>-zzname1_ku.
              ls_item_devir-zzname1_li = <ls_items>-zzname1_li.
              ls_item_devir-u_bktxt =  ls_item_devir-sgtxt  = |{ text-004 }({ <ls_items>-konto })|.
              insert ls_item_devir into ct_items index lv_tabix.

              ls_item_sum_-bukrs = <ls_items>-bukrs.
              ls_item_sum_-konto = <ls_items>-konto.
              ls_item_sum_-zzname1_ku = <ls_items>-zzname1_ku.
              ls_item_sum_-zzname1_li = <ls_items>-zzname1_li.
              ls_item_sum_-zuonr = text-dvy.
              ls_item_sum_-color = lc_yellow.
              ls_item_sum_-u_bktxt =  ls_item_sum_-sgtxt  = |{ text-004 }({ <ls_items>-konto })|.
              insert ls_item_sum_ into ct_items index lv_tabix + 1.


            else.
              ls_item_sum_-bukrs = <ls_items>-bukrs.
              ls_item_sum_-konto = <ls_items>-konto.
              ls_item_sum_-zzname1_ku = <ls_items>-zzname1_ku.
              ls_item_sum_-zzname1_li = <ls_items>-zzname1_li.
              ls_item_sum_-zuonr = text-dvy.
              ls_item_sum_-color = lc_yellow.

              if ls_item_sum_-dmshb lt 0.
                ls_item_sum_-zzalacak_upb = ls_item_sum_-dmshb.
              else.
                ls_item_sum_-zzborc_upb = ls_item_sum_-dmshb.
              endif.

              ls_item_sum_-u_bktxt =  ls_item_sum_-sgtxt  = |{ text-004 }({ <ls_items>-konto })|.
              ls_item_devir = ls_item_sum_.
              append ls_item_sum_ to lt_item_devir.
              insert lines of lt_item_devir into ct_items index lv_tabix.
              refresh lt_item_devir.
            endif.
            clear ls_item_sum_.

          endif.

          if <ls_items>-shkzg eq c_alacak.
            <ls_items>-zzalacak_upb = <ls_items>-dmshb * -1.
            <ls_items>-zzalacak_2pb = <ls_items>-dmbe2 * -1.
            <ls_items>-zzalacak_3pb = <ls_items>-dmbe3 * -1.
          else.
            <ls_items>-zzborc_upb = <ls_items>-dmshb.
            <ls_items>-zzborc_2pb = <ls_items>-dmbe2.
            <ls_items>-zzborc_3pb = <ls_items>-dmbe3.
          endif.

          <ls_items>-zzbakiye_upb =  ls_item_devir-dmshb =
                                     ls_item_devir-dmshb +
                                     <ls_items>-dmshb.

          <ls_items>-zzbakiye_2pb =  ls_item_devir-dmbe2 =
                                     ls_item_devir-dmbe2 +
                                     <ls_items>-dmbe2.

          <ls_items>-zzbakiye_3pb =  ls_item_devir-dmbe3 =
                                     ls_item_devir-dmbe3 +
                                     <ls_items>-dmbe3.


          lv_konto_temp = <ls_items>-konto.


        endloop.
* dönem sonu bakiyeleri ekle
        loop at lt_konto into ls_konto.
          refresh lt_item_sum.
          clear ls_item_sum.
          clear ls_item_sum_.
          loop at ct_items assigning <ls_items>
            where bukrs eq ls_konto-bukrs
              and konto eq ls_konto-konto.
            lv_tabix = sy-tabix.
            check <ls_items>-color ne lc_yellow.
            check <ls_items>-hwaer is not initial.
            ls_item_sum-bukrs = <ls_items>-bukrs.
            ls_item_sum-konto = <ls_items>-konto.
            ls_item_sum-gsber = <ls_items>-gsber.
            ls_item_sum-wrshb = <ls_items>-wrshb.
            ls_item_sum-waers = <ls_items>-waers.
            ls_item_sum-dmshb = <ls_items>-dmshb.
            ls_item_sum-hwaer = <ls_items>-hwaer.
            ls_item_sum-zuonr = text-dng.
            ls_item_sum-color = lc_green.
            ls_item_sum-u_bktxt =  ls_item_sum-sgtxt  = |{ text-005 }({ <ls_items>-konto })|.

            ls_item_sum_-bukrs = <ls_items>-bukrs.
            ls_item_sum_-konto = <ls_items>-konto.
            ls_item_sum_-u_bktxt =  ls_item_sum-sgtxt  = |{ text-005 }({ <ls_items>-konto })|.
            ls_item_sum_-zuonr = text-dny.
            ls_item_sum_-color = lc_yellow.
            add ls_item_sum-dmshb to ls_item_sum_-dmshb.
            ls_item_sum_-hwaer = ls_item_sum-hwaer.

            collect ls_item_sum into lt_item_sum.
          endloop.

          add 1 to lv_tabix.
          append ls_item_sum_ to lt_item_sum.
            APPEND INITIAL LINE TO lt_item_sum.

          insert lines of lt_item_sum into ct_items index lv_tabix.

        endloop.
      endif.
    else.

      loop at ct_items assigning field-symbol(<ls_items1>)
          where  zzawtyp eq  c_satis_faturasi.
        if <ls_items1>-zzawtyp eq c_satis_faturasi.
          append value #( vbeln = <ls_items1>-zzawkey(10)
                         ) to lt_vbrk_key.
        endif.
      endloop.
      sort lt_vbrk_key by vbeln.
      delete adjacent duplicates from lt_vbrk_key comparing vbeln.
      if lt_vbrk_key[] is not initial.
        select * from vbrp into table lt_vbrp
                 for all entries in lt_vbrk_key
                 where vbeln eq lt_vbrk_key-vbeln.
        delete adjacent duplicates from lt_vbrp comparing vbeln.
        if lt_vbrp[] is not initial.
          select * from vbkd into table lt_vbkd
                 for all entries in lt_vbrp
                 where vbeln eq lt_vbrp-aubel.
        endif.
      endif.
      loop at ct_items assigning <ls_items>
              where zzawtyp eq c_satis_faturasi.
        read table lt_vbrp with key vbeln = <ls_items>-zzawkey(10)
                                           assigning <ls_vbrp>.
        if sy-subrc eq 0.
          if <ls_vbrp>-vgtyp = 'J' or <ls_vbrp>-vgtyp = 'T'.
            <ls_items>-zzteslimat = <ls_vbrp>-vgbel.
          endif.
          read table lt_vbkd with key vbeln = <ls_vbrp>-aubel
                                       assigning <ls_vbkd>.
          if sy-subrc eq 0.
            <ls_items>-zzbstkd = <ls_vbkd>-bstkd.
          endif.
        endif.
      endloop.


    endif.


  endmethod.


  METHOD get_bkpf_xblnr.

    DATA lt_doc TYPE tt_doc_xblnr.

    CHECK ct_doc[] IS NOT INITIAL.

    SELECT bukrs belnr gjahr xblnr
           INTO CORRESPONDING FIELDS OF TABLE lt_doc
           FROM bkpf
           FOR ALL ENTRIES IN ct_doc
           WHERE bukrs EQ ct_doc-bukrs
             AND belnr EQ ct_doc-belnr
             AND gjahr EQ ct_doc-gjahr.

    SORT lt_doc BY bukrs belnr gjahr.

    LOOP AT ct_doc ASSIGNING FIELD-SYMBOL(<ls_doc_tar>).

      READ TABLE lt_doc ASSIGNING FIELD-SYMBOL(<ls_doc_src>)
                        WITH KEY bukrs = <ls_doc_tar>-bukrs
                                 belnr = <ls_doc_tar>-belnr
                                 gjahr = <ls_doc_tar>-gjahr
                        BINARY SEARCH.

      IF sy-subrc EQ 0.
        <ls_doc_tar>-xblnr = <ls_doc_src>-xblnr.
      ELSE.
        CLEAR <ls_doc_tar>-xblnr.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_company_long_text.

    ASSIGN gt_company_long_text[ KEY primary_key
                                 COMPONENTS bukrs = iv_bukrs
                               ] TO FIELD-SYMBOL(<ls_clt>).

    IF sy-subrc NE 0.

      DATA(ls_clt) = VALUE t_company_long_text( bukrs = iv_bukrs ).

      SELECT SINGLE adrnr, butxt
             INTO @DATA(ls_t001)
             FROM t001
             WHERE bukrs EQ @iv_bukrs.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( iv_bukrs )
            tabname  = c_tabname_t001.
      ENDIF.

      ls_clt-text = ls_t001-butxt.

      IF ls_t001-adrnr IS NOT INITIAL.

        SELECT SINGLE name1, name2, name3, name4
               INTO @DATA(ls_adrc)
               FROM adrc
               WHERE addrnumber EQ @ls_t001-adrnr
                 AND date_from  LE @sy-datum
                 AND date_to    GE @sy-datum
               ##WARN_OK .

        IF ls_adrc-name1 IS NOT INITIAL OR
           ls_adrc-name2 IS NOT INITIAL OR
           ls_adrc-name3 IS NOT INITIAL OR
           ls_adrc-name4 IS NOT INITIAL.

          ls_clt-text = |{ ls_adrc-name1 } { ls_adrc-name2 } { ls_adrc-name3 } { ls_adrc-name4 }|.

        ENDIF.

      ENDIF.

      INSERT ls_clt INTO TABLE gt_company_long_text ASSIGNING <ls_clt>.

    ENDIF.

    rv_text = <ls_clt>-text.

  ENDMETHOD.


  METHOD get_iban_codes.

    IF iv_get_vendor IS NOT INITIAL.

      SELECT lfa1~lifnr, tiban~*
        APPENDING CORRESPONDING FIELDS OF TABLE @rt_tiban
        FROM lfa1
             INNER JOIN lfbk ON lfbk~lifnr EQ lfa1~lifnr
             INNER JOIN tiban ON tiban~banks EQ lfbk~banks AND
                                 tiban~bankl EQ lfbk~bankl AND
                                 tiban~bankn EQ lfbk~bankn AND
                                 tiban~bkont EQ lfbk~bkont
        WHERE lfa1~lifnr IN @it_lifnr AND
              tiban~iban IN @it_iban
        ##TOO_MANY_ITAB_FIELDS.

    ENDIF.

    IF iv_get_client IS NOT INITIAL.

      SELECT kna1~lifnr, tiban~*
        APPENDING CORRESPONDING FIELDS OF TABLE @rt_tiban
        FROM kna1
             INNER JOIN knbk ON knbk~kunnr EQ kna1~kunnr
             INNER JOIN tiban ON tiban~banks EQ knbk~banks AND
                                 tiban~bankl EQ knbk~bankl AND
                                 tiban~bankn EQ knbk~bankn AND
                                 tiban~bkont EQ knbk~bkont
        WHERE kna1~lifnr IN @it_kunnr AND
              tiban~iban IN @it_iban
        ##TOO_MANY_ITAB_FIELDS.

    ENDIF.

  ENDMETHOD.


  method modify_report_falgll03_flbxn.
*--------------------------------------------------------------------*
* written by mehmet sertkaya 17.12.2015 15:54:51
*--------------------------------------------------------------------*
* FBL*N veya FAGLL03 işlem kodundan iki ayrı yapıda tablo gelebilir.
*--------------------------------------------------------------------*
    data : lt_bkpf          type sorted table of bkpf with non-unique key bukrs belnr gjahr,
           lt_bseg          type sorted table of ty_bseg with non-unique key bukrs belnr gjahr,
           lt_skat          type sorted table of skat with unique key primary_key components spras ktopl saknr,
           lt_anla          type sorted table of anla with unique key primary_key components bukrs anln1 anln2,
*           ls_skat          type skat,
           lt_temp          type table of ty_bseg,
           ls_temp          type ty_bseg,
           ls_rfpos         type rfposxext,
           lv_bkpf_keyc     type char21,
           lv_bkpf_key      type bkpf_key,
           ls_bkpf_addon    type bkpf_addon,
           lt_kunnr         type table of kna1-kunnr,
           lt_lifnr         type table of lfa1-lifnr,
           lt_kna1          type sorted table of t_kna1 with unique key kunnr,
           lt_lfa1          type sorted table of lfa1 with non-unique key lifnr,
           lv_yevmiye       type char100,
           lv_karsit_hesap  type char100,
           lv_yevmiyex      type char1,
           lv_karsit_hesapx type char1,
           lv_main_program  type sy-repid,
           lr_ktopl         type range of ktopl.

    field-symbols :  <lt_data>  type standard table.

    assign ir_data->* to <lt_data>.
    check sy-subrc eq 0.
    check <lt_data> is not initial.

    call function 'SAPGUI_PROGRESS_INDICATOR' ##FM_SUBRC_OK
      exporting
*       percentage =
        text   = text-001
      exceptions
        others = 1.


*--------------------------------------------------------------------*
* Seçim ekranı girişlerini oku
*--------------------------------------------------------------------*
    case sy-tcode.
      when 'FBL1N'. lv_main_program = 'RFITEMAP'.
      when 'FBL3N'. lv_main_program = 'RFITEMGL'.
      when 'FBL5N'. lv_main_program = 'RFITEMAR'.
    endcase.

    if lv_main_program is not initial.
      lv_yevmiye      = |({ lv_main_program })P_YEVMIY|.
      lv_karsit_hesap = |({ lv_main_program })P_KARSIT|.
      assign (lv_yevmiye) to field-symbol(<lv_yevmiye>).
      if sy-subrc eq 0.
        lv_yevmiyex = <lv_yevmiye>.
      endif.
      assign (lv_karsit_hesap) to field-symbol(<lv_karsit_hesap>).
      if sy-subrc eq 0.
        lv_karsit_hesapx = <lv_karsit_hesap>.
      endif.
    endif.

*--------------------------------------------------------------------*
* Ek verileri doldur
*--------------------------------------------------------------------*
* anahtar alanları doldur
    clear : ls_rfpos.

    loop at <lt_data> assigning field-symbol(<ls_data>).
      move-corresponding <ls_data> to ls_rfpos.
      clear ls_temp.
      ls_temp-bukrs = ls_rfpos-bukrs.
      ls_temp-belnr = ls_rfpos-belnr.
      ls_temp-gjahr = ls_rfpos-gjahr.
      collect ls_temp into lt_temp .

      if ls_rfpos-konto is not initial and
         ls_rfpos-koart eq 'D'.
        collect ls_rfpos-konto into lt_kunnr.
        collect ls_rfpos-filkd into lt_kunnr.
      endif.
      if ls_rfpos-konto is not initial and
         ls_rfpos-koart eq 'K'.
        collect ls_rfpos-konto into lt_lifnr.
      endif.
    endloop.

    if sy-tcode = 'FBL5N'.
      select * into table @data(lt_sube)
      from zfit_belge_sube
      for all entries in @lt_temp
      where bukrs eq @lt_temp-bukrs and
            belnr eq @lt_temp-belnr and
            gjahr eq @lt_temp-gjahr.
      sort lt_sube by bukrs belnr gjahr.
      loop at lt_sube reference into data(lr_sube).
        collect lr_sube->subehes into lt_kunnr.
      endloop.
    endif.
*--------------------------------------------------------------------*
* müşteri ve satıcılar
*--------------------------------------------------------------------*
    if lt_kunnr is not initial.
      select kunnr name1
         into table lt_kna1 from kna1
         for all entries in lt_kunnr
         where kunnr eq lt_kunnr-table_line.
    endif.
    if lt_lifnr is not initial.
      select * into table lt_lfa1 from lfa1
         for all entries in lt_lifnr
         where lifnr eq lt_lifnr-table_line.
    endif.

    check lt_temp is not initial.

* belge başlık oku
    select * into table lt_bkpf
      from bkpf
      for all entries in lt_temp
      where bukrs eq lt_temp-bukrs and
            belnr eq lt_temp-belnr and
            gjahr eq lt_temp-gjahr.

* belge kalemlerini oku
    select bukrs
           belnr
           gjahr
           buzei
           kunnr
           lifnr
           hkont
           koart
           vbeln
           vbel2
           posn2
           anln1
           anln2
      into CORRESPONDING FIELDS OF table lt_bseg ##TOO_MANY_ITAB_FIELDS
      from bseg
      for all entries in lt_temp
      where bukrs eq lt_temp-bukrs and
            belnr eq lt_temp-belnr and
            gjahr eq lt_temp-gjahr.
**********************************************
* ek alanları bul

    select * from t001 into table @data(lt_t001)
      for all entries in @lt_temp
      where bukrs eq @lt_temp-bukrs.

    loop at lt_t001 assigning field-symbol(<ls_t001>).
      append initial line to lr_ktopl reference into data(ls_ktopl).
      ls_ktopl->sign = 'I'.
      ls_ktopl->option = 'EQ'.
      ls_ktopl->low = <ls_t001>-ktopl.
    endloop.

    select * from skat into table lt_skat
      for all entries in lt_bseg
      where spras eq sy-langu
        and ktopl in lr_ktopl
        and saknr eq lt_bseg-hkont.

    select * from anla into table lt_anla
      for all entries in lt_bseg
      where bukrs eq lt_bseg-bukrs
        and anln1 eq lt_bseg-anln1
        and anln2 eq lt_bseg-anln2.

    loop at <lt_data> assigning <ls_data>.

      clear : ls_temp,lt_temp,ls_rfpos.

      move-corresponding <ls_data> to ls_temp.
      move-corresponding <ls_data> to ls_rfpos.


*   Net vade tarihi düzeltilmesi.
      "--------->> add by mehmet sertkaya 21.12.2016 13:18:13
*     if ls_rfpos-blart eq 'TK'.
* Bütün Belge türlerinde yapılacağı için bu kısım kaldırıldı.
      "-----------------------------<<
      data(lv_zfbdt) = ls_rfpos-zfbdt.
      if ls_rfpos-zbd3t is not initial .
        lv_zfbdt = lv_zfbdt + ls_rfpos-zbd3t.
      elseif ls_rfpos-zbd2t is not initial .
        lv_zfbdt = lv_zfbdt + ls_rfpos-zbd2t.
      elseif ls_rfpos-zbd1t is not initial .
        lv_zfbdt = lv_zfbdt + ls_rfpos-zbd1t.
      endif.
      if lv_zfbdt <> ls_rfpos-faedt .
        ls_rfpos-faedt = lv_zfbdt.
      endif.
*    endif.


*--------------------------------------------------------------------*
* Referans anahtar
*--------------------------------------------------------------------*
      read table lt_bkpf assigning field-symbol(<ls_bkpf>)
                         with table key bukrs = ls_temp-bukrs
                                        belnr = ls_temp-belnr
                                        gjahr = ls_temp-gjahr.

      if sy-subrc eq 0.
        ls_rfpos-zzstblg = <ls_bkpf>-stblg.
        ls_rfpos-zzstjah = <ls_bkpf>-stjah.
        ls_rfpos-zzawtyp = <ls_bkpf>-awtyp.
        ls_rfpos-zzawkey = <ls_bkpf>-awkey.
        ls_rfpos-zzxreversal = <ls_bkpf>-xreversal.
        ls_rfpos-zzxstov     = <ls_bkpf>-xstov.
      endif.



*--------------------------------------------------------------------*
* Yevmiye No
*--------------------------------------------------------------------*
      if lv_yevmiyex eq abap_true.
        lv_bkpf_keyc = |{ sy-mandt }{ ls_rfpos-bukrs }{ ls_rfpos-belnr }{ ls_rfpos-gjahr }|.
        lv_bkpf_key  = lv_bkpf_keyc.
        call function 'OPEN_FI_PERFORM_00003320_P'
          exporting
            i_bkpf_key   = lv_bkpf_key
            i_land1      = 'TR'
          importing
            e_bkpf_addon = ls_bkpf_addon.

        ls_rfpos-zzyevmiyeno = ls_bkpf_addon-belnr_alt.
      endif.
*--------------------------------------------------------------------*
* Müşteri & Satıcı Adı
*--------------------------------------------------------------------*
      case ls_rfpos-koart.
        when 'K'."satıcı adı
          read table lt_lfa1 assigning field-symbol(<ls_lfa1>)
                             with table key lifnr = ls_rfpos-konto.
          if sy-subrc eq 0.
            ls_rfpos-zzname1_li = <ls_lfa1>-name1.
          endif.
        when 'D'."müşteri adı
          read table lt_kna1 assigning field-symbol(<ls_kna1>)
                             with table key kunnr = ls_rfpos-konto.
          if sy-subrc eq 0.
            ls_rfpos-zzname1_ku = <ls_kna1>-name1.
          endif.
        when others.
      endcase.
      if ls_rfpos-filkd is not initial.
        read table lt_kna1 assigning <ls_kna1>
                           with table key kunnr = ls_rfpos-filkd.
        if sy-subrc eq 0.
          ls_rfpos-zzfilkd_text = <ls_kna1>-name1.
        endif.
      endif.

      read table lt_sube assigning field-symbol(<ls_sube>)
                       with key bukrs = ls_temp-bukrs
                                  belnr = ls_temp-belnr
                                  gjahr = ls_temp-gjahr
                                  binary search.
      if sy-subrc = 0.

        ls_rfpos-filkd = <ls_sube>-subehes.
*{  EDIT  Berrin Ulus 02.02.2016 18:27:22
*             ls_rfpos-ZZFILKD_TEXT = <ls_sube>-subead.
        read table lt_kna1 assigning <ls_kna1>
                           with table key kunnr = <ls_sube>-subehes.
        if sy-subrc eq 0.
          ls_rfpos-zzfilkd_text = <ls_kna1>-name1.
        endif.
*}  EDIT  Berrin Ulus 02.02.2016 18:27:22

      endif.
*--------------------------------------------------------------------*
* Karşıt hesap
*--------------------------------------------------------------------*
      if lv_karsit_hesapx eq abap_true.
        call function 'GET_GKONT'
          exporting
            belnr           = ls_temp-belnr
            bukrs           = ls_temp-bukrs
            buzei           = ls_temp-buzei
            gjahr           = ls_temp-gjahr
            gknkz           = '3'
          importing
            gkont           = ls_temp-gkont
          exceptions
            belnr_not_found = 1
            buzei_not_found = 2
            gknkz_not_found = 3
            others          = 4 ##FM_SUBRC_OK.
      endif.
      loop at lt_bseg assigning field-symbol(<ls_bseg>)
                      where bukrs eq ls_temp-bukrs and
                            belnr eq ls_temp-belnr and
                            gjahr eq ls_temp-gjahr .

        append <ls_bseg> to lt_temp.
        if ls_temp-vbeln is initial.
          ls_temp-vbeln = <ls_bseg>-vbeln.
        endif.
        if ls_temp-vbel2 is initial.
          ls_temp-vbel2 = <ls_bseg>-vbel2.
        endif.
        if ls_temp-posn2 is initial.
          ls_temp-posn2 = <ls_bseg>-posn2.
        endif.
        if sy-tcode eq 'FBL3N' or sy-tcode eq 'FAGLL03'.
          if <ls_bseg>-kunnr is not initial and <ls_bseg>-koart eq c_koart_kunnr.
            ls_temp-gkont = <ls_bseg>-kunnr.
          elseif <ls_bseg>-lifnr is not initial and <ls_bseg>-koart eq c_koart_lifnr.
            ls_temp-gkont = <ls_bseg>-lifnr.
          endif.
        elseif ( sy-tcode eq 'FBL1N' and ls_temp-konto eq ls_temp-gkont ) or
               ( sy-tcode eq 'FBL5N' and ls_temp-konto eq ls_temp-gkont ).
          if <ls_bseg>-hkont(4) ne '0320' and
             <ls_bseg>-hkont(4) ne '0120'.
            ls_temp-gkont = <ls_bseg>-hkont.
          endif.
        endif.
      endloop.
      "--------->> add by mehmet sertkaya 26.01.2017 14:35:23
      " HAR-13267 - Ekom faturalarında şube adı ve şube hesabı alanl
      " bu koşullara uyan az belge olduğundan performans dikkate alınmadı

      if ls_rfpos-zuonr(2) eq 'EX' and ls_rfpos-filkd is initial.

        select single bkpf~* into @data(ls_bkpf)
          from
            zexpgrp
            inner join zexp_ex_fat on zexp_ex_fat~ziinum eq zexpgrp~iinum
            inner join bkpf on
              bkpf~xblnr eq zexp_ex_fat~vbeln and
              bkpf~blart eq 'RV'
          where zexpgrp~ramno eq @ls_rfpos-zuonr(12)
          ##WARN_OK.

        select single filkd
          into ls_rfpos-filkd
          from bseg
          where (
            bukrs eq ls_bkpf-bukrs and
            belnr eq ls_bkpf-belnr and
            gjahr eq ls_bkpf-gjahr and
            filkd ne space
          ) ##WARN_OK.

        select single name1 into ls_rfpos-zzfilkd_text from kna1
           where kunnr eq ls_rfpos-filkd.
      endif.
      "-----------------------------<<
      if ls_rfpos-zzawtyp = 'MKPF'.

        select single bwart
          into ls_rfpos-zzbwart
          from mseg
          where (
            mblnr eq ls_rfpos-zzawkey(10) and
            mjahr eq ls_rfpos-zzawkey+10(4)
          ) ##WARN_OK .

      elseif ls_rfpos-zzawtyp = 'VBRK'.

        select single b~bwart
          into ls_rfpos-zzbwart
          from
            vbrp as a
            inner join lips as b on a~vgbel = b~vbeln
          where a~vbeln eq ls_rfpos-zzawkey(10)
          ##WARN_OK.

        "--------->> add by mehmet sertkaya 16.08.2017 13:13:28
        "VOL-1311 Ekstre (Müşteri/Satıcı/Defter-i Kebir)

        select single vbkd~bstdk
          into @data(lv_bstdk)
          from
            vbrk
            inner join vbrp on vbrp~vbeln eq vbrk~vbeln
            inner join vbkd on vbkd~vbeln eq vbrp~aubel
          where (
            vbrk~vbeln eq @ls_rfpos-zzawkey(10)  and
            vbkd~bstdk ne '00000000'
          ) ##WARN_OK.

        if sy-subrc eq 0 and
           lv_bstdk is not initial.
          ls_rfpos-bldat = lv_bstdk.
        endif.
        "-----------------------------<<

      endif.
      "--------->> add by mustafa sarıbaş 6.10.2017 10:00:00
      "VOL-1311 Ekstre (Müşteri/Satıcı/Defter-i Kebir)
      if ls_rfpos-blart eq 'YP'.

        select single lifnr from bseg into ls_rfpos-zzlifnr
          where bukrs eq ls_rfpos-bukrs
            and gjahr eq ls_rfpos-sgtxt+13(4)
            and belnr eq ls_rfpos-sgtxt+0(10)
            and buzei eq ls_rfpos-sgtxt+10(3).

        select single name1 from lfa1 into ls_rfpos-zzname1_li
          where lifnr eq ls_rfpos-zzlifnr.

        select single kunnr from bseg into ls_rfpos-zzkunnr
          where bukrs eq ls_rfpos-bukrs
            and gjahr eq ls_rfpos-sgtxt+13(4)
            and belnr eq ls_rfpos-sgtxt+0(10)
            and buzei eq ls_rfpos-sgtxt+10(3).

        select single name1 from kna1 into ls_rfpos-zzname1_ku
          where kunnr eq ls_rfpos-zzkunnr.

      endif.
      if ls_rfpos-koart eq 'K'.
        loop at lt_bseg assigning field-symbol(<ls_bseg_hkont>)
                        where bukrs eq ls_rfpos-bukrs and
                              belnr eq ls_rfpos-belnr and
                              gjahr eq ls_rfpos-gjahr .

          ls_rfpos-zzkarshsp = |{ ls_rfpos-zzkarshsp } { <ls_bseg_hkont>-hkont }|.
        endloop.
      endif.

      read table lt_t001 into data(ls_t001)
      with key bukrs =  ls_rfpos-bukrs.

      assign lt_skat[ key primary_key components spras = sy-langu ktopl = ls_t001-ktopl saknr = ls_rfpos-hkont ]-txt50 to field-symbol(<lv_txt50_gl>).
      if sy-subrc eq 0.
        ls_rfpos-zzname1_gl = <lv_txt50_gl>.
      endif.

     if ls_rfpos-bschl BETWEEN '70' and '75'.
* anln1 ve 2 alanları boş, onları okuyoruz.
      READ TABLE lt_bseg ASSIGNING FIELD-SYMBOL(<ls_bseg_read>)
        WITH TABLE KEY bukrs = ls_rfpos-bukrs
                       belnr = ls_rfpos-belnr
                       gjahr = ls_rfpos-gjahr .
      assign lt_anla[ key primary_key components bukrs = ls_rfpos-bukrs anln1 = <ls_bseg_read>-anln1 anln2 = <ls_bseg_read>-anln2 ]-txt50 to field-symbol(<lv_txt50_dv>).
      if sy-subrc eq 0.
        ls_rfpos-zzname1_dv = <lv_txt50_dv>.
      endif.
     ENDIF.


      move-corresponding ls_rfpos to <ls_data>.
      move-corresponding ls_temp to <ls_data>.


    endloop.




  endmethod.


  METHOD update_xblnr.

    DATA: lv_mblnr_initial TYPE mblnr,
          lv_vbeln_initial TYPE vbeln_vl,
          lv_rbeln_initial TYPE re_belnr.

    LOOP AT it_xblnr ASSIGNING FIELD-SYMBOL(<ls_xblnr>).

      CALL FUNCTION 'J_1B_NFE_UPDATE_XBLNR'
        EXPORTING
          iv_xblnr = <ls_xblnr>-xblnr
          iv_rbeln = lv_rbeln_initial
          iv_mblnr = lv_mblnr_initial
          iv_vbeln = lv_vbeln_initial
          iv_bukrs = <ls_xblnr>-bukrs
          iv_belnr = <ls_xblnr>-belnr
          iv_gjahr = <ls_xblnr>-gjahr.

      CHECK iv_commit_each_doc EQ abap_true.
      COMMIT WORK AND WAIT.

    ENDLOOP.

    CHECK iv_commit_each_doc EQ abap_false.
    COMMIT WORK AND WAIT.

  ENDMETHOD.
ENDCLASS.