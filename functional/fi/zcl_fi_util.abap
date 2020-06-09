class zficl_util definition
  public
  final
  create public .

  public section.
*"* public components of class ZFICL_UTIL
*"* do not include other source files here!!!

    constants c_vgabe_inv_entry type vgabe value '2'.       "#EC NOTEXT
    constants c_awtyp_rmrp type awtyp value 'RMRP'.         "#EC NOTEXT
    constants c_vgabe_goods_rcpt type vgabe value '1'.      "#EC NOTEXT

    class-methods ensure_skb1_existence
      importing
        !pfd_i_bukrs type skb1-bukrs
        !pfd_i_saknr type skb1-saknr
      raising
        zcx_bc_table_content.

    class-methods get_tax_rate
      importing
        !pfd_i_kschl type kscha
        !pfd_i_mwskz type mwskz
      returning
        value(pfd_r_kbetr) type kbetr_kond
      raising
        zcx_fi_mwskz .
    class-methods split_tax_amount
      importing
        !pfd_i_mwskz type mwskz
        !pfd_i_kschl type kscha
      exporting
        !pfd_e_fwste type fwste
      changing
        !pfd_c_wrbtr type wrbtr
      raising
        zcx_fi_mwskz .
    class-methods display_fi_doc
      importing
        !pfd_i_bukrs type bukrs
        !pfd_i_belnr type belnr_d
        !pfd_i_gjahr type gjahr .
    class-methods replace_mgfg_account
      importing
        !pfd_i_bukrs type bukrs
        !pfd_i_belnr type belnr_d
        !pfd_i_buzei type buzei
        !pfd_i_gjahr type gjahr
      changing
        !pfd_c_hkont type hkont .
    class-methods get_awkey_range_of_po
      importing
        !pfd_i_ebeln type ebeln
        !pfd_i_ebelp type ebelp
        !pfd_i_vgabe type vgabe
      returning
        value(pit_r_awkey) type trgr_awkey
      raising
        zcx_fi_genel .
    class-methods get_bseg_material
      importing
        !pfd_i_bukrs type bukrs
        !pfd_i_belnr type belnr_d
        !pfd_i_gjahr type gjahr
        !pfd_i_buzei type buzei
      exporting
        !pfd_e_matnr type matnr
        !pfd_e_maktx type maktx .
    class-methods validate_stcd2_numeric
      importing
        !pfd_i_stcd2 type stcd2
      raising
        zcx_fi_stcd2 .
    class-methods validate_waers
      importing
        !pfd_i_waers type waers
      raising
        zcx_fi_waers .
    type-pools abap .
    class-methods is_acc_dogih
      importing
        !pfd_i_bukrs type bukrs
        !pfd_i_hkont type hkont
      returning
        value(pfd_r_dogih) type abap_bool .
    class-methods is_acc_tax_relevant
      importing
        !pfd_i_bukrs type bukrs
        !pfd_i_saknr type saknr
      returning
        value(pfd_r_tax) type abap_bool .

  protected section.
*"* protected components of class ZFICL_UTIL
*"* do not include other source files here!!!
  private section.
*"* private components of class ZFICL_UTIL
*"* do not include other source files here!!!

    constants c_aland_tr type aland value 'TR'.             "#EC NOTEXT
    constants c_kappl_tx type kappl value 'TX'.             "#EC NOTEXT
    constants c_option_eq type char2 value 'EQ'.            "#EC NOTEXT
    constants c_sign_i type char1 value 'I'.                "#EC NOTEXT
    constants c_tabname_skb1 type tabname value 'SKB1'.

    types:
      begin of t_acc_tax,
        bukrs type skb1-bukrs,
        saknr type skb1-saknr,
        mwskz type skb1-mwskz,
      end of t_acc_tax,

      tt_acc_tax type hashed table of t_acc_tax
                 with unique key primary_key
                 components bukrs saknr,

      begin of t_skb1_subrc,
        bukrs type skb1-bukrs,
        saknr type skb1-saknr,
        subrc type sysubrc,
      end of t_skb1_subrc,

      tt_skb1_subrc type hashed table of t_skb1_subrc with unique key primary_key components bukrs saknr.

    class-data:
      git_skb1_subrc type tt_skb1_subrc.


endclass.



class zficl_util implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>DISPLAY_FI_DOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_BUKRS                    TYPE        BUKRS
* | [--->] PFD_I_BELNR                    TYPE        BELNR_D
* | [--->] PFD_I_GJAHR                    TYPE        GJAHR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method display_fi_doc.

    set parameter id:
      'BLN' field pfd_i_belnr,
      'BUK' field pfd_i_bukrs,
      'GJR' field pfd_i_gjahr.

    call transaction 'FB03' and skip first screen.

  endmethod.                    "display_fi_doc


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>ENSURE_SKB1_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_BUKRS                    TYPE        SKB1-BUKRS
* | [--->] PFD_I_SAKNR                    TYPE        SKB1-SAKNR
* | [!CX!] ZCX_BC_TABLE_CONTENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ensure_skb1_existence.

    data:
      lfd_objectid type cdobjectv,
      lwa_ss like line of git_skb1_subrc.

    field-symbols:
      <lwa_ss> like line of git_skb1_subrc.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Buffer'dan faydalanarak, şirket kodu için hesap kaydı olup olmadığına bak
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    read table git_skb1_subrc assigning <lwa_ss>
      with table key primary_key components
      bukrs = pfd_i_bukrs
      saknr = pfd_i_saknr.


    if sy-subrc ne 0.

      lwa_ss-bukrs = pfd_i_bukrs.
      lwa_ss-saknr = pfd_i_saknr.

      select single mandt into sy-mandt
        from skb1
        where bukrs eq lwa_ss-bukrs and
              saknr eq lwa_ss-saknr
        ##write_ok.

      lwa_ss-subrc = sy-subrc.

      insert lwa_ss into table git_skb1_subrc assigning <lwa_ss>.

    endif.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Kayıt yoksa hata üret
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    check <lwa_ss>-subrc is not initial.

    concatenate <lwa_ss>-bukrs <lwa_ss>-saknr into lfd_objectid separated by space.

    raise exception type zcx_bc_table_content
      exporting
        textid = zcx_bc_table_content=>kayit_yok
        tabname = c_tabname_skb1
        objectid = lfd_objectid.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>GET_AWKEY_RANGE_OF_PO
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_EBELN                    TYPE        EBELN
* | [--->] PFD_I_EBELP                    TYPE        EBELP
* | [--->] PFD_I_VGABE                    TYPE        VGABE
* | [<-()] PIT_R_AWKEY                    TYPE        TRGR_AWKEY
* | [!CX!] ZCX_FI_GENEL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_awkey_range_of_po.

    statics: lit_buffer type tt_awkey_range.

    data: lit_ekbe type tt_ekbe,
          lrd_ekbe type ref to t_ekbe,

          lwa_buffer type t_awkey_range,
          lrd_buffer type ref to t_awkey_range,

          lrd_awkey  type ref to trgs_awkey.


    refresh pit_r_awkey.

    read table lit_buffer reference into lrd_buffer
               with table key primary_key
               components ebeln = pfd_i_ebeln
                          ebelp = pfd_i_ebelp
                          vgabe = pfd_i_vgabe.

    if sy-subrc ne 0.

      lwa_buffer-ebeln = pfd_i_ebeln.
      lwa_buffer-ebelp = pfd_i_ebelp.
      lwa_buffer-vgabe = pfd_i_vgabe.

      select gjahr belnr into corresponding fields of table lit_ekbe
        from ekbe
        where ebeln eq lwa_buffer-ebeln
          and ebelp eq lwa_buffer-ebelp
          and vgabe eq lwa_buffer-vgabe.                    "#EC WARNOK

      loop at lit_ekbe reference into lrd_ekbe.
        append initial line to lwa_buffer-awkey reference into lrd_awkey.
        lrd_awkey->option = c_option_eq.
        lrd_awkey->sign = c_sign_i.
        concatenate lrd_ekbe->belnr lrd_ekbe->gjahr into lrd_awkey->low.
      endloop.

      sort lwa_buffer-awkey by low.
      delete adjacent duplicates from lwa_buffer-awkey comparing low.

      insert lwa_buffer into table lit_buffer reference into lrd_buffer.

    endif.

    pit_r_awkey[] = lrd_buffer->awkey[].

    if pit_r_awkey[] is initial.
      raise exception type zcx_fi_genel
        exporting
          textid = zcx_fi_genel=>no_data_found.
    endif.

  endmethod.                    "get_awkey_range_of_po


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>GET_BSEG_MATERIAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_BUKRS                    TYPE        BUKRS
* | [--->] PFD_I_BELNR                    TYPE        BELNR_D
* | [--->] PFD_I_GJAHR                    TYPE        GJAHR
* | [--->] PFD_I_BUZEI                    TYPE        BUZEI
* | [<---] PFD_E_MATNR                    TYPE        MATNR
* | [<---] PFD_E_MAKTX                    TYPE        MAKTX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_bseg_material.

    clear: pfd_e_matnr, pfd_e_maktx.

    select single matnr into pfd_e_matnr
      from bseg
      where bukrs eq pfd_i_bukrs
        and belnr eq pfd_i_belnr
        and gjahr eq pfd_i_gjahr
        and buzei eq pfd_i_buzei.

    check pfd_e_matnr is not initial.

    try.
        zbccl_ent_malzeme=>get_material_text( exporting pfd_i_matnr = pfd_e_matnr
                                                        pfd_i_langu = sy-langu
                                              changing  pfd_c_maktx = pfd_e_maktx ).
      catch cx_root ##no_handler .
    endtry.

  endmethod.                    "get_bseg_material


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>GET_TAX_RATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_KSCHL                    TYPE        KSCHA
* | [--->] PFD_I_MWSKZ                    TYPE        MWSKZ
* | [<-()] PFD_R_KBETR                    TYPE        KBETR_KOND
* | [!CX!] ZCX_FI_MWSKZ
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_tax_rate.

    statics:
      lit_buffer type tt_tax_rate.

    data:
      lwa_buffer type t_tax_rate,
      lrd_buffer type ref to t_tax_rate.

* Temizlik
    clear pfd_r_kbetr.

* Paranoya: Parametre kontrolü
    assert id zfi condition pfd_i_kschl is not initial.

* Vergi göstergesi boş olamaz
    if pfd_i_mwskz is initial.
      raise exception type zcx_fi_mwskz
        exporting
          textid = zcx_fi_mwskz=>vergi_gostergesi_yok
          mwskz  = pfd_i_mwskz.
    endif.

* Varsa Buffer'dan döndürelim
    read table lit_buffer reference into lrd_buffer
      with table key primary_key
      components kschl = pfd_i_kschl
                 mwskz = pfd_i_mwskz.

    if sy-subrc eq 0.
      pfd_r_kbetr = lrd_buffer->kbetr.
      return.
    endif.

* Değeri okuyalım ve yoksa hata verelim
    select single kbetr into pfd_r_kbetr
      from konp
      where knumh eq ( select knumh from a003
                       where kappl eq c_kappl_tx
                         and kschl eq pfd_i_kschl
                         and aland eq c_aland_tr
                         and mwskz eq pfd_i_mwskz ).        "#EC WARNOK

    if sy-subrc ne 0.
      raise exception type zcx_fi_mwskz
        exporting
          textid = zcx_fi_mwskz=>vergi_orani_bulunamadi
          mwskz  = pfd_i_mwskz.
    endif.

* Buffer'a ek
    lwa_buffer-kschl = pfd_i_kschl.
    lwa_buffer-mwskz = pfd_i_mwskz.
    lwa_buffer-kbetr = pfd_r_kbetr.

    insert lwa_buffer into table lit_buffer.

  endmethod.                    "get_tax_rate


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>IS_ACC_DOGIH
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_BUKRS                    TYPE        BUKRS
* | [--->] PFD_I_HKONT                    TYPE        HKONT
* | [<-()] PFD_R_DOGIH                    TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method is_acc_dogih.

    select single mandt into sy-mandt ##write_ok
           from zfit_dogih
           where bukrs eq pfd_i_bukrs
             and hkont eq pfd_i_hkont.

    check sy-subrc eq 0.

    pfd_r_dogih = abap_true.

  endmethod.                    "is_acc_dogih


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>IS_ACC_TAX_RELEVANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_BUKRS                    TYPE        BUKRS
* | [--->] PFD_I_SAKNR                    TYPE        SAKNR
* | [<-()] PFD_R_TAX                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method is_acc_tax_relevant.

    statics lit_cache type tt_acc_tax.

    data: lwa_cache type t_acc_tax,
          lfd_saknr type saknr.

    field-symbols <lwa_cache> type t_acc_tax.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = pfd_i_saknr
      importing
        output = lfd_saknr.

    read table lit_cache assigning <lwa_cache>
                         with table key primary_key
                         components bukrs = pfd_i_bukrs
                                    saknr = lfd_saknr.

    if sy-subrc ne 0.
      lwa_cache-bukrs = pfd_i_bukrs.
      lwa_cache-saknr = lfd_saknr.

      select single mwskz into lwa_cache-mwskz from skb1 where bukrs eq lwa_cache-bukrs
                                                           and saknr eq lwa_cache-saknr.

      insert lwa_cache into table lit_cache assigning <lwa_cache>.

    endif.

    check <lwa_cache> is assigned and <lwa_cache>-mwskz is not initial.
    pfd_r_tax = abap_true.

  endmethod.                    "IS_ACC_TAX_RELEVANT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>REPLACE_MGFG_ACCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_BUKRS                    TYPE        BUKRS
* | [--->] PFD_I_BELNR                    TYPE        BELNR_D
* | [--->] PFD_I_BUZEI                    TYPE        BUZEI
* | [--->] PFD_I_GJAHR                    TYPE        GJAHR
* | [<-->] PFD_C_HKONT                    TYPE        HKONT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method replace_mgfg_account.

    data: begin of lwa_bkpf,
            bukrs type bkpf-bukrs,
            belnr type bkpf-belnr,
            gjahr type bkpf-gjahr,
          end of lwa_bkpf,

          begin of lwa_po,
            ebeln type ekpo-ebeln,
            ebelp type ekpo-ebelp,
          end of lwa_po.

    data : lit_hesap type table of zfit_hesap,
           lrd_hesap type ref to zfit_hesap.

    data : lit_hsp type range of hkont,
           lwa_hsp like line of lit_hsp.

    select hkont from zfit_hesap
      into corresponding fields of table lit_hesap ##too_many_itab_fields .

    loop at lit_hesap reference into lrd_hesap.
      lwa_hsp-option = 'EQ'.
      lwa_hsp-sign = 'I'.
      lwa_hsp-low = lrd_hesap->hkont.
      append lwa_hsp to lit_hsp.
    endloop.

* Bu işlem sadece MG / FG hesabında yapılır
* CHECK pfd_c_hkont EQ c_hkont_mgfg.

* Yeni durum, sadece MG / FG hesap değil, zfit_hesap bakım tablosundaki
* tüm hesaplar için yapılacak
    check pfd_c_hkont in lit_hsp.


* Rapordaki mali yıl , belge no, kalem ve şirket kodu alınarak BSEG’e gidilir.
* BSEG-EBELN ve EBELP alanlarından SAS no ve kalem bilgisi alınır

    select single ebeln ebelp
           into corresponding fields of lwa_po
           from bseg
           where bukrs eq pfd_i_bukrs
             and belnr eq pfd_i_belnr
             and gjahr eq pfd_i_gjahr
             and buzei eq pfd_i_buzei.

* EKBE ‘de bu SAS ve kaleme ait EKBE-VGABE = “1” olan EKBE-MBLNR alınır.
* BKPF-AWKEY = EKBE-MBLNR olan belgenin mali kalem,belge,şirket kodu alınıp BSEG’e gidilir.
* Elimizde belge,şirket kodu,mali yıl, SAS ve kalem bilgileri ile BSEG-HKONT bulunur ve
* rapordaki ana hesap alanına yazılır.

    zimcl_ksb1_helper=>get_fi_doc_of_po( exporting pfd_i_ebeln = lwa_po-ebeln
                                                   pfd_i_ebelp = lwa_po-ebelp
                                                   pfd_i_vgabe = c_vgabe_goods_rcpt
                                         importing pfd_e_musik = lwa_bkpf-bukrs
                                                   pfd_e_mubel = lwa_bkpf-belnr
                                                   pfd_e_muyil = lwa_bkpf-gjahr ).

    check lwa_bkpf-belnr is not initial.

    select single hkont into pfd_c_hkont
      from bseg
      where bukrs eq lwa_bkpf-bukrs
        and belnr eq lwa_bkpf-belnr
        and gjahr eq lwa_bkpf-gjahr
        and hkont ne pfd_c_hkont.                           "#EC WARNOK

  endmethod.                    "replace_mgfg_account


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>SPLIT_TAX_AMOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_MWSKZ                    TYPE        MWSKZ
* | [--->] PFD_I_KSCHL                    TYPE        KSCHA
* | [<---] PFD_E_FWSTE                    TYPE        FWSTE
* | [<-->] PFD_C_WRBTR                    TYPE        WRBTR
* | [!CX!] ZCX_FI_MWSKZ
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method split_tax_amount.

    data:
      lfd_wrbtr type wrbtr.

* Temizlik
    clear pfd_e_fwste.

* Vergi göstergesi yoksa, yapacak bir şey yok
    check pfd_i_mwskz is not initial.

* Vergi oranını alalım
* Vergi tutarı = Tutar x vergi oranı / 1000
* Yeni tutar = eski tutar - vergi tutarı
    lfd_wrbtr = pfd_c_wrbtr / ( 1 + ( get_tax_rate( pfd_i_kschl = pfd_i_kschl
                                                    pfd_i_mwskz = pfd_i_mwskz ) / 1000 ) ).

    pfd_e_fwste = pfd_c_wrbtr - lfd_wrbtr.
    pfd_c_wrbtr = lfd_wrbtr.

  endmethod.                    "split_tax_amount


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>VALIDATE_STCD2_NUMERIC
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_STCD2                    TYPE        STCD2
* | [!CX!] ZCX_FI_STCD2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method validate_stcd2_numeric.

    data: lfd_stcd2_val(11) type n ##needed,
          lrc_cx_root       type ref to cx_root.

    try.
        move exact pfd_i_stcd2 to lfd_stcd2_val.

      catch cx_root into lrc_cx_root.

        raise exception type zcx_fi_stcd2
          exporting
            textid   = zcx_fi_stcd2=>numerik_degil
            previous = lrc_cx_root
            stcd2    = pfd_i_stcd2.

    endtry.

  endmethod.                    "validate_stcd2_numeric


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZFICL_UTIL=>VALIDATE_WAERS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_WAERS                    TYPE        WAERS
* | [!CX!] ZCX_FI_WAERS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method validate_waers.

    select single mandt into sy-mandt ##write_ok
           from tcurc
           where waers eq pfd_i_waers.

    check sy-subrc ne 0.

    raise exception type zcx_fi_waers
      exporting
        textid = zcx_fi_waers=>tanimsiz
        waers  = pfd_i_waers.

  endmethod.                    "validate_waers
endclass.
