class zmmcl_open_po definition
  public
  final
  create public .

public section.
*"* public components of class ZMMCL_OPEN_PO
*"* do not include other source files here!!!

  class-methods get_open_quan
    importing
      !pfd_i_ebeln type ebeln
      !pfd_i_ebelp type ebelp
    exporting
      !pfd_e_oporq type zmmd_oporq
      !pfd_e_oinvq type zmmd_oinvq
      !pfd_e_fgrda type zmmd_fgrda
      !pfd_e_deliq type zmmd_deliq
      !pfd_e_mgysm type zmmd_mgysm
    changing
      !prc_c_log type ref to zbccl_log optional .
protected section.
*"* protected components of class ZMMCL_OPEN_PO
*"* do not include other source files here!!!
private section.
*"* private components of class ZMMCL_OPEN_PO
*"* do not include other source files here!!!

  types:
    begin of t_ekpo_open,
      ebelp type ekpo-ebelp,
      ktmng type ekpo-ktmng,
      menge type ekpo-menge,
      meins type ekpo-meins,
      wepos type ekpo-wepos,
      elikz type ekpo-elikz,
      repos type ekpo-repos,
      erekz type ekpo-erekz,
    end of t_ekpo_open .
  types:
    tt_ekpo_open type hashed table of t_ekpo_open
                 with unique key primary_key components ebelp .
  types:
    begin of t_mb,
      mblnr type mkpf-mblnr,
      mjahr type mkpf-mjahr,
      budat type mkpf-budat,
      zeile type mseg-zeile,
      ebeln type mseg-ebeln,
      ebelp type mseg-ebelp,
    end of t_mb .
  types:
    tt_mb type standard table of t_mb with default key .
  types:
    begin of t_open_quan,
      ebeln     type zmms_ssmg_alv-ebeln,
      items     type bapiekpo_tp,
      histo_det type bapiekbe_tp,
      histo     type bapiekbes_tp,
      mb        type tt_mb,
      ekpo      type tt_ekpo_open,
    end of t_open_quan .
  types:
    tt_open_quan type hashed table of t_open_quan
                 with unique key primary_key
                 components ebeln .

  constants c_bwart_101 type bwart value '101'. "#EC NOTEXT
  constants c_bwart_105 type bwart value '105'. "#EC NOTEXT
  constants c_msgid_zmm type symsgid value 'ZMM'. "#EC NOTEXT

  type-pools abap .
  class-methods does_history_entry_exist
    importing
      !pfd_i_ebelp type ebelp
      !pit_i_histo type bapiekbes_tp
    returning
      value(pfd_r_exist) type abap_bool .
  class-methods get_invoiced_zimp_quan
    importing
      !pfd_i_ebeln type ebeln
      !pfd_i_ebelp type ebelp
      !pfd_i_meins type meins
    returning
      value(pfd_r_menge) type bstmg
    raising
      cx_reca_symsg .
  class-methods get_open_quan__42
    importing
      !prd_i_buffer type ref to t_open_quan
      !prd_i_ekpo type ref to t_ekpo_open
      !prd_i_item type ref to bapiekpo
      !pfd_i_ebeln type ebeln
    changing
      !pfd_c_oporq type zmmd_oporq
      !pfd_c_oinvq type zmmd_oinvq
      !pfd_c_mgysm type zmmd_mgysm
      !prc_c_log type ref to zbccl_log optional .
  class-methods get_open_quan__43
    importing
      !prd_i_item type ref to bapiekpo
      !prd_i_buffer type ref to t_open_quan
      !prd_i_ekpo type ref to t_ekpo_open
    changing
      !pfd_c_oporq type zmmd_oporq
      !pfd_c_mgysm type zmmd_mgysm
      !pfd_c_oinvq type zmmd_oinvq .
  class-methods get_open_quan__def
    importing
      !prd_i_buffer type ref to t_open_quan
      !prd_i_ekpo type ref to t_ekpo_open
      !prd_i_item type ref to bapiekpo
    changing
      !pfd_c_mgysm type zmmd_mgysm
      !pfd_c_oporq type zmmd_oporq
      !pfd_c_oinvq type zmmd_oinvq .
  class-methods get_oporq
    importing
      !prd_i_buffer type ref to t_open_quan
      !pfd_i_ebelp type ebelp
    returning
      value(pfd_r_oporq) type zmmd_oporq .
  class-methods has_pending_goods_receipt
    importing
      !pfd_i_elikz type elikz
      !pfd_i_wepos type wepos
    returning
      value(pfd_r_pending) type abap_bool .
  class-methods has_pending_invoice
    importing
      !pfd_i_repos type repos
      !pfd_i_erekz type erekz
    returning
      value(pfd_r_pending) type abap_bool .
endclass.



class zmmcl_open_po implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>DOES_HISTORY_ENTRY_EXIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_EBELP                    TYPE        EBELP
* | [--->] PIT_I_HISTO                    TYPE        BAPIEKBES_TP
* | [<-()] PFD_R_EXIST                    TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method does_history_entry_exist.

  read table pit_i_histo transporting no fields with key po_item = pfd_i_ebelp.

  if sy-subrc eq 0.
    pfd_r_exist = abap_true.
  else.
    pfd_r_exist = abap_false.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>GET_INVOICED_ZIMP_QUAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_EBELN                    TYPE        EBELN
* | [--->] PFD_I_EBELP                    TYPE        EBELP
* | [--->] PFD_I_MEINS                    TYPE        MEINS
* | [<-()] PFD_R_MENGE                    TYPE        BSTMG
* | [!CX!] CX_RECA_SYMSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_invoiced_zimp_quan.

  statics:
    lit_buffer type tt_zimpmen.

  data:
    lit_menge type tt_menge,
    lrd_menge type ref to t_menge,

    lwa_buffer type t_zimpmen,
    lrd_buffer type ref to t_zimpmen,

    lfd_matnr  type matnr.

* Temizlik

  clear pfd_r_menge.

* Buffer'da var mı? Yoksa okuyalım (performans)

  read table lit_buffer reference into lrd_buffer
       with table key primary_key
       components ebeln = pfd_i_ebeln
                  ebelp = pfd_i_ebelp
                  meins = pfd_i_meins.

  if sy-subrc ne 0.

    lwa_buffer-ebeln = pfd_i_ebeln.
    lwa_buffer-ebelp = pfd_i_ebelp.
    lwa_buffer-meins = pfd_i_meins.

    select single matnr into lfd_matnr from ekpo where ebeln eq pfd_i_ebeln
                                                   and ebelp eq pfd_i_ebelp.

    select
        sum( menge ) as menge
        meins
      into corresponding fields of table lit_menge
      from zimppo
      where lebeln eq pfd_i_ebeln
        and lbstp  eq pfd_i_ebelp
      group by meins.

    try.

        loop at lit_menge reference into lrd_menge.

          lwa_buffer-menge = lwa_buffer-menge + zbccl_ent_malzeme=>convert_unit( pfd_i_matnr  = lfd_matnr
                                                                                 pfd_i_source = lrd_menge->meins
                                                                                 pfd_i_target = lwa_buffer-meins
                                                                                 pfd_i_quan   = lrd_menge->menge ).


        endloop.

      catch cx_reca_symsg into lwa_buffer-cx_symsg.
        clear lwa_buffer-menge.

      catch cx_root.
        assert id zmm condition 1 eq 0.

    endtry.

    insert lwa_buffer into table lit_buffer reference into lrd_buffer.

  endif.

* Exception veya değer döndürme

  if lrd_buffer->cx_symsg is not initial.
    raise exception lrd_buffer->cx_symsg.
  endif.

  pfd_r_menge = lrd_buffer->menge.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_OPEN_PO=>GET_OPEN_QUAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_EBELN                    TYPE        EBELN
* | [--->] PFD_I_EBELP                    TYPE        EBELP
* | [<---] PFD_E_OPORQ                    TYPE        ZMMD_OPORQ
* | [<---] PFD_E_OINVQ                    TYPE        ZMMD_OINVQ
* | [<---] PFD_E_FGRDA                    TYPE        ZMMD_FGRDA
* | [<---] PFD_E_DELIQ                    TYPE        ZMMD_DELIQ
* | [<---] PFD_E_MGYSM                    TYPE        ZMMD_MGYSM
* | [<-->] PRC_C_LOG                      TYPE REF TO ZBCCL_LOG(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_open_quan.

  statics:
    lit_buffer  type tt_open_quan.

  data:
    lit_return  type isi_bapireturn_tt,

    lwa_buffer  type t_open_quan,
    lrd_buffer  type ref to t_open_quan,

    lfd_bsart   type ekko-bsart,

    lrd_ekpo    type ref to t_ekpo_open,
    lrd_histo   type ref to bapiekbes,
    lrd_item    type ref to bapiekpo,
    lrd_mb      type ref to t_mb.

* ______________________________________________________________________
* Hazırlık
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

* Temizlik

  clear: pfd_e_oporq, pfd_e_oinvq, pfd_e_fgrda, pfd_e_deliq, pfd_e_mgysm.

* ______________________________________________________________________
* Sistemden okunacak değerler
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

* BAPI_PO_GETDETAIL çağırılacak ve PO_ITEM_HISTORY_TOTALS alınacak
* Performans için Buffer kullanıyoruz

  read table lit_buffer reference into lrd_buffer
             with table key primary_key
             components ebeln = pfd_i_ebeln.

  if sy-subrc ne 0.

    lwa_buffer-ebeln = pfd_i_ebeln.

    call function 'BAPI_PO_GETDETAIL'
      exporting
        purchaseorder          = lwa_buffer-ebeln
        history                = abap_true
      tables
        po_items               = lwa_buffer-items
        po_item_history        = lwa_buffer-histo_det
        po_item_history_totals = lwa_buffer-histo
        return                 = lit_return.

    if lit_return[] is not initial and prc_c_log is not initial.
      prc_c_log->add_bapireturn( lit_return ).

      loop at lit_return transporting no fields where type in zbccl_log=>get_crit_msgty_range( ).
        prc_c_log->add_t100_msg( pfd_i_msgid = 'ZMM'
                                 pfd_i_msgno = '376'
                                 pfd_i_msgty = zbccl_log=>c_msgty_w
                                 pfd_i_msgv1 = lwa_buffer-ebeln ).

        exit.
      endloop.

    endif.

    if lwa_buffer-histo_det[] is not initial.

      select
          mkpf~mblnr mkpf~mjahr mkpf~budat
          ms1~zeile ms1~ebeln ms1~ebelp
        into corresponding fields of table lwa_buffer-mb
        from
          mseg as ms1
          inner join mkpf on mkpf~mblnr eq ms1~mblnr
                         and mkpf~mjahr eq ms1~mjahr
        for all entries in lwa_buffer-histo_det
        where
          ( ms1~mjahr eq lwa_buffer-histo_det-doc_year ) and
          ( ms1~mblnr eq lwa_buffer-histo_det-mat_doc ) and
          ( ms1~zeile eq lwa_buffer-histo_det-matdoc_itm ) and
          ( ms1~bwart eq c_bwart_101 or
            ms1~bwart eq c_bwart_105 ) and
          ( ms1~smbln eq '' or ms1~smbln eq '0000000000' or ms1~smbln is null ) and
          ( not exists ( select mandt from mseg as ms2
                         where mjahr eq ms1~mjahr
                           and smbln eq ms1~mblnr
                           and smblp eq ms1~zeile ) ).

      sort lwa_buffer-mb by ebeln ebelp budat.

    endif.

    select ebelp ktmng menge meins wepos elikz repos erekz
      into corresponding fields of table lwa_buffer-ekpo
      from ekpo
      where ebeln eq lwa_buffer-ebeln.

    insert lwa_buffer into table lit_buffer reference into lrd_buffer.

  endif.

* ______________________________________________________________________
* Açık miktar hesapları
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

  if pfd_e_oporq is requested or
     pfd_e_oinvq is requested or
     pfd_e_mgysm is requested.

*   Hazırlık
    select single bsart into lfd_bsart
      from zmmv_ekko
      where ebeln eq pfd_i_ebeln.

    read table lrd_buffer->items reference into lrd_item
         with key po_number = pfd_i_ebeln
                  po_item   = pfd_i_ebelp.

    assert id zmm condition sy-subrc eq 0.

    read table lrd_buffer->ekpo reference into lrd_ekpo
         with table key primary_key
         components ebelp = pfd_i_ebelp.

    assert id zmm condition sy-subrc eq 0.

*   Belge türüne göre hesap
    case lfd_bsart.

*     Transit
      when zmmcl_purch_util=>c_bsart_transit.

        get_open_quan__43( exporting prd_i_item   = lrd_item
                                     prd_i_buffer = lrd_buffer
                                     prd_i_ekpo   = lrd_ekpo
                           changing  pfd_c_oporq  = pfd_e_oporq
                                     pfd_c_mgysm  = pfd_e_mgysm
                                     pfd_c_oinvq  = pfd_e_oinvq ).

*     Yurtdışı
      when zmmcl_purch_util=>c_bsart_yurtdisi.

        get_open_quan__42( exporting prd_i_buffer = lrd_buffer
                                     prd_i_ekpo   = lrd_ekpo
                                     prd_i_item   = lrd_item
                                     pfd_i_ebeln  = pfd_i_ebeln
                           changing  pfd_c_oporq  = pfd_e_oporq
                                     pfd_c_oinvq  = pfd_e_oinvq
                                     pfd_c_mgysm  = pfd_e_mgysm
                                     prc_c_log    = prc_c_log ).

*     Diğer belge türleri
      when others.

        get_open_quan__def( exporting prd_i_buffer = lrd_buffer
                                      prd_i_ekpo   = lrd_ekpo
                                      prd_i_item   = lrd_item
                            changing  pfd_c_mgysm  = pfd_e_mgysm
                                      pfd_c_oporq  = pfd_e_oporq
                                      pfd_c_oinvq  = pfd_e_oinvq ).

    endcase.

  endif.

* ______________________________________________________________________
* İlk mal giriş tarihi
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

  if pfd_e_fgrda is requested.

    read table lrd_buffer->mb reference into lrd_mb
         with key ebeln = pfd_i_ebeln
                  ebelp = pfd_i_ebelp.

    if sy-subrc eq 0.
      pfd_e_fgrda = lrd_mb->budat.
    endif.

  endif.

* ______________________________________________________________________
* Teslimat miktarı
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

  if pfd_e_deliq is requested.

    loop at lrd_buffer->histo reference into lrd_histo
            where po_item eq pfd_i_ebelp.

      add lrd_histo->deliv_qty to pfd_e_deliq.
      exit.

    endloop.

  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>GET_OPEN_QUAN__42
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRD_I_BUFFER                   TYPE REF TO T_OPEN_QUAN
* | [--->] PRD_I_EKPO                     TYPE REF TO T_EKPO_OPEN
* | [--->] PRD_I_ITEM                     TYPE REF TO BAPIEKPO
* | [--->] PFD_I_EBELN                    TYPE        EBELN
* | [<-->] PFD_C_OPORQ                    TYPE        ZMMD_OPORQ
* | [<-->] PFD_C_OINVQ                    TYPE        ZMMD_OINVQ
* | [<-->] PFD_C_MGYSM                    TYPE        ZMMD_MGYSM
* | [<-->] PRC_C_LOG                      TYPE REF TO ZBCCL_LOG(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_open_quan__42.

  data:
    lrc_cx_root type ref to cx_root,
    lrd_histo   type ref to bapiekbes.

* açık fatura miktarı: son fatura tıkı varsa sıfır
* diğer durumlarda: sum(DELIV_QTY) - ZIMPPO miktarları

  check prd_i_item is not initial.

  if prd_i_item->final_inv eq abap_true.
    pfd_c_oinvq = 0.

  else.

    if does_history_entry_exist( pfd_i_ebelp = prd_i_ekpo->ebelp
                                 pit_i_histo = prd_i_buffer->histo ) eq abap_false.
      pfd_c_oinvq = prd_i_ekpo->menge.
    else.
      loop at prd_i_buffer->histo reference into lrd_histo where po_item eq prd_i_ekpo->ebelp.
        pfd_c_oinvq = lrd_histo->deliv_qty.
        exit.
      endloop.
    endif.

    try.
        pfd_c_oinvq = pfd_c_oinvq - get_invoiced_zimp_quan( pfd_i_ebeln = pfd_i_ebeln
                                                            pfd_i_ebelp = prd_i_ekpo->ebelp
                                                            pfd_i_meins = prd_i_item->unit ).
      catch cx_root into lrc_cx_root.

        if prc_c_log is not initial.

          prc_c_log->add_exception( lrc_cx_root ).

          prc_c_log->add_t100_msg( pfd_i_msgid = c_msgid_zmm
                                   pfd_i_msgno = '282'
                                   pfd_i_msgty = zbccl_log=>c_msgty_e
                                   pfd_i_msgv1 = pfd_i_ebeln
                                   pfd_i_msgv2 = prd_i_ekpo->ebelp ).

        endif.

    endtry.

  endif.

* Açık SAS miktarı: açık mal girişi varsa: SAS miktarı - 105'ler; yoksa: 0
  if has_pending_goods_receipt( pfd_i_wepos = prd_i_ekpo->wepos
                                pfd_i_elikz = prd_i_ekpo->elikz ) eq abap_true.

    pfd_c_oporq = prd_i_ekpo->menge.

    loop at prd_i_buffer->histo reference into lrd_histo where po_item eq prd_i_ekpo->ebelp.
      subtract lrd_histo->deliv_qty from pfd_c_oporq.
      exit.
    endloop.

  else.
    pfd_c_oporq = 0.
  endif.

* Gümrükten çekilecek miktar: Blocked quan
  loop at prd_i_buffer->histo reference into lrd_histo where po_item eq prd_i_ekpo->ebelp.
    pfd_c_mgysm = lrd_histo->blocked_qy.
    exit.
  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>GET_OPEN_QUAN__43
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRD_I_ITEM                     TYPE REF TO BAPIEKPO
* | [--->] PRD_I_BUFFER                   TYPE REF TO T_OPEN_QUAN
* | [--->] PRD_I_EKPO                     TYPE REF TO T_EKPO_OPEN
* | [<-->] PFD_C_OPORQ                    TYPE        ZMMD_OPORQ
* | [<-->] PFD_C_MGYSM                    TYPE        ZMMD_MGYSM
* | [<-->] PFD_C_OINVQ                    TYPE        ZMMD_OINVQ
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_open_quan__43.

  data lrd_histo type ref to bapiekbes.

* açık sas miktarı: 0
  pfd_c_oporq = 0.

* Gümrükten çekilecek miktar: 0
  pfd_c_mgysm = 0.

* açık fatura miktarı: son fatura tıkı varsa sıfır
* diğer durumlarda: açık fatura varsa: EKPO-MENGE - fatura girişleri, yoksa: 0

  check prd_i_item is not initial.

  if prd_i_item->final_inv eq abap_true.
    pfd_c_oinvq = 0.
  else.

    if has_pending_invoice( pfd_i_repos = prd_i_ekpo->repos
                            pfd_i_erekz = prd_i_ekpo->erekz ) eq abap_true.

      pfd_c_oinvq = prd_i_ekpo->menge.

      loop at prd_i_buffer->histo reference into lrd_histo where po_item eq prd_i_ekpo->ebelp.
        subtract lrd_histo->iv_qty from pfd_c_oinvq.
      endloop.

    else.
      pfd_c_oinvq = 0.
    endif.

  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>GET_OPEN_QUAN__DEF
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRD_I_BUFFER                   TYPE REF TO T_OPEN_QUAN
* | [--->] PRD_I_EKPO                     TYPE REF TO T_EKPO_OPEN
* | [--->] PRD_I_ITEM                     TYPE REF TO BAPIEKPO
* | [<-->] PFD_C_MGYSM                    TYPE        ZMMD_MGYSM
* | [<-->] PFD_C_OPORQ                    TYPE        ZMMD_OPORQ
* | [<-->] PFD_C_OINVQ                    TYPE        ZMMD_OINVQ
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_open_quan__def.

  data lrd_histo   type ref to bapiekbes.

  check prd_i_item is not initial.

* Açık SAS miktarı: ME2N'deki gibi
  pfd_c_oporq = get_oporq( prd_i_buffer = prd_i_buffer
                           pfd_i_ebelp  = prd_i_ekpo->ebelp ).

* Gümrükten çekilecek miktar: sıfır
  pfd_c_mgysm = 0.

* açık fatura miktarı: son fatura tıkı varsa sıfır
* diğer durumlarda: açık fatura varsa: sum(DELIV_QTY) - sum(IV_QTY); yoksa: 0

  if prd_i_item->final_inv eq abap_true.
    pfd_c_oinvq = 0.
  else.

    if has_pending_invoice( pfd_i_repos = prd_i_ekpo->repos
                            pfd_i_erekz = prd_i_ekpo->erekz ) eq abap_true.

      if does_history_entry_exist( pfd_i_ebelp = prd_i_ekpo->ebelp
                                   pit_i_histo = prd_i_buffer->histo ) eq abap_false.

        pfd_c_oinvq = prd_i_ekpo->menge.
      else.

        loop at prd_i_buffer->histo reference into lrd_histo where po_item eq prd_i_ekpo->ebelp.
          add lrd_histo->deliv_qty to pfd_c_oinvq.
          subtract lrd_histo->iv_qty from pfd_c_oinvq.
          exit.
        endloop.

      endif.

    else.
      pfd_c_oinvq = 0.
    endif.

  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>GET_OPORQ
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRD_I_BUFFER                   TYPE REF TO T_OPEN_QUAN
* | [--->] PFD_I_EBELP                    TYPE        EBELP
* | [<-()] PFD_R_OPORQ                    TYPE        ZMMD_OPORQ
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_oporq.

  data:
    lrd_ekpo      type ref to bapiekpo,
    lrd_ekpo_open type ref to t_ekpo_open,
    lrd_histo     type ref to bapiekbes.

* //////////////////////////////////////////////////////////////////////
* Aşağıdaki mantık, ME2N arkasında çalışan LMEREPI02 içindeki
* OPEN_VALUE_DLV Method'undan alınmıştır.
* //////////////////////////////////////////////////////////////////////

* Temizlik

  clear pfd_r_oporq.

* İlgili SAS satırını okuyalım.

  read table prd_i_buffer->items reference into lrd_ekpo with key po_item = pfd_i_ebelp.
  assert id zmm condition sy-subrc eq 0.
  check lrd_ekpo is not initial.

* Eğer kalem tipi (PSTYP) 9 ise (Hizmet), açık SAS miktarı sıfırdır.
* Kalem silinmişse de açık SAS miktarı sıfırdır.
* Değeri atayıp Method'dan çıkabiliriz

  check ( lrd_ekpo->item_cat ne zmmcl_purch_util=>c_pstyp_hizmet )
    and ( lrd_ekpo->delete_ind eq abap_false )
    and ( not ( lrd_ekpo->del_compl ne space or lrd_ekpo->gr_ind eq space ) ).

* Standart hesaplama:
* SAS miktarını açık SAS miktarı olarak alalım.
* Akabinde, her bir EKBES satırı için üzerine ek yapacağız.

  read table prd_i_buffer->ekpo reference into lrd_ekpo_open with key ebelp = pfd_i_ebelp.
  assert id zmm condition sy-subrc eq 0.

  if has_pending_goods_receipt( pfd_i_wepos = lrd_ekpo_open->wepos
                                pfd_i_elikz = lrd_ekpo_open->elikz ) eq abap_true.

    pfd_r_oporq = lrd_ekpo_open->menge.

    loop at prd_i_buffer->histo reference into lrd_histo where po_item eq pfd_i_ebelp.
      pfd_r_oporq = pfd_r_oporq - abs( lrd_histo->deliv_qty + lrd_histo->blocked_qy ).
      exit.
    endloop.

    if pfd_r_oporq lt 0.
      pfd_r_oporq = 0.
    endif.

  else.
    pfd_r_oporq = 0.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>HAS_PENDING_GOODS_RECEIPT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_ELIKZ                    TYPE        ELIKZ
* | [--->] PFD_I_WEPOS                    TYPE        WEPOS
* | [<-()] PFD_R_PENDING                  TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method has_pending_goods_receipt.

  if pfd_i_wepos eq abap_true and pfd_i_elikz eq abap_false.
    pfd_r_pending = abap_true.
  else.
    pfd_r_pending = abap_false.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZMMCL_OPEN_PO=>HAS_PENDING_INVOICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_REPOS                    TYPE        REPOS
* | [--->] PFD_I_EREKZ                    TYPE        EREKZ
* | [<-()] PFD_R_PENDING                  TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method has_pending_invoice.

  if pfd_i_repos eq abap_true and pfd_i_erekz eq abap_false.
    pfd_r_pending = abap_true.
  else.
    pfd_r_pending = abap_false.
  endif.

endmethod.
endclass.