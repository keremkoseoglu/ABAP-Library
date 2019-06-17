class zcl_mm_toolkit definition
  public
  final
  create public .

public section.

  types:
    tt_hiy type standard table of zmms_hiy .
  types t_werks_lgort type zsdt_werks_lgort .
  types:
    tt_werks_lgort type standard table of t_werks_lgort .
  types:
    begin of t_prdha_kostl,
        werks type werks_d,
        prdha type prodh_d,
        kostl type kostl,
      end of t_prdha_kostl .
  types:
    tt_prdha_kostl type hashed table of t_prdha_kostl with unique key primary_key components werks prdha .
  types:
    begin of ty_txt_material,
        matnr type matnr,
        maktx type maktx,
      end of ty_txt_material .
  types:
    begin of t_karak_malzeme,
        matnr              type matnr,
        zbslkkunag         type kunag,
        zklmurtmtur        type zsdd_zklmurtmtur,
        zklmkat            type zsdd_kat,
        zklmgrm2           type atwrt,
        zklmgrm2_d         type zsdd_grm2,
        zklmrenk           type zrenk,
        zklmhrmn           type zsdd_hrmn,
        zklmhcmyum         type zsdd_hcmyum,
        zklmozlkrk         type zsdd_ozlkrk,
      end of t_karak_malzeme .
  types:
    begin of t_karak_parti,
        matnr              type matnr,
        charg              type charg_d,
        gr_m2              type zsdd_gr_m2_char,
        kat_adedi          type zsdd_kat_adedi_char,
        mihver_boru_capi   type zsdd_mihver_boru_capi_char,
        bobin_capi         type zsdd_bobin_capi_char,
        bobin_genisligi    type zsdd_bobin_genisligi_char,
        grup_no            type zsdd_grup_no_char,
        gr_m2_d            type zsdd_gr_m2,
        kat_adedi_d        type zsdd_kat_adedi,
        mihver_boru_capi_d type  zsdd_mihver_boru_capi,
        bobin_capi_d       type zsdd_bobin_capi,
        bobin_genisligi_d  type zsdd_bobin_genisligi,
        zkagit_makine      type zsdd_arbpl,
        lobm_hsdat         type hsdat,
      end of t_karak_parti .
  types:
    tt_karak_parti type hashed table of t_karak_parti with unique key primary_key components matnr charg .
  types:
    tt_karak_malzeme type hashed table of t_karak_malzeme with unique key primary_key components matnr .

  constants c_atnam_zgr_m2 type atnam value 'ZGR_M2' ##NO_TEXT.
  constants c_atnam_zkagit_makine type atnam value 'ZKAGIT_MAKINE' ##NO_TEXT.
  constants c_atnam_zkat_adedi type atnam value 'ZKAT_ADEDI' ##NO_TEXT.
  constants c_atnam_zmihver_boru_capi type atnam value 'ZMIHVER_BORU_CAPI' ##NO_TEXT.
  constants c_atnam_zbobin_capi type atnam value 'ZBOBIN_CAPI' ##NO_TEXT.
  constants c_atnam_zbobin_genisligi type atnam value 'ZBOBIN_GENISLIGI' ##NO_TEXT.
  constants c_atnam_LOBM_HSDAT type atnam value 'LOBM_HSDAT' ##NO_TEXT.
  constants c_atnam_grup_no type atnam value 'ZGRUP' ##NO_TEXT.
  constants c_atnam_zbslkkunag type atnam value 'ZBSLKKUNAG' ##NO_TEXT.
  constants c_atnam_zklmurtmtur type atnam value 'ZKLMURTMTUR' ##NO_TEXT.
  constants c_atnam_zklmkat type atnam value 'ZKLMKAT' ##NO_TEXT.
  constants c_atnam_zklmgrm2 type atnam value 'ZKLMGRM2' ##NO_TEXT.
  constants c_atnam_zklmrenk type atnam value 'ZKLMRENK' ##NO_TEXT.
  constants c_atnam_zklmhrmn type atnam value 'ZKLMHRMN' ##NO_TEXT.
  constants c_atnam_zklmhcmyum type atnam value 'ZKLMHCMYUM' ##NO_TEXT.
  constants c_atnam_zklmozlkrk type atnam value 'ZKLMOZLKRK' ##NO_TEXT.
  class-data gs_return type bapiret2 .
  class-data gt_werks_lgort type tt_werks_lgort .

  class-methods check_material_existence
    importing
      !iv_matnr type matnr
    raising
      zcx_mm_material .
  class-methods goodsmvt_getdetail
    importing
      value(iv_docnumb) type mblnr
      value(iv_docyear) type mjahr
    exporting
      !es_header type zmms_gm_head
      !et_items type zmmtt_gm_item
      !et_return type bapiret2_t
    exceptions
      doc_not_found .
  class-methods get_charg_werks_lgort
    returning
      value(rv_charg) type zsdt_werks_lgort-charg .
  class-methods get_karak_malzeme
    importing
      !iv_matnr type matnr
    returning
      value(rs_ret) type t_karak_malzeme .
  class-methods get_karak_parti
    importing
      !iv_matnr type matnr
      !iv_charg type charg_d
    returning
      value(rs_ret) type t_karak_parti .
  class-methods get_mmbe_sas_stock
    importing
      !iv_matnr type matnr
      !iv_werks type werks_d
      !iv_lgort type lgort_d optional
    returning
      value(rv_menge) type menge_d .
  class-methods get_hiy
    changing
      !ct_data type any table .
  class-methods is_creating_material
    importing
      !iv_tcode type sytcode default sy-tcode
    returning
      value(rv_yes) type abap_bool .
  class-methods is_displaying_material
    importing
      !iv_tcode type sytcode default sy-tcode
    returning
      value(rv_yes) type abap_bool .
  class-methods is_modifying_material
    importing
      !iv_tcode type sytcode default sy-tcode
    returning
      value(rv_yes) type abap_bool .
  class-methods is_updating_material
    importing
      !iv_tcode type sytcode default sy-tcode
    returning
      value(rv_yes) type abap_bool .
  class-methods get_karak_text
    importing
      !iv_matnr type matnr
      !iv_charg type charg_d
    returning
      value(rt_lines) type tline_tab .
  class-methods save_karak_text
    importing
      !iv_matnr type matnr
      !iv_charg type charg_d
      !it_lines type tline_tab .
  class-methods get_material_class_data
    importing
      !iv_matnr type matnr
      !iv_classnum type bapi1003_key-classnum
      value(iv_classtype) type bapi1003_key-classtype default '001'
      value(iv_keydate) type sy-datum optional
      value(iv_unvaluated_chars) type flag default 'X'
    exporting
      !et_num type tt_bapi1003_alloc_values_num
      !et_char type tt_bapi1003_alloc_values_char
      !et_curr type tt_bapi1003_alloc_values_curr
      !et_return type bapiret2_t
    raising
      zcx_mm_material .
  protected section.
private section.

  constants C_LANGU type THEAD-TDSPRAS value 'T' ##NO_TEXT.
  constants C_TABLE_MARA type BAPI1003_KEY-OBJECTTABLE value 'MARA' ##NO_TEXT.
  constants C_TDID type THEAD-TDID value 'VERM' ##NO_TEXT.
  constants C_TDOBJECT type THEAD-TDOBJECT value 'CHARGE' ##NO_TEXT.
  class-data GT_KARAK_PARTI_MULTITON type TT_KARAK_PARTI .
  class-data GT_KARAK_MALZEME_MULTITON type TT_KARAK_MALZEME .
  constants C_TCODE_CREATE_MAT type SYTCODE value 'MM01' ##NO_TEXT.
  constants C_TCODE_DISPLAY_MAT type SYTCODE value 'MM03' ##NO_TEXT.
  constants C_TCODE_EDIT_MAT type SYTCODE value 'MM02' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_MM_TOOLKIT IMPLEMENTATION.


  method check_material_existence.

    select single mandt into sy-mandt ##write_ok
           from mara
           where matnr eq iv_matnr.

    check sy-subrc ne 0.

    raise exception type zcx_mm_material
      exporting
        matnr  = iv_matnr
        textid = zcx_mm_material=>undefined.

  endmethod.


  method get_charg_werks_lgort.
*   Elit 24 e özel bir geliştirme başka bir yerde kullanılmasın bulunan ilk kaydı alıyor.
    if gt_werks_lgort is initial.
      select * up to 1 rows from zsdt_werks_lgort into table gt_werks_lgort.
      if sy-subrc <> 0.
        append initial line to gt_werks_lgort.
      endif.
    endif.
    assign gt_werks_lgort[ 1 ] to field-symbol(<ls_werks_lgort>).
    if sy-subrc = 0.
      rv_charg = <ls_werks_lgort>-charg.
    endif.
  endmethod.


  method get_hiy.

    data : lt_hiy   type standard table of prodh_disp,
           lv_comp  type char50,
           ls_datax type zmms_hiy,
           lt_datax type sorted table of zmms_hiy
                    with unique key prdha.

    loop at ct_data assigning field-symbol(<ls_data>).

      assign component 'PRDHA' of structure <ls_data> to field-symbol(<lv_data_prdha>).
      check sy-subrc eq 0.
      check <lv_data_prdha> is not initial.

      read table lt_datax into ls_datax with table key prdha = <lv_data_prdha>.
      if sy-subrc eq 0.
        move-corresponding ls_datax to <ls_data>.
      else.
        clear : lt_hiy,ls_datax.
        call function 'RV_PRODUKTHIERARCHIE_PRED_GET'
          exporting
            spras               = sy-langu
            node                = <lv_data_prdha>
          tables
            disp_t179           = lt_hiy
          exceptions
            node_does_not_exist = 1.
        if sy-subrc eq 0.
          clear ls_datax.
          move-corresponding <ls_data> to ls_datax.
          loop at lt_hiy assigning field-symbol(<ls_hiy>).
            lv_comp = |PRODH{ <ls_hiy>-stufe }|.
            assign component lv_comp of structure ls_datax to field-symbol(<lv_prodh>).
            if sy-subrc eq 0.
              <lv_prodh> = <ls_hiy>-prodh.
            endif.
            lv_comp = |VTEXT{ <ls_hiy>-stufe }|.
            assign component lv_comp of structure ls_datax to field-symbol(<lv_vtext>).
            if sy-subrc eq 0.
              <lv_vtext> = <ls_hiy>-vtext.
            endif.
          endloop.
          move-corresponding ls_datax to <ls_data>.
          insert ls_datax into table lt_datax.
        endif.
      endif.

    endloop.
  endmethod.


  method get_karak_malzeme.

    data: lt_allocvaluesnum         type table of  bapi1003_alloc_values_num,
          lt_allocvaluesnum_sorted  type sorted table of  bapi1003_alloc_values_num with unique key charact,
          lt_allocvalueschar_sorted type sorted table of  bapi1003_alloc_values_char with unique key charact,
          lt_allocvalueschar        type table of  bapi1003_alloc_values_char,
          lt_allocvaluescurr        type table of  bapi1003_alloc_values_curr,
          lt_return                 type table of bapiret2,
          lv_objectkey              type bapi1003_key-object.

    field-symbols : <lv_value>  type atflv.
    field-symbols : <lv_value_c>  type atwrt.

    check iv_matnr is not initial.
    assign gt_karak_malzeme_multiton[ key primary_key components matnr = iv_matnr  ] to field-symbol(<ls_kpm>).

    if sy-subrc ne 0.

      data(ls_kpm) = value t_karak_malzeme( matnr = iv_matnr ).

      lv_objectkey = ls_kpm-matnr.

      clear  : lt_allocvaluesnum ,
               lt_allocvalueschar,
               lt_allocvaluescurr.

      call function 'BAPI_OBJCL_GETDETAIL'
        exporting
          objectkey       = lv_objectkey
          objecttable     = 'MARA'
          classnum        = 'Z_YARI_URUN'
          classtype       = '300'
        tables
          allocvaluesnum  = lt_allocvaluesnum
          allocvalueschar = lt_allocvalueschar
          allocvaluescurr = lt_allocvaluescurr
          return          = lt_return.

      sort lt_allocvaluesnum by charact.
      sort lt_allocvalueschar by charact.
      lt_allocvaluesnum_sorted  = lt_allocvaluesnum.
      lt_allocvalueschar_sorted = lt_allocvalueschar.

      assign lt_allocvalueschar_sorted[ charact = c_atnam_zbslkkunag ]-value_char to <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zbslkkunag = <lv_value_c>.
      endif.

      assign lt_allocvalueschar_sorted[ charact = c_atnam_zklmurtmtur ]-value_char to <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zklmurtmtur = <lv_value_c>.
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zklmkat ]-value_from to <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-zklmkat = <lv_value>.
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zklmgrm2 ]-value_from to <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-zklmgrm2_d = <lv_value>.
        ls_kpm-zklmgrm2   = zcl_bc_ddic_toolkit=>float_to_char( <lv_value> ).
      endif.

      assign lt_allocvalueschar_sorted[ charact = c_atnam_zklmrenk ]-value_char to <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zklmrenk = <lv_value_c>.
      endif.

      assign lt_allocvalueschar_sorted[ charact = c_atnam_zklmhrmn ]-value_char to <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zklmhrmn = <lv_value_c>.
      endif.

      assign lt_allocvalueschar_sorted[ charact = c_atnam_zklmhcmyum ]-value_char to <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zklmhcmyum = <lv_value_c>.
      endif.

      assign lt_allocvalueschar_sorted[ charact = c_atnam_zklmozlkrk ]-value_char to  <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zklmozlkrk = <lv_value_c>.
      endif.

      insert ls_kpm into table gt_karak_malzeme_multiton assigning <ls_kpm>.

    endif.

    rs_ret = <ls_kpm>.

  endmethod.


  method get_karak_parti.

    data: lt_allocvaluesnum         type table of  bapi1003_alloc_values_num,
          lt_allocvaluesnum_sorted  type sorted table of  bapi1003_alloc_values_num with unique key charact,
          lt_allocvalueschar_sorted type sorted table of  bapi1003_alloc_values_char with unique key charact,
          lt_allocvalueschar        type table of  bapi1003_alloc_values_char,
          lt_allocvaluescurr        type table of  bapi1003_alloc_values_curr,
          lt_return                 type table of bapiret2,
          lv_objectkey              type bapi1003_key-object,
          lv_hsdat                  type cawn-atwrt.

    field-symbols : <lv_value>  type atflv.

    "--------->> add by mehmet sertkaya 27.01.2017 09:54:33
    check iv_charg is not initial.
    "-----------------------------<<
    assign gt_karak_parti_multiton[ key primary_key components matnr = iv_matnr
                                                               charg = iv_charg ] to field-symbol(<ls_kpm>).

    if sy-subrc ne 0.

      data(ls_kpm) = value t_karak_parti( matnr = iv_matnr
                                          charg = iv_charg ).

      lv_objectkey = |{ ls_kpm-matnr }{ ls_kpm-charg }|.

      clear  : lt_allocvaluesnum ,
               lt_allocvalueschar,
               lt_allocvaluescurr.

      call function 'BAPI_OBJCL_GETDETAIL'
        exporting
          objectkey       = lv_objectkey
          objecttable     = 'MCH1'
          classnum        = 'Z_YARI_URUN'
          classtype       = '023'
        tables
          allocvaluesnum  = lt_allocvaluesnum
          allocvalueschar = lt_allocvalueschar
          allocvaluescurr = lt_allocvaluescurr
          return          = lt_return.

      sort lt_allocvaluesnum by charact.
      sort lt_allocvalueschar by charact.

      lt_allocvaluesnum_sorted  = lt_allocvaluesnum.
      lt_allocvalueschar_sorted = lt_allocvalueschar.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zgr_m2 ]-value_from to <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-gr_m2_d = <lv_value>.
        ls_kpm-gr_m2 = zcl_bc_ddic_toolkit=>float_to_char( <lv_value> ).
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zkat_adedi ]-value_from to  <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-kat_adedi_d = <lv_value>.
        ls_kpm-kat_adedi = zcl_bc_ddic_toolkit=>float_to_char( <lv_value> ).
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zmihver_boru_capi ]-value_from to  <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-mihver_boru_capi_d = <lv_value>.
        ls_kpm-mihver_boru_capi = zcl_bc_ddic_toolkit=>float_to_char( <lv_value> ).
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zbobin_capi ]-value_from to  <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-bobin_capi_d = <lv_value>.
        ls_kpm-bobin_capi = zcl_bc_ddic_toolkit=>float_to_char( <lv_value> ).
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_zbobin_genisligi ]-value_from to  <lv_value>.
      if sy-subrc eq 0.
        ls_kpm-bobin_genisligi_d = <lv_value>.
        ls_kpm-bobin_genisligi = zcl_bc_ddic_toolkit=>float_to_char( <lv_value> ).
      endif.

      assign lt_allocvaluesnum_sorted[ charact = c_atnam_lobm_hsdat ]-value_from to  <lv_value>.
      if sy-subrc eq 0.
        call function 'CTCV_CONVERT_FLOAT_TO_DATE'
          exporting
            float = <lv_value>
          importing
            date  = lv_hsdat.
        ls_kpm-lobm_hsdat = lv_hsdat.
      endif.



      assign lt_allocvalueschar_sorted[ charact = c_atnam_grup_no ]-value_char to field-symbol(<lv_value_c>).
      if sy-subrc eq 0.
        ls_kpm-grup_no = <lv_value_c>.
      endif.


      assign lt_allocvalueschar_sorted[ charact = c_atnam_zkagit_makine ]-value_char to <lv_value_c>.
      if sy-subrc eq 0.
        ls_kpm-zkagit_makine = <lv_value_c>.
      endif.

      insert ls_kpm into table gt_karak_parti_multiton assigning <ls_kpm>.

    endif.

    rs_ret = <ls_kpm>.

  endmethod.


  METHOD get_karak_text.

    DATA: lv_name TYPE thead-tdname.

    CHECK iv_charg IS NOT INITIAL AND iv_matnr IS NOT INITIAL.

    lv_name+0(18) = iv_matnr.
    lv_name+22(10) = iv_charg.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = c_tdid
        language                = c_langu
        name                    = lv_name
        object                  = c_tdobject
      TABLES
        lines                   = rt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
    ENDIF.


  ENDMETHOD.


  method get_material_class_data.
    data: lt_alloclist type standard table of bapi1003_alloc_list
          with default key.
    data: lt_return type standard table of BAPIRET2
          with default key.
    data: lv_date type sy-datum.


    data:lv_objkey TYPE BAPI1003_KEY-OBJECT.
    lv_objkey = iv_matnr.

    lv_date = sy-datum.
    if iv_keydate is not initial.
      lv_date = iv_keydate.
    endif.

    check iv_matnr is not initial.
    check iv_classnum is not initial.
* Malzeme Kontrolü
    select single mandt into sy-mandt ##WRITE_OK
      from mara where matnr = iv_matnr.
    check sy-subrc = 0.
* Class kontrolü
    call function 'BAPI_OBJCL_GETCLASSES'
      exporting
        OBJECTKEY_IMP   = lv_objkey
        objecttable_imp = c_table_mara
        classtype_imp   = iv_classtype
        read_valuations = abap_true
        keydate         = lv_date
        language        = sy-langu
      tables
        alloclist       = lt_alloclist
        return          = et_return.
    read table lt_alloclist transporting no fields with key classnum = iv_classnum.
    if sy-subrc <> 0.
* Raise Exception.
    endif.

** Verilerin Çekilmesi.


call function 'BAPI_OBJCL_GETDETAIL'
  exporting
   OBJECTKEY              = lv_objkey
    objecttable            = c_table_mara
    classnum               = iv_classnum
    classtype              = iv_classtype
   KEYDATE                = lv_date
   UNVALUATED_CHARS       = abap_true
   LANGUAGE               = SY-LANGU

  tables
    allocvaluesnum         = et_num
    allocvalueschar        = et_char
    allocvaluescurr        = et_curr
    return                 = et_return .
* Fieldcat'in oluşturulması.
  endmethod.


  method get_mmbe_sas_stock.

    data: begin of ls_xtab ,
            werks type ekpo-werks,
            lgort type ekpo-lgort,
            matnr type ekpo-matnr,
            menge type ekpo-menge,
            mengk type ekpo-menge,
          end of ls_xtab,
          lt_xtab  like table of ls_xtab,
          lt_werks type range of werks_d,
          lt_lgort type range of lgort_d.

    clear rv_menge.


    select single meins into @data(lv_meins) from mara
       where matnr eq @iv_matnr.

    append value #( sign = 'I'
                    option = 'EQ'
                    low = iv_werks ) to lt_werks.

    if iv_lgort is not initial.

      append value #( sign = 'I'
                      option = 'EQ'
                      low = iv_lgort ) to lt_lgort.
    endif.

    call function 'MB_ADD_PURCHASE_ORDER_QUANTITY'
      exporting
        x_elikz = space
        x_loekz = space
        x_matnr = iv_matnr
        x_meins = lv_meins
      tables
        xtab    = lt_xtab
        xwerks  = lt_werks.

    loop at lt_xtab into ls_xtab where lgort in lt_lgort.
      add ls_xtab-menge to rv_menge.
    endloop.

  endmethod.


  method goodsmvt_getdetail.

    data : ls_header       type bapi2017_gm_head_02.
    data : lt_items        type table of bapi2017_gm_item_show.
    data : lt_txt_material type table of ty_txt_material,
           lt_txt_depo     type table of ty_txt_depo.

    call function 'BAPI_GOODSMVT_GETDETAIL'
      exporting
        materialdocument = iv_docnumb
        matdocumentyear  = iv_docyear
      importing
        goodsmvt_header  = ls_header
      tables
        goodsmvt_items   = lt_items
        return           = et_return.

    loop at et_return assigning field-symbol(<fs_return>)
      where type ca 'AEX'.exit.
    endloop.

    if sy-subrc = 0.
      raise doc_not_found.
    endif.

*=====================================================*
*   Başlık Main alanlar / ES_HEADER
*=====================================================*
    es_header = corresponding #( ls_header ).


*=====================================================*
*   Başlık Ek Alanlar   / ES_HEADER
*=====================================================*
    read table lt_items assigning field-symbol(<fs_item>) index 1.

*   Şirket kodu  /
*   Üretim yeri  /
*   Depo   no    /
*   Hareket türü /
    select single  bukrs                   ##WARN_OK
                   werks
                   lgnum
                   bwart from mseg
             into (es_header-bukrs,
                   es_header-werks,
                   es_header-lgnum,
                   es_header-bwart)
            where  mblnr = iv_docnumb
              and  mjahr = iv_docyear.

*   Şirket kodu tanım
    select single  butxt from t001
             into  es_header-butxt
            where  bukrs = es_header-bukrs.

*   Üretim yeri tanımı
    select single  name1 from t001w
             into  es_header-werksxt
            where  werks = es_header-werks.

*   SA Grubu / Satıcı
    select single  ekgrp lifnr from ekko
             into  (es_header-ekgrp,es_header-lifnr)
            where  ebeln = <fs_item>-po_number.

*   Satıcı tanım
    select single  name1 from lfa1
             into  es_header-lifnrxt
            where  lifnr = es_header-lifnr.

*   Telefon
    select single  tel_number from t024
             into  es_header-tel_number
            where  ekgrp = es_header-ekgrp.

*   PO Number
    es_header-po_number = <fs_item>-po_number.


*=====================================================*
*   Kalem Main Alanlar / ET_ITEMS
*=====================================================*
    et_items = corresponding #( lt_items ).


*=====================================================*
*   Kalem Ek Alanlar   / ET_ITEMS
*=====================================================*
    if et_items is not initial.

*     Malzeme metin
      select matnr
             maktx
        from makt into table lt_txt_material
        for all entries in et_items
        where matnr = et_items-material
          and spras = sy-langu.

*     Depo yeri tanım
      select werks
             lgort
             lgobe
        from t001l into table lt_txt_depo
        for all entries in et_items
        where werks = et_items-plant
          and lgort = et_items-stge_loc.

    endif.

    loop at et_items assigning field-symbol(<fs_items>).

      try.
          <fs_items>-maktx = lt_txt_material[ matnr = <fs_items>-material ]-maktx.
        catch cx_sy_itab_line_not_found.

          select single txz01 into <fs_items>-maktx
                 from ekpo
                 where ebeln eq <fs_items>-po_number
                   and ebelp eq <fs_items>-po_item.

      endtry.


      try.
          <fs_items>-lgobe = lt_txt_depo[ werks = <fs_items>-plant
                                         lgort = <fs_items>-stge_loc ]-lgobe.
        catch cx_sy_itab_line_not_found ##NO_HANDLER.
      endtry.

    endloop.

  endmethod.


  method is_creating_material.
    rv_yes = xsdbool( iv_tcode eq c_tcode_create_mat ).
  endmethod.


  method is_displaying_material.
    rv_yes = xsdbool( iv_tcode eq c_tcode_display_mat ).
  endmethod.


  method is_modifying_material.

    rv_yes = xsdbool(
      is_creating_material( iv_tcode ) eq abap_true or
      is_updating_material( iv_tcode ) eq abap_true
    ).

  endmethod.


  method is_updating_material.
    rv_yes = xsdbool( iv_tcode eq c_tcode_edit_mat ).
  endmethod.


  METHOD save_karak_text.

    DATA: ls_header TYPE thead.

    CHECK iv_charg IS NOT INITIAL AND iv_matnr IS NOT INITIAL.

    ls_header-tdname+0(18) = iv_matnr.
    ls_header-tdname+22(10) = iv_charg.
    ls_header-tdid = c_tdid.
    ls_header-tdspras = c_langu.
    ls_header-tdobject = c_tdobject.

    data(lt_lines) = it_lines.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = ls_header
*       insert          = 'X'
        savemode_direct = 'X'
      TABLES
        lines           = lt_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.