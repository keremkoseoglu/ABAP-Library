class zmmcl_util definition
  public
  final
  create public .

public section.
*"* public components of class ZMMCL_UTIL
*"* do not include other source files here!!!

  class-methods validate_meins
    importing
      !pfd_i_meins type meins
    raising
      zcx_mm_uom .
  class-methods get_periv_of_werks
    importing
      !pfd_i_werks type werks_d
    returning
      value(pfd_r_periv) type periv
    raising
      zcx_mm_uretim_yeri .
  class-methods check_vendor_existence
    importing
      !pfd_i_lifnr type lifnr
    exporting
      !pfd_e_lifnr type lifnr
    raising
      zcx_mm_lifnr .
  class-methods check_ekorg_existence
    importing
      !pfd_i_ekorg type ekorg
    raising
      zcx_mm_ekorg .
  class-methods check_werks_existence
    importing
      !pfd_i_werks type werks_d
    raising
      zcx_mm_uretim_yeri .
  class-methods check_meins_existence
    importing
      !pfd_i_meins type meins
    raising
      zcx_mm_meins .
  class-methods check_urztp_existence
    importing
      !pfd_i_urztp type urztp
    raising
      zcx_mm_urztp .
  class-methods check_ekgrp_existence
    importing
      !pfd_i_ekgrp type ekgrp
    raising
      zcx_mm_ekgrp .
  class-methods validate_mvmt_indic_in_itab
    importing
      !prc_i_itab type ref to data
      !pfd_i_bwart_fnam type fieldname default 'BWART'
    raising
      zcx_mm_bwart .
protected section.
*"* protected components of class ZMMCL_UTIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZMMCL_UTIL
*"* do not include other source files here!!!
endclass.



class zmmcl_util implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>CHECK_EKGRP_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_EKGRP                    TYPE        EKGRP
* | [!CX!] ZCX_MM_EKGRP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_ekgrp_existence.

  select single mandt into sy-mandt ##write_ok
         from t024
         where ekgrp eq pfd_i_ekgrp.

  check sy-subrc ne 0.

  raise exception type zcx_mm_ekgrp
    exporting
      textid = zcx_mm_ekgrp=>tanimsiz
      ekgrp  = pfd_i_ekgrp.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>CHECK_EKORG_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_EKORG                    TYPE        EKORG
* | [!CX!] ZCX_MM_EKORG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_ekorg_existence.

  select single mandt into sy-mandt ##write_ok
         from t024e
         where ekorg eq pfd_i_ekorg.

  check sy-subrc ne 0.

  raise exception type zcx_mm_ekorg
    exporting
      textid = zcx_mm_ekorg=>tanimsiz
      ekorg  = pfd_i_ekorg.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>CHECK_MEINS_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_MEINS                    TYPE        MEINS
* | [!CX!] ZCX_MM_MEINS
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_meins_existence.

  select single mandt into sy-mandt ##write_ok
         from t006
         where msehi eq pfd_i_meins.

  check sy-subrc ne 0.

  raise exception type zcx_mm_meins
    exporting
      textid = zcx_mm_meins=>tanimsiz
      meins  = pfd_i_meins.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>CHECK_URZTP_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_URZTP                    TYPE        URZTP
* | [!CX!] ZCX_MM_URZTP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_urztp_existence.

  select single mandt into sy-mandt ##write_ok
         from t069
         where urztp eq pfd_i_urztp.

  check sy-subrc ne 0.

  raise exception type zcx_mm_urztp
    exporting
      textid = zcx_mm_urztp=>tanimsiz
      urztp  = pfd_i_urztp.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>CHECK_VENDOR_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_LIFNR                    TYPE        LIFNR
* | [<---] PFD_E_LIFNR                    TYPE        LIFNR
* | [!CX!] ZCX_MM_LIFNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_vendor_existence.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = pfd_i_lifnr
    importing
      output = pfd_e_lifnr.

  select single mandt into sy-mandt ##write_ok
         from zbcv_lfa1
         where lifnr eq pfd_e_lifnr.

  check sy-subrc ne 0.

  raise exception type zcx_mm_lifnr
    exporting
      textid = zcx_mm_lifnr=>tanimsiz
      lifnr  = pfd_e_lifnr.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>CHECK_WERKS_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_WERKS                    TYPE        WERKS_D
* | [!CX!] ZCX_MM_URETIM_YERI
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_werks_existence.

  select single mandt into sy-mandt ##write_ok
         from t001w
         where werks eq pfd_i_werks.

  check sy-subrc ne 0.

  raise exception type zcx_mm_uretim_yeri
    exporting
      textid = zcx_mm_uretim_yeri=>tanimsiz
      werks  = pfd_i_werks.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>GET_PERIV_OF_WERKS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_WERKS                    TYPE        WERKS_D
* | [<-()] PFD_R_PERIV                    TYPE        PERIV
* | [!CX!] ZCX_MM_URETIM_YERI
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_periv_of_werks.

  data: lfd_bukrs type t001k-bukrs,
        lfd_bwkey type t001w-bwkey.

* Üretim yerine ait değerleme birimi
  select single bwkey into lfd_bwkey
         from t001w
         where werks eq pfd_i_werks.

  if sy-subrc ne 0.
    raise exception type zcx_mm_uretim_yeri
      exporting
        textid = zcx_mm_uretim_yeri=>tanimsiz
        werks  = pfd_i_werks.
  endif.

* Değerleme birimine ait şirket kodu

  select single bukrs into lfd_bukrs
         from t001k
         where bwkey eq lfd_bwkey.

  if sy-subrc ne 0.
    raise exception type zcx_mm_uretim_yeri
      exporting
        textid = zcx_mm_uretim_yeri=>degerleme_birimi_yok
        werks  = pfd_i_werks.
  endif.

* Şirket koduna ait PERIV

  select single periv into pfd_r_periv from t001 where bukrs eq lfd_bukrs.
  assert id zmm condition sy-subrc eq 0.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>VALIDATE_MEINS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_MEINS                    TYPE        MEINS
* | [!CX!] ZCX_MM_UOM
* +--------------------------------------------------------------------------------------</SIGNATURE>
method validate_meins.

* Ölçü birimi boşsa hata üretelim

  if pfd_i_meins is initial.
    raise exception type zcx_mm_uom
      exporting
        textid = zcx_mm_uom=>olcu_birimi_tanimsiz.
  endif.

* Ölçü birimi sistemde tanımlı mı? Değilse hata üretelim
* (Buffer var)

  select single mandt into sy-mandt ##write_ok
    from t006
    where msehi eq pfd_i_meins.

  if sy-subrc ne 0.
    raise exception type zcx_mm_uom
      exporting
        textid = zcx_mm_uom=>olcu_birimi_tanimsiz
        meins  = pfd_i_meins.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMMCL_UTIL=>VALIDATE_MVMT_INDIC_IN_ITAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRC_I_ITAB                     TYPE REF TO DATA
* | [--->] PFD_I_BWART_FNAM               TYPE        FIELDNAME (default ='BWART')
* | [!CX!] ZCX_MM_BWART
* +--------------------------------------------------------------------------------------</SIGNATURE>
method validate_mvmt_indic_in_itab.

  field-symbols: <lfd_bwart> type bwart,
                 <lit_itab>  type standard table,
                 <lwa_itab>  type any.

  data : begin of ls_itabx,
         werks type t001w-werks,
         matnr type mara-matnr,
         erfme type erfme,
         lgort type lgort_d,
         charg type mch1-charg,
         erfmg type erfmg,
         end of ls_itabx,

         lt_wmdvsx  type table of bapiwmdvs,
         lt_wmdvex  type table of bapiwmdve,
         ls_vex     type bapiwmdve,
         lv_qty     type bapiwmdve-com_qty.

  constants lc_rule_z4 type bapit441v-prreg value 'Z4'.

  check prc_i_itab is not initial.
  assign prc_i_itab->* to <lit_itab>.
  check <lit_itab>[] is not initial.

  loop at <lit_itab> assigning <lwa_itab>.
    assign component pfd_i_bwart_fnam of structure <lwa_itab> to <lfd_bwart>.
* COMMENTED BY SELIM CAGLAR 19.10.2016 16:45:00
*    SELECT SINGLE mandt INTO sy-mandt ##write_ok
*           FROM t156sc
*           WHERE bwart EQ <lfd_bwart>
*             AND kzbew EQ 'B'.
* END OF COMMENT BY SELIM CAGLAR 19.10.2016 16:45:00
* INSERTED BY SELIM CAGLAR 19.10.2016 16:45:10
    move-corresponding <lwa_itab> to ls_itabx.
    check ls_itabx is not initial.
    case <lfd_bwart>.
      when '321'.
        refresh : lt_wmdvsx,lt_wmdvex.
        clear : lv_qty.
        call function 'BAPI_MATERIAL_AVAILABILITY'
          exporting
            plant      = ls_itabx-werks
            material   = ls_itabx-matnr
            unit       = ls_itabx-erfme
            stge_loc   = ls_itabx-lgort
            batch      = ls_itabx-charg
            check_rule = lc_rule_z4
          tables
            wmdvsx     = lt_wmdvsx
            wmdvex     = lt_wmdvex.

        loop at lt_wmdvex into ls_vex.
          add ls_vex-com_qty to lv_qty.
        endloop.
        " Miktar kontrolü yapalım..
        if ls_itabx-erfmg > lv_qty.

          raise exception type zcx_mm_bwart
            exporting
              textid = zcx_mm_bwart=>kullanılabilirlik_kontrolu
              erfme  = ls_itabx-erfme
              erfmg  = ls_itabx-erfmg.
        endif.

      when others.
    endcase.
* END OF INSERT BY SELIM CAGLAR 19.10.2016 16:45:10
* COMMENTED BY SELIM CAGLAR 19.10.2016 16:53:59
*    CHECK sy-subrc EQ 0.
*    RAISE EXCEPTION TYPE zcx_mm_bwart
*      EXPORTING
*        textid = zcx_mm_bwart=>hareket_turu_kullanilamaz
*        bwart  = <lfd_bwart>.
* END OF COMMENT BY SELIM CAGLAR 19.10.2016 16:53:59
  endloop.

endmethod.
endclass.
