class zhrcl_util definition
  public
  final
  create public .

public section.
*"* public components of class ZHRCL_UTIL
*"* do not include other source files here!!!

  class-methods get_uname_of_employee
    importing
      !pfd_i_pernr type persno
    returning
      value(pfd_r_uname) type syuname
    raising
      zcx_hr_pernr .
  class-methods get_email_of_employee
    importing
      !pfd_i_pernr type persno
    returning
      value(pfd_r_email) type ad_smtpadr
    raising
      zcx_hr_pernr .
  class-methods get_full_name_of_employee
    importing
      !pfd_i_pernr type persno
    returning
      value(pfd_r_ename) type zfmd_ename
    raising
      zcx_hr_pernr .
  class-methods get_position_text_of_user
    importing
      !pfd_i_uname type xubname
    returning
      value(pfd_r_stext) type hr_mcstext .
  class-methods get_email_address
    importing
      !pfd_i_pernr type persno optional
      !pfd_i_uname type xubname optional
    returning
      value(pfd_r_email) type ad_smtpadr
    raising
      zcx_bc_ent_kisi
      zcx_hr_pernr .
  type-pools abap .
  class-methods is_employee_working
    importing
      !pfd_i_pernr type persno
      !pfd_i_date type dats default sy-datum
    returning
      value(pfd_r_working) type abap_bool .
  protected section.
*"* protected components of class ZHRCL_UTIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZHRCL_UTIL
*"* do not include other source files here!!!

  types:
    begin of t_name,
             pernr type pa0002-pernr,
             vorna type pa0002-vorna,
             nachn type pa0002-nachn,
             ename type zfmd_ename,
           end of t_name .
  types:
    tt_name type hashed table of t_name
                   with unique key primary_key components pernr .
  types:
    begin of t_email,
        uname type xubname,
        pernr type persno,
        email type ad_smtpadr,
      end of t_email .
  types:
    tt_email type hashed table of t_email
        with unique key primary_key components uname pernr .

  constants c_otype_pers type otype value 'P'. "#EC NOTEXT
  constants c_otype_position type otype value 'S'. "#EC NOTEXT
  constants c_plvar_01 type plvar value '01'. "#EC NOTEXT
  constants c_relat_008 type relat value '008'. "#EC NOTEXT
  constants c_rsign_b type rsign value 'B'. "#EC NOTEXT
  constants c_sclas_position type sclas value 'S'. "#EC NOTEXT
  constants c_subty_email type subty value '0010'. "#EC NOTEXT
  constants c_subty_uname type subty value '0001'. "#EC NOTEXT
endclass.



class zhrcl_util implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZHRCL_UTIL=>GET_EMAIL_ADDRESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_PERNR                    TYPE        PERSNO(optional)
* | [--->] PFD_I_UNAME                    TYPE        XUBNAME(optional)
* | [<-()] PFD_R_EMAIL                    TYPE        AD_SMTPADR
* | [!CX!] ZCX_BC_ENT_KISI
* | [!CX!] ZCX_HR_PERNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_email_address.

    statics: lit_email type tt_email.

    data: lwa_email type t_email,

          lfd_pernr_initial type persno,
          lfd_uname_initial type xubname,

          lrd_email type ref to t_email.

* ______________________________________________________________________
* Ön kontroller
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

    check ( pfd_i_uname is initial     and pfd_i_pernr is not initial )
       or ( pfd_i_uname is not initial and pfd_i_pernr is initial     ).

* ______________________________________________________________________
* Daha önce Buffer'dan okuduysak, Buffer'dan döndürelim
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

    if pfd_i_pernr is initial.

      read table lit_email reference into lrd_email
        with table key primary_key
        components uname = pfd_i_uname
                   pernr = lfd_pernr_initial.

    else.

      read table lit_email reference into lrd_email
        with table key primary_key
        components pernr = pfd_i_pernr
                   uname = lfd_uname_initial.

    endif.


    if sy-subrc eq 0.
      pfd_r_email = lrd_email->email.
      return.
    endif.

* ______________________________________________________________________
* E-Posta belirleme
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

    clear pfd_r_email.

    if pfd_i_pernr is initial.

      select single adr6~smtp_addr
        into pfd_r_email
        from
          usr21
          inner join adr6 on adr6~addrnumber eq usr21~addrnumber
                         and adr6~persnumber eq usr21~persnumber
        where
          usr21~bname eq pfd_i_uname and
          adr6~date_from le sy-datum and
          ( adr6~smtp_addr ne space and adr6~smtp_addr is not null ). "#EC WARNOK

      if pfd_r_email is initial.
        raise exception type zcx_bc_ent_kisi
          exporting
            textid = zcx_bc_ent_kisi=>eposta_adresi_yok
            bname  = pfd_i_uname.
      endif.

    else.

      pfd_r_email = get_email_of_employee( pfd_i_pernr ).

    endif.

* ______________________________________________________________________
* Buffer
* ˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙˙

    lwa_email-uname = pfd_i_uname.
    lwa_email-pernr = pfd_i_pernr.
    lwa_email-email = pfd_r_email.
    insert lwa_email into table lit_email.

  endmethod.                    "get_email_address


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZHRCL_UTIL=>GET_EMAIL_OF_EMPLOYEE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_PERNR                    TYPE        PERSNO
* | [<-()] PFD_R_EMAIL                    TYPE        AD_SMTPADR
* | [!CX!] ZCX_HR_PERNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_email_of_employee.

    clear pfd_r_email.

    select single usrid_long into pfd_r_email
      from pa0105
      where pernr eq pfd_i_pernr
        and subty eq c_subty_email
        and begda le sy-datum
        and endda ge sy-datum.                              "#EC WARNOK

    if pfd_r_email is initial.
      raise exception type zcx_hr_pernr
        exporting
          textid = zcx_hr_pernr=>eposta_yok
          pernr  = pfd_i_pernr.
    endif.

  endmethod.                    "GET_EMAIL_OF_EMPLOYEE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZHRCL_UTIL=>GET_FULL_NAME_OF_EMPLOYEE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_PERNR                    TYPE        PERSNO
* | [<-()] PFD_R_ENAME                    TYPE        ZFMD_ENAME
* | [!CX!] ZCX_HR_PERNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_full_name_of_employee.

    statics: lit_name type tt_name.

    data: lwa_name  type t_name,
          lrd_name  type ref to t_name.

    read table lit_name reference into lrd_name
                        with table key primary_key
                        components pernr = pfd_i_pernr.

    if sy-subrc ne 0.

      lwa_name-pernr = pfd_i_pernr.

      select single vorna nachn
             into corresponding fields of lwa_name
             from pa0002
             where pernr eq pfd_i_pernr
               and begda le sy-datum
               and endda ge sy-datum.

      concatenate lwa_name-vorna lwa_name-nachn into lwa_name-ename separated by space.

      insert lwa_name into table lit_name reference into lrd_name.

    endif.

    if lrd_name->ename is initial.
      raise exception type zcx_hr_pernr
        exporting
          textid = zcx_hr_pernr=>ad_soyad_yok
          pernr  = pfd_i_pernr.
    endif.

    pfd_r_ename = lrd_name->ename.

  endmethod.                    "get_full_name_of_employee


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZHRCL_UTIL=>GET_POSITION_TEXT_OF_USER
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_UNAME                    TYPE        XUBNAME
* | [<-()] PFD_R_STEXT                    TYPE        HR_MCSTEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_position_text_of_user.

    data:
      lfd_pernr type pa0105-pernr,
      lfd_sobid type hrp1001-sobid.

* Temizlik
    clear pfd_r_stext.

* Personel kodu
    select single pernr into lfd_pernr
      from pa0105
      where subty eq c_subty_uname
        and begda le sy-datum
        and endda ge sy-datum
        and usrid eq pfd_i_uname.                           "#EC WARNOK

    check sy-subrc eq 0.

* Pozisyon
    select single sobid into lfd_sobid
      from hrp1001
      where otype eq c_otype_pers
        and objid eq lfd_pernr
        and plvar eq c_plvar_01
        and rsign eq c_rsign_b
        and relat eq c_relat_008
        and sclas eq c_sclas_position
        and begda le sy-datum
        and endda ge sy-datum.                              "#EC WARNOK

    check sy-subrc eq 0.

* Pozisyon metni
    select single mc_stext into pfd_r_stext
      from hrp1000
      where otype eq c_otype_position
        and objid eq lfd_sobid
        and begda le sy-datum
        and endda ge sy-datum.                              "#EC WARNOK

  endmethod.                    "get_position_text_of_user


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZHRCL_UTIL=>GET_UNAME_OF_EMPLOYEE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_PERNR                    TYPE        PERSNO
* | [<-()] PFD_R_UNAME                    TYPE        SYUNAME
* | [!CX!] ZCX_HR_PERNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_uname_of_employee.

    clear pfd_r_uname.

    select single usrid into pfd_r_uname
      from pa0105
      where pernr eq pfd_i_pernr
        and subty eq c_subty_uname
        and begda le sy-datum
        and endda ge sy-datum.                              "#EC WARNOK

    if pfd_r_uname is initial.
      raise exception type zcx_hr_pernr
        exporting
          textid = zcx_hr_pernr=>sap_user_yok
          pernr  = pfd_i_pernr.
    endif.

  endmethod.                    "get_uname_of_employee


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZHRCL_UTIL=>IS_EMPLOYEE_WORKING
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_PERNR                    TYPE        PERSNO
* | [--->] PFD_I_DATE                     TYPE        DATS (default =SY-DATUM)
* | [<-()] PFD_R_WORKING                  TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method is_employee_working.

  select single mandt into sy-mandt
         from pa0000
         where pernr eq pfd_i_pernr
           and begda le pfd_i_date
           and endda ge pfd_i_date
           and stat2 eq '3' ##write_ok.

  check sy-subrc eq 0.

  pfd_r_working = abap_true.

endmethod.
endclass.
