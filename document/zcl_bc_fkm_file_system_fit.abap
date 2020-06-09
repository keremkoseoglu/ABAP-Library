class zbccl_fkm_file_system_fit definition
  public
  final
  create public .

  public section.

    interfaces zbcif_fkm_file_system_imp .
  protected section.

  private section.

    types: begin of t_fitkey,
             guid_file      type zbct_fkm_fitlnk-guid_file,
             instance_ident type zbct_fkm_fitlnk-instance_ident,
             id             type zbct_fkm_fitlnk-id,
             uuid           type zbct_fkm_fitlnk-uuid,
           end of t_fitkey,

           tt_fitkey type standard table of t_fitkey with default key.

    constants: c_clsname_me     type seoclsname                value 'ZBCCL_FKM_FILE_SYSTEM_FIT',
               c_fileext_pdf    type zbcs_fkm_filelist-fileext value 'PDF',
               c_fsyid_fit      type zbcd_fkm_fsyid            value 'FIT',
               c_icon_green     type icon_d                    value '@08@',
               c_icon_red       type icon_d                    value '@0A@',
               c_icon_yellow    type icon_d                    value '@09@',
               c_meth_add       type char30                    value 'ADD_FILE',
               c_meth_pick_file type char30                    value 'PICK_FILE',
               c_option_eq      type ddoption                  value 'EQ',
               c_scheme_vkn     type /fite/inv_1_de065         value 'VKN',
               c_sign_i         type ddsign                    value 'I',
               c_siste_fit      type zbcd_siste                value 'FIT'.
endclass.



class zbccl_fkm_file_system_fit implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~ADD_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRC_I_INVOICE                  TYPE REF TO ZBCCL_FKM_INVOICE
* | [<---] PWA_E_FILE                     TYPE        ZBCS_FKM_FILELIST
* | [!CX!] ZCX_BC_METHOD_CALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~add_file.

    data lrc_cx_sro type ref to zcx_bc_ent_system_read_only.

    create object lrc_cx_sro
      exporting
        textid = zcx_bc_ent_system_read_only=>read_only
        siste  = c_siste_fit.

    raise exception type zcx_bc_method_call
      exporting
        textid   = zcx_bc_method_call=>method_error
        previous = lrc_cx_sro
        clsname  = c_clsname_me
        methname = c_meth_add.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~ADD_FILE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~CHECK_DOC_EXISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_GUID                     TYPE        ZBCD_FKM_GUID_FILE
* | [<-()] PFD_R_EXISTS                   TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~check_doc_existence.

    select single mandt into sy-mandt ##write_ok
           from zbct_fkm_fitlnk
           where guid_file eq pfd_i_guid.

    pfd_r_exists = boolc( sy-subrc eq 0 ).

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~CHECK_DOC_EXISTENCE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~GET_COMPANY_CODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_GUID                     TYPE        ZBCD_FKM_GUID_FILE
* | [<-()] PFD_R_BUKRS                    TYPE        BUKRS
* | [!CX!] ZCX_BC_METHOD_CALL
* | [!CX!] ZCX_BC_UNSUPPORTED_OPERATION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~get_company_code.

    select single zfit4~bukrs into pfd_r_bukrs
           from zbct_fkm_fitlnk as zlnk
                inner join /fite/inv_1_t001 as zfit1 on zfit1~invoice_id    eq zlnk~id
                                                    and zfit1~envelope_uuid eq zlnk~instance_ident
                                                    and zfit1~invoice_uuid  eq zlnk~uuid
                inner join /fite/inv_1_t004 as zfit4 on zfit4~system_id     eq zfit1~system_id
           where zlnk~guid_file eq pfd_i_guid.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~GET_COMPANY_CODE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~GET_CREATION_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_GUID                     TYPE        ZBCD_FKM_GUID_FILE
* | [<-()] PFD_R_DATE                     TYPE        DATS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~get_creation_date.

    data lfd_id type zbct_fkm_fitlnk-id.

    select single id into lfd_id from zbct_fkm_fitlnk where guid_file eq pfd_i_guid.
    check sy-subrc eq 0.

    select min( cre_date )
           into pfd_r_date
           from /fite/inv_1_t001
           where invoice_id eq lfd_id.

  endmethod.                    "zbcif_fkm_file_system_imp~get_creation_date


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~GET_FILE_ICONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] PIT_I_GUID                     TYPE        TT_GUID_FILE
* | [<-()] PIT_R_ICON                     TYPE        TT_GUID_FILE_ICON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~get_file_icons.

    data: lit_fitkey     type tt_fitkey,
          lit_guid       type zbcif_fkm_file_system_imp=>tt_guid_file,
          lit_env_status type standard table of /fite/inv_1_s017,
          lit_inv_id     type standard table of /fite/inv_1_s011,
          lit_system_id  type standard table of /fite/inv_1_s007,

          lwa_icon       like line of pit_r_icon.

    field-symbols: <lwa_env_status> like line of lit_env_status,
                   <lwa_fitkey>     like line of lit_fitkey,
                   <lwa_guid>       like line of lit_guid,
                   <lwa_inv_id>     like line of lit_inv_id.

    check pit_i_guid[] is not initial.

    select guid_file instance_ident id uuid
           into corresponding fields of table lit_fitkey
           from zbct_fkm_fitlnk
           for all entries in pit_i_guid
           where guid_file eq pit_i_guid-guid_file.

    check lit_fitkey[] is not initial. " Paranoya
    sort lit_fitkey by guid_file.

    loop at lit_fitkey assigning <lwa_fitkey>.
      append initial line to lit_inv_id assigning <lwa_inv_id>.
      <lwa_inv_id>-option = c_option_eq.
      <lwa_inv_id>-sign   = c_sign_i.
      <lwa_inv_id>-low    = <lwa_fitkey>-id.
    endloop.

    call function '/FITE/INV_1_F026'
      exporting
        i_rb_all2     = abap_true
        i_rb_unanw    = abap_true
        i_rb_accep    = abap_true
        i_rb_refus    = abap_true
        i_resend      = abap_true
        i_dont_mark   = abap_false
      tables
        it_system_id  = lit_system_id
        it_inv_id     = lit_inv_id
        et_env_status = lit_env_status.

    check lit_env_status[] is not initial. " Paranoya
    sort lit_env_status by invoice_id.

    lit_guid[] = pit_i_guid[].
    sort lit_guid by guid_file.
    delete adjacent duplicates from lit_guid comparing guid_file.

    loop at lit_guid assigning <lwa_guid>.

      clear lwa_icon.
      lwa_icon-guid_file = <lwa_guid>-guid_file.

      read table lit_fitkey assigning <lwa_fitkey>
                            with key guid_file = lwa_icon-guid_file
                            binary search.
      check sy-subrc eq 0. " Paranoya

      read table lit_env_status assigning <lwa_env_status>
                                with key invoice_id = <lwa_fitkey>-id
                                binary search.
      check sy-subrc eq 0.

      case <lwa_env_status>-traffic_light.
        when '1'. lwa_icon-icon = c_icon_red.
        when '2'. lwa_icon-icon = c_icon_yellow.
        when '3'. lwa_icon-icon = c_icon_green.
      endcase.

      insert lwa_icon into table pit_r_icon.

    endloop.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~GET_FILE_ICONS


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~GET_FILE_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRC_I_INVOICE                  TYPE REF TO ZBCCL_FKM_INVOICE
* | [<-()] PIT_R_LIST                     TYPE        ZBCTT_FKM_FILELIST
* | [!CX!] ZCX_BC_METHOD_CALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~get_file_list.


    data: lwa_list    like line of pit_r_list,
          lfd_fkmno   type zbcd_fkm_fkmno.

    lfd_fkmno = prc_i_invoice->get_head( )-fkmno.
    check lfd_fkmno is not initial.

    select guid_file id as filename
           into corresponding fields of table pit_r_list ##too_many_itab_fields
           from zbct_fkm_fitlnk
           where fkmno eq lfd_fkmno.

    lwa_list-fsyid   = c_fsyid_fit.
    lwa_list-fileext = c_fileext_pdf.

    modify pit_r_list from lwa_list
                      transporting fsyid fileext
                      where fsyid is initial.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~GET_FILE_LIST


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~GET_PDF
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRC_I_INVOICE                  TYPE REF TO ZBCCL_FKM_INVOICE
* | [<-()] PIT_R_PDF                      TYPE        ZBCCL_WF=>TT_PDF
* | [!CX!] ZCX_BC_METHOD_CALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~get_pdf.

    data: lit_file      type zbctt_fkm_filelist,
          lit_fit9      type standard table of /fite/inv_1_t009,
          lit_fitlnk    type standard table of zbct_fkm_fitlnk,
          lit_hex       type standard table of blob,
          lit_solix     type solix_tab,

          lfd_receiver  type stcd2,
          lfd_sender    type stcd2.

    field-symbols: <lwa_fit9>   like line of lit_fit9,
                   <lwa_fitlnk> like line of lit_fitlnk,
                   <lwa_pdf>    like line of pit_r_pdf.

    lit_file = zbcif_fkm_file_system_imp~get_file_list( prc_i_invoice ).
    check lit_file[] is not initial.

    select * into table lit_fitlnk
           from zbct_fkm_fitlnk
           for all entries in lit_file
           where guid_file eq lit_file-guid_file.

    check lit_fitlnk[] is not initial.

    select * into table lit_fit9
           from /fite/inv_1_t009
           for all entries in lit_fitlnk
           where instance_ident   eq lit_fitlnk-instance_ident
             and contact_type_ide eq 'VKN_TCKN'.

    check lit_fit9[] is not initial.
    sort lit_fit9 by contact_type.

    loop at lit_fitlnk assigning <lwa_fitlnk>.

      read table lit_fit9 assigning <lwa_fit9>
                          with key instance_ident = <lwa_fitlnk>-instance_ident
                          contact_type = 'S'
                          binary search.
      check sy-subrc eq 0.
      lfd_sender = <lwa_fit9>-contact.


      read table lit_fit9 assigning <lwa_fit9>
                          with key instance_ident = <lwa_fitlnk>-instance_ident
                          contact_type = 'R'
                          binary search.
      check sy-subrc eq 0.
      lfd_receiver = <lwa_fit9>-contact.

      clear lit_hex[].

      call function '/FITE/INV_1_F002'
        exporting
          i_vkn          = lfd_receiver
          i_sender_vkn   = lfd_sender
          i_receiver_vkn = lfd_receiver
          i_invoice_id   = <lwa_fitlnk>-id
          i_get_hex      = abap_true
          i_save_pdf     = abap_true
        tables
          et_hex         = lit_hex.

      check lit_hex[] is not initial.

      append initial line to pit_r_pdf assigning <lwa_pdf>.
      <lwa_pdf>-name = <lwa_fitlnk>-id.

      lit_solix[] = lit_hex[].
      <lwa_pdf>-pdf = cl_bcs_convert=>solix_to_xstring( lit_solix ).

    endloop.

  endmethod.                    "zbcif_fkm_file_system_imp~get_pdf


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~OPEN_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_GUID_FILE                TYPE        ZBCD_FKM_GUID_FILE
* | [!CX!] ZCX_BC_METHOD_CALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~open_file.

    data: lit_id_rng type range of /fite/inv_1_de038,
          lwa_fitlnk type zbct_fkm_fitlnk,
          lfd_system type /fite/inv_1_de249.

    field-symbols <lwa_id_rng> like line of lit_id_rng.

    select single * into lwa_fitlnk from zbct_fkm_fitlnk where guid_file eq pfd_i_guid_file.
    check sy-subrc eq 0.

    select single system_id into lfd_system from /fite/inv_1_t001 where invoice_id eq lwa_fitlnk-id.
    check sy-subrc eq 0.

    append initial line to lit_id_rng assigning <lwa_id_rng>.
    <lwa_id_rng>-option = c_option_eq.
    <lwa_id_rng>-sign   = c_sign_i.
    <lwa_id_rng>-low    = lwa_fitlnk-id.

    submit /fite/inv_1_r015 with p_system eq lfd_system
                            with s_invid  in lit_id_rng
           and return.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~OPEN_FILE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~PICK_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRC_I_INVOICE                  TYPE REF TO ZBCCL_FKM_INVOICE
* | [<---] PWA_E_FILE                     TYPE        ZBCS_FKM_FILELIST
* | [<-->] PWA_C_HEAD                     TYPE        ZBCS_FKM_INVHEA_DOC_CHG(optional)
* | [!CX!] ZCX_BC_METHOD_CALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~pick_file.

    data: lrc_cx_root type ref to cx_root,

          lit_fit     type zbctt_fkm_fit_sel,
          lwa_lnk     type zbct_fkm_fitlnk,

          lfd_amo     type /fite/inv_1_de070,
          lfd_fkmno   type zbcd_fkm_fkmno.

    field-symbols <lwa_fit> like line of lit_fit.

    try.

*       Temizlik & dosya seçimi

        clear pwa_e_file.

        lfd_fkmno = prc_i_invoice->get_head( )-fkmno.

        if lfd_fkmno is initial.
          raise exception type zcx_bc_fkm_fkmno
            exporting
              textid = zcx_bc_fkm_fkmno=>fkmno_initial.
        endif.

        lit_fit = zbccl_fkm_fit_sel=>get_fit_list_via_gui( ).

        read table lit_fit index 1 assigning <lwa_fit>.
        check sy-subrc eq 0.

*       Veritabanına kayıt

        lwa_lnk-guid_file      = cl_reca_guid=>get_new_guid( ).
        lwa_lnk-fkmno          = lfd_fkmno.
        lwa_lnk-instance_ident = <lwa_fit>-instance_ident.
        lwa_lnk-id             = <lwa_fit>-id.
        lwa_lnk-uuid           = <lwa_fit>-uuid.
        lwa_lnk-ernam          = lwa_lnk-aenam = sy-uname.
        lwa_lnk-erdat          = lwa_lnk-aedat = sy-datum.
        lwa_lnk-erzet          = lwa_lnk-aezet = sy-uzeit.

        insert zbct_fkm_fitlnk from lwa_lnk.

*       Dosya bilgilerini döndür

        pwa_e_file-fsyid     = c_fsyid_fit.
        pwa_e_file-filename  = lwa_lnk-id.
        pwa_e_file-fileext   = c_fileext_pdf.
        pwa_e_file-guid_file = lwa_lnk-guid_file.

*       Başlık bilgilerini değiştir

        if <lwa_fit>-scheme_id eq c_scheme_vkn.
          select single: kunnr into pwa_c_head-kunnr from kna1 where stcd2 eq <lwa_fit>-party_ident_id,
                         lifnr into pwa_c_head-lifnr from lfa1 where stcd2 eq <lwa_fit>-party_ident_id.
        endif.

        pwa_c_head-ppino = <lwa_fit>-id.
        pwa_c_head-wrbtr = <lwa_fit>-lmt_tax_incl_amo.
        pwa_c_head-waers = <lwa_fit>-lmt_tax_incl_cur.
        pwa_c_head-stext = <lwa_fit>-asp_party_name.

        concatenate <lwa_fit>-issue_date+0(4)
                    <lwa_fit>-issue_date+5(2)
                    <lwa_fit>-issue_date+8(2) into pwa_c_head-bldat.

        if <lwa_fit>-lmt_tax_incl_cur eq <lwa_fit>-lmt_tax_excl_cur.
          pwa_c_head-bmwst = <lwa_fit>-lmt_tax_incl_amo - <lwa_fit>-lmt_tax_excl_amo.
        else.

          call function 'CONVERT_TO_LOCAL_CURRENCY'
            exporting
              date             = pwa_c_head-bldat
              foreign_amount   = <lwa_fit>-lmt_tax_excl_amo
              foreign_currency = <lwa_fit>-lmt_tax_excl_cur
              local_currency   = <lwa_fit>-lmt_tax_incl_cur
            importing
              local_amount     = lfd_amo
            exceptions
              no_rate_found    = 1
              overflow         = 2
              no_factors_found = 3
              no_spread_found  = 4
              derived_2_times  = 5
              others           = 6.

          if sy-subrc eq 0.
            pwa_c_head-bmwst = <lwa_fit>-lmt_tax_incl_amo - lfd_amo.
          endif.

        endif.

      catch cx_root into lrc_cx_root.

        raise exception type zcx_bc_method_call
          exporting
            textid   = zcx_bc_method_call=>method_error
            previous = lrc_cx_root
            clsname  = c_clsname_me
            methname = c_meth_pick_file.

    endtry.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~PICK_FILE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_FKM_FILE_SYSTEM_FIT->ZBCIF_FKM_FILE_SYSTEM_IMP~REMOVE_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRC_I_INVOICE                  TYPE REF TO ZBCCL_FKM_INVOICE
* | [--->] PFD_I_GUID                     TYPE        ZBCD_FKM_GUID_FILE
* | [!CX!] ZCX_BC_METHOD_CALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zbcif_fkm_file_system_imp~remove_file.

    delete from zbct_fkm_fitlnk where guid_file eq pfd_i_guid.

  endmethod.                    "ZBCIF_FKM_FILE_SYSTEM_IMP~REMOVE_FILE
endclass.