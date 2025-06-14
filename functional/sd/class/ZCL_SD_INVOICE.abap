CLASS zcl_sd_invoice DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_bkpf_key,
             bukrs TYPE bkpf-bukrs,
             belnr TYPE bkpf-belnr,
             gjahr TYPE bkpf-gjahr,
           END OF t_bkpf_key,

           tt_bkpf_key TYPE STANDARD TABLE OF t_bkpf_key WITH DEFAULT KEY,
           tt_vbeln    TYPE STANDARD TABLE OF vbeln_vf WITH DEFAULT KEY,

           BEGIN OF t_header,
             vbeln TYPE vbrk-vbeln,
             vkorg TYPE vbrk-vkorg,
             vtweg TYPE vbrk-vtweg,
             fksto TYPE vbrk-fksto,
             rfbsk TYPE vbrk-rfbsk,
             spart TYPE vbrk-spart,
             fkart TYPE vbrk-fkart,
             knumv TYPE vbrk-knumv,
             bukrs TYPE vbrk-bukrs,
           END OF t_header,

           BEGIN OF t_item,
             posnr TYPE vbrp-posnr,
             matnr TYPE vbrp-matnr,
             fkimg TYPE vbrp-fkimg,
             vrkme TYPE vbrp-vrkme,
             taxm1 TYPE vbrp-taxm1,
           END OF t_item,

           tt_item TYPE HASHED TABLE OF t_item
                   WITH UNIQUE KEY primary_key COMPONENTS posnr
                   WITH NON-UNIQUE SORTED KEY k1 COMPONENTS matnr.

    TYPES t_e__analysis_result TYPE zcl_sd_invoice_edoc_analyser=>analysis_result_dict.

    TYPES: BEGIN OF t_partner,
             parvw TYPE vbpa-parvw,
             kunnr TYPE vbpa-kunnr,
             adrnr TYPE vbpa-adrnr,
             land1 TYPE vbpa-land1,
           END OF t_partner,

           tt_partners TYPE HASHED TABLE OF t_partner
                       WITH UNIQUE KEY primary_key
                       COMPONENTS parvw.

    TYPES: BEGIN OF t_condition,
             knumv TYPE konv-knumv,
             kposn TYPE konv-kposn,
             stunr TYPE konv-stunr,
             zaehk TYPE konv-zaehk,
             kschl TYPE konv-kschl,
             kbetr TYPE konv-kbetr,
             waers TYPE konv-waers,
           END OF t_condition,

           tt_condition TYPE STANDARD TABLE OF t_condition WITH EMPTY KEY.

    CONSTANTS: BEGIN OF c_rfbsk,
                 acc_doc_posted TYPE rfbsk VALUE 'C',
               END OF c_rfbsk.

    CONSTANTS: BEGIN OF c_parvw,
                 sold_to TYPE parvw VALUE 'AG',
                 bill_to TYPE parvw VALUE 'RE',
                 payer   TYPE parvw VALUE 'RG',
                 ship_to TYPE parvw VALUE 'WE',
               END OF c_parvw.

    CONSTANTS default_max_wait TYPE i VALUE 60.

    DATA gv_vbeln  TYPE vbeln_vf READ-ONLY.
    DATA gs_header TYPE t_header READ-ONLY.

    CLASS-METHODS wait_until_invoice_created
      IMPORTING iv_vbeln TYPE vbeln_vf
                max_wait TYPE i DEFAULT default_max_wait
      RAISING   zcx_sd_invoice.

    CLASS-METHODS wait_until_invoice_lockable
      IMPORTING iv_vbeln    TYPE vbeln_vf
                iv_max_wait TYPE i DEFAULT default_max_wait
      RAISING   zcx_bc_lock.

    CLASS-METHODS get_instance
      IMPORTING iv_vbeln         TYPE vbeln_vf
                iv_bypass_buffer TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ro_obj)    TYPE REF TO zcl_sd_invoice
      RAISING   zcx_bc_table_content.

    METHODS:
      analyse_earc       RETURNING VALUE(rs_result)     TYPE t_e__analysis_result,
      analyse_einv       RETURNING VALUE(rs_result)     TYPE t_e__analysis_result,
      analyse_e_         RETURNING VALUE(rs_result)     TYPE t_e__analysis_result,

      get_fi_docs        RETURNING VALUE(rt_key)        TYPE tt_bkpf_key,
      get_items          RETURNING VALUE(rt_item)       TYPE tt_item,
      get_partners       RETURNING VALUE(rt_partners)   TYPE tt_partners,
      get_earc_url       RETURNING VALUE(result)        TYPE string,
      get_einv_url       RETURNING VALUE(result)        TYPE string,
      get_e_url          RETURNING VALUE(result)        TYPE string,

      is_earc_dispatched RETURNING VALUE(rv_dispatched) TYPE abap_bool,
      is_einv_dispatched RETURNING VALUE(rv_dispatched) TYPE abap_bool,

      get_conditions     RETURNING VALUE(rt_conditions) TYPE tt_condition.

    METHODS is_export RETURNING VALUE(result) TYPE abap_bool
                      RAISING   zcx_fi_company_code_def.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_multiton,
        vbeln TYPE vbeln_vf,
        obj   TYPE REF TO zcl_sd_invoice,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS vbeln,

      BEGIN OF t_lazy_flg,
        fi_docs      TYPE abap_bool,
        items        TYPE abap_bool,
        partners     TYPE abap_bool,
        conditions   TYPE abap_bool,
        earc_url     TYPE abap_bool,
        earc_url_tmp TYPE abap_bool,
        einv_url     TYPE abap_bool,
        einv_url_tmp TYPE abap_bool,
        e_url        TYPE abap_bool,
        is_export    TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        fi_docs      TYPE tt_bkpf_key,
        items        TYPE tt_item,
        partners     TYPE tt_partners,
        conditions   TYPE tt_condition,
        earc_url     TYPE string,
        earc_url_tmp TYPE string,
        einv_url     TYPE string,
        einv_url_tmp TYPE string,
        e_url        TYPE string,
        is_export    TYPE abap_bool,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy.

    CONSTANTS: awtyp            TYPE bkpf-awtyp VALUE 'VBRK',
               initial_posnr    TYPE posnr      VALUE '000000',
               tabname_head     TYPE tabname    VALUE 'VBRK',
               default_vkn      TYPE char11     VALUE '11111111111'.

    CONSTANTS: BEGIN OF earc_url_part,
                 vkn  TYPE text5 VALUE '{vkn}',
                 uuid TYPE text6 VALUE '{uuid}',
               END OF earc_url_part.

    CONSTANTS: BEGIN OF einv_url_part,
                 vkn  TYPE text5 VALUE '{vkn}',
                 uuid TYPE text6 VALUE '{uuid}',
               END OF einv_url_part.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA gs_lazy            TYPE t_lazy.
    DATA lazy_edoc_analyser TYPE REF TO zcl_sd_invoice_edoc_analyser.

    METHODS constructor
      IMPORTING iv_vbeln TYPE vbeln_vf
      RAISING   zcx_bc_table_content.

    METHODS get_edoc_analyser RETURNING VALUE(result) TYPE REF TO zcl_sd_invoice_edoc_analyser.
    METHODS get_earc_url_tmp  RETURNING VALUE(result) TYPE string.
    METHODS get_einv_url_tmp  RETURNING VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_sd_invoice IMPLEMENTATION.
  METHOD get_conditions.
    IF gs_lazy-flg-conditions = abap_false.

      SELECT knumv, kposn, stunr, zaehk, kschl, kbetr, waers "#EC CI_NOORDER
             FROM prcd_elements
             WHERE knumv = @gs_header-knumv
             INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-conditions.

      gs_lazy-flg-conditions = abap_true.
    ENDIF.

    rt_conditions = gs_lazy-val-conditions.
  ENDMETHOD.

  METHOD is_export.
    IF gs_lazy-flg-is_export = abap_false.
      DO 1 TIMES.
        DATA(partners) = get_partners( ).

        TRY.
            DATA(sold_to_partner) = REF #( partners[ parvw = c_parvw-sold_to ] ).
          CATCH cx_sy_itab_line_not_found.
            EXIT.
        ENDTRY.

        DATA(company) = zcl_fi_company=>get_instance( gs_header-bukrs ).

        gs_lazy-val-is_export = xsdbool( sold_to_partner->land1 <> company->gs_def-land1 ).
      ENDDO.

      gs_lazy-flg-is_export = abap_true.
    ENDIF.

    result = gs_lazy-val-is_export.
  ENDMETHOD.

  METHOD get_edoc_analyser.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy Initialization mantığında analiz sınıfı döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    result = me->lazy_edoc_analyser.

    IF result IS INITIAL.
      result = NEW #( me->gv_vbeln ).
    ENDIF.
  ENDMETHOD.

  METHOD get_earc_url.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Arşiv faturası varsa, bu faturaya ait URL'yi döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF gs_lazy-flg-earc_url = abap_false.
      DO 1 TIMES.
        DATA(earc_analysis) = analyse_earc( ).
        CHECK earc_analysis-fit_doc_found = abap_true.

        SELECT SINGLE id_vkn FROM /fite/arc_1_t004
               WHERE system_id = @earc_analysis-system_id
               INTO @DATA(vkn).

        IF vkn IS INITIAL.
          vkn = me->default_vkn.
        ENDIF.

        gs_lazy-val-earc_url = get_earc_url_tmp( ).
        CHECK gs_lazy-val-earc_url IS NOT INITIAL.

        REPLACE ALL OCCURRENCES OF: me->earc_url_part-vkn  IN gs_lazy-val-earc_url WITH vkn,
                                    me->earc_url_part-uuid IN gs_lazy-val-earc_url WITH earc_analysis-invoice_uuid.
      ENDDO.

      gs_lazy-flg-earc_url = abap_true.
    ENDIF.

    result = gs_lazy-val-earc_url.
  ENDMETHOD.

  METHOD get_einv_url.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-fatura faturası varsa, bu faturaya ait URL'yi döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF gs_lazy-flg-einv_url = abap_false.
      DO 1 TIMES.
        DATA(einv_analysis) = analyse_einv( ).
        CHECK einv_analysis-fit_doc_found = abap_true.

        SELECT SINGLE id_vkn FROM /fite/inv_1_t004
               WHERE system_id = @einv_analysis-system_id
               INTO @DATA(vkn).

        IF vkn IS INITIAL.
          vkn = me->default_vkn.
        ENDIF.

        gs_lazy-val-einv_url = get_einv_url_tmp( ).
        CHECK gs_lazy-val-einv_url IS NOT INITIAL.

        REPLACE ALL OCCURRENCES OF: me->einv_url_part-vkn  IN gs_lazy-val-einv_url WITH vkn,
                                    me->einv_url_part-uuid IN gs_lazy-val-einv_url WITH einv_analysis-invoice_uuid.
      ENDDO.

      gs_lazy-flg-einv_url = abap_true.
    ENDIF.

    result = gs_lazy-val-einv_url.
  ENDMETHOD.

  METHOD get_e_url.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Varsa, E-Arşiv fatura URL'sini döndürür
    " Varsa, E-Fatura fatura URL'sini döndürür
    " İkisi de yoksa, boşluk döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF gs_lazy-flg-e_url = abap_false.
      gs_lazy-val-e_url = get_earc_url( ).

      IF gs_lazy-val-e_url IS INITIAL.
        gs_lazy-val-e_url = get_einv_url( ).
      ENDIF.

      gs_lazy-flg-e_url = abap_true.
    ENDIF.

    result = gs_lazy-val-e_url.
  ENDMETHOD.

  METHOD constructor.
    DATA lv_vbeln TYPE vbeln_vf.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = iv_vbeln
      IMPORTING output = lv_vbeln.

    SELECT SINGLE vbeln, vkorg, vtweg, fksto, rfbsk, spart, fkart, knumv, bukrs
           FROM vbrk
           WHERE vbeln = @lv_vbeln
           INTO CORRESPONDING FIELDS OF @gs_header.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                objectid = CONV #( iv_vbeln )
                                                tabname  = me->tabname_head ).
    ENDIF.

    gv_vbeln = gs_header-vbeln.
  ENDMETHOD.

  METHOD get_fi_docs.
    IF gs_lazy-flg-fi_docs = abap_false.
      DATA(lv_awkey) = CONV bkpf-awkey( gv_vbeln ).

      SELECT bukrs, belnr, gjahr FROM bkpf
             WHERE awtyp = @me->awtyp
               AND awkey = @lv_awkey
             INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-fi_docs.

      gs_lazy-flg-fi_docs = abap_true.
    ENDIF.

    rt_key = gs_lazy-val-fi_docs.
  ENDMETHOD.

  METHOD wait_until_invoice_created.
    DO max_wait TIMES.
      SELECT SINGLE mandt FROM vbrk
             WHERE vbeln = @iv_vbeln
             INTO @sy-mandt ##WRITE_OK.

      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    RAISE EXCEPTION NEW zcx_sd_invoice( textid   = zcx_sd_invoice=>invoice_not_found
                                        vbeln_vf = iv_vbeln ).
  ENDMETHOD.

  METHOD wait_until_invoice_lockable.
    DO iv_max_wait TIMES.
      CALL FUNCTION 'ENQUEUE_EVVBRKE'
        EXPORTING  vbeln          = iv_vbeln
        EXCEPTIONS foreign_lock   = 1
                   system_failure = 2
                   OTHERS         = 3.

      CASE sy-subrc.
        WHEN 0.
          CALL FUNCTION 'DEQUEUE_EVVBRKE'
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
        CATCH zcx_bc_table_content INTO ls_mt-cx ##NO_HANDLER.
      ENDTRY.

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.
    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.
  ENDMETHOD.

  METHOD get_partners.
    IF gs_lazy-flg-partners = abap_false.
      SELECT DISTINCT parvw, kunnr, adrnr, land1
             FROM vbpa
             WHERE vbeln = @gv_vbeln
               AND posnr = @me->initial_posnr
             INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-partners.
    ENDIF.

    rt_partners = gs_lazy-val-partners.
  ENDMETHOD.

  METHOD get_earc_url_tmp.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Arşiv adresine ait şablonu döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->gs_lazy-flg-earc_url_tmp IS INITIAL.
      SELECT SINGLE url FROM zsdt_earc_url
             WHERE sysid = @sy-sysid
             INTO @me->gs_lazy-val-earc_url_tmp.

      me->gs_lazy-flg-earc_url_tmp = abap_true.
    ENDIF.

    result = me->gs_lazy-val-earc_url_tmp.
  ENDMETHOD.

  METHOD get_einv_url_tmp.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " E-Fatura adresine ait şablonu döndürür
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->gs_lazy-flg-einv_url_tmp IS INITIAL.
      SELECT SINGLE url FROM zsdt_einv_url
             WHERE sysid = @sy-sysid
             INTO @me->gs_lazy-val-einv_url_tmp.

      me->gs_lazy-flg-einv_url_tmp = abap_true.
    ENDIF.

    result = me->gs_lazy-val-einv_url_tmp.
  ENDMETHOD.

  METHOD get_items.
    IF gs_lazy-flg-items = abap_false.
      SELECT posnr, matnr, fkimg, vrkme, taxm1
             FROM vbrp
             WHERE vbeln = @gs_header-vbeln
             INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-items.

      gs_lazy-flg-items = abap_true.
    ENDIF.

    rt_item = gs_lazy-val-items.
  ENDMETHOD.

  METHOD analyse_earc.
    rs_result = get_edoc_analyser( )->analyse_earc( ).
  ENDMETHOD.

  METHOD analyse_einv.
    rs_result = get_edoc_analyser( )->analyse_einv( ).
  ENDMETHOD.

  METHOD analyse_e_.
    rs_result = get_edoc_analyser( )->analyse_e_( ).
  ENDMETHOD.

  METHOD is_earc_dispatched.
    rv_dispatched = get_edoc_analyser( )->is_earc_dispatched( ).
  ENDMETHOD.

  METHOD is_einv_dispatched.
    rv_dispatched = get_edoc_analyser( )->is_einv_dispatched( ).
  ENDMETHOD.
ENDCLASS.