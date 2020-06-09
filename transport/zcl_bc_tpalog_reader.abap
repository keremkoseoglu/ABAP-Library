CLASS zcl_bc_tpalog_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:

      BEGIN OF t_date_range,
        begda TYPE begda,
        endda TYPE endda,
      END OF t_date_range,

      BEGIN OF t_gekod,
        gekod TYPE zbcd_jira_gekod,
      END OF t_gekod,

      tt_gekod TYPE STANDARD TABLE OF t_gekod WITH DEFAULT KEY,

      BEGIN OF t_list,
        trkorr     TYPE trkorr,
        as4text    TYPE as4text,
        trfunction TYPE trfunction,
        tarsystem  type tarsystem,
        status     TYPE zbcd_tcr_devst,
        trdate     TYPE dats,
        trtime     TYPE tims,
        as4user    type tr_as4user,
      END OF t_list,

      tt_list TYPE STANDARD TABLE OF t_list WITH DEFAULT KEY,

      BEGIN OF t_sys_data,
        system TYPE fieldname,
        list   TYPE tt_list,
      END OF t_sys_data,

      tt_sys_data
        TYPE HASHED TABLE OF t_sys_data
        WITH UNIQUE KEY primary_key COMPONENTS system.

    CLASS-METHODS format_gekod_tab_input
      CHANGING
        !ct_gekod TYPE tt_gekod.

    METHODS get_list
      IMPORTING
        !iv_sysnam      TYPE tmscsys-sysnam
        !it_trkorr_rng  TYPE cts_organizer_tt_wd_request OPTIONAL
        !it_gekod       TYPE tt_gekod OPTIONAL
        !it_sys_data    TYPE tt_sys_data OPTIONAL
        !is_date_range  TYPE t_date_range OPTIONAL
        !iv_read_master TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_list)  TYPE tt_list
      RAISING
        zcx_bc_tpalog_read.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_master,
        trkorr     TYPE e070-trkorr,
        trfunction TYPE e070-trfunction,
        tarsystem  type e070-tarsystem,
        as4text    TYPE e07t-as4text,
      END OF t_master,

      tt_master   TYPE HASHED TABLE OF t_master WITH UNIQUE KEY primary_key COMPONENTS trkorr,

      tt_tpalog   TYPE STANDARD TABLE OF tpalog WITH DEFAULT KEY,
      tt_tpalog_s TYPE STANDARD TABLE OF tpalog WITH DEFAULT KEY WITH NON-UNIQUE SORTED KEY k1 COMPONENTS trkorr retcode,

      BEGIN OF t_trkorr,
        trkorr TYPE tpalog-trkorr,
        trtime TYPE tpalog-trtime,
      END OF t_trkorr,

      tt_trkorr TYPE STANDARD TABLE OF t_trkorr WITH DEFAULT KEY.

    CONSTANTS c_trstep_main_imp TYPE tpalog-trstep VALUE 'I'.

    DATA:
      gs_date_range TYPE t_date_range,
      gt_list       TYPE zcl_bc_tpalog_reader=>tt_list,
      gt_sys_data   TYPE zcl_bc_tpalog_reader=>tt_sys_data,
      gt_tpalog     TYPE tt_tpalog_s,
      gt_trkorr_rng TYPE cts_organizer_tt_wd_request,
      gv_sysnam     TYPE tmscsys-sysnam.

    METHODS:
      get_req_date
        IMPORTING !iv_trtime     TYPE tpalog-trtime
        RETURNING VALUE(rv_date) TYPE dats,

      get_req_status
        IMPORTING !iv_trkorr       TYPE tpalog-trkorr
        RETURNING VALUE(rv_status) TYPE zbcd_tcr_devst,

      get_req_time
        IMPORTING !iv_trtime     TYPE tpalog-trtime
        RETURNING VALUE(rv_date) TYPE tims,

      parse_tpalog,

      read_master,

      read_tpalog
        RAISING
          zcx_bc_tpalog_read,

      req_has_retcode
        IMPORTING
          !iv_trkorr    TYPE tpalog-trkorr
          !iv_retcode   TYPE tpalog-retcode
        RETURNING
          VALUE(rv_has) TYPE abap_bool.

ENDCLASS.



CLASS zcl_bc_tpalog_reader IMPLEMENTATION.


  METHOD format_gekod_tab_input.

    LOOP AT ct_gekod ASSIGNING FIELD-SYMBOL(<ls_gekod_imp>).
      CONDENSE <ls_gekod_imp>-gekod.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_list.

    CLEAR gt_list.

    gs_date_range = is_date_range.
    gt_sys_data   = it_sys_data.
    gt_trkorr_rng = it_trkorr_rng.
    gv_sysnam     = iv_sysnam.

    read_tpalog( ).
    parse_tpalog( ).

    IF iv_read_master EQ abap_true.
      read_master( ).
    ENDIF.

    rt_list = gt_list.

  ENDMETHOD.


  METHOD get_req_date.
    rv_date = iv_trtime+0(8).
  ENDMETHOD.


  METHOD get_req_status.

    rv_status = COND #(
      WHEN req_has_retcode(
        iv_trkorr  = iv_trkorr
        iv_retcode = '0000'
      ) EQ abap_true
      THEN zcl_bc_tcr_model=>c_status_imported

      WHEN req_has_retcode(
        iv_trkorr  = iv_trkorr
        iv_retcode = '0004'
      ) EQ abap_true
      THEN zcl_bc_tcr_model=>c_status_imported

      WHEN req_has_retcode(
        iv_trkorr  = iv_trkorr
        iv_retcode = '0008'
      ) EQ abap_true
      THEN zcl_bc_tcr_model=>c_status_error

      WHEN req_has_retcode(
        iv_trkorr  = iv_trkorr
        iv_retcode = '0012'
      ) EQ abap_true
      THEN zcl_bc_tcr_model=>c_status_error

      ELSE zcl_bc_tcr_model=>c_status_unknown
    ).

  ENDMETHOD.


  METHOD get_req_time.
    rv_date = iv_trtime+8(6).
  ENDMETHOD.


  METHOD parse_tpalog.

*   Hazırlık
    CLEAR gt_list.

*   Tekrarsız Request listesi
    DATA(lt_trkorr) = CORRESPONDING tt_trkorr( gt_tpalog ).
    SORT lt_trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr.

*   Her bir Request için durumu belirleyelim

    gt_list = VALUE #( FOR ls_tr IN lt_trkorr (
      trkorr = ls_tr-trkorr
      status = get_req_status( ls_tr-trkorr )
      trdate = get_req_date( ls_tr-trtime )
      trtime = get_req_time( ls_tr-trtime )
    ) ).

  ENDMETHOD.


  METHOD read_master.

    CHECK gt_list IS NOT INITIAL.

    DATA(lt_master) = VALUE tt_master( ).

    SELECT
            e070~trkorr, e070~trfunction, e070~tarsystem,
            e07t~as4text
        INTO CORRESPONDING FIELDS OF TABLE @lt_master
        FROM
            e070
            LEFT JOIN e07t ON
                e07t~trkorr EQ e070~trkorr AND
                e07t~langu EQ @sy-langu
        FOR ALL ENTRIES IN @gt_list
        WHERE e070~trkorr EQ @gt_list-trkorr.


    LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<ls_list>).

      ASSIGN lt_master[
          KEY primary_key
          COMPONENTS trkorr = <ls_list>-trkorr
      ] TO FIELD-SYMBOL(<ls_master>).

      CHECK sy-subrc EQ 0.

      <ls_list>-as4text = <ls_master>-as4text.
      <ls_list>-trfunction = <ls_master>-trfunction.
      <ls_list>-tarsystem = <ls_master>-tarsystem.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_tpalog.

    DATA:
      lt_dat    TYPE STANDARD TABLE OF tab512,
      lt_fld    TYPE STANDARD TABLE OF rfc_db_fld,
      lt_opt    TYPE STANDARD TABLE OF rfc_db_opt,
      lt_tpalog TYPE tt_tpalog,
      lt_trkorr TYPE tt_trkorr.

    TRY.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " Hazırlık
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        CLEAR gt_tpalog[].

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " Okunacak Request'leri WHERE koşulu olarak ekle (varsa)
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        ASSIGN gt_sys_data[
          KEY primary_key
          COMPONENTS system = zcl_bc_tcr_model=>c_fld_devst
        ] TO FIELD-SYMBOL(<ls_sys_data>).

        IF sy-subrc EQ 0.
          APPEND LINES OF CORRESPONDING tt_trkorr( <ls_sys_data>-list ) TO lt_trkorr.
        ENDIF.

        IF gt_trkorr_rng IS NOT INITIAL.
          SELECT trkorr
            APPENDING CORRESPONDING FIELDS OF TABLE lt_trkorr
            FROM e070
            WHERE trkorr IN gt_trkorr_rng
            ##TOO_MANY_ITAB_FIELDS .
        ENDIF.

        DATA(lv_trkorr_appended) = abap_false.

        LOOP AT lt_trkorr ASSIGNING FIELD-SYMBOL(<ls_trkorr>).

          IF lv_trkorr_appended EQ abap_false.
            APPEND VALUE #( text = '(' ) TO lt_opt.
            lv_trkorr_appended = abap_true.
          ELSE.
            APPEND VALUE #( text = 'OR' ) TO lt_opt.
          ENDIF.

          APPEND VALUE #( text = |TRKORR = '{ <ls_trkorr>-trkorr }'| ) TO lt_opt.

        ENDLOOP.

        IF lv_trkorr_appended EQ abap_true.
          APPEND VALUE #( text = ')' ) TO lt_opt.
        ENDIF.


        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " Tarih aralığını WHERE koşulu olarak ekle (varsa)
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        IF gs_date_range IS NOT INITIAL.

          ASSERT gs_date_range-begda IS NOT INITIAL AND
                 gs_date_range-endda IS NOT INITIAL AND
                 gs_date_range-begda LE gs_date_range-endda.

          IF lt_opt IS INITIAL.
            APPEND VALUE #( text = '(' ) TO lt_opt.
          ELSE.
            APPEND VALUE #( text = 'AND (' ) TO lt_opt.
          ENDIF.

          APPEND VALUE #( text = |TRTIME >= { gs_date_range-begda }000000| ) TO lt_opt.
          APPEND VALUE #( text = |AND TRTIME <= { gs_date_range-endda }235959| ) TO lt_opt.

          APPEND VALUE #( text = ')' ) TO lt_opt.

        ENDIF.


        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " Oku
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



        CALL FUNCTION 'RFC_READ_TABLE' DESTINATION gv_sysnam
          EXPORTING
            query_table          = 'TPALOG'
          TABLES
            options              = lt_opt
            fields               = lt_fld
            data                 = lt_dat
          EXCEPTIONS
            table_not_available  = 1
            table_without_data   = 2
            option_not_valid     = 3
            field_not_valid      = 4
            not_authorized       = 5
            data_buffer_exceeded = 6
            OTHERS               = 7 ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'RFC_READ_TABLE' ).

        lt_tpalog = lt_dat.

        gt_tpalog = VALUE #( FOR ls_t IN lt_tpalog WHERE ( trstep EQ c_trstep_main_imp ) ( ls_t ) ).

      CATCH zcx_bc_tpalog_read INTO DATA(lo_tr).
        RAISE EXCEPTION lo_tr.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_tpalog_read
          EXPORTING
            textid   = zcx_bc_tpalog_read=>read_error
            previous = lo_diaper
            sysnam   = gv_sysnam.

    ENDTRY.

  ENDMETHOD.


  METHOD req_has_retcode.

    LOOP AT gt_tpalog
      TRANSPORTING NO FIELDS
      USING KEY k1
      WHERE
        trkorr  EQ iv_trkorr AND
        retcode EQ iv_retcode.

      rv_has = abap_true.
      RETURN.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.