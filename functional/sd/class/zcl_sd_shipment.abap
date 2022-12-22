CLASS zcl_sd_shipment DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_add_cost,
        add01   TYPE vttk-add01,
        add02   TYPE vttk-add02,
        distz   TYPE vttk-distz,
        gesztda TYPE vttk-gesztda,
        text3   TYPE vttk-text3,
      END OF t_add_cost,

      BEGIN OF t_head,
        tknum      TYPE vttk-tknum,
        dplbg      TYPE vttk-dplbg,
        vsart      TYPE vttk-vsart,
        exti1      TYPE vttk-exti1,
        exti2      TYPE vttk-exti2,
        tndr_trkid TYPE vttk-tndr_trkid,
        dpabf      TYPE vttk-dpabf,
        upabf      TYPE vttk-upabf,
        tdlnr      type vttk-tdlnr,
      END OF t_head,

      BEGIN OF t_item,
        tpnum    TYPE vttp-tpnum,
        vbeln    TYPE vttp-vbeln,
        delivery TYPE REF TO zcl_sd_delivery,
        dlv_cx   TYPE REF TO zcx_sd_delivery_def,
      END OF t_item,

      tt_item
        TYPE STANDARD TABLE OF t_item
        WITH DEFAULT KEY,

      tt_tknum
        TYPE STANDARD TABLE OF tknum
        WITH DEFAULT KEY,

      BEGIN OF t_txt_existence,
        tknum        TYPE vttk-tknum,
        chg_carrier  TYPE abap_bool,
        from_carrier TYPE abap_bool,
        to_carrier   TYPE abap_bool,
      END OF t_txt_existence,

      tt_txt_existence
        TYPE HASHED TABLE OF t_txt_existence
        WITH UNIQUE KEY primary_key COMPONENTS tknum,

      BEGIN OF t_vttk_key,
        tknum TYPE vttk-tknum,
      END OF t_vttk_key,

      tt_vttk_key
        TYPE STANDARD TABLE OF t_vttk_key
        WITH DEFAULT KEY,

      BEGIN OF t_vttp_key,
        tknum TYPE vttp-tknum,
        tpnum TYPE vttp-tpnum,
      END OF t_vttp_key,

      tt_vttp_key
        TYPE STANDARD TABLE OF t_vttp_key
        WITH DEFAULT KEY,

      tt_vttp_key_sort
        TYPE STANDARD TABLE OF t_vttp_key
        WITH DEFAULT KEY
        WITH NON-UNIQUE SORTED KEY k1 COMPONENTS tknum.

    CONSTANTS:
      c_bapi_flag_chg TYPE char1      VALUE 'C',
      c_bapi_flag_del TYPE char1      VALUE 'D',
      c_csv_sep       type ABAP_CHAR1 value CL_ABAP_CHAR_UTILITIES=>NEWLINE,
      c_tabname_item  TYPE tabname    VALUE 'VTTP'.

    DATA:
      gs_head TYPE t_head.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_tknum     TYPE tknum
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_shipment
        RAISING   zcx_sd_shipment_def,

      get_txt_existence
        IMPORTING !it_tknum           TYPE tt_tknum
        RETURNING VALUE(rt_existence) TYPE tt_txt_existence.

    METHODS:
      accept
        IMPORTING
          !io_visitor TYPE REF TO zif_sd_shipment_visitor
          !io_log     TYPE REF TO zcl_bc_applog_facade OPTIONAL
        RAISING
          zcx_bc_class_method,

      ensure_shipment_has_dlv
        IMPORTING !iv_vbeln TYPE vbeln_vl
        RAISING
                  zcx_sd_delivery_def
                  zcx_sd_shipment_dlv,

      get_items RETURNING VALUE(rt_item) TYPE tt_item,

      get_text_chg_carrier RETURNING VALUE(rt_text) TYPE tline_tab,
      get_text_from_carrier RETURNING VALUE(rt_text) TYPE tline_tab,
      get_text_to_carrier RETURNING VALUE(rt_text) TYPE tline_tab,

      set_add_cost
        IMPORTING
          !is_add_cost       TYPE t_add_cost
          !iv_managed_commit TYPE abap_bool DEFAULT abap_true
        EXPORTING
          !et_return         TYPE bapiret2_tab,

      set_vehicle
        IMPORTING
          !iv_exti1          TYPE vttk-exti1 OPTIONAL
          !iv_exti2          TYPE vttk-exti2 OPTIONAL
          !iv_tndr_trkid     TYPE vttk-tndr_trkid OPTIONAL
          !iv_managed_commit TYPE abap_bool DEFAULT abap_true
        EXPORTING
          !et_return         TYPE bapiret2_tab,

      set_shipment_end_date
        EXPORTING
          !et_return TYPE bapiret2_tab,

      set_string_from_carrier
        IMPORTING !iv_string TYPE string
        RAISING   zcx_bc_save_text,

      set_tline_from_carrier
        IMPORTING !it_text TYPE tline_tab
        RAISING   zcx_bc_save_text,

      set_string_to_carrier
        IMPORTING !iv_string TYPE string
        RAISING   zcx_bc_save_text,

      set_tline_to_carrier
        IMPORTING !it_text TYPE tline_tab
        RAISING   zcx_bc_save_text.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_deadline     TYPE STANDARD TABLE OF bapishipmentheaderdeadline WITH DEFAULT KEY,
      tt_deadline_act TYPE STANDARD TABLE OF bapishipmentheaderdeadlineact WITH DEFAULT KEY,

      BEGIN OF t_lazy_flg,
        item              TYPE abap_bool,
        text_from_carrier TYPE abap_bool,
        text_to_carrier   TYPE abap_bool,
        text_chg_carrier  TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        item              TYPE tt_item,
        text_from_carrier TYPE tline_tab,
        text_to_carrier   TYPE tline_tab,
        text_chg_carrier  TYPE tline_tab,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_multiton,
        tknum TYPE tknum,
        obj   TYPE REF TO zcl_sd_shipment,
        cx    TYPE REF TO zcx_sd_shipment_def,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS tknum,

      tt_tdname
        TYPE STANDARD TABLE OF stxh-tdname
        WITH DEFAULT KEY.

    CONSTANTS:
      c_meins_km               TYPE meins          VALUE 'KM',
      c_msgid_zsd              TYPE symsgid        VALUE 'ZSD',
      c_tabname_head           TYPE tabname        VALUE 'VTTK',
      c_tdid_text_chg_carrier  TYPE thead-tdid     VALUE 'Z003',
      c_tdid_text_from_carrier TYPE thead-tdid     VALUE 'Z002',
      c_tdid_text_to_carrier   TYPE thead-tdid     VALUE 'Z001',
      c_tdobject_head          TYPE thead-tdobject VALUE 'VTTK'.

    DATA:
      gs_lazy TYPE t_lazy.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !iv_tknum TYPE tknum
        RAISING   zcx_sd_shipment_def.

ENDCLASS.



CLASS zcl_sd_shipment IMPLEMENTATION.

  METHOD accept.
    io_visitor->visit(
      io_shipment = me
      io_log      = io_log
    ).
  ENDMETHOD.

  METHOD constructor.

    SELECT SINGLE
           tknum, dplbg, vsart, exti1, exti2, tndr_trkid,
           dpabf, upabf, tdlnr
           FROM vttk
           WHERE tknum = @iv_tknum
           INTO CORRESPONDING FIELDS OF @gs_head.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_sd_shipment_def
        EXPORTING
          tknum    = iv_tknum
          previous = NEW zcx_bc_table_content( textid    = zcx_bc_table_content=>entry_missing
                                               objectid  = CONV #( iv_tknum )
                                               tabname   = c_tabname_head ).
    ENDIF.
  ENDMETHOD.

  METHOD ensure_shipment_has_dlv.

    DATA(lt_item) = get_items( ).

    ASSIGN lt_item[
        vbeln = iv_vbeln
      ] TO FIELD-SYMBOL(<ls_item>).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_sd_shipment_dlv
        EXPORTING
          tknum = gs_head-tknum
          vbeln = iv_vbeln.
    ENDIF.

    IF <ls_item>-dlv_cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_item>-dlv_cx.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    DATA lv_tknum TYPE vttk-tknum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_tknum
      IMPORTING
        output = lv_tknum.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        tknum = lv_tknum
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.

      DATA(ls_mt) = VALUE t_multiton( tknum = lv_tknum ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-tknum ).
        CATCH zcx_sd_shipment_def INTO DATA(lo_def).
          ls_mt-cx = lo_def.
        CATCH cx_root INTO DATA(lo_diaper).
          ls_mt-cx = NEW zcx_sd_shipment_def(
              textid   = zcx_sd_shipment_def=>read_error
              previous = lo_diaper
              tknum    = ls_mt-tknum
          ).
      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

  METHOD get_items.

    IF gs_lazy-flg-item = abap_false.

      SELECT tpnum, vbeln
        FROM vttp
        WHERE tknum = @gs_head-tknum
        INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-item.

      LOOP AT gs_lazy-val-item ASSIGNING FIELD-SYMBOL(<ls_item>).

        TRY.
            <ls_item>-delivery = zcl_sd_delivery=>get_instance( <ls_item>-vbeln ).
          CATCH zcx_sd_delivery_def INTO <ls_item>-dlv_cx ##no_handler .
        ENDTRY.

      ENDLOOP.

      gs_lazy-flg-item = abap_true.

    ENDIF.

    rt_item = gs_lazy-val-item.

  ENDMETHOD.

  METHOD get_text_chg_carrier.

    IF gs_lazy-flg-text_chg_carrier = abap_false.

      DATA(lv_name) = CONV tdobname( gs_head-tknum ).

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = c_tdid_text_chg_carrier
          language                = sy-langu
          name                    = lv_name
          object                  = c_tdobject_head
        TABLES
          lines                   = gs_lazy-val-text_chg_carrier
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8
          ##FM_SUBRC_OK.

      gs_lazy-flg-text_chg_carrier = abap_true.

    ENDIF.

    rt_text = gs_lazy-val-text_chg_carrier.

  ENDMETHOD.

  METHOD get_text_from_carrier.

    IF gs_lazy-flg-text_from_carrier = abap_false.

      DATA(lv_name) = CONV tdobname( gs_head-tknum ).

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = c_tdid_text_from_carrier
          language                = sy-langu
          name                    = lv_name
          object                  = c_tdobject_head
        TABLES
          lines                   = gs_lazy-val-text_from_carrier
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8
          ##FM_SUBRC_OK.

      gs_lazy-flg-text_from_carrier = abap_true.

    ENDIF.

    rt_text = gs_lazy-val-text_from_carrier.

  ENDMETHOD.

  METHOD get_text_to_carrier.

    IF gs_lazy-flg-text_to_carrier = abap_false.

      DATA(lv_name) = CONV tdobname( gs_head-tknum ).

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = c_tdid_text_to_carrier
          language                = sy-langu
          name                    = lv_name
          object                  = c_tdobject_head
        TABLES
          lines                   = gs_lazy-val-text_to_carrier
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8
          ##FM_SUBRC_OK.

      gs_lazy-flg-text_to_carrier = abap_true.

    ENDIF.

    rt_text = gs_lazy-val-text_to_carrier.

  ENDMETHOD.

  METHOD get_txt_existence.

    CHECK it_tknum IS NOT INITIAL.

    DATA(lt_tdname) = VALUE tt_tdname(
      FOR lv_tknum IN it_tknum
      ( lv_tknum )
    ).

    SELECT tdname, tdid
      FROM stxh
      FOR ALL ENTRIES IN @lt_tdname
      WHERE
        tdobject = @c_tdobject_head AND
        tdname = @lt_tdname-table_line
      INTO TABLE @DATA(lt_stxh).

    LOOP AT lt_stxh ASSIGNING FIELD-SYMBOL(<ls_stxh>).

      ASSIGN rt_existence[
          KEY primary_key COMPONENTS
          tknum = <ls_stxh>-tdname
        ] TO FIELD-SYMBOL(<ls_ex>).

      IF sy-subrc <> 0.
        INSERT VALUE #(
            tknum = <ls_stxh>-tdname
          ) INTO TABLE rt_existence
          ASSIGNING <ls_ex>.
      ENDIF.

      CASE <ls_stxh>-tdid.
        WHEN c_tdid_text_chg_carrier  . <ls_ex>-chg_carrier  = abap_true.
        WHEN c_tdid_text_from_carrier . <ls_ex>-from_carrier = abap_true.
        WHEN c_tdid_text_to_carrier   . <ls_ex>-to_carrier   = abap_true.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_shipment_end_date.

    CLEAR et_return.

    DATA(ls_head) = VALUE bapishipmentheader(
      shipment_num   = gs_head-tknum
    ).

    DATA(ls_heada) = VALUE bapishipmentheaderaction( ).

    GET TIME STAMP FIELD DATA(lv_ts).

    DATA(lt_deadline) = VALUE tt_deadline( (
      time_type      = 'HDRSTSEPDT'
      time_stamp_utc = lv_ts
      time_zone      = sy-zonlo
    ) ).

    DATA(lt_deadline_act) = VALUE tt_deadline_act( (
      time_type      = c_bapi_flag_chg
      time_stamp_utc = c_bapi_flag_chg
      time_zone      = c_bapi_flag_chg
    ) ).

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata           = ls_head
        headerdataaction     = ls_heada
      TABLES
        headerdeadline       = lt_deadline
        headerdeadlineaction = lt_deadline_act
        return               = et_return.

  ENDMETHOD.

  METHOD set_string_from_carrier.

    DATA:
      lt_tline      TYPE tline_tab,
      lv_text(2000) TYPE c.

    lv_text = iv_string.

    CALL FUNCTION 'C14W_STRING_TO_TLINE'
      EXPORTING
        i_string    = lv_text
      TABLES
        e_tline_tab = lt_tline.

    MODIFY lt_tline
      FROM VALUE #( tdformat = '*' )
      TRANSPORTING tdformat
      WHERE tdformat <> '*'.

    set_tline_from_carrier( lt_tline ).

  ENDMETHOD.

  METHOD set_tline_from_carrier.

    DATA(ls_head) = VALUE thead(
      tdobject = c_tdobject_head
      tdid     = c_tdid_text_from_carrier
      tdname   = gs_head-tknum
      tdspras  = sy-langu
    ).

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = ls_head
        "insert          = abap_true
        savemode_direct = abap_true
      TABLES
        lines           = it_text
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5
        ##FM_SUBRC_OK.

    zcx_bc_save_text=>raise_if_sysubrc_not_initial( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD set_string_to_carrier.

    DATA:
      lt_tline      TYPE tline_tab,
      lv_text(2000) TYPE c.

    lv_text = iv_string.

    CALL FUNCTION 'C14W_STRING_TO_TLINE'
      EXPORTING
        i_string    = lv_text
      TABLES
        e_tline_tab = lt_tline.

    MODIFY lt_tline
      FROM VALUE #( tdformat = '*' )
      TRANSPORTING tdformat
      WHERE tdformat <> '*'.

    set_tline_to_carrier( lt_tline ).

  ENDMETHOD.

  METHOD set_tline_to_carrier.

    DATA(ls_head) = VALUE thead(
      tdobject = c_tdobject_head
      tdid     = c_tdid_text_to_carrier
      tdname   = gs_head-tknum
      tdspras  = sy-langu
    ).

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = ls_head
        "insert          = abap_true
        savemode_direct = abap_true
      TABLES
        lines           = it_text
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5
        ##FM_SUBRC_OK.

    zcx_bc_save_text=>raise_if_sysubrc_not_initial( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD set_add_cost.

    DATA lt_return TYPE bapiret2_tab.

    " ______________________________
    " Hazırlık ve kontroller

    CLEAR et_return.

    IF is_add_cost-gesztda IS NOT INITIAL AND
       is_add_cost-add02 IS INITIAL.

      APPEND VALUE #(
          type   = zcl_bc_applog_facade=>c_msgty_e
          id     = c_msgid_zsd
          number = '753'
        ) TO et_return
        ASSIGNING FIELD-SYMBOL(<ls_return>).

      MESSAGE
        ID     <ls_return>-id
        TYPE   <ls_return>-type
        NUMBER <ls_return>-number
        INTO   <ls_return>-message.

      RETURN.

    ENDIF.

    " ______________________________
    " BAPI çağır

    DATA(ls_head) = VALUE bapishipmentheader(
      shipment_num   = gs_head-tknum
      distance       = is_add_cost-distz
      distance_unit  = c_meins_km
      time_total_act = is_add_cost-gesztda
      suppl_2        = is_add_cost-add02
      suppl_1        = is_add_cost-add01
      text_3         = is_add_cost-text3
    ).

    DATA(ls_heada) = VALUE bapishipmentheaderaction(
      distance          = COND #( WHEN ls_head-distance IS INITIAL THEN c_bapi_flag_del ELSE c_bapi_flag_chg )
      distance_unit     = c_bapi_flag_chg
      distance_unit_iso = c_bapi_flag_chg
      time_total_act    = COND #( WHEN ls_head-time_total_act = '0' THEN c_bapi_flag_del ELSE c_bapi_flag_chg )
      suppl_2           = COND #( WHEN ls_head-suppl_2 IS INITIAL THEN c_bapi_flag_del ELSE c_bapi_flag_chg )
      suppl_1           = COND #( WHEN ls_head-suppl_1 IS INITIAL THEN c_bapi_flag_del ELSE c_bapi_flag_chg )
      text_3            = COND #( WHEN ls_head-text_3  IS INITIAL THEN c_bapi_flag_del ELSE c_bapi_flag_chg )
    ).

    " ______________________________
    " BAPI

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = ls_head
        headerdataaction = ls_heada
      TABLES
        return           = lt_return.

    APPEND LINES OF lt_return TO et_return.

    " ______________________________
    " İstendiyse, Commit'i yönet

    IF iv_managed_commit = abap_true.

      LOOP AT lt_return
        TRANSPORTING NO FIELDS
        WHERE type IN zcl_bc_applog_facade=>get_crit_msgty_range( ).

        ROLLBACK WORK.
        RETURN.

      ENDLOOP.

      COMMIT WORK AND WAIT.

    ENDIF.

  ENDMETHOD.

  METHOD set_vehicle.

    " ______________________________
    " Parametre hazırlığı

    CLEAR et_return.

    DATA(ls_head) = VALUE bapishipmentheader(
      shipment_num  = gs_head-tknum
      external_id_1 = iv_exti1
      external_id_2 = iv_exti2
      tendering_carrier_track_id = iv_tndr_trkid
    ).

    DATA(ls_heada) = VALUE bapishipmentheaderaction(
      external_id_1 = COND #(
        WHEN
          iv_exti1 IS SUPPLIED AND
          iv_exti1 IS NOT INITIAL
        THEN
          c_bapi_flag_chg
        WHEN
          iv_exti1 IS SUPPLIED AND
          iv_exti1 IS INITIAL
        THEN
          c_bapi_flag_del
        ELSE
          space
      )
      external_id_2 = COND #(
        WHEN
          iv_exti2 IS SUPPLIED AND
          iv_exti2 IS NOT INITIAL
        THEN
          c_bapi_flag_chg
        WHEN
          iv_exti2 IS SUPPLIED AND
          iv_exti2 IS INITIAL
        THEN
          c_bapi_flag_del
        ELSE
          space
      )
      tendering_carrier_track_id = COND #(
        WHEN
          iv_tndr_trkid IS SUPPLIED AND
          iv_tndr_trkid IS NOT INITIAL
        THEN
          c_bapi_flag_chg
        WHEN
          iv_tndr_trkid IS SUPPLIED AND
          iv_tndr_trkid IS INITIAL
        THEN
          c_bapi_flag_del
        ELSE
          space
      )
    ).

    " ______________________________
    " BAPI

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = ls_head
        headerdataaction = ls_heada
      TABLES
        return           = et_return.

    " ______________________________
    " İstendiyse, Commit'i yönet

    CHECK iv_managed_commit = abap_true.

    LOOP AT et_return
      TRANSPORTING NO FIELDS
      WHERE type IN zcl_bc_applog_facade=>get_crit_msgty_range( ).

      ROLLBACK WORK.
      RETURN.

    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.

ENDCLASS.