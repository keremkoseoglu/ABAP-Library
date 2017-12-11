CLASS zcl_sd_shipment DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_head,
        tknum      TYPE vttk-tknum,
        dplbg      TYPE vttk-dplbg,
        vsart      TYPE vttk-vsart,
        exti1      TYPE vttk-exti1,
        exti2      TYPE vttk-exti2,
        tndr_trkid TYPE vttk-tndr_trkid,
      END OF t_head,

      BEGIN OF t_item,
        tpnum    TYPE vttp-tpnum,
        vbeln    TYPE vttp-vbeln,
        delivery TYPE REF TO zcl_sd_delivery,
        dlv_cx   TYPE REF TO zcx_sd_delivery_def,
      END OF t_item,

      tt_item
        TYPE STANDARD TABLE OF t_item
        WITH DEFAULT KEY.

    DATA:
      gs_head TYPE t_head.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_tknum     TYPE tknum
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_shipment
        RAISING   zcx_sd_shipment_def.

    METHODS:
      accept
        importing !io_visitor type ref to zif_sd_Shipment_visitor
        raising   zcx_Bc_class_method,

      get_items RETURNING VALUE(rt_item) TYPE tt_item,

      get_text_from_carrier RETURNING VALUE(rt_text) TYPE tline_tab,
      get_text_to_carrier RETURNING VALUE(rt_text) TYPE tline_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flg,
        item              TYPE abap_bool,
        text_from_carrier TYPE abap_bool,
        text_to_carrier   TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        item              TYPE tt_item,
        text_from_carrier TYPE tline_tab,
        text_to_carrier   TYPE tline_tab,
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
        WITH UNIQUE KEY primary_key COMPONENTS tknum.

    CONSTANTS:
      c_tabname_head           TYPE tabname VALUE 'VTTK',
      c_tdid_text_from_carrier TYPE thead-tdid VALUE 'Z02',
      c_tdid_text_to_carrier   TYPE thead-tdid VALUE 'Z01',
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

  method accept.
    io_visitor->visit( me ).
  endmethod.

  METHOD constructor.

    SELECT SINGLE
      tknum, dplbg, vsart, exti1, exti2, tndr_trkid
      FROM vttk
      WHERE tknum EQ @iv_tknum
      INTO CORRESPONDING FIELDS OF @gs_head.

    IF sy-subrc NE 0.

      RAISE EXCEPTION TYPE zcx_sd_shipment_def
        EXPORTING
          tknum    = iv_tknum
          previous = NEW zcx_bc_table_content(
            textid    = zcx_bc_table_content=>entry_missing
            objectid  = CONV #( iv_tknum )
            tabname   = c_tabname_head
          ).

    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        tknum = iv_tknum
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( tknum = iv_tknum ).

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

    IF gs_lazy-flg-item EQ abap_false.

      SELECT tpnum, vbeln
        FROM vttp
        WHERE tknum EQ @gs_head-tknum
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

  METHOD get_text_from_carrier.

    IF gs_lazy-flg-text_from_carrier EQ abap_false.

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

    IF gs_lazy-flg-text_to_carrier EQ abap_false.

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

ENDCLASS.