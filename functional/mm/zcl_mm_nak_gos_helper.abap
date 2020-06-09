CLASS zcl_mm_nak_gos_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      attach_dlv_gos_doc
        importing
          !io_shipment    TYPE REF TO zcl_sd_shipment
          !iv_tpnum       TYPE vttp-tpnum
          !iv_filename    type clike
          !iv_description type clike
          !iv_hex_String  type xstring
          !iv_docty       type zmmd_nak_docty
        raising
          cx_Sy_itab_line_not_found
          zcx_bc_gos_doc_attach
          zcx_sd_delivery_def,

      del_dlv_gos_doc
        importing
          !io_shipment    TYPE REF TO zcl_sd_shipment
          !iv_tpnum       TYPE vttp-tpnum
          !is_docid       type SO_ENTRYID
        raising
          cx_Sy_itab_line_not_found
          zcx_bc_gos_doc_delete
          zcx_sd_delivery_def,

      get_dlv_gos_doc_content
        importing
          !io_shipment  TYPE REF TO zcl_sd_shipment
          !iv_tpnum     TYPE vttp-tpnum
          !iv_docid     type SO_ENTRYID
        exporting
          !es_data      type ZMMS_NAK_TESLIMAT_DOSYA
          !ev_content   type xstring
        raising
          zcx_bc_gos_doc_content
          zcx_sd_delivery_def,

      get_dlv_gos_docs
        IMPORTING
          !io_shipment  TYPE REF TO zcl_sd_shipment
          !iv_tpnum     TYPE vttp-tpnum
          !it_docty_rng type ZMM_TT_DOCTY_RANGE optional
        returning
          value(rt_docs) type ZMMTT_NAK_TESLIMAT_DOSYA
        RAISING
          zcx_bc_gos_doc_content
          zcx_sd_delivery_def.

  PROTECTED SECTION.
  PRIVATE SECTION.

    class-methods:
      convert_doc_content_to_ntd
        importing
          !iv_tknum type tknum
          !iv_tpnum type tpnum
          !iv_vbeln type vbeln_vl
          !is_doc   type zcl_bc_gos_toolkit=>t_doc_content
        returning
          value(rs_ntd) type zmms_nak_teslimat_dosya,

      get_ship_itm_gos_docs
        IMPORTING
          !io_shipment  TYPE REF TO zcl_sd_shipment
          !iv_tpnum     TYPE vttp-tpnum
        exporting
          !et_doc  type zcl_Bc_gos_toolkit=>tt_doc_content
          !es_item type zcl_sd_Shipment=>t_item
        RAISING
          zcx_bc_gos_doc_content
          zcx_sd_delivery_def.

ENDCLASS.



CLASS zcl_mm_nak_gos_helper IMPLEMENTATION.

  method attach_dlv_gos_doc.

    DATA(lt_item) = io_shipment->get_items( ).

    data(ls_item) = lt_item[ tpnum = iv_tpnum ].

    zcl_sd_delivery=>get_instance(
        ls_item-vbeln
      )->attach_gos_doc(
        iv_filename    = iv_filename
        iv_description = |{ iv_docty }:{ iv_filename }|
        iv_hex_string  = iv_Hex_String
      ).

  endmethod.

  method del_dlv_gos_doc.

    DATA(lt_item) = io_shipment->get_items( ).

    data(ls_item) = lt_item[ tpnum = iv_tpnum ].

    zcl_sd_delivery=>get_instance(
        ls_item-vbeln
      )->del_gos_doc( is_docid ).

  endmethod.

  method convert_doc_content_to_ntd.

    rs_ntd = value #(
      tknum    = iv_tknum
      tpnum    = iv_tpnum
      vbeln    = iv_vbeln
      filename = is_doc-filename
      mimetype = is_doc-mime_type
      doc_id   = is_doc-doc_data-doc_id
      docty    = is_doc-doc_data-obj_descr+0(3)
    ).

  endmethod.

  method get_dlv_gos_doc_content.

    clear:
      es_data,
      ev_content.

    get_ship_itm_gos_docs(
      EXPORTING
        io_shipment = io_shipment
        iv_tpnum    = iv_tpnum
      IMPORTING
        et_doc      = data(lt_doc)
        es_item     = data(ls_ship_itm)
    ).

    assign lt_doc[
        doc_Data-doc_id = iv_docid
      ] to field-symbol(<ls_doc>).

    check sy-subrc eq 0.

    ev_content = <ls_doc>-hex_string.

    es_data = convert_doc_content_to_ntd(
      iv_tknum = io_shipment->gs_head-tknum
      iv_tpnum = iv_tpnum
      iv_vbeln = ls_ship_itm-vbeln
      is_doc   = <ls_doc>
    ).

  endmethod.

  method get_ship_itm_gos_docs.

    clear:
      es_item,
      et_doc.

    DATA(lt_item) = io_shipment->get_items( ).

    try.
        es_item = lt_item[ tpnum = iv_tpnum ].
      catch cx_Sy_itab_line_not_found.
        return.
    endtry.

    et_doc = zcl_sd_delivery=>get_instance(
        es_item-vbeln
      )->get_gos_docs( ).

  endmethod.

  METHOD get_dlv_gos_docs.

    get_ship_itm_gos_docs(
      EXPORTING
        io_shipment = io_shipment
        iv_tpnum    = iv_tpnum
      IMPORTING
        et_doc      = data(lt_doc)
        es_item     = data(ls_ship_itm)
    ).

    rt_docs = value #(
      for ls_doc in lt_doc (
        convert_doc_content_to_ntd(
          iv_tknum = io_shipment->gs_head-tknum
          iv_tpnum = iv_tpnum
          iv_vbeln = ls_ship_itm-vbeln
          is_doc   = ls_doc
        )
      )
    ).

    delete rt_docs where not docty in it_docty_rng.

  ENDMETHOD.

ENDCLASS.