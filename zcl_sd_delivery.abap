CLASS zcl_sd_delivery DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_head,
        vbeln    TYPE likp-vbeln,
        kunnr    TYPE likp-kunnr,
        customer TYPE REF TO zcl_sd_customer,
        btgew    TYPE likp-btgew,
        gewei    TYPE likp-gewei,
        vstel    TYPE likp-vstel,
      END OF t_head,

      BEGIN OF t_item,
        posnr       TYPE lips-posnr,
        lgort       TYPE lips-lgort,
        werks       TYPE lips-werks,
        storage_loc TYPE REF TO zcl_sd_storage_location,
        stloc_cx    TYPE REF TO zcx_sd_stloc_def,
        vgbel       TYPE lips-vgbel,
      END OF t_item,

      tt_item
        TYPE STANDARD TABLE OF t_item
        WITH DEFAULT KEY,

      BEGIN OF t_weight,
        btgew TYPE likp-btgew,
        gewei TYPE likp-gewei,
      END OF t_weight,

      tt_vbeln TYPE STANDARD TABLE OF vbeln WITH DEFAULT KEY.

    DATA:
      gs_head TYPE t_head.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_vbeln     TYPE likp-vbeln
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_delivery
        RAISING   zcx_sd_delivery_def,

      get_total_weight_of_deliveries
        IMPORTING
          !it_vbeln       TYPE tt_vbeln
          !iv_gewei       TYPE likp-gewei
        RETURNING
          VALUE(rv_btgew) TYPE likp-btgew
        RAISING
          zcx_bc_function_subrc
          zcx_sd_delivery_def.

    METHODS:
      attach_gos_doc
        IMPORTING
          !iv_filename    TYPE clike
          !iv_description TYPE clike
          !iv_hex_string  TYPE xstring
        RAISING
          zcx_bc_gos_doc_attach,

      del_gos_doc
        IMPORTING
          !is_docid TYPE so_entryid
        RAISING
          zcx_bc_gos_doc_delete,

      get_gos_docs
        RETURNING VALUE(rt_doc) TYPE zcl_bc_gos_toolkit=>tt_doc_content
        RAISING   zcx_bc_gos_doc_content,

      get_items
        RETURNING VALUE(rt_item) TYPE tt_item,

      get_orders RETURNING VALUE(rt_vbeln) TYPE tt_vbeln,

      get_total_weight
        RETURNING VALUE(rs_weight) TYPE t_weight
        RAISING   zcx_bc_function_subrc.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flg,
        item         TYPE abap_bool,
        orders       TYPE abap_bool,
        total_weight TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        item         TYPE tt_item,
        orders       TYPE tt_vbeln,
        total_weight TYPE t_weight,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_multiton,
        vbeln TYPE likp-vbeln,
        cx    TYPE REF TO zcx_sd_delivery_def,
        obj   TYPE REF TO zcl_sd_delivery,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS vbeln.

    CONSTANTS:
      BEGIN OF c_pkstk,
        completed TYPE pkstk VALUE 'C',
      END OF c_pkstk,

      BEGIN OF c_vpobj,
        delivery TYPE vpobj VALUE '01',
      END OF c_vpobj,

      c_gos_classname TYPE bapibds01-classname VALUE 'LIKP'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    DATA:
      gs_lazy TYPE t_lazy.

    METHODS:
      constructor
        IMPORTING !iv_vbeln TYPE likp-vbeln
        RAISING   zcx_sd_delivery_def.

ENDCLASS.



CLASS zcl_sd_delivery IMPLEMENTATION.

  METHOD attach_gos_doc.

    zcl_bc_gos_toolkit=>attach_doc(
      iv_filename    = iv_filename
      iv_description = iv_description
      iv_hex_string  = iv_hex_string
      is_key = VALUE #(
        classname = c_gos_classname
        objkey    = gs_head-vbeln
      )
    ).

  ENDMETHOD.

  METHOD constructor.

    TRY.

        SELECT SINGLE vbeln, kunnr, btgew, gewei, vstel
          FROM likp
          WHERE vbeln EQ @iv_vbeln
          INTO CORRESPONDING FIELDS OF @gs_head.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_sd_delivery_def
            EXPORTING
              vbeln = iv_vbeln.
        ENDIF.

        gs_head-customer = zcl_sd_customer=>get_instance( gs_head-kunnr ).

      CATCH zcx_sd_delivery_def INTO DATA(lo_def).
        RAISE EXCEPTION lo_def.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_sd_delivery_def
          EXPORTING
            textid   = zcx_sd_delivery_def=>invalid_data
            previous = lo_diaper
            vbeln    = iv_vbeln.

    ENDTRY.

  ENDMETHOD.

  METHOD del_gos_doc.

    zcl_bc_gos_toolkit=>delete_doc(
      is_folder_id = VALUE #(
        objtp = is_docid+0(3)
        objyr = is_docid+3(2)
        objno = is_docid+5(12)
      )
      is_object_id = VALUE #(
        objtp = is_docid+17(3)
        objyr = is_docid+20(2)
        objno = is_docid+22(12)
      )
      is_key = VALUE #(
        classname = c_gos_classname
        objkey    = gs_head-vbeln
      )
    ).

  ENDMETHOD.

  METHOD get_gos_docs.

    rt_doc = zcl_bc_gos_toolkit=>get_doc_content(
      VALUE #(
         classname = c_gos_classname
         objkey    = gs_head-vbeln
      )
    ).

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        vbeln = iv_vbeln
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( vbeln = iv_vbeln ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-vbeln ).
        CATCH zcx_sd_delivery_def INTO ls_mt-cx ##no_Handler.
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

      SELECT posnr, lgort, werks, vgbel
        FROM lips
        WHERE vbeln EQ @gs_head-vbeln
        INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-item
        ##too_many_itab_fields.

      LOOP AT gs_lazy-val-item ASSIGNING FIELD-SYMBOL(<ls_item>).

        TRY.
            <ls_item>-storage_loc = zcl_sd_storage_location=>get_instance(
              iv_werks = <ls_item>-werks
              iv_lgort = <ls_item>-lgort
            ).

          CATCH zcx_sd_stloc_def INTO <ls_item>-stloc_cx ##no_handler .
        ENDTRY.

      ENDLOOP.

      gs_lazy-flg-item = abap_true.

    ENDIF.

    rt_item = gs_lazy-val-item.

  ENDMETHOD.

  METHOD get_orders.

    IF gs_lazy-flg-orders IS INITIAL.

      gs_lazy-val-orders = VALUE #(
        FOR GROUPS _vgbel OF _item IN get_items( )
        GROUP BY _item-vgbel
        ( _vgbel )
      ).

      SORT gs_lazy-val-orders.
      DELETE ADJACENT DUPLICATES FROM gs_lazy-val-orders.
      DELETE gs_lazy-val-orders WHERE table_line IS INITIAL.

      gs_lazy-flg-orders = abap_true.
    ENDIF.

    rt_vbeln = gs_lazy-val-orders.

  ENDMETHOD.

  METHOD get_total_weight.

    IF gs_lazy-flg-total_weight IS INITIAL.

      gs_lazy-val-total_weight-btgew = gs_head-btgew.
      gs_lazy-val-total_weight-gewei = gs_head-gewei.

      DATA(lv_vpobjkey) = CONV vpobjkey( gs_head-vbeln ).

      SELECT tarag, gewei
        FROM vekp
        WHERE
          vpobj    EQ @c_vpobj-delivery AND
          vpobjkey EQ @lv_vpobjkey
        INTO TABLE @DATA(lt_vekp).

      LOOP AT lt_vekp ASSIGNING FIELD-SYMBOL(<ls_vekp>).
        IF <ls_vekp>-gewei EQ gs_lazy-val-total_weight-gewei.
          ADD <ls_vekp>-tarag TO gs_lazy-val-total_weight-btgew.
        ELSE.
          DATA(lv_tarag_in_likp_gewei) = CONV vekp-tarag( 0 ).

          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input                = <ls_vekp>-tarag
              unit_in              = <ls_vekp>-gewei
              unit_out             = gs_lazy-val-total_weight-gewei
            IMPORTING
              output               = lv_tarag_in_likp_gewei
            EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10
              ##FM_SUBRC_OK.

          zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).
          ADD lv_tarag_in_likp_gewei TO gs_lazy-val-total_weight-btgew.

        ENDIF.

      ENDLOOP.


      gs_lazy-flg-total_weight = abap_true.
    ENDIF.

    rs_weight = gs_lazy-val-total_weight.

  ENDMETHOD.

  METHOD get_total_weight_of_deliveries.

    LOOP AT it_vbeln ASSIGNING FIELD-SYMBOL(<lv_vbeln>).

      DATA(ls_weight) = zcl_sd_delivery=>get_instance( <lv_vbeln> )->get_total_weight( ).

      DATA(lv_dlv_weight_converted) = CONV likp-btgew( 0 ).

      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = ls_weight-btgew
          unit_in              = ls_weight-gewei
          unit_out             = iv_gewei
        IMPORTING
          output               = lv_dlv_weight_converted
        EXCEPTIONS
          conversion_not_found = 1
          division_by_zero     = 2
          input_invalid        = 3
          output_invalid       = 4
          overflow             = 5
          type_invalid         = 6
          units_missing        = 7
          unit_in_not_found    = 8
          unit_out_not_found   = 9
          OTHERS               = 10
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).

      ADD lv_dlv_weight_converted TO rv_btgew.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.