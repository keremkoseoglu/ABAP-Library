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
        vstel    type likp-vstel,
      END OF t_head,

      BEGIN OF t_item,
        posnr       TYPE lips-posnr,
        lgort       TYPE lips-lgort,
        werks       TYPE lips-werks,
        storage_loc TYPE REF TO zcl_sd_storage_location,
        stloc_cx    TYPE REF TO zcx_sd_stloc_def,
      END OF t_item,

      tt_item
        TYPE STANDARD TABLE OF t_item
        WITH DEFAULT KEY.

    DATA:
      gs_head TYPE t_head.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_vbeln     TYPE likp-vbeln
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_delivery
        RAISING   zcx_sd_delivery_def.

    METHODS:
      attach_gos_doc
        importing
          !iv_filename    type clike
          !iv_description type clike
          !iv_hex_String  type xstring
        RAISING
          zcx_bc_gos_doc_attach,

      del_gos_doc
        importing
          !is_docid type SO_ENTRYID
        raising
          zcx_bc_gos_doc_delete,

      get_gos_docs
        returning value(rt_doc) type zcl_Bc_gos_toolkit=>tt_doc_content
        raising   zcx_bc_gos_doc_content,

      get_items
        RETURNING VALUE(rt_item) TYPE tt_item.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flg,
        item TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        item TYPE tt_item,
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

    constants:
      c_gos_classname type bapibds01-classname value 'LIKP'.

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

  method attach_gos_doc.

    zcl_bc_gos_toolkit=>attach_doc(
      iv_filename    = iv_filename
      iv_description = iv_description
      iv_hex_string  = iv_hex_string
      is_key = value #(
        classname = c_gos_classname
        objkey    = gs_head-vbeln
      )
    ).

  endmethod.

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

  method del_gos_doc.

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
        is_key = value #(
          classname = c_gos_classname
          objkey    = gs_head-vbeln
        )
      ).

  endmethod.

  method get_gos_docs.

    rt_doc = zcl_bc_gos_toolkit=>get_doc_content(
      value #(
         classname = c_gos_classname
         objkey    = gs_head-vbeln
      )
    ).

  endmethod.

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

      SELECT posnr, lgort, werks
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

ENDCLASS.