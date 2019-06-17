CLASS zcl_sd_sales_order DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_vbak,
        vkorg TYPE vbak-vkorg,
        vtweg TYPE vbak-vtweg,
        spart TYPE vbak-spart,
      END OF t_vbak,

      BEGIN OF t_ord_dlv_map,
        vbeln_va TYPE vbeln_va,
        posnr_va TYPE posnr_va,
        vbeln_vl TYPE vbeln_vl,
        posnr_vl TYPE posnr_vl,
      END OF t_ord_dlv_map,

      tt_ord_dlv_map
        TYPE STANDARD TABLE OF t_ord_dlv_map
        WITH DEFAULT KEY,

      BEGIN OF t_ord_inv_map,
        vbeln_va TYPE vbeln_va,
        posnr_va TYPE posnr_va,
        vbeln_vl TYPE vbeln_vl,
        posnr_vl TYPE posnr_vl,
        vbeln_vf TYPE vbeln_vf,
        posnr_vf TYPE posnr_vf,
        fkart    TYPE fkart,
        kunrg    TYPE vbrk-kunrg,
      END OF t_ord_inv_map,

      tt_ord_inv_map
        TYPE STANDARD TABLE OF t_ord_inv_map
        WITH DEFAULT KEY,

      BEGIN OF t_partner,
        parvw TYPE vbpa-parvw,
        adrnr TYPE vbpa-adrnr,
        name1 TYPE adrc-name1,
        name2 TYPE adrc-name2,
        name3 TYPE adrc-name3,
        name4 TYPE adrc-name4,
      END OF t_partner.

    DATA:
      gv_vbeln TYPE vbeln_va READ-ONLY.

    CONSTANTS:
      BEGIN OF c_parvw,
        payer TYPE parvw VALUE 'RG',
      END OF c_parvw.

    CLASS-METHODS:
      get_instance
        IMPORTING
          !iv_vbeln     TYPE vbeln_va
          !iv_wait      TYPE i OPTIONAL
        RETURNING
          VALUE(ro_obj) TYPE REF TO zcl_sd_sales_order
        RAISING
          zcx_bc_table_content.

    METHODS:
      ensure_item_exists
        IMPORTING !iv_posnr_va TYPE vbap-posnr
        RAISING   cx_no_entry_in_table,

      get_deliveries
        RETURNING VALUE(rt_map) TYPE tt_ord_dlv_map,

      get_header
        RETURNING VALUE(rs_vbak) TYPE t_vbak,

      get_invoices
        RETURNING VALUE(rt_map) TYPE tt_ord_inv_map,

      get_partner
        IMPORTING !iv_parvw         TYPE parvw
        RETURNING VALUE(rs_partner) TYPE t_partner
        RAISING   zcx_bc_table_content,

      get_sole_item_number
        RETURNING VALUE(rv_posnr) TYPE posnr_va
        RAISING   zcx_bc_table_content,

      is_invoiced RETURNING VALUE(rv_invoiced) TYPE abap_bool,
      is_scope_fat_ayristi RETURNING VALUE(rv_tek) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        vbeln TYPE vbeln_va,
        obj   TYPE REF TO zcl_sd_sales_order,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS vbeln,

      BEGIN OF t_vbap,
        posnr TYPE vbap-posnr,
      END OF t_vbap,

      tt_vbap
        TYPE STANDARD TABLE OF t_vbap
        WITH DEFAULT KEY,

      tt_partner
        TYPE HASHED TABLE OF t_partner
        WITH UNIQUE KEY primary_key COMPONENTS parvw,

      BEGIN OF t_lazy_flg,
        dlv      TYPE abap_bool,
        inv      TYPE abap_bool,
        partners TYPE abap_bool,
        sfa      TYPE abap_bool,
        vbak     TYPE abap_bool,
        vbap     TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        dlv      TYPE tt_ord_dlv_map,
        inv      TYPE tt_ord_inv_map,
        partners TYPE tt_partner,
        sfa      TYPE abap_bool,
        vbak     TYPE t_vbak,
        vbap     TYPE tt_vbap,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      tt_fkart_rng TYPE RANGE OF fkart.

    CONSTANTS:
      BEGIN OF c_tabname,
        head    TYPE tabname VALUE 'VBAK',
        item    TYPE tabname VALUE 'VBAP',
        partner TYPE tabname VALUE 'VBPA',
      END OF c_tabname,

      BEGIN OF c_vbtyp,
        dlv TYPE vbtyp VALUE 'J',
        inv TYPE vbtyp VALUE 'M',
      END OF c_vbtyp.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA gs_lazy TYPE t_lazy.

    METHODS:
      read_partners_lazy,
      read_vbak_lazy,
      read_vbap_lazy.

ENDCLASS.

CLASS zcl_sd_sales_order IMPLEMENTATION.

  METHOD ensure_item_exists.

    read_vbap_lazy( ).

    IF NOT line_exists( gs_lazy-val-vbap[ posnr = iv_posnr_va ] ).
      RAISE EXCEPTION TYPE cx_no_entry_in_table
        EXPORTING
          table_name = CONV #( c_tabname-item )
          entry_name = CONV #( iv_posnr_va ).
    ENDIF.

  ENDMETHOD.

  METHOD get_deliveries.

    IF gs_lazy-flg-dlv IS INITIAL.

      SELECT
          vbelv AS vbeln_va,
          posnv AS posnr_va,
          vbeln AS vbeln_vl,
          posnn AS posnr_vl
        FROM vbfa
        WHERE
          vbelv   EQ @gv_vbeln AND
          vbtyp_n EQ @c_vbtyp-dlv
        INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-dlv.

      gs_lazy-flg-dlv = abap_true.

    ENDIF.

    rt_map = gs_lazy-val-dlv.

  ENDMETHOD.

  METHOD get_header.
    read_vbak_lazy( ).
    rs_vbak = gs_lazy-val-vbak.
  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        vbeln = iv_vbeln
      ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton(
        vbeln = iv_vbeln
      ).

      ls_multiton-obj = NEW #( ).

      DATA(lv_waited_sec) = 0.

      DO.

        SELECT SINGLE vbeln
          FROM vbak
          WHERE vbeln EQ @ls_multiton-vbeln
          INTO @ls_multiton-obj->gv_vbeln.

        IF sy-subrc EQ 0.
          EXIT.
        ELSE.
          ADD 1 TO lv_waited_sec.

          IF lv_waited_sec GT iv_wait.

            ls_multiton-cx = NEW #(
              textid   = zcx_bc_table_content=>entry_missing
              objectid = CONV #( ls_multiton-vbeln )
              tabname  = c_tabname-head
            ).

            EXIT.
          ENDIF.

          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

      INSERT ls_multiton
        INTO TABLE gt_multiton
        ASSIGNING <ls_multiton>.

    ENDIF.

    IF <ls_multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_multiton>-cx.
    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.

  METHOD get_invoices.

    IF gs_lazy-flg-inv IS INITIAL.

      DATA(lt_dlv) = get_deliveries( ).

      IF lt_dlv IS NOT INITIAL.

        SELECT
            vbfa~vbelv,
            vbfa~posnv,
            vbfa~vbeln,
            vbfa~posnn,
            vbrk~fkart,
            vbrk~kunrg
          FROM
            vbfa
            INNER JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
          FOR ALL ENTRIES IN @lt_dlv
          WHERE
            vbelv   EQ @lt_dlv-vbeln_vl AND
            posnv   EQ @lt_dlv-posnr_vl AND
            vbtyp_n EQ @c_vbtyp-inv
          INTO TABLE @DATA(lt_vbfa).

        LOOP AT lt_dlv ASSIGNING FIELD-SYMBOL(<ls_dlv>).

          LOOP AT lt_vbfa
            ASSIGNING FIELD-SYMBOL(<ls_vbfa>)
            WHERE
              vbelv EQ <ls_dlv>-vbeln_vl AND
              posnv EQ <ls_dlv>-posnr_vl.

            APPEND VALUE #(
                vbeln_va = <ls_dlv>-vbeln_va
                posnr_va = <ls_dlv>-posnr_va
                vbeln_vl = <ls_dlv>-vbeln_vl
                posnr_vl = <ls_dlv>-posnr_vl
                vbeln_vf = <ls_vbfa>-vbeln
                posnr_vf = <ls_vbfa>-posnn
                fkart    = <ls_vbfa>-fkart
                kunrg    = <ls_vbfa>-kunrg
              ) TO gs_lazy-val-inv.

          ENDLOOP.
        ENDLOOP.

      ENDIF.

      gs_lazy-flg-inv = abap_true.

    ENDIF.

    rt_map = gs_lazy-val-inv.

  ENDMETHOD.

  METHOD get_partner.

    read_partners_lazy( ).

    ASSIGN gs_lazy-val-partners[
        KEY primary_key COMPONENTS
        parvw = iv_parvw
      ] TO FIELD-SYMBOL(<ls_partner>).

    IF sy-subrc EQ 0.
      rs_partner = <ls_partner>.
    ELSE.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = |{ gv_vbeln } { iv_parvw }|
          tabname  = c_tabname-partner.
    ENDIF.

  ENDMETHOD.

  METHOD get_sole_item_number.

    read_vbap_lazy( ).

    CASE lines( gs_lazy-val-vbap ).
      WHEN 0.

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( gv_vbeln )
            tabname  = c_tabname-item.

      WHEN 1.
        rv_posnr = gs_lazy-val-vbap[ 1 ]-posnr.

      WHEN OTHERS.

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>multiple_entries_for_key
            objectid = CONV #( gv_vbeln )
            tabname  = c_tabname-item.

    ENDCASE.

  ENDMETHOD.

  METHOD is_invoiced.
    rv_invoiced = xsdbool(  get_invoices( ) IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_scope_fat_ayristi.

    IF gs_lazy-flg-sfa IS INITIAL.

      DATA(lt_inv) = get_invoices( ).

      IF lt_inv IS NOT INITIAL.

        SELECT mandt
          FROM zsdt_fat_ayristi
          FOR ALL ENTRIES IN @lt_inv
          WHERE
            fkart EQ @lt_inv-fkart AND
            kunnr EQ @lt_inv-kunrg
          INTO TABLE @DATA(lt_dummy).

        gs_lazy-val-sfa = xsdbool( lt_dummy IS NOT INITIAL ).

      ENDIF.

      gs_lazy-flg-sfa = abap_true.

    ENDIF.

    rv_tek = gs_lazy-val-sfa.

  ENDMETHOD.

  METHOD read_partners_lazy.

    DATA lv_posnr_initial TYPE vbpa-posnr.

    CHECK gs_lazy-flg-partners EQ abap_false.

    SELECT DISTINCT
        vbpa~parvw, vbpa~adrnr,
        adrc~name1, adrc~name2, adrc~name3, adrc~name4
      FROM
        vbpa
        INNER JOIN adrc ON adrc~addrnumber EQ vbpa~adrnr
      WHERE
        vbpa~vbeln     EQ @gv_vbeln AND
        vbpa~posnr     EQ @lv_posnr_initial AND
        adrc~date_from LE @sy-datum AND
        adrc~date_to   GE @sy-datum
      INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-partners.

    gs_lazy-flg-partners = abap_true.

  ENDMETHOD.

  METHOD read_vbak_lazy.

    CHECK gs_lazy-flg-vbak EQ abap_false.

    SELECT SINGLE vkorg, vtweg, spart
      INTO CORRESPONDING FIELDS OF @gs_lazy-val-vbak
      FROM vbak
      WHERE vbeln EQ @gv_vbeln.

    ASSERT sy-subrc EQ 0.

    gs_lazy-flg-vbak = abap_true.

  ENDMETHOD.

  METHOD read_vbap_lazy.

    CHECK gs_lazy-flg-vbap EQ abap_false.

    SELECT posnr
      FROM vbap
      WHERE vbeln EQ @gv_vbeln
      INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-vbap.

    ASSERT sy-subrc EQ 0.

    gs_lazy-flg-vbap = abap_true.

  ENDMETHOD.

ENDCLASS.