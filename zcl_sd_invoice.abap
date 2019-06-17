CLASS zcl_sd_invoice DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_bkpf_key,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
      END OF t_bkpf_key,

      tt_bkpf_key TYPE STANDARD TABLE OF t_bkpf_key WITH DEFAULT KEY,
      tt_vbeln    TYPE STANDARD TABLE OF vbeln_vf   WITH DEFAULT KEY.

    DATA:
      gv_vbeln TYPE vbeln_vf.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_vbeln     TYPE vbeln_vf
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_invoice
        RAISING   zcx_bc_table_content.

    METHODS:
      get_fi_docs RETURNING VALUE(rt_key) TYPE tt_bkpf_key,

      is_earc_dispatched RETURNING VALUE(rv_dispatched) TYPE abap_bool,
      is_einv_dispatched RETURNING VALUE(rv_dispatched) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        vbeln TYPE vbeln_vf,
        obj   TYPE REF TO zcl_sd_invoice,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS vbeln,

      BEGIN OF t_lazy_flg,
        fi_docs TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        fi_docs TYPE tt_bkpf_key,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      tt_system_id_rng TYPE RANGE OF /fite/arc_1_t004-system_id.

    CONSTANTS:
      c_awtyp         TYPE bkpf-awtyp                  VALUE 'VBRK',
      c_inv_module_sd TYPE /fite/arc_1_t001-inv_module VALUE 'SD',
      c_tabname_head  TYPE tabname                     VALUE 'VBRK'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    DATA:
      gs_lazy TYPE t_lazy.

    METHODS:
      constructor
        IMPORTING !iv_vbeln TYPE vbeln_vf
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_sd_invoice IMPLEMENTATION.

  METHOD constructor.

    DATA lv_vbeln TYPE vbeln_vf.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_vbeln
      IMPORTING
        output = lv_vbeln.

    SELECT SINGLE vbeln
      FROM vbrk
      WHERE vbeln EQ @lv_vbeln
      INTO @gv_vbeln.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_vbeln )
          tabname  = c_tabname_head.
    ENDIF.

  ENDMETHOD.

  METHOD get_fi_docs.

    IF gs_lazy-flg-fi_docs EQ abap_false.

      DATA(lv_awkey) = CONV bkpf-awkey( gv_vbeln ).

      SELECT bukrs, belnr, gjahr
        FROM bkpf
        WHERE
          awtyp EQ @c_awtyp AND
          awkey EQ @lv_awkey
        INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-fi_docs.

      gs_lazy-flg-fi_docs = abap_true.

    ENDIF.

    rt_key = gs_lazy-val-fi_docs.

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
        CATCH zcx_bc_table_content INTO ls_mt-cx ##no_handler .
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

  METHOD is_earc_dispatched.

    DATA lt_system_id_rng TYPE tt_system_id_rng.

    SELECT
        @zcl_bc_ddic_toolkit=>c_sign_i    AS sign,
        @zcl_bc_ddic_toolkit=>c_option_eq AS option,
        _a4~system_id AS low
      FROM
        vbrk
        INNER JOIN /fite/arc_1_t004 AS _a4 ON _a4~bukrs EQ vbrk~bukrs
      WHERE vbrk~vbeln EQ @gv_vbeln
      INTO CORRESPONDING FIELDS OF TABLE @lt_system_id_rng
      ##too_many_itab_fields.

    IF lt_system_id_rng IS INITIAL.
      RETURN.
    ENDIF.

    SELECT status
      FROM /fite/arc_1_t001
      WHERE
        system_id      IN @lt_system_id_rng AND
        invoice_number EQ @gv_vbeln AND
        inv_module     EQ @c_inv_module_sd
      ORDER BY
        cre_date DESCENDING,
        cre_time DESCENDING
      INTO TABLE @DATA(lt_statu)
      UP TO 1 ROWS.

    IF lt_statu IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN lt_statu[ 1 ] TO FIELD-SYMBOL(<ls_statu>).

    rv_dispatched = xsdbool(
      <ls_statu>-status EQ '1' OR
      <ls_statu>-status EQ '2' OR
      <ls_statu>-status EQ '3'
    ).

  ENDMETHOD.

  METHOD is_einv_dispatched.

    DATA lt_system_id_rng TYPE tt_system_id_rng.

    SELECT
        @zcl_bc_ddic_toolkit=>c_sign_i    AS sign,
        @zcl_bc_ddic_toolkit=>c_option_eq AS option,
        _a4~system_id AS low
      FROM
        vbrk
        INNER JOIN /fite/inv_1_t004 AS _a4 ON _a4~bukrs EQ vbrk~bukrs
      WHERE vbrk~vbeln EQ @gv_vbeln
      INTO CORRESPONDING FIELDS OF TABLE @lt_system_id_rng
      ##too_many_itab_fields.

    IF lt_system_id_rng IS INITIAL.
      RETURN.
    ENDIF.

    SELECT status
      FROM /fite/inv_1_t001
      WHERE
        system_id      IN @lt_system_id_rng AND
        invoice_number EQ @gv_vbeln AND
        inv_module     EQ @c_inv_module_sd
      ORDER BY
        cre_date DESCENDING,
        cre_time DESCENDING
      INTO TABLE @DATA(lt_statu)
      UP TO 1 ROWS.

    IF lt_statu IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN lt_statu[ 1 ] TO FIELD-SYMBOL(<ls_statu>).

    rv_dispatched = xsdbool( <ls_statu>-status EQ '2' ).

  ENDMETHOD.

ENDCLASS.