CLASS zcl_sd_sales_doc_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_auart TYPE STANDARD TABLE OF auart WITH DEFAULT KEY.

    DATA:
      gs_def TYPE tvak READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_auart     TYPE auart
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_sales_doc_type
        RAISING   zcx_bc_table_content,

      proactive_cache IMPORTING !it_auart TYPE tt_auart optional.

    METHODS:
      get_text RETURNING VALUE(rv_bezei) TYPE tvakt-bezei.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flg,
        bezei TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        bezei TYPE tvakt-bezei,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_multiton,
        auart TYPE auart,
        obj   TYPE REF TO zcl_sd_sales_doc_type,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS auart.

    CONSTANTS c_tabname_def TYPE tabname VALUE 'TVAK'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA gs_lazy TYPE t_lazy.

    METHODS:
      constructor
        IMPORTING
          !is_def   TYPE tvak
          !iv_bezei TYPE tvakt-bezei OPTIONAL,

      set_text IMPORTING !iv_bezei TYPE tvakt-bezei.

ENDCLASS.

CLASS zcl_sd_sales_doc_type IMPLEMENTATION.

  METHOD constructor.
    gs_def = is_def.

    IF iv_bezei IS SUPPLIED.
      set_text( iv_bezei ).
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        auart = iv_auart
      ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( auart = iv_auart ).

      SELECT SINGLE *
        FROM tvak
        WHERE auart EQ @ls_multiton-auart
        INTO @DATA(ls_def).

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-auart )
            tabname  = c_tabname_def.
      ENDIF.

      ls_multiton-obj = NEW #( ls_def ).

      INSERT ls_multiton
        INTO TABLE gt_multiton
        ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.

  METHOD get_text.

    IF gs_lazy-flg-bezei EQ abap_false.
      SELECT SINGLE bezei
        FROM tvakt
        WHERE (
          spras EQ @sy-langu AND
          auart EQ @gs_def-auart
        )
        INTO @gs_lazy-val-bezei.

      gs_lazy-flg-bezei = abap_true.
    ENDIF.

    rv_bezei = gs_lazy-val-bezei.

  ENDMETHOD.

  METHOD proactive_cache.

    DATA lt_auart TYPE tt_auart.

    LOOP AT it_auart ASSIGNING FIELD-SYMBOL(<lv_auart>).
      CHECK NOT line_exists( gt_multiton[ auart = <lv_auart> ] ).
      APPEND <lv_auart> TO lt_auart.
    ENDLOOP.

    SELECT
        tvak~*,
        tvakt~bezei
      FROM
        tvak
        LEFT JOIN tvakt ON
          tvakt~spras EQ @sy-langu AND
          tvakt~auart EQ tvak~auart
      FOR ALL ENTRIES IN @lt_auart
      WHERE tvak~auart EQ @lt_auart-table_line
      INTO TABLE @DATA(lt_tvak).

    loop at lt_tvak assigning field-symbol(<ls_tvak>).

      check not line_exists( gt_multiton[ auart = <ls_tvak>-tvak-auart ] ).

      insert value #(
          auart = <ls_tvak>-tvak-auart
          obj   = NEW #(
              is_def   = <ls_tvak>-tvak
              iv_bezei = <ls_tvak>-bezei
            )
        ) into table gt_multiton.

    endloop.

  ENDMETHOD.

  METHOD set_text.
    gs_lazy-flg-bezei = abap_true.
    gs_lazy-val-bezei = iv_bezei.
  ENDMETHOD.

ENDCLASS.