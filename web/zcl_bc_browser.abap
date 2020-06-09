CLASS zcl_bc_browser DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_list_param,
        wdsup TYPE RANGE OF zbct_browser-wdsup,
      END OF t_list_param,

      BEGIN OF t_mt,
        browser_id TYPE zbct_browser-browser_id,
        obj        TYPE REF TO zcl_bc_browser,
      END OF t_mt,

      tt_mt TYPE HASHED TABLE OF t_mt WITH UNIQUE KEY primary_key COMPONENTS browser_id.

    DATA gs_def TYPE zbct_browser.

    METHODS:
      accept
        IMPORTING !io_visitor TYPE REF TO zif_bc_browser_visitor
        RAISING   zcx_bc_class_method.

    CLASS-METHODS:
      get_browsers
        IMPORTING !is_param    TYPE t_list_param
        RETURNING VALUE(rt_mt) TYPE tt_mt
        RAISING   zcx_bc_table_content,

      get_instance
        IMPORTING !iv_id        TYPE zbct_browser-browser_id
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_browser
        RAISING   zcx_bc_table_content,

      get_wd_support_text
        RETURNING VALUE(rv_text) TYPE string
        RAISING   zcx_bc_table_content.



  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_clazy_flg,
        wd_support_text TYPE abap_bool,
      END OF t_clazy_flg,

      BEGIN OF t_clazy_val,
        wd_support_text TYPE string,
      END OF t_clazy_val,

      BEGIN OF t_clazy,
        flg TYPE t_clazy_flg,
        val TYPE t_clazy_val,
      END OF t_clazy.

    CONSTANTS:
      c_browser_placeholder TYPE char5 VALUE '_BRW_',
      c_tabname_def         TYPE tabname VALUE 'ZBCT_BROWSER'.

    CLASS-DATA:
      gs_clazy TYPE t_clazy,
      gt_mt    TYPE tt_mt.

ENDCLASS.



CLASS ZCL_BC_BROWSER IMPLEMENTATION.


  METHOD accept.
    io_visitor->visit( me ).
  ENDMETHOD.


  METHOD get_browsers.

    SELECT browser_id
      INTO TABLE @DATA(lt_id)
      FROM zbct_browser
      WHERE wdsup IN @is_param-wdsup.

    rt_mt = VALUE #( FOR ls_id IN lt_id (
      browser_id = ls_id-browser_id
      obj = get_instance( ls_id-browser_id )
    ) ).

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_mt[ KEY primary_key COMPONENTS browser_id = iv_id ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_mt( browser_id = iv_id ).

      ls_mt-obj = NEW #( ).

      SELECT SINGLE *
        INTO @ls_mt-obj->gs_def
        FROM zbct_browser WHERE
        browser_id EQ @ls_mt-browser_id.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_mt-browser_id )
            tabname  = c_tabname_def.
      ENDIF.

      INSERT ls_mt INTO TABLE gt_mt ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.


  METHOD get_wd_support_text.

    DATA:
      lv_brw TYPE text100,
      lv_msg TYPE text100.

    IF gs_clazy-flg-wd_support_text EQ abap_false.

      DATA(lt_browser) = get_browsers( VALUE #(
        wdsup = VALUE #( (  option = zcl_bc_ddic_toolkit=>c_option_eq
                            sign   = zcl_bc_ddic_toolkit=>c_sign_i
                            low    = abap_true
                       ) )
      ) ).

      CASE lines( lt_browser ).
        WHEN 0.
          gs_clazy-val-wd_support_text = space.
        WHEN 1.

          MESSAGE s282(zbc) INTO lv_msg.

          LOOP AT lt_browser ASSIGNING FIELD-SYMBOL(<ls_browser>).
            REPLACE ALL OCCURRENCES OF c_browser_placeholder
              IN lv_msg
              WITH <ls_browser>-obj->gs_def-browser_txt.

            EXIT.
          ENDLOOP.

          gs_clazy-val-wd_support_text = lv_msg.

        WHEN 2.

          CLEAR lv_brw.

          LOOP AT lt_browser ASSIGNING <ls_browser>.

            lv_brw = COND #(
              WHEN lv_brw IS INITIAL THEN <ls_browser>-obj->gs_def-browser_txt
              ELSE |{ lv_brw } { text-348 } { <ls_browser>-obj->gs_def-browser_txt }|
            ).

          ENDLOOP.

          MESSAGE s282(zbc) INTO lv_msg.

          REPLACE ALL OCCURRENCES OF c_browser_placeholder
              IN lv_msg
              WITH lv_brw.

          gs_clazy-val-wd_support_text = lv_msg.

        WHEN OTHERS.

          CLEAR lv_brw.

          LOOP AT lt_browser ASSIGNING <ls_browser>.

            lv_brw = COND #(
              WHEN lv_brw IS INITIAL THEN <ls_browser>-obj->gs_def-browser_txt
              ELSE |{ lv_brw }, { <ls_browser>-obj->gs_def-browser_txt }|
            ).

          ENDLOOP.

          MESSAGE s282(zbc) INTO lv_msg.

          REPLACE ALL OCCURRENCES OF c_browser_placeholder
              IN lv_msg
              WITH lv_brw.

          gs_clazy-val-wd_support_text = lv_msg.

      ENDCASE.

      gs_clazy-flg-wd_support_text = abap_true.
    ENDIF.

    rv_text = gs_clazy-val-wd_support_text.

  ENDMETHOD.
ENDCLASS.