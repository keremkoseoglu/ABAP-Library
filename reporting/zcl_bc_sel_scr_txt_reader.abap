CLASS zcl_bc_sel_scr_txt_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_sel_scr,
        fnam TYPE textpoolky,
        ftxt TYPE textpooltx,
      END OF t_sel_scr,

      tt_sel_scr TYPE STANDARD TABLE OF t_sel_scr WITH DEFAULT KEY.

    CLASS-METHODS get_instance RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_sel_scr_txt_reader.

    METHODS get_sel_scr
      IMPORTING
        !iv_repid    TYPE sy-repid
        !iv_tabname  TYPE tabname OPTIONAL
      RETURNING
        VALUE(rt_ss) TYPE tt_sel_scr.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_field_cache,
        repid   TYPE sy-repid,
        sel_scr TYPE tt_sel_scr,
      END OF t_field_cache,

      tt_field_cache
        TYPE HASHED TABLE OF t_field_cache
        WITH UNIQUE KEY primary_key COMPONENTS repid,

      tt_rsdynpar    TYPE STANDARD TABLE OF rsdynpar WITH DEFAULT KEY,
      tt_rsel_info   TYPE STANDARD TABLE OF rsel_info WITH DEFAULT KEY.

    CLASS-DATA go_singleton TYPE REF TO zcl_bc_sel_scr_txt_reader.

    CONSTANTS:
      c_entry_empty     TYPE textpooltx VALUE 'D       .',
      c_tabname_default TYPE tabname    VALUE 'TADIR'.

    DATA gt_field_cache TYPE tt_field_cache.

ENDCLASS.



CLASS ZCL_BC_SEL_SCR_TXT_READER IMPLEMENTATION.


  METHOD get_instance.

    IF go_singleton IS INITIAL.
      go_singleton = NEW #(  ).
    ENDIF.

    ro_obj = go_singleton.

  ENDMETHOD.


  METHOD get_sel_scr.

    DATA:
      lt_field_info TYPE tt_rsel_info,
      lt_field_name TYPE tt_rsdynpar,
      lt_seltext    TYPE fc00_t_sel_screen_textpool.

    ASSIGN gt_field_cache[
      KEY primary_key
      COMPONENTS repid = iv_repid
    ] TO FIELD-SYMBOL(<ls_field_cache>).

    IF sy-subrc NE 0.

      DATA(ls_field_cache) = VALUE t_field_cache( repid = iv_repid ).

      DATA(lv_tabname) = COND #(
        WHEN iv_tabname IS INITIAL THEN c_tabname_default
        ELSE iv_tabname
      ).

      CALL FUNCTION 'RS_REPORTSELECTSCREEN_INFO'
        EXPORTING
          report              = iv_repid
        TABLES
          field_info          = lt_field_info
          field_names         = lt_field_name
        EXCEPTIONS
          no_selections       = 1
          report_not_existent = 2
          subroutine_pool     = 3
          OTHERS              = 4
          ##FM_SUBRC_OK.

      SORT lt_field_info BY name.

      CALL FUNCTION 'FC_SEL_SCREEN_SELTEXTS_READ'
        EXPORTING
          e_repid    = ls_field_cache-repid
          e_tabname  = lv_tabname
        IMPORTING
          it_seltext = lt_seltext.

      LOOP AT lt_seltext ASSIGNING FIELD-SYMBOL(<ls_seltext>).

        APPEND VALUE #(
          fnam = <ls_seltext>-key
        ) TO ls_Field_Cache-sel_scr ASSIGNING FIELD-SYMBOL(<ls_ss>).

        IF <ls_seltext>-entry EQ c_entry_empty.

          READ TABLE lt_field_info
            ASSIGNING FIELD-SYMBOL(<ls_field_info>)
            WITH KEY name = <ls_ss>-fnam
            BINARY SEARCH.

          IF sy-subrc EQ 0 AND
             <ls_field_info>-dbfield IS NOT INITIAL.

            <ls_ss>-ftxt = zcl_bc_abap_table=>get_dbfield_text( <ls_field_info>-dbfield ).

          ENDIF.

        ELSE.
          <ls_ss>-ftxt = <ls_seltext>-entry.
        ENDIF.

        CHECK <ls_ss>-ftxt IS INITIAL.
        <ls_ss>-ftxt = <ls_ss>-fnam.

      ENDLOOP.

      INSERT ls_field_cache INTO TABLE gt_field_cache ASSIGNING <ls_field_cache>.

    ENDIF.

    rt_ss = <ls_field_cache>-sel_scr.

  ENDMETHOD.
ENDCLASS.