class ZCL_BC_ALV definition
  public
  final
  create public .

public section.

  types:
    begin of ty_mapping,
            fname type slis_fieldcat_alv-fieldname,
            fname_to type slis_fieldcat_alv-fieldname,
          end of ty_mapping .
  types:
    tt_mapping type standard table of ty_mapping .

  data GS_LAYOUT type SLIS_LAYOUT_ALV .
  data GS_VARIANT type DISVARIANT .
  data GT_FCAT type SLIS_T_FIELDCAT_ALV .
  data GS_FCAT type SLIS_FIELDCAT_ALV .
  data GV_ITAB_NAME type DD02L-TABNAME .
  data GV_INCL_NAME type SY-REPID .
  data GV_PROG_NAME type SY-REPID .
  data GV_STRC_NAME type DD02L-TABNAME .
  data GV_STAT_NAME type SLIS_FORMNAME .
  data GV_COMD_NAME type SLIS_FORMNAME .
  class-data C_SAVE_A type CHAR1 value 'A' ##NO_TEXT.
  class-data C_SAVE_U type CHAR1 value 'U' ##NO_TEXT.
  data GV_TOPP_NAME type SLIS_FORMNAME .
  data GV_HTML_END_NAME type SLIS_FORMNAME .
  class-data COLOR_RED type CHAR4 value 'C610' ##NO_TEXT.
  class-data COLOR_GREEN type CHAR4 value 'C510' ##NO_TEXT.
  class-data COLOR_OPEN_GREEN type CHAR4 value 'C500' ##NO_TEXT.
  class-data COLOR_YELLOW type CHAR4 value 'C310' ##NO_TEXT.
  class-data COLOR_OPEN_YELLOW type CHAR4 value 'C300' ##NO_TEXT.
  class-data C_COMMAND_CLICK type SY-UCOMM value '&IC1' ##NO_TEXT.
  class-data GT_MAPPING type TT_MAPPING .
  class-data COLOR_BLUE type CHAR4 value 'C110' ##NO_TEXT.
  class-data COLOR_BEJ type CHAR4 value 'C200' ##NO_TEXT.
  class-data COLOR_OPEN_BLUE type CHAR4 value 'C410' ##NO_TEXT.
  class-data COLOR_ORANGE type CHAR4 value 'C710' ##NO_TEXT.
  class-data COLOR_BLUE0 type CHAR4 value 'C100' ##NO_TEXT.
  class-data COLOR_BEJ0 type CHAR4 value 'C200' ##NO_TEXT.
  class-data COLOR_OPEN_BLUE0 type CHAR4 value 'C400' ##NO_TEXT.
  class-data COLOR_ORANGE0 type CHAR4 value 'C700' ##NO_TEXT.
  constants C_TOP type SLIS_ALV_EVENT-FORM value 'TOP_OF_PAGE' ##NO_TEXT.
  CONSTANTS : BEGIN OF c_edit,
               enable  type c VALUE 'E',
               disable type c VALUE 'D',
              END OF c_edit.
  methods CONSTRUCTOR
    importing
      value(IV_ITAB_NAME) type DD02L-TABNAME optional
      value(IV_STRC_NAME) type DD02L-TABNAME optional
      value(IV_INCL_NAME) type SY-REPID optional
      value(IV_PROG_NAME) type SY-REPID optional
      value(IV_STAT_NAME) type SLIS_FORMNAME optional
      value(IV_COMD_NAME) type SLIS_FORMNAME optional
      value(IV_TOPP_NAME) type SLIS_FORMNAME optional
      value(IV_HTML_END_NAME) type SLIS_FORMNAME optional
      value(IS_LAYOUT) type SLIS_LAYOUT_ALV optional
      value(IR_DATA) type ref to DATA optional
      value(IS_VARIANT) type DISVARIANT optional .
  methods S_MERGE
    importing
      !IV_STR_NAME type DD02L-TABNAME optional
    changing
      !RT_FCAT type SLIS_T_FIELDCAT_ALV optional .
  methods T_MERGE
    importing
      !IV_ITAB_NAME type DD02L-TABNAME optional
      !IV_INCL_NAME type SY-REPID optional
      !IV_PROG_NAME type SY-REPID optional
    changing
      !RT_FCAT type SLIS_T_FIELDCAT_ALV optional .
  methods SET_DEFAULT_LAYOUT
    returning
      value(RS_LAYOUT) type SLIS_LAYOUT_ALV .
  methods DISPLAY
    importing
      !TABLE type ref to DATA
      value(IS_VARIANT) type DISVARIANT optional
      value(IS_TOP_OF_PAGE) type FLAG default SPACE
      value(IV_DEFAULT) type FLAG default 'X' .
  methods SET_FCAT
    importing
      value(T_FCAT) type SLIS_T_FIELDCAT_ALV .
  methods GET_FCAT
    returning
      value(T_FCAT) type SLIS_T_FIELDCAT_ALV .
  methods SET_LAYOUT
    importing
      value(IS_LAYOUT) type SLIS_LAYOUT_ALV .
  methods SET_HOTSPOT
    importing
      !IV_FIELDNAME type SLIS_FIELDCAT_ALV-FIELDNAME .
  class-methods CALL_TRANSACTION
    importing
      value(IS_SELFIELD) type SLIS_SELFIELD
      value(IR_DATA) type ref to DATA optional
      value(IT_MAPPING) type TT_MAPPING optional .
  class-methods FIND_SELECTION_PAREMETERS
    importing
      !IM_V_REPID type RSVAR-REPORT default SY-REPID
    exporting
      !EX_T_HEADER type SLIS_T_LISTHEADER .
  methods SET_TEXT
    importing
      !IV_FIELDNAME type SLIS_FIELDCAT_ALV-FIELDNAME
      !IV_TEXT type TEXT50 .
  class-methods LVC_MERGE
    importing
      !IV_STR_NAME type TABNAME
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  class-methods MODIFY_CELL
    importing
      value(IT_CHANGED) type ref to CL_ALV_CHANGED_DATA_PROTOCOL
      value(IV_ROW_ID) type LVC_S_MODI-ROW_ID
      value(IT_FNAME) type LVC_T_FNAM optional
      value(IT_FCAT) type LVC_T_FCAT optional
      !IS_DATA type ANY .
  class-methods EXCLUDE_TB_FUNCTIONS
    changing
      !CT_EXC type UI_FUNCTIONS .
  class-methods REGISTER_EDIT
    changing
      !CO_GRID type ref to CL_GUI_ALV_GRID .
  class-methods REFRESH
    importing
      !IO_GRID type ref to CL_GUI_ALV_GRID .
  class-methods SET_STYLE
    importing
      !IV_FIELD type LVC_FNAME
      !IV_TYPE type CHAR1
    changing
      !CT_STYL type LVC_T_STYL .
  class-methods GUI_REFRESH .
  class-methods CHECK_CHANGED .

  CLASS-METHODS popup_alv
    IMPORTING
      !iv_start_column TYPE int4 DEFAULT 15
      !iv_start_line TYPE int4 DEFAULT 5
      !iv_end_column TYPE int4 OPTIONAL
      !iv_end_line TYPE int4 OPTIONAL
      !iv_title TYPE lvc_title OPTIONAL
      VALUE(it_data) TYPE STANDARD TABLE
      !iv_repid TYPE sy-repid .

protected section.
private section.

  data GT_EVENTS type SLIS_T_EVENT .
  data GT_EVENT_EXIT type SLIS_T_EVENT_EXIT .

  methods FCAT_FILL_FROM_ITAB
    importing
      !TABLE type ref to DATA .

ENDCLASS.



CLASS ZCL_BC_ALV IMPLEMENTATION.


  method call_transaction.
    data : ls_data type zbcs_alv_goto_transaction.


    check is_selfield-fieldname is not initial.
    check is_selfield-value is not initial.
    clear ls_data.

    field-symbols <lt_data> type standard table.
    assign ir_data->* to <lt_data>.
    if sy-subrc eq 0.
     read table <lt_data> assigning field-symbol(<ls_data>) index is_selfield-tabindex.
     if sy-subrc eq 0.
       move-corresponding <ls_data> to ls_data.

*--------------------------------------------------------------------*
* MAPPING var mı ?
*--------------------------------------------------------------------*
       loop at it_mapping assigning field-symbol(<ls_mapping>).
         assign component <ls_mapping>-fname of structure <ls_data> to field-symbol(<lv_value>).
         if sy-subrc eq 0.
           assign component <ls_mapping>-fname_to of structure ls_data to field-symbol(<lv_value_to>).
           if sy-subrc eq 0.
             <lv_value_to> = <lv_value>.
           endif.
         endif.
         if is_selfield-fieldname eq <ls_mapping>-fname.
           is_selfield-fieldname = <ls_mapping>-fname_to.
         endif.
       endloop.
     endif.
    else.
     assign component is_selfield-fieldname of structure ls_data to <lv_value>.
     if sy-subrc eq 0.
      <lv_value> = is_selfield-value.
     endif.
    endif.

    check ls_data is not initial.
*--------------------------------------------------------------------*
* INIT VALUE
*--------------------------------------------------------------------*
    if ls_data-gjahr is initial.
     ls_data-gjahr = sy-datum(4).
    endif.
    if ls_data-mjahr is initial.
     ls_data-mjahr = sy-datum(4).
    endif.
*--------------------------------------------------------------------*
* CAll Transaction
*--------------------------------------------------------------------*
        case is_selfield-fieldname.
          when 'BANFN'.
            set parameter id 'BAN' field ls_data-banfn.
            call transaction 'ME53N' and skip first screen.
          when 'CHARG'.
            set parameter id 'MAT' field ls_data-matnr.
            set parameter id 'CHA' field ls_data-charg.
            call transaction 'MSC3N' and skip first screen.
          when 'EBELN'.
            set parameter id 'BES' field ls_data-ebeln.
            call transaction 'ME23N' and skip first screen.
          when 'MMBELNR'.
            set parameter id 'RBN' field ls_data-mmbelnr.
            set parameter id 'GJR' field ls_data-gjahr.
            call transaction 'MIR4' and skip first screen.
          when 'MBLNR'.
            set parameter id 'MBN' field ls_data-mblnr.
            set parameter id 'MJA' field ls_data-mjahr.
            call transaction 'MB03' and skip first screen.
          when 'VBELN_VA'.
            set parameter id 'AUN' field ls_data-vbeln_va.
            call transaction 'VA03' and skip first screen.
          when 'VBELN_VL'.
            set parameter id 'VL' field ls_data-vbeln_vl.
            call transaction 'VL03N' and skip first screen.
          when 'VBELN_VF'.
            set parameter id 'VF' field ls_data-vbeln_vf.
            call transaction 'VF03' and skip first screen.
          when 'MATNR'.
            set parameter id 'MAT' field ls_data-matnr.
            call transaction 'MM03' and skip first screen.
          when 'PRUEFLOS'.
            set parameter id 'QLS' field ls_data-prueflos.
            call transaction 'QA03' and skip first screen.
          when 'AUFNR'.
            set parameter id 'ANR' field ls_data-aufnr.
            call transaction 'CO03' and skip first screen.
          when 'LIFNR'.
            set parameter id 'LIF' field ls_data-lifnr.
            call transaction 'XK03' and skip first screen.
          when 'KUNNR'.
            set parameter id 'KUN' field ls_data-lifnr.
            call transaction 'XD03' and skip first screen.
          when 'FIBELNR'.
            set parameter id 'BLN' field ls_data-fibelnr.
            set parameter id 'BUK' field ls_data-bukrs.
            set parameter id 'GJR' field ls_data-gjahr.
            call transaction 'FB03' and skip first screen.
      endcase.

  endmethod.


  METHOD check_changed.
    DATA lo_ref_grid TYPE REF TO cl_gui_alv_grid.
    IF lo_ref_grid IS INITIAL.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_ref_grid.
    ENDIF.
    IF NOT lo_ref_grid IS INITIAL.
      CALL METHOD lo_ref_grid->check_changed_data .
    ENDIF.


  ENDMETHOD.


  METHOD constructor.

    gv_itab_name = iv_itab_name.
    gv_strc_name = iv_strc_name.
    gv_incl_name = iv_incl_name.
    gv_prog_name = iv_prog_name.
    gv_stat_name = iv_stat_name.
    gv_comd_name = iv_comd_name.
    gv_topp_name = iv_topp_name.
    gv_html_end_name = iv_html_end_name.

    IF gv_prog_name  IS INITIAL.
       gv_prog_name  = sy-cprog.
    ENDIF.

    IF gv_incl_name IS NOT INITIAL AND
       gv_prog_name IS INITIAL.
       gv_prog_name = gv_incl_name.
    ENDIF.

    IF is_layout IS NOT INITIAL .
       gs_layout = is_layout.
    ELSE.
       me->set_default_layout( ).
    ENDIF.

    IF is_variant IS NOT INITIAL.
       gs_variant = is_variant.
    ENDIF.

    IF gv_itab_name IS NOT INITIAL.
      CALL METHOD me->t_merge
        CHANGING
          rt_fcat = me->gt_fcat.

    ELSEIF gv_strc_name IS NOT INITIAL.
      CALL METHOD me->s_merge
        CHANGING
          rt_fcat = me->gt_fcat.
    ELSE.
      fcat_fill_from_itab( ir_data ).
    ENDIF.


    TRY.
        me->gt_fcat[ fieldname = 'SEL' ]-tech = abap_true.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    TRY.
        me->gt_fcat[ fieldname = 'SELKZ' ]-tech = abap_true.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    TRY.
        me->gt_fcat[ fieldname = 'CHK' ]-tech = abap_true.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    TRY.
        me->gt_fcat[ fieldname = 'MARK' ]-tech = abap_true.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    IF ir_data IS SUPPLIED.
       display( ir_data ).
    ENDIF.

  ENDMETHOD.


  method display.

    check gt_fcat[] is not initial.

    field-symbols : <ft_table> type standard table.

    assign table->* to <ft_table>.

    if is_variant-report is not initial.
      gs_variant = is_variant.
    endif.

    if is_top_of_page ne space.
      append value #( name = c_top form = c_top )  to gt_events.
    endif.

    if gv_comd_name is initial.

      call function 'REUSE_ALV_GRID_DISPLAY'
        exporting
          i_bypassing_buffer     = 'X'
          i_save                 = 'A'
          i_callback_program     = gv_prog_name
          i_callback_top_of_page = gv_topp_name
          i_callback_html_end_of_list = gv_html_end_name
          is_variant             = gs_variant
          is_layout              = gs_layout
          it_fieldcat            = gt_fcat
          i_default              = iv_default
          it_events              = gt_events[]
          it_event_exit          = gt_event_exit[]
        tables
          t_outtab               = <ft_table>.

    else.

      call function 'REUSE_ALV_GRID_DISPLAY'
        exporting
          i_bypassing_buffer       = 'X'
          i_save                   = 'A'
          i_callback_program       = gv_prog_name
          i_callback_pf_status_set = gv_stat_name
          i_callback_user_command  = gv_comd_name
          i_callback_top_of_page   = gv_topp_name
          i_callback_html_end_of_list = gv_html_end_name
          is_variant               = gs_variant
          is_layout                = gs_layout
          it_fieldcat              = gt_fcat
          i_default              = iv_default
          it_events                = gt_events[]
          it_event_exit            = gt_event_exit[]
        tables
          t_outtab                 = <ft_table>.
    endif.


  endmethod.


  METHOD exclude_tb_functions.

    DATA ls_exclude TYPE ui_func.

    FREE: ct_exc.

    ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND ls_exclude TO ct_exc.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND ls_exclude TO ct_exc.
    APPEND  cl_gui_alv_grid=>mc_fc_info   TO ct_exc.
    APPEND  cl_gui_alv_grid=>mc_fc_graph  TO ct_exc.
    APPEND  cl_gui_alv_grid=>mc_fc_print  TO ct_exc.

  ENDMETHOD.


 METHOD fcat_fill_from_itab.

    DATA : delete_initial_line.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN table->* TO <table>.
    CHECK sy-subrc IS INITIAL.

    IF <table> IS INITIAL.
       delete_initial_line = abap_true.
       APPEND INITIAL LINE TO <table> ASSIGNING FIELD-SYMBOL(<line>).
    ELSE.
      READ TABLE <table> ASSIGNING <line> INDEX 1.
    ENDIF.

    DATA(line_def)  = cl_abap_typedescr=>describe_by_data_ref( REF #( <line> ) ).
    gv_strc_name = line_def->get_relative_name( ).
    me->s_merge( CHANGING rt_fcat = me->gt_fcat ).

    IF delete_initial_line EQ abap_true.
       DELETE <table> INDEX 1.
    ENDIF.

  ENDMETHOD.


  method find_selection_paremeters.


* Text Table
    data: tt type table of textpool,
          ts type textpool.

* Selection Table
    data: t_st type table of rsparams.
    data: st type rsparams.
    data: wa type slis_listheader.

    read textpool im_v_repid language sy-langu into tt.

* Programda Title Olarak Girilen Değeri Rapor İsmi Yap
    read table tt into ts with key id = 'R' .
    if sy-subrc eq 0 .
      wa-typ = 'A'.
      wa-info = ts-entry .
      append wa to ex_t_header.
    endif.

    call function 'RS_REFRESH_FROM_SELECTOPTIONS'
      exporting
        curr_report     = im_v_repid
      tables
        selection_table = t_st
      exceptions
        not_found       = 1
        no_report       = 2
        others          = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    loop at t_st into st .

      clear wa.
      wa-typ = 'S'.
      read table tt into ts with key key = st-selname .
      if sy-subrc eq 0 .
        wa-key = ts-entry+8(32).
      endif.

      if st-kind eq 'P' .
*     Parameters
        wa-info = st-low.
        append wa to ex_t_header.
      else .
*     Selection Options
        if st-sign = 'I'.
          case st-option.
            when 'EQ'.
              wa-info = st-low.
            when 'BT'.
              concatenate st-low '-' st-high into wa-info.
            when 'NB'.
              concatenate '(' st-low '-' st-high ')' into wa-info.
            when 'GT'.
              concatenate '>' st-low into wa-info.
            when 'GE'.
              concatenate '>=' st-low into wa-info.
            when 'LT'.
              concatenate '<' st-low into wa-info.
            when 'LE'.
              concatenate '<=' st-low into wa-info.
            when 'NE'.
              concatenate '(' st-low ')' into wa-info.
          endcase.
        else.
          case st-option.
            when 'EQ'.
              concatenate '(' st-low ')' into wa-info.
            when 'BT'.
              concatenate '(' st-low '-' st-high ')' into wa-info.
            when 'NB'.
              concatenate st-low '-' st-high into wa-info.
            when 'GT'.
              concatenate '(>' st-low ')' into wa-info.
            when 'GE'.
              concatenate '(>=' st-low ')' into wa-info.
            when 'LT'.
              concatenate '(<' st-low ')' into wa-info.
            when 'LE'.
              concatenate '(<=' st-low ')' into wa-info.
            when 'NE'.
              wa-info = st-low.
          endcase.
        endif.
        append wa to ex_t_header.
      endif.
    endloop.

  endmethod.


  method get_fcat.
    t_fcat[] = me->gt_fcat[].
  endmethod.


  METHOD gui_refresh.

  cl_gui_cfw=>set_new_ok_code( 'REFRESH' ).

  ENDMETHOD.


  method LVC_MERGE.

    clear : rt_fcat.
    call function 'LVC_FIELDCATALOG_MERGE'
      exporting
        i_structure_name       = iv_str_name
      changing
        ct_fieldcat            = rt_fcat
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.

    if sy-subrc <> 0.
     message id sy-msgid type 'I' number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                    display like 'E'.
    endif.
  endmethod.


  METHOD modify_cell.

    DATA :
        lo_struc TYPE REF TO cl_abap_structdescr,
        lt_comp  TYPE        cl_abap_structdescr=>component_table,
        lv_fname TYPE        lvc_fname.

    CHECK iv_row_id IS NOT INITIAL.
    CHECK it_changed IS NOT INITIAL.
    IF it_fname IS INITIAL.
       lo_struc  ?= cl_abap_structdescr=>describe_by_data( is_data ).
       lt_comp = lo_struc->get_components( ).
       LOOP AT lt_comp INTO DATA(ls_comp).
         lv_fname =  ls_comp-name.
         COLLECT lv_fname INTO it_fname.
       ENDLOOP.
    ENDIF.
    CHECK it_fname IS NOT INITIAL.
    LOOP AT it_fname INTO lv_fname.
       ASSIGN COMPONENT lv_fname OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_value>).
       CHECK sy-subrc EQ 0.
        it_changed->modify_cell(
            EXPORTING
              i_row_id    = iv_row_id
              i_fieldname = lv_fname
              i_value     = <lv_value> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD popup_alv.

    DATA :  lo_alv TYPE REF TO cl_salv_table,
                lr_columns    TYPE REF TO cl_salv_columns_table,
                lr_column     TYPE REF TO cl_salv_column_table.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = lo_alv
            CHANGING
              t_table      = it_data ).

        CATCH cx_salv_msg.
      ENDTRY.

      DATA: lr_functions TYPE REF TO cl_salv_functions_list,
            lr_display  TYPE REF TO cl_salv_display_settings.

      lr_functions = lo_alv->get_functions( ).
      lr_functions->set_all( 'X' ).

      lr_display = lo_alv->get_display_settings( ).
      lr_display->set_list_header( iv_title ).


      lr_columns = lo_alv->get_columns( ).
      lr_columns->set_optimize( abap_true ).


      IF lo_alv IS BOUND.
          lo_alv->set_screen_popup(
            start_column = iv_start_column
            end_column   = iv_end_column
            start_line   = iv_start_line
            end_line     = iv_end_line
            ).

         lo_alv->set_screen_status( pfstatus = 'DUMMY'
                                    report = iv_repid ).

         lo_alv->display( ).

      ENDIF.

  ENDMETHOD.


  METHOD refresh.
    DATA ls_stable TYPE lvc_s_stbl VALUE 'XX'.
    io_grid->refresh_table_display( EXPORTING is_stable = ls_stable ).
  ENDMETHOD.


  METHOD register_edit.

*   Registering the EDIT Event

      CALL METHOD co_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        EXCEPTIONS
          error      = 1
          OTHERS     = 2 .

*   Set editable cells to ready for input initially

      co_grid->set_ready_for_input(
        EXPORTING
          i_ready_for_input = 1 ).

      co_grid->set_toolbar_interactive( ).

  ENDMETHOD.


  method set_default_layout.
   rs_layout = value slis_layout_alv( colwidth_optimize = abap_true
                                      zebra             = abap_true ).
   gs_layout = rs_layout.
  endmethod.


  method set_fcat.
    me->gt_fcat[] = t_fcat[].
  endmethod.


  method set_hotspot.
    read table gt_fcat assigning field-symbol(<ls_fcat>) with key fieldname = iv_fieldname.
    if sy-subrc eq 0.
     <ls_fcat>-hotspot = abap_true.
    endif.
  endmethod.


  method set_layout.
     gs_layout = is_layout.
  endmethod.


  METHOD set_style.

  DATA ls_styl TYPE LINE OF lvc_t_styl.


   READ TABLE ct_styl INTO ls_styl WITH KEY fieldname = iv_field.
   IF sy-subrc IS INITIAL.
     IF iv_type EQ 'D'.
      ls_styl-style = cl_gui_alv_grid=>mc_style_disabled.
     ELSE.
      ls_styl-style = cl_gui_alv_grid=>mc_style_enabled.
     ENDIF.
     MODIFY ct_styl FROM ls_styl INDEX sy-tabix.
   ELSE.
     ls_styl-fieldname = iv_field.
     IF iv_type EQ 'D'.
      ls_styl-style = cl_gui_alv_grid=>mc_style_disabled.
     ELSE.
      ls_styl-style = cl_gui_alv_grid=>mc_style_enabled.
     ENDIF.
     INSERT ls_styl INTO TABLE  ct_styl.
   ENDIF.
  ENDMETHOD.


  method set_text.

    read table gt_fcat assigning field-symbol(<ls_fcat>) with key fieldname = iv_fieldname.
    if sy-subrc eq 0.
    <ls_fcat>-seltext_m =
    <ls_fcat>-seltext_s =
    <ls_fcat>-seltext_l =
    <ls_fcat>-reptext_ddic = iv_text.
    endif.

  endmethod.


  method s_merge.

    refresh rt_fcat.
    refresh gt_fcat.

    if iv_str_name is not initial.
      gv_strc_name = iv_str_name.
    endif.

    call function 'REUSE_ALV_FIELDCATALOG_MERGE'
        exporting
          i_structure_name       = gv_strc_name
        changing
          ct_fieldcat            = rt_fcat[]
        exceptions
          inconsistent_interface = 1
          program_error          = 2
          others                 = 3.
    if sy-subrc <> 0.
      message id sy-msgid
              type sy-msgty
              number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      gt_fcat[] = rt_fcat[].
    endif.
  endmethod.


  method t_merge.

    refresh rt_fcat.
    refresh gt_fcat.

    if iv_itab_name is not initial.
     gv_itab_name = iv_itab_name.
    endif.

    if iv_incl_name is not initial.
      gv_incl_name = iv_incl_name.
    endif.

    if iv_prog_name is not initial.
     gv_prog_name = iv_prog_name.
    endif.

    call function 'REUSE_ALV_FIELDCATALOG_MERGE'
        exporting
          i_program_name         = gv_prog_name
          i_internal_tabname     = gv_itab_name
          i_inclname             = gv_incl_name
        changing
          ct_fieldcat            = rt_fcat[]
        exceptions
          inconsistent_interface = 1
          program_error          = 2
          others                 = 3.
    if sy-subrc <> 0.
      message id sy-msgid
              type sy-msgty
              number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      gt_fcat[] = rt_fcat[].
    endif.
  endmethod.
ENDCLASS.