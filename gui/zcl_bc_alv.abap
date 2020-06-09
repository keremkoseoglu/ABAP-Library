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
  class-data COLOR_YELLOW type CHAR4 value 'C310' ##NO_TEXT.
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

  methods CONSTRUCTOR
    importing
      value(IV_ITAB_NAME) type DD02L-TABNAME optional
      value(IV_STRC_NAME) type DD02L-TABNAME optional
      value(IV_INCL_NAME) type SY-REPID optional
      value(IV_PROG_NAME) type SY-REPID optional
      value(IV_STAT_NAME) type SLIS_FORMNAME optional
      value(IV_COMD_NAME) type SLIS_FORMNAME optional
      value(IV_TOPP_NAME) type SLIS_FORMNAME optional
      value(IV_HTML_END_NAME) type SLIS_FORMNAME optional .
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
      value(IS_TOP_OF_PAGE) type FLAG default SPACE .
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
protected section.
private section.

  data GT_EVENTS type SLIS_T_EVENT .
  data GT_EVENT_EXIT type SLIS_T_EVENT_EXIT .
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


  method constructor.

    gv_itab_name = iv_itab_name.
    gv_strc_name = iv_strc_name.
    gv_incl_name = iv_incl_name.
    gv_prog_name = iv_prog_name.
    gv_stat_name = iv_stat_name.
    gv_comd_name = iv_comd_name.
    gv_topp_name = iv_topp_name.
    gv_html_end_name = iv_html_end_name.

    me->set_default_layout( ).

    if gv_itab_name is not initial.
      call method me->t_merge
        changing
          rt_fcat = me->gt_fcat.

    elseif gv_strc_name is not initial.
      call method me->s_merge
        changing
          rt_fcat = me->gt_fcat.
    endif.


    try.
        me->gt_fcat[ fieldname = 'SEL' ]-tech = abap_true.
      catch cx_sy_itab_line_not_found ##NO_HANDLER.
    endtry.

    try.
        me->gt_fcat[ fieldname = 'SELKZ' ]-tech = abap_true.
      catch cx_sy_itab_line_not_found ##NO_HANDLER.
    endtry.

    try.
        me->gt_fcat[ fieldname = 'MARK' ]-tech = abap_true.
      catch cx_sy_itab_line_not_found ##NO_HANDLER.
    endtry.


  endmethod.


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
          it_events                = gt_events[]
          it_event_exit            = gt_event_exit[]
        tables
          t_outtab                 = <ft_table>.
    endif.


  endmethod.


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