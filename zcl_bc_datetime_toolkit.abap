CLASS zcl_bc_datetime_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.



    CLASS-METHODS:

      check_selection_date_limit
        IMPORTING
          VALUE(iv_tcode)  TYPE sy-tcode
          VALUE(it_values) TYPE zbctt_date_limit_values
        RAISING
          zcx_bc_genel ,

      is_weekday_checked
        importing
          !iv_datum type datum
          !is_wdays type ZSDS_DAY_CHECKBOX
        returning
          value(rv_checked) type abap_bool,

      get_date_range_of_date_period
        IMPORTING
          !iv_date        TYPE sydatum DEFAULT sy-datum
          !iv_periv       TYPE periv DEFAULT 'K4'
        RETURNING
          VALUE(rt_range) TYPE date_t_range
        RAISING
          zcx_bc_function_subrc,

      get_day_in_week
        IMPORTING !iv_date      TYPE sydatum
        RETURNING VALUE(rv_day) TYPE i,

      get_last_day_of_period
        IMPORTING
          !iv_perio       TYPE char1
          !iv_datum       TYPE sy-datum
        RETURNING
          VALUE(rv_datum) TYPE sy-datum
        RAISING
          zcx_bc_symsg,

      get_mutual_checked_days
        importing !it_day type ZSDTT_DAY_CHECKBOX
        returning value(rs_day) type ZSDs_DAY_CHECKBOX,

      is_factory_workday
        IMPORTING
          !iv_datum         TYPE sydatum
          !iv_calid         TYPE scal-fcalid
        RETURNING
          VALUE(rv_workday) TYPE abap_bool
        RAISING
          zcx_bc_function_subrc,

      last_day
        IMPORTING !iv_day       TYPE datum
        RETURNING VALUE(rv_day) TYPE datum ,

      previous_month_last_day
        EXPORTING
          !ev_last_month TYPE postper_kk
          !ev_last_day   TYPE datum .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_diw_cache,
        datum TYPE sydatum,
        day   TYPE i,
      END OF t_diw_cache,

      tt_diw_cache
        TYPE HASHED TABLE OF t_diw_cache
        WITH UNIQUE KEY primary_key COMPONENTS datum,

      BEGIN OF t_drodp_multiton,
        periv TYPE periv,
        date  TYPE sydatum,
        range TYPE date_t_range,
      END OF t_drodp_multiton,

      tt_drodp_multiton TYPE HASHED TABLE OF t_drodp_multiton WITH UNIQUE KEY primary_key COMPONENTS periv date,

      begin of t_fwd_cache,
        datum   type sydatum,
        calid   type scal-fcalid,
        cx      type ref to zcx_bc_function_subrc,
        workday type abap_bool,
      end of t_fwd_Cache,

      tt_fwd_cache
        type hashed table of t_fwd_Cache
        with unique key primary_key components datum calid.

    CLASS-DATA:
      gt_diw_cache      TYPE tt_diw_cache,
      gt_fwd_cache      type tt_Fwd_cache,
      gt_drodp_multiton TYPE tt_drodp_multiton.

    CLASS-METHODS selection_screen_text
      IMPORTING
        VALUE(iv_repid) TYPE rsvar-report
        VALUE(iv_field) TYPE zsdd_field
      RETURNING
        VALUE(rv_text)  TYPE scrtext_l .

ENDCLASS.



CLASS ZCL_BC_DATETIME_TOOLKIT IMPLEMENTATION.


  METHOD check_selection_date_limit.
    "--------->> written by mehmet sertkaya 20.01.2017 14:52:44
    CHECK iv_tcode  IS NOT INITIAL.
    CHECK it_values IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(lt_limit) FROM zbct_date_limit
       FOR ALL ENTRIES IN @it_values
       WHERE tcode EQ @iv_tcode AND
             field EQ @it_values-field.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE pgmna INTO @DATA(lv_repid) FROM tstc
       WHERE tcode EQ @iv_tcode.
    LOOP AT lt_limit ASSIGNING FIELD-SYMBOL(<ls_limit>).
      READ TABLE it_values ASSIGNING FIELD-SYMBOL(<ls_value>)
                           WITH KEY field = <ls_limit>-field.
      IF sy-subrc EQ 0.

        LOOP AT <ls_value>-value ASSIGNING FIELD-SYMBOL(<ls_date>).
          IF <ls_limit>-limit LT
           ( <ls_date>-high - <ls_date>-low ) AND
             <ls_date>-high NE '00000000'.

            DATA(lv_fieldtext) = selection_screen_text( EXPORTING
                                   iv_repid = lv_repid
                                   iv_field = <ls_limit>-field ).
            RAISE EXCEPTION TYPE zcx_bc_genel
              EXPORTING
                textid    = zcx_bc_genel=>invalid_daterange
                fieldtext = lv_fieldtext
                limit     = <ls_limit>-limit.
          ENDIF.
        ENDLOOP.
        IF sy-subrc <> 0.
          lv_fieldtext = selection_screen_text( EXPORTING
                                     iv_repid = lv_repid
                                     iv_field = <ls_limit>-field ).
          RAISE EXCEPTION TYPE zcx_bc_genel
            EXPORTING
              textid    = zcx_bc_genel=>invalid_daterange
              fieldtext = lv_fieldtext
              limit     = <ls_limit>-limit.
        ENDIF.


      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_date_range_of_date_period.

    ASSIGN gt_drodp_multiton[ KEY primary_key COMPONENTS
        periv = iv_periv
        date = iv_date
    ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_drodp_multiton(
          periv = iv_periv
          date  = iv_date
      ).

      APPEND VALUE #(
          option = zcl_bc_ddic_toolkit=>c_option_bt
          sign   = zcl_bc_ddic_toolkit=>c_sign_i
      ) TO ls_mt-range ASSIGNING FIELD-SYMBOL(<ls_range>).

      DATA(lv_poper) = CONV poper( ls_mt-date+4(2) ).

      CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
        EXPORTING
          i_gjahr        = ls_mt-date+0(4)
          i_periv        = ls_mt-periv
          i_poper        = lv_poper
        IMPORTING
          e_date         = <ls_range>-low
        EXCEPTIONS
          input_false    = 1
          t009_notfound  = 2
          t009b_notfound = 3
          OTHERS         = 4
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'FIRST_DAY_IN_PERIOD_GET' ).

      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
        EXPORTING
          i_gjahr        = ls_mt-date+0(4)
          i_periv        = ls_mt-periv
          i_poper        = lv_poper
        IMPORTING
          e_date         = <ls_range>-high
        EXCEPTIONS
          input_false    = 1
          t009_notfound  = 2
          t009b_notfound = 3
          OTHERS         = 4
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_IN_PERIOD_GET' ).

      INSERT ls_mt INTO TABLE gt_drodp_multiton ASSIGNING <ls_mt>.

    ENDIF.

    rt_range = <ls_mt>-range.

  ENDMETHOD.


  METHOD get_day_in_week.

    data lv_wotnr type p.

    ASSIGN gt_diw_cache[
        datum = iv_date
      ] TO FIELD-SYMBOL(<ls_diw>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_diw_cache( datum = iv_date ).

      CALL FUNCTION 'DAY_IN_WEEK'
        EXPORTING
          datum = ls_cache-datum
        IMPORTING
          wotnr = lv_wotnr.

      ls_cache-day = lv_wotnr.

      INSERT ls_cache
        INTO TABLE gt_diw_cache
        ASSIGNING <ls_diw>.

    ENDIF.

    rv_day = <ls_diw>-day.

  ENDMETHOD.


  METHOD get_last_day_of_period.
    DATA: lv_week  TYPE scal-week,
          lv_date  TYPE scal-date,
          ls_symsg TYPE recasymsg.
    TRY .
        CASE iv_perio.
          WHEN 'W'.
            CALL FUNCTION 'DATE_GET_WEEK'
              EXPORTING
                date         = iv_datum
              IMPORTING
                week         = lv_week
              EXCEPTIONS
                date_invalid = 1.
            IF sy-subrc <> 0.
              MOVE-CORRESPONDING syst TO ls_symsg ##ENH_OK.
              RAISE EXCEPTION TYPE zcx_bc_symsg
                EXPORTING
                  ms_symsg = ls_symsg.
            ENDIF.


            CALL FUNCTION 'WEEK_GET_FIRST_DAY'
              EXPORTING
                week         = lv_week
              IMPORTING
                date         = lv_date
              EXCEPTIONS
                week_invalid = 1.
            IF sy-subrc <> 0.
              MOVE-CORRESPONDING syst TO ls_symsg ##ENH_OK.
              RAISE EXCEPTION TYPE zcx_bc_symsg
                EXPORTING
                  ms_symsg = ls_symsg.
            ENDIF.
            rv_datum = lv_date + 6.
          WHEN 'M'.
            CALL FUNCTION 'LAST_DAY_OF_MONTHS'
              EXPORTING
                day_in            = iv_datum
              IMPORTING
                last_day_of_month = rv_datum
              EXCEPTIONS
                day_in_no_date    = 1.
            IF sy-subrc <> 0.
              MOVE-CORRESPONDING syst TO ls_symsg ##ENH_OK.
              RAISE EXCEPTION TYPE zcx_bc_symsg
                EXPORTING
                  ms_symsg = ls_symsg.
            ENDIF.
          WHEN OTHERS.
            ls_symsg-msgty = 'E'.
            ls_symsg-msgid = '00'.
            ls_symsg-msgno = '000'.
            ls_symsg-msgv1 = 'Hatalı Periyot Tipi (M-Aylık / W-Haftalık olabilir).' ##NO_TEXT.
            RAISE EXCEPTION TYPE zcx_bc_symsg
              EXPORTING
                ms_symsg = ls_symsg.
        ENDCASE.
      CATCH zcx_bc_symsg INTO DATA(lo_cx_rc).
        RAISE EXCEPTION TYPE zcx_bc_symsg
          EXPORTING
            previous = lo_cx_rc.
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_bc_symsg
          EXPORTING
            previous = lo_cx_root.
    ENDTRY.
  ENDMETHOD.


  method get_mutual_checked_days.

    check it_day is not initial.

    rs_day = value #(
      monda = abap_True
      tuesd = abap_true
      wedne = abap_true
      thurs = abap_True
      frida = abap_true
      satur = abap_true
      sunda = abap_true
    ).

    loop at it_day assigning field-symbol(<ls_day>).

      if <ls_day>-monda eq abap_False.
        rs_day-monda = abap_False.
      endif.

      if <ls_day>-tuesd eq abap_False.
        rs_day-tuesd = abap_False.
      endif.

      if <ls_day>-wedne eq abap_False.
        rs_day-wedne = abap_False.
      endif.

      if <ls_day>-thurs eq abap_False.
        rs_day-thurs = abap_False.
      endif.

      if <ls_day>-frida eq abap_False.
        rs_day-frida = abap_False.
      endif.

      if <ls_day>-satur eq abap_False.
        rs_day-satur = abap_False.
      endif.

      if <ls_day>-sunda eq abap_False.
        rs_day-sunda = abap_False.
      endif.

    endloop.

  endmethod.


  method is_factory_workday.

    assign gt_fwd_cache[
        key primary_key components
        datum = iv_datum
        calid = iv_calid
      ] to field-symbol(<ls_fwd>).

    if sy-subrc ne 0.

      data(ls_fwd) = value t_fwd_cache(
        datum = iv_datum
        calid = iv_calid
      ).

      try.

          call function 'DATE_CHECK_WORKINGDAY'
            EXPORTING
              date                       = ls_fwd-datum
              factory_calendar_id        = ls_fwd-calid
              message_type               = zcl_bc_applog_facade=>c_msgty_s
            EXCEPTIONS
              date_after_range           = 1
              date_before_range          = 2
              date_invalid               = 3
              date_no_workingday         = 4
              factory_calendar_not_found = 5
              message_type_invalid       = 6
              others                     = 7.

          case sy-subrc.
            when 0.
              ls_fwd-workday = abap_true.
            when 4.
              ls_fwd-workday = abap_False.
            when others.
              zcx_Bc_function_subrc=>raise_if_sysubrc_not_initial( 'DATE_CHECK_WORKINGDAY' ).
          endcase.

        catch zcx_bc_function_subrc into datA(lo_fun).
          ls_fwd-cx = lo_fun.
      endtry.

      insert ls_Fwd
        into table gt_fwd_cache
        assigning <ls_Fwd>.

    endif.

    if <ls_fwd>-cx is not initial.
      raise exception <ls_fwd>-cx.
    endif.

    rv_workday = <ls_fwd>-workday.

  endmethod.


  method is_weekday_checked.

    data(lv_day_in_week) = get_day_in_week( iv_datum ).

    rv_checked = xsdbool(
      ( lv_day_in_week eq 1 and is_wdays-monda eq abap_true ) OR
      ( lv_day_in_week eq 2 and is_wdays-tuesd eq abap_true ) OR
      ( lv_day_in_week eq 3 and is_wdays-wedne eq abap_true ) OR
      ( lv_day_in_week eq 4 and is_wdays-thurs eq abap_true ) OR
      ( lv_day_in_week eq 5 and is_wdays-frida eq abap_true ) OR
      ( lv_day_in_week eq 6 and is_wdays-satur eq abap_true ) OR
      ( lv_day_in_week eq 7 and is_wdays-sunda eq abap_true )
    ).

  endmethod.


  METHOD last_day.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = iv_day
      IMPORTING
        e_date = rv_day.
  ENDMETHOD.


  METHOD previous_month_last_day.
    ev_last_day = sy-datum.
    ev_last_day+6(2) = '01'.
    ev_last_day = ev_last_day - 1.
    ev_last_month = ev_last_day(6).
  ENDMETHOD.


  METHOD selection_screen_text.

    DATA : lt_field_info     TYPE STANDARD TABLE OF rsel_info,
           lt_field_names    TYPE STANDARD TABLE OF rsdynpar,
           lv_structure_name TYPE dd02l-tabname,
           lv_field          TYPE dd04l-rollname,
           lt_fieldcat       TYPE slis_t_fieldcat_alv,
           lt_dd04t_tab_a    TYPE STANDARD TABLE OF dd04t,
           lt_textpool       TYPE TABLE OF textpool.

    READ TEXTPOOL iv_repid INTO lt_textpool LANGUAGE sy-langu.

    READ TABLE lt_textpool ASSIGNING FIELD-SYMBOL(<ls_text>)
               WITH KEY key = iv_field.
    IF sy-subrc EQ 0.
      IF <ls_text>-entry NE 'D       .'.
        rv_text = <ls_text>-entry+8(40).
      ELSE.
        CALL FUNCTION 'RS_REPORTSELECTSCREEN_INFO'
          EXPORTING
            report              = iv_repid
*           DEFAULT_VALUES      = 'X'
          TABLES
            field_info          = lt_field_info
*           DEF_VALUES          = DEF_VALUES
            field_names         = lt_field_names
          EXCEPTIONS
            no_selections       = 1
            report_not_existent = 2
            subroutine_pool     = 3.
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          READ TABLE lt_field_info ASSIGNING FIELD-SYMBOL(<ls_info>)
                     WITH KEY name = iv_field.
          IF sy-subrc EQ 0.
            IF <ls_info>-dbfield CA '-'.
              SPLIT <ls_info>-dbfield AT '-'
               INTO lv_structure_name lv_field.

              CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
                EXPORTING
                  i_structure_name       = lv_structure_name
                CHANGING
                  ct_fieldcat            = lt_fieldcat
                EXCEPTIONS
                  inconsistent_interface = 1
                  program_error          = 2.
              IF sy-subrc EQ 0.
                READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
                   WITH KEY fieldname = lv_field.
                IF sy-subrc EQ 0.
                  rv_text = <ls_fcat>-seltext_l.
                ELSE.
                  rv_text = iv_field.
                ENDIF.
              ELSE.
                rv_text = iv_field.
              ENDIF.

            ELSE.

              CALL FUNCTION 'DD_DTEL_GET'
                EXPORTING
                  langu         = sy-langu
                  roll_name     = lv_field
                TABLES
                  dd04t_tab_a   = lt_dd04t_tab_a
                EXCEPTIONS
                  illegal_value = 1.
              IF sy-subrc EQ 0.
                READ TABLE lt_dd04t_tab_a ASSIGNING FIELD-SYMBOL(<ls_a>)
                     WITH KEY rollname = lv_field.
                IF sy-subrc EQ 0.
                  rv_text = <ls_a>-ddtext.
                ELSE.
                  rv_text = iv_field.
                ENDIF.
              ELSE.
                rv_text = iv_field.
              ENDIF.
            ENDIF.
          ELSE.
            rv_text = iv_field.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      rv_text = iv_field.
    ENDIF.

  ENDMETHOD.
ENDCLASS.