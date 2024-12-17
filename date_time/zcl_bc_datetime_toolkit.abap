CLASS zcl_bc_datetime_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: perbl_range TYPE RANGE OF perbl,
           perbl_list  TYPE STANDARD TABLE OF perbl WITH KEY table_line.

    CONSTANTS: BEGIN OF c_seconds,
                 per_hour   TYPE i VALUE 3600,
                 per_minute TYPE i VALUE 60,
               END OF c_seconds,

               c_default_periv TYPE periv VALUE 'K4'.

    CLASS-METHODS:
      add_minutes_to_date_time
        IMPORTING iv_minutes TYPE i
        CHANGING  cv_date    TYPE dats
                  cv_time    TYPE tims,

      check_selection_date_limit
        IMPORTING iv_tcode  TYPE sy-tcode
                  it_values TYPE zbctt_date_limit_values
        RAISING   zcx_bc_genel,

      date_to_char
        IMPORTING !date         TYPE dats
        RETURNING VALUE(output) TYPE char10,

      date_to_jahrper
        IMPORTING iv_date           TYPE dats
        RETURNING VALUE(rv_jahrper) TYPE jahrper,

      is_weekday_checked
        IMPORTING iv_datum          TYPE datum
                  is_wdays          TYPE zsds_day_checkbox
        RETURNING VALUE(rv_checked) TYPE abap_bool,

      get_date_range_of_date_period
        IMPORTING iv_date         TYPE sydatum DEFAULT sy-datum
                  iv_periv        TYPE periv   DEFAULT c_default_periv
        RETURNING VALUE(rt_range) TYPE date_t_range
        RAISING   zcx_bc_function_subrc,

      get_date_range_of_period_range
        IMPORTING gjahr_low     TYPE gjahr
                  monat_low     TYPE monat
                  gjahr_high    TYPE gjahr
                  monat_high    TYPE monat
        RETURNING VALUE(output) TYPE date_t_range,

      get_day_in_week
        IMPORTING iv_date       TYPE sydatum
        RETURNING VALUE(rv_day) TYPE i,

      get_each_date_between_dates
        IMPORTING iv_begda            TYPE begda
                  iv_endda            TYPE endda
        RETURNING VALUE(rt_each_date) TYPE datum_tab
        RAISING   zcx_bc_method_parameter,

      get_hours_between_times
        IMPORTING iv_from_date    TYPE sydatum
                  iv_from_time    TYPE syuzeit
                  iv_to_date      TYPE sydatum
                  iv_to_time      TYPE syuzeit
        RETURNING VALUE(rv_hours) TYPE i,

      get_incoming_invoice_post_date
        IMPORTING iv_issue_date     TYPE dats
                  iv_latest_gi_date TYPE dats      OPTIONAL
                  iv_enable_popup   TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rv_date)    TYPE dats
        RAISING   zcx_bc_date,

      get_last_day_of_period
        IMPORTING iv_perio        TYPE char1
                  iv_datum        TYPE sy-datum
        RETURNING VALUE(rv_datum) TYPE sy-datum
        RAISING   zcx_bc_symsg,

      get_last_day_of_perbl
        IMPORTING perbl         TYPE jahrperbl
        RETURNING VALUE(output) TYPE sydatum,

      get_last_day_of_alv_perio
        IMPORTING alv_perio     TYPE wahd_alv_perio
        RETURNING VALUE(output) TYPE sydatum,

      get_minutes_between_times
        IMPORTING iv_from_date      TYPE sydatum
                  iv_from_time      TYPE syuzeit
                  iv_to_date        TYPE sydatum
                  iv_to_time        TYPE syuzeit
        RETURNING VALUE(rv_minutes) TYPE i,

      get_mutual_checked_days
        IMPORTING it_day        TYPE zsdtt_day_checkbox
        RETURNING VALUE(rs_day) TYPE zsds_day_checkbox,

      is_factory_workday
        IMPORTING iv_datum          TYPE sydatum
                  iv_calid          TYPE scal-fcalid DEFAULT 'TR'
        RETURNING VALUE(rv_workday) TYPE abap_bool
        RAISING   zcx_bc_date,

      last_day
        IMPORTING iv_day        TYPE datum
        RETURNING VALUE(rv_day) TYPE datum,

      previous_month_last_day
        EXPORTING ev_last_month TYPE postper_kk
                  ev_last_day   TYPE datum,

      subtract_days_from_month_end
        IMPORTING iv_jahrper     TYPE jahrper
                  iv_days        TYPE i
        RETURNING VALUE(rv_date) TYPE dats
        RAISING   zcx_bc_function_subrc,

      subtract_durat_from_date_time
        IMPORTING iv_duration TYPE i
                  iv_unit     TYPE t006-msehi
        CHANGING  cv_date     TYPE sydatum
                  cv_time     TYPE syuzeit
        RAISING   zcx_bc_function_subrc,

      subtract_month_from_jahrper
        IMPORTING iv_jahrper        TYPE jahrper
                  iv_month          TYPE i
        RETURNING VALUE(rv_jahrper) TYPE jahrper.

    CLASS-METHODS explode_perbl_range
      IMPORTING perbl_rng     TYPE perbl_range
      RETURNING VALUE(result) TYPE perbl_list.

    CLASS-METHODS calculate_gross_time
      IMPORTING iv_begda        TYPE cva_date
                iv_endda        TYPE cva_date
                iv_begzt        TYPE cva_time
                iv_endzt        TYPE cva_time
      CHANGING  cv_mesaj        TYPE bapi_msg
      RETURNING VALUE(rv_gross) TYPE zppd_tsure.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_drodp_multiton,
             periv TYPE periv,
             date  TYPE sydatum,
             range TYPE date_t_range,
           END OF t_drodp_multiton,

           tt_drodp_multiton TYPE HASHED TABLE OF t_drodp_multiton WITH UNIQUE KEY primary_key COMPONENTS periv date,

           BEGIN OF t_fwd_cache,
             datum   TYPE sydatum,
             calid   TYPE scal-fcalid,
             cx      TYPE REF TO zcx_bc_date,
             workday TYPE abap_bool,
           END OF t_fwd_cache,

           tt_fwd_cache        TYPE HASHED TABLE OF t_fwd_cache
                        WITH UNIQUE KEY primary_key COMPONENTS datum calid,

           tt_iipd_adhoc_cache TYPE HASHED TABLE OF zbct_iipd_adhoc
                               WITH UNIQUE KEY primary_key COMPONENTS perio.

    TYPES: BEGIN OF perbl_last_day_dict,
             perbl TYPE jahrperbl,
             datum TYPE sydatum,
           END OF perbl_last_day_dict,

           perbl_last_day_set TYPE HASHED TABLE OF perbl_last_day_dict
                              WITH UNIQUE KEY primary_key COMPONENTS perbl.

    CONSTANTS: BEGIN OF c_clsname,
                 me TYPE seoclsname VALUE 'ZCL_BC_DATETIME_TOOLKIT',
               END OF c_clsname.

    CONSTANTS: BEGIN OF c_perbl,
                 december TYPE perbl VALUE '012',
               END OF c_perbl.

    CLASS-DATA: gt_fwd_cache        TYPE tt_fwd_cache,
                gt_drodp_multiton   TYPE tt_drodp_multiton,
                gt_iipd_adhoc_cache TYPE tt_iipd_adhoc_cache,
                gv_iipd_adhoc_read  TYPE abap_bool.

    CLASS-DATA perbl_last_day_cache TYPE perbl_last_day_set.

    CLASS-METHODS:
      get_iipd_limit_day RETURNING VALUE(rv_day) TYPE day_nr,

      popup_date_req
        IMPORTING iv_date        TYPE datum
        RETURNING VALUE(rv_date) TYPE datum,

      selection_screen_text
        IMPORTING VALUE(iv_repid) TYPE rsvar-report
                  VALUE(iv_field) TYPE zsdd_field
        RETURNING VALUE(rv_text)  TYPE scrtext_l.

ENDCLASS.


CLASS zcl_bc_datetime_toolkit IMPLEMENTATION.
  METHOD add_minutes_to_date_time.
    DATA(lv_itime) = CONV p2012-anzhl( iv_minutes / 60 ) ##NUMBER_OK.

    CALL FUNCTION 'CATT_ADD_TO_TIME'
      EXPORTING idate = cv_date
                itime = cv_time
                stdaz = lv_itime
      IMPORTING edate = cv_date
                etime = cv_time.
  ENDMETHOD.

  METHOD check_selection_date_limit.
    CHECK     iv_tcode  IS NOT INITIAL
          AND it_values IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(lt_limit)
           FROM zbct_date_limit
           FOR ALL ENTRIES IN @it_values
           WHERE tcode = @iv_tcode
             AND field = @it_values-field.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE pgmna INTO @DATA(lv_repid) FROM tstc WHERE tcode = @iv_tcode.

    LOOP AT lt_limit ASSIGNING FIELD-SYMBOL(<ls_limit>).
      ASSIGN it_values[ field = <ls_limit>-field ] TO FIELD-SYMBOL(<ls_value>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT <ls_value>-value ASSIGNING FIELD-SYMBOL(<ls_date>).

        IF     <ls_limit>-limit  < ( <ls_date>-high - <ls_date>-low )
           AND <ls_date>-high   <> '00000000'.

          DATA(lv_fieldtext) = selection_screen_text( iv_repid = lv_repid
                                                      iv_field = <ls_limit>-field ).

          RAISE EXCEPTION NEW zcx_bc_genel( textid    = zcx_bc_genel=>invalid_daterange
                                            fieldtext = lv_fieldtext
                                            limit     = <ls_limit>-limit ).
        ENDIF.
      ENDLOOP.

      IF sy-subrc <> 0.
        lv_fieldtext = selection_screen_text( iv_repid = lv_repid
                                              iv_field = <ls_limit>-field ).

        RAISE EXCEPTION NEW zcx_bc_genel( textid    = zcx_bc_genel=>invalid_daterange
                                          fieldtext = lv_fieldtext
                                          limit     = <ls_limit>-limit ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD date_to_char.
    WRITE date TO output.
  ENDMETHOD.

  METHOD date_to_jahrper.
    rv_jahrper = |{ iv_date+0(4) }0{ iv_date+4(2) }|.
  ENDMETHOD.

  METHOD get_date_range_of_date_period.
    ASSIGN gt_drodp_multiton[ KEY primary_key COMPONENTS periv = iv_periv
                                                         date  = iv_date ]
           TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.

      DATA(ls_mt) = VALUE t_drodp_multiton( periv = iv_periv
                                            date  = iv_date ).

      APPEND VALUE #( option = zcl_bc_ddic_toolkit=>c_option_bt
                      sign   = zcl_bc_ddic_toolkit=>c_sign_i )
             TO ls_mt-range ASSIGNING FIELD-SYMBOL(<ls_range>).

      DATA(lv_poper) = CONV poper( ls_mt-date+4(2) ).

      CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
        EXPORTING  i_gjahr        = ls_mt-date+0(4)
                   i_periv        = ls_mt-periv
                   i_poper        = lv_poper
        IMPORTING  e_date         = <ls_range>-low
        EXCEPTIONS input_false    = 1
                   t009_notfound  = 2
                   t009b_notfound = 3
                   OTHERS         = 4
        ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'FIRST_DAY_IN_PERIOD_GET' ).

      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
        EXPORTING  i_gjahr        = ls_mt-date+0(4)
                   i_periv        = ls_mt-periv
                   i_poper        = lv_poper
        IMPORTING  e_date         = <ls_range>-high
        EXCEPTIONS input_false    = 1
                   t009_notfound  = 2
                   t009b_notfound = 3
                   OTHERS         = 4
        ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_IN_PERIOD_GET' ).
      INSERT ls_mt INTO TABLE gt_drodp_multiton ASSIGNING <ls_mt>.
    ENDIF.

    rt_range = <ls_mt>-range.
  ENDMETHOD.

  METHOD get_date_range_of_period_range.
    ##FIXME. " İlk fırsatta, bu yordam ZCL_BC_FINITE_YEAR_PERIOD_RNG'e transfer edilecek.

    DATA(first_day) = CONV dats( |{ gjahr_low }{ monat_low }01| ).

    DATA(last_day) = get_last_day_of_period( iv_perio = 'M'
                                             iv_datum = CONV #( |{ gjahr_high }{ monat_high }01| ) ).

    output = VALUE #( ( sign   = zcl_bc_ddic_toolkit=>c_sign_i
                        option = zcl_bc_ddic_toolkit=>c_option_bt
                        low    = first_day
                        high   = last_day ) ).
  ENDMETHOD.

  METHOD get_day_in_week.
    rv_day = ycl_addict_datetime_toolkit=>get_day_in_week( iv_date ).
  ENDMETHOD.

  METHOD get_each_date_between_dates.
    IF iv_begda > iv_endda.
      RAISE EXCEPTION NEW zcx_bc_method_parameter( textid       = zcx_bc_method_parameter=>param_pair_inconsistent
                                                   class_name   = c_clsname-me
                                                   method_name  = 'GET_EACH_DATE_BETWEEN_DATES'
                                                   param_name   = 'BEGDA'
                                                   param_name_2 = 'ENDDA' ).
    ENDIF.

    DATA(lv_date_cursor) = iv_begda.

    WHILE lv_date_cursor <= iv_endda.
      APPEND lv_date_cursor TO rt_each_date.
      lv_date_cursor += 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_hours_between_times.
    cl_abap_tstmp=>td_subtract( EXPORTING date1    = iv_to_date
                                          time1    = iv_to_time
                                          date2    = iv_from_date
                                          time2    = iv_from_time
                                IMPORTING res_secs = rv_hours ).

    rv_hours /= c_seconds-per_hour.
  ENDMETHOD.

  METHOD get_iipd_limit_day.
    IF gv_iipd_adhoc_read = abap_false.
      SELECT * FROM zbct_iipd_adhoc INTO CORRESPONDING FIELDS OF TABLE gt_iipd_adhoc_cache. "#EC CI_NOWHERE
      gv_iipd_adhoc_read = abap_true.
    ENDIF.

    rv_day = VALUE #( gt_iipd_adhoc_cache[ KEY primary_key COMPONENTS perio = |{ sy-datum+0(4) }0{ sy-datum+4(2) }|
                      ]-day_nr
                      DEFAULT 24 ) ##NUMBER_OK.
  ENDMETHOD.

  METHOD get_incoming_invoice_post_date.
    DATA(lv_limit_day) = get_iipd_limit_day( ).

    DATA(lv_first_day_of_prev_period) = CONV sydatum(
        |{ SWITCH numc4( sy-datum+4(2) WHEN '01' THEN sy-datum+0(4) - 1 ELSE sy-datum+0(4) ) }| &&
        |{ SWITCH numc2( sy-datum+4(2) WHEN '01' THEN '12' ELSE sy-datum+4(2) - 1 ) }| &&
        |01| ).

    DATA(lv_first_day_of_curr_period) = CONV sydatum( |{ sy-datum+0(6) }01| ).

    DATA(lv_calculated_keydate) = COND sydatum(
        WHEN iv_issue_date+0(6) = sy-datum+0(6) " Fatura tarihi güncel dönemdeyse
                                                                   THEN iv_issue_date " o halde fatura tarihini al

        WHEN iv_issue_date+0(6) = lv_first_day_of_prev_period+0(6) " Fatura tarihi bir önceki dönemdeyse
                                                                   THEN COND #( WHEN sy-datum+6(2) <= lv_limit_day " Bugün ayın 24'ünü geçmediyse
                                                                                THEN iv_issue_date " o halde fatura tarihini al
                                                                                ELSE lv_first_day_of_curr_period ) " 24'ünü geçtik -> Bulunduğumuz dönemin ilk gününü al

        WHEN iv_issue_date+0(6) < lv_first_day_of_prev_period+0(6) " Fatura tarihi bir önceki dönemden de eskiyse
                                                                   THEN COND #( WHEN sy-datum+6(2) <= lv_limit_day " Bugün ayın 24'ünü geçmediyse
                                                                                THEN lv_first_day_of_prev_period " Önceki dönemin ilk gününü al
                                                                                ELSE lv_first_day_of_curr_period ) ) ##NUMBER_OK. " 24'ünü geçtik -> Bulunduğumuz dönemin ilk gününü al

    IF     iv_latest_gi_date     IS SUPPLIED
       AND iv_latest_gi_date     IS NOT INITIAL
       AND lv_calculated_keydate  < iv_latest_gi_date. " Bulduğumuz tarih mal girişinden eskiyse
      lv_calculated_keydate = iv_latest_gi_date. " Mal giriş tarihini al
    ENDIF.

    rv_date = COND #( WHEN sy-batch        = abap_true
                        OR iv_enable_popup = abap_false
                      THEN lv_calculated_keydate
                      ELSE popup_date_req( lv_calculated_keydate ) ).

    IF rv_date IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_date( textid = zcx_bc_date=>cant_determine_post_date ).
    ENDIF.
  ENDMETHOD.

  METHOD get_last_day_of_period.
    DATA: lv_week  TYPE scal-week,
          lv_date  TYPE scal-date,
          ls_symsg TYPE recasymsg.

    TRY.
        CASE iv_perio.
          WHEN 'W'.
            CALL FUNCTION 'DATE_GET_WEEK'
              EXPORTING  date         = iv_datum
              IMPORTING  week         = lv_week
              EXCEPTIONS date_invalid = 1.
            IF sy-subrc <> 0.
              MOVE-CORRESPONDING syst TO ls_symsg ##ENH_OK.
              RAISE EXCEPTION NEW zcx_bc_symsg( ms_symsg = ls_symsg ).
            ENDIF.

            CALL FUNCTION 'WEEK_GET_FIRST_DAY'
              EXPORTING  week         = lv_week
              IMPORTING  date         = lv_date
              EXCEPTIONS week_invalid = 1.
            IF sy-subrc <> 0.
              MOVE-CORRESPONDING syst TO ls_symsg ##ENH_OK.
              RAISE EXCEPTION NEW zcx_bc_symsg( ms_symsg = ls_symsg ).
            ENDIF.
            rv_datum = lv_date + 6.

          WHEN 'M'.
            CALL FUNCTION 'LAST_DAY_OF_MONTHS'
              EXPORTING  day_in            = iv_datum
              IMPORTING  last_day_of_month = rv_datum
              EXCEPTIONS day_in_no_date    = 1.
            IF sy-subrc <> 0.
              MOVE-CORRESPONDING syst TO ls_symsg ##ENH_OK.
              RAISE EXCEPTION NEW zcx_bc_symsg( ms_symsg = ls_symsg ).
            ENDIF.

          WHEN OTHERS.
            ls_symsg-msgty = 'E'.
            ls_symsg-msgid = '00'.
            ls_symsg-msgno = '000'.
            ls_symsg-msgv1 = 'Hatalı Periyot Tipi (M-Aylık / W-Haftalık olabilir).' ##NO_TEXT.
            RAISE EXCEPTION NEW zcx_bc_symsg( ms_symsg = ls_symsg ).
        ENDCASE.

      CATCH zcx_bc_symsg INTO DATA(lo_cx_rc).
        RAISE EXCEPTION NEW zcx_bc_symsg( previous = lo_cx_rc ).
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION NEW zcx_bc_symsg( previous = lo_cx_root ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_last_day_of_perbl.
    DATA(cache) = REF #( zcl_bc_datetime_toolkit=>perbl_last_day_cache ).

    ASSIGN cache->*[ KEY primary_key COMPONENTS perbl = perbl ]
           TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE perbl_last_day_dict( perbl = perbl
                                                   datum = get_last_day_of_period(
                                                               iv_perio = 'M'
                                                               iv_datum = |{ perbl+0(4) }{ perbl+5(2) }01| ) ).

      INSERT new_cache INTO TABLE cache->* ASSIGNING <cache>.
    ENDIF.

    output = <cache>-datum.
  ENDMETHOD.

  METHOD get_last_day_of_alv_perio.
    DATA(perbl) = CONV jahrperbl( |{ alv_perio+0(4) }0{ alv_perio+4(2) }| ).
    output = get_last_day_of_perbl( perbl ).
  ENDMETHOD.

  METHOD get_minutes_between_times.
    cl_abap_tstmp=>td_subtract( EXPORTING date1    = iv_to_date
                                          time1    = iv_to_time
                                          date2    = iv_from_date
                                          time2    = iv_from_time
                                IMPORTING res_secs = rv_minutes ).

    rv_minutes /= c_seconds-per_minute.
  ENDMETHOD.

  METHOD get_mutual_checked_days.
    CHECK it_day IS NOT INITIAL.

    rs_day = VALUE #( monda = abap_true
                      tuesd = abap_true
                      wedne = abap_true
                      thurs = abap_true
                      frida = abap_true
                      satur = abap_true
                      sunda = abap_true ).

    LOOP AT it_day ASSIGNING FIELD-SYMBOL(<ls_day>).

      IF <ls_day>-monda = abap_false.
        rs_day-monda = abap_false.
      ENDIF.

      IF <ls_day>-tuesd = abap_false.
        rs_day-tuesd = abap_false.
      ENDIF.

      IF <ls_day>-wedne = abap_false.
        rs_day-wedne = abap_false.
      ENDIF.

      IF <ls_day>-thurs = abap_false.
        rs_day-thurs = abap_false.
      ENDIF.

      IF <ls_day>-frida = abap_false.
        rs_day-frida = abap_false.
      ENDIF.

      IF <ls_day>-satur = abap_false.
        rs_day-satur = abap_false.
      ENDIF.

      IF <ls_day>-sunda = abap_false.
        rs_day-sunda = abap_false.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD is_factory_workday.
    ASSIGN gt_fwd_cache[ KEY primary_key COMPONENTS datum = iv_datum
                                                    calid = iv_calid ]
           TO FIELD-SYMBOL(<ls_fwd>).

    IF sy-subrc <> 0.
      DATA(ls_fwd) = VALUE t_fwd_cache( datum = iv_datum
                                        calid = iv_calid ).

      TRY.
          CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
            EXPORTING  date                       = ls_fwd-datum
                       factory_calendar_id        = ls_fwd-calid
                       message_type               = zcl_bc_applog_facade=>c_msgty_s
            EXCEPTIONS date_after_range           = 1
                       date_before_range          = 2
                       date_invalid               = 3
                       date_no_workingday         = 4
                       factory_calendar_not_found = 5
                       message_type_invalid       = 6
                       OTHERS                     = 7.

          CASE sy-subrc.
            WHEN 0.
              ls_fwd-workday = abap_true.
            WHEN 4.
              ls_fwd-workday = abap_false.
            WHEN OTHERS.
              zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'DATE_CHECK_WORKINGDAY' ).
          ENDCASE.

        CATCH zcx_bc_function_subrc INTO DATA(lo_fun).
          ls_fwd-cx = NEW #( textid   = zcx_bc_date=>cant_determine_if_workday
                             previous = lo_fun
                             datum    = iv_datum ).
      ENDTRY.

      INSERT ls_fwd INTO TABLE gt_fwd_cache ASSIGNING <ls_fwd>.
    ENDIF.

    IF <ls_fwd>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_fwd>-cx.
    ENDIF.

    rv_workday = <ls_fwd>-workday.
  ENDMETHOD.

  METHOD is_weekday_checked.
    DATA(lv_day_in_week) = get_day_in_week( iv_datum ).

    rv_checked = xsdbool(    ( lv_day_in_week = 1 AND is_wdays-monda = abap_true )
                          OR ( lv_day_in_week = 2 AND is_wdays-tuesd = abap_true )
                          OR ( lv_day_in_week = 3 AND is_wdays-wedne = abap_true )
                          OR ( lv_day_in_week = 4 AND is_wdays-thurs = abap_true )
                          OR ( lv_day_in_week = 5 AND is_wdays-frida = abap_true )
                          OR ( lv_day_in_week = 6 AND is_wdays-satur = abap_true )
                          OR ( lv_day_in_week = 7 AND is_wdays-sunda = abap_true ) ).
  ENDMETHOD.

  METHOD last_day.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING i_date = iv_day
      IMPORTING e_date = rv_day.
  ENDMETHOD.

  METHOD popup_date_req.
    DATA: lv_title   TYPE c LENGTH 30,
          lv_retcode TYPE c LENGTH 1 ##NEEDED,
          lt_fields  TYPE TABLE OF sval.

    lv_title = TEXT-002.

    CLEAR rv_date.

    APPEND VALUE #( tabname   = 'SYST'
                    fieldname = 'DATUM'
                    fieldtext = TEXT-003
                    value     = iv_date )
           TO lt_fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING  popup_title = lv_title
      IMPORTING  returncode  = lv_retcode
      TABLES     fields      = lt_fields
      EXCEPTIONS OTHERS      = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF lv_retcode = 'A'.
      RETURN.
    ENDIF.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF     <ls_field>-fieldname  = 'DATUM'
         AND <ls_field>-value     IS NOT INITIAL.
        rv_date = <ls_field>-value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD previous_month_last_day.
    ev_last_day = sy-datum.
    ev_last_day+6(2) = '01'.
    ev_last_day -= 1.
    ev_last_month = ev_last_day(6).
  ENDMETHOD.

  METHOD selection_screen_text.
    DATA: lt_field_info     TYPE STANDARD TABLE OF rsel_info,
          lt_field_names    TYPE STANDARD TABLE OF rsdynpar,
          lv_structure_name TYPE dd02l-tabname,
          lv_field          TYPE dd04l-rollname,
          lt_fieldcat       TYPE slis_t_fieldcat_alv,
          lt_dd04t_tab_a    TYPE STANDARD TABLE OF dd04t,
          lt_textpool       TYPE TABLE OF textpool.

    READ TEXTPOOL iv_repid INTO lt_textpool LANGUAGE sy-langu.

    ASSIGN lt_textpool[ key = iv_field ] TO FIELD-SYMBOL(<ls_text>).
    IF sy-subrc = 0.
      IF <ls_text>-entry <> 'D       .'.
        rv_text = <ls_text>-entry+8(40).
      ELSE.
        CALL FUNCTION 'RS_REPORTSELECTSCREEN_INFO'
          EXPORTING  report              = iv_repid
          TABLES     field_info          = lt_field_info
                     field_names         = lt_field_names
          EXCEPTIONS no_selections       = 1
                     report_not_existent = 2
                     subroutine_pool     = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          ASSIGN lt_field_info[ name = iv_field ] TO FIELD-SYMBOL(<ls_info>).
          IF sy-subrc = 0.
            IF <ls_info>-dbfield CA '-'.
              SPLIT <ls_info>-dbfield AT '-'
                    INTO lv_structure_name lv_field.

              CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
                EXPORTING  i_structure_name       = lv_structure_name
                CHANGING   ct_fieldcat            = lt_fieldcat
                EXCEPTIONS inconsistent_interface = 1
                           program_error          = 2.
              IF sy-subrc = 0.
                ASSIGN lt_fieldcat[ fieldname = lv_field ] TO FIELD-SYMBOL(<ls_fcat>).
                IF sy-subrc = 0.
                  rv_text = <ls_fcat>-seltext_l.
                ELSE.
                  rv_text = iv_field.
                ENDIF.
              ELSE.
                rv_text = iv_field.
              ENDIF.

            ELSE.

              CALL FUNCTION 'DD_DTEL_GET'
                EXPORTING  langu         = sy-langu
                           roll_name     = lv_field
                TABLES     dd04t_tab_a   = lt_dd04t_tab_a
                EXCEPTIONS illegal_value = 1.
              IF sy-subrc = 0.
                ASSIGN lt_dd04t_tab_a[ rollname = lv_field ] TO FIELD-SYMBOL(<ls_a>).
                IF sy-subrc = 0.
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

  METHOD subtract_days_from_month_end.
    DATA(lv_year)  = iv_jahrper+0(4).
    DATA(lv_month) = CONV t009b-poper( iv_jahrper+5(2) ).

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
      EXPORTING  i_gjahr        = lv_year
                 i_periv        = c_default_periv
                 i_poper        = lv_month
      IMPORTING  e_date         = rv_date
      EXCEPTIONS input_false    = 1
                 t009_notfound  = 2
                 t009b_notfound = 3
                 OTHERS         = 4
      ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_IN_PERIOD_GET' ).
    rv_date -= iv_days.
  ENDMETHOD.

  METHOD subtract_durat_from_date_time.
    DATA: lv_end_date TYPE sydatum,
          lv_end_time TYPE syuzeit.

    DATA(lv_start_date) = cv_date.
    DATA(lv_start_time) = cv_time.
    DATA(lv_negative_duration) = iv_duration * -1.

    CALL FUNCTION 'END_TIME_DETERMINE'
      EXPORTING  duration                   = lv_negative_duration
                 unit                       = iv_unit
      IMPORTING  end_date                   = lv_end_date
                 end_time                   = lv_end_time
      CHANGING   start_date                 = lv_start_date
                 start_time                 = lv_start_time
      EXCEPTIONS factory_calendar_not_found = 1
                 date_out_of_calendar_range = 2
                 date_not_valid             = 3
                 unit_conversion_error      = 4
                 si_unit_missing            = 5
                 parameters_no_valid        = 6
                 OTHERS                     = 7
      ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'END_TIME_DETERMINE' ).
    cv_date = lv_end_date.
    cv_time = lv_end_time.
  ENDMETHOD.

  METHOD subtract_month_from_jahrper.
    DATA: lv_year  TYPE numc4,
          lv_month TYPE numc2.

    lv_year  = iv_jahrper+0(4).
    lv_month = iv_jahrper+5(2).

    DO iv_month TIMES.
      lv_month -= 1.
      IF lv_month IS INITIAL.
        lv_year -= 1.
        lv_month = 12 ##NUMBER_OK.
      ENDIF.
    ENDDO.

    rv_jahrper = |{ lv_year }0{ lv_month }|.
  ENDMETHOD.

  METHOD explode_perbl_range.
    DATA(perbl) = CONV perbl( '001' ).

    WHILE perbl <= c_perbl-december.
      IF perbl IN perbl_rng.
        APPEND perbl TO result.
      ENDIF.

      perbl += 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD calculate_gross_time.
    DATA : lv_days TYPE i,
           lv_time TYPE cva_time,
           lv_hour TYPE i,
           lv_minu TYPE i,
           lv_sec  TYPE i.

    CLEAR : rv_gross,
            cv_mesaj.

    CALL FUNCTION 'SCOV_TIME_DIFF'
      EXPORTING  im_date1              = iv_begda
                 im_date2              = iv_endda
                 im_time1              = iv_begzt
                 im_time2              = iv_endzt
      IMPORTING  ex_days               = lv_days
                 ex_time               = lv_time
      EXCEPTIONS start_larger_than_end = 1
                 OTHERS                = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 INTO cv_mesaj.
      RETURN.
    ENDIF.

    " gün dakikaya çevrilir
    rv_gross = ( lv_days * 24 * 60 ).

    " saat dakikaya çevrilir
    lv_hour = lv_time(2).
    lv_minu = lv_time+2(2).
    lv_sec  = lv_time+4(2).

    rv_gross += ( lv_hour * 60 ) + lv_minu + ( lv_sec / 60 ).
  ENDMETHOD.
ENDCLASS.