CLASS zcl_bc_datetime_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      c_default_periv TYPE periv VALUE 'K4'.

    CLASS-METHODS:

      add_minutes_to_date_time
        IMPORTING
          !iv_minutes TYPE i
        CHANGING
          !cv_date    TYPE dats
          !cv_time    TYPE tims,

      check_selection_date_limit
        IMPORTING
          !iv_tcode  TYPE sy-tcode
          !it_values TYPE zbctt_date_limit_values
        RAISING
          zcx_bc_genel ,

      date_to_jahrper
        IMPORTING !iv_date          TYPE dats
        RETURNING VALUE(rv_jahrper) TYPE jahrper,

      is_weekday_checked
        IMPORTING
          !iv_datum         TYPE datum
          !is_wdays         TYPE zsds_day_checkbox
        RETURNING
          VALUE(rv_checked) TYPE abap_bool,

      get_date_range_of_date_period
        IMPORTING
          !iv_date        TYPE sydatum DEFAULT sy-datum
          !iv_periv       TYPE periv   DEFAULT c_default_periv
        RETURNING
          VALUE(rt_range) TYPE date_t_range
        RAISING
          zcx_bc_function_subrc,

      get_day_in_week
        IMPORTING !iv_date      TYPE sydatum
        RETURNING VALUE(rv_day) TYPE i,

      get_hours_between_times
        IMPORTING
          !iv_from_date   TYPE sydatum
          !iv_from_time   TYPE syuzeit
          !iv_to_date     TYPE sydatum
          !iv_to_time     TYPE syuzeit
        RETURNING
          VALUE(rv_hours) TYPE i,

      get_incoming_invoice_post_date
        IMPORTING
          !iv_issue_date     TYPE dats
          !iv_latest_gi_date TYPE dats OPTIONAL
          !iv_enable_popup   TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(rv_date)     TYPE dats
        RAISING
          zcx_bc_date,

      get_last_day_of_period
        IMPORTING
          !iv_perio       TYPE char1
          !iv_datum       TYPE sy-datum
        RETURNING
          VALUE(rv_datum) TYPE sy-datum
        RAISING
          zcx_bc_symsg,

      get_minutes_between_times
        IMPORTING
          !iv_from_date     TYPE sydatum
          !iv_from_time     TYPE syuzeit
          !iv_to_date       TYPE sydatum
          !iv_to_time       TYPE syuzeit
        RETURNING
          VALUE(rv_minutes) TYPE i,

      get_mutual_checked_days
        IMPORTING !it_day       TYPE zsdtt_day_checkbox
        RETURNING VALUE(rs_day) TYPE zsds_day_checkbox,

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
          !ev_last_day   TYPE datum ,

      subtract_days_from_month_end
        IMPORTING
          !iv_jahrper    TYPE jahrper
          !iv_days       TYPE i
        RETURNING
          VALUE(rv_date) TYPE dats
        RAISING
          zcx_bc_function_subrc,

      subtract_durat_from_date_time
        IMPORTING
          !iv_duration TYPE i
          !iv_unit     TYPE t006-msehi
        CHANGING
          !cv_date     TYPE sydatum
          !cv_time     TYPE syuzeit
        RAISING
          zcx_bc_function_subrc,

      subtract_month_from_jahrper
        IMPORTING
          !iv_jahrper       TYPE jahrper
          !iv_month         TYPE i
        RETURNING
          VALUE(rv_jahrper) TYPE jahrper.

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

      BEGIN OF t_fwd_cache,
        datum   TYPE sydatum,
        calid   TYPE scal-fcalid,
        cx      TYPE REF TO zcx_bc_function_subrc,
        workday TYPE abap_bool,
      END OF t_fwd_cache,

      tt_fwd_cache
        TYPE HASHED TABLE OF t_fwd_cache
        WITH UNIQUE KEY primary_key COMPONENTS datum calid,

      tt_iipd_adhoc_cache
        type hashed table of ZBCT_IIPD_ADHOC
        with unique key primary_key components perio.

    CONSTANTS:
      c_seconds_per_hour   TYPE i VALUE 3600,
      c_seconds_per_minute TYPE i VALUE 60.

    CLASS-DATA:
      gt_diw_cache        TYPE tt_diw_cache,
      gt_fwd_cache        TYPE tt_fwd_cache,
      gt_drodp_multiton   TYPE tt_drodp_multiton,
      gt_iipd_adhoc_cache type tt_iipd_adhoc_cache,
      gv_iipd_adhoc_read  type abap_bool.

    CLASS-METHODS:
      get_iipd_limit_day returning value(rv_day)  type day_nr,

      popup_date_req
        IMPORTING
          !iv_date       TYPE datum
        RETURNING
          VALUE(rv_date) TYPE datum ,

      selection_screen_text
        IMPORTING
          VALUE(iv_repid) TYPE rsvar-report
          VALUE(iv_field) TYPE zsdd_field
        RETURNING
          VALUE(rv_text)  TYPE scrtext_l .

ENDCLASS.



CLASS zcl_bc_datetime_toolkit IMPLEMENTATION.

  METHOD add_minutes_to_date_time.

    DATA(lv_itime) = CONV p2012-anzhl( iv_minutes / 60 ) ##NUMBER_OK.

    CALL FUNCTION 'CATT_ADD_TO_TIME'
      EXPORTING
        idate = cv_date
        itime = cv_time
        stdaz = lv_itime
      IMPORTING
        edate = cv_date
        etime = cv_time.

  ENDMETHOD.

  METHOD check_selection_date_limit.

    CHECK
      iv_tcode  IS NOT INITIAL AND
      it_values IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(lt_limit) FROM zbct_date_limit
       FOR ALL ENTRIES IN @it_values
       WHERE tcode EQ @iv_tcode AND
             field EQ @it_values-field.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

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

  METHOD date_to_jahrper.
    rv_jahrper = |{ iv_date+0(4) }0{ iv_date+4(2) }|.
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

    DATA lv_wotnr TYPE p.

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

  METHOD get_hours_between_times.

    cl_abap_tstmp=>td_subtract(
      EXPORTING
        date1    = iv_to_date
        time1    = iv_to_time
        date2    = iv_from_date
        time2    = iv_from_time
      IMPORTING
        res_secs = rv_hours
    ).

    DIVIDE rv_hours BY c_seconds_per_hour.

  ENDMETHOD.

  method get_iipd_limit_day.

    if gv_iipd_adhoc_read eq abap_false.
      select * from ZBCT_IIPD_ADHOC into corresponding fields of table gt_iipd_adhoc_cache. "#EC CI_NOWHERE
      gv_iipd_adhoc_Read = abap_true.
    endif.

    rv_day = value #(
      gt_iipd_adhoc_Cache[
          key primary_key components
          perio = |{ sy-datum+0(4) }0{ sy-datum+4(2) }|
        ]-day_nr
      default 24
    ) ##NUMBER_OK.

  endmethod.

  METHOD get_incoming_invoice_post_date.

    data(lv_limit_day) = get_iipd_limit_day( ).

    DATA(lv_first_day_of_prev_period) = CONV sydatum(
      |{ SWITCH numc4( sy-datum+4(2) WHEN '01' THEN sy-datum+0(4) - 1 ELSE sy-datum+0(4) ) }| &&
      |{ SWITCH numc2( sy-datum+4(2) WHEN '01' THEN '12' ELSE sy-datum+4(2) - 1 ) }| &&
      '01'
    ).

    DATA(lv_first_day_of_curr_period) = CONV sydatum( |{ sy-datum+0(6) }01| ).

    DATA(lv_calculated_keydate) = COND sydatum(
      WHEN iv_issue_date+0(6) EQ sy-datum+0(6) " Fatura tarihi güncel dönemdeyse
      THEN iv_issue_date " o halde fatura tarihini al

      WHEN iv_issue_date+0(6) EQ lv_first_day_of_prev_period+0(6) " Fatura tarihi bir önceki dönemdeyse
      THEN COND #(
        WHEN sy-datum+6(2) LE lv_limit_day " Bugün ayın 24'ünü geçmediyse
        THEN iv_issue_date " o halde fatura tarihini al
        ELSE lv_first_day_of_curr_period " 24'ünü geçtik -> Bulunduğumuz dönemin ilk gününü al
      )

      WHEN iv_issue_date+0(6) LT lv_first_day_of_prev_period+0(6) " Fatura tarihi bir önceki dönemden de eskiyse
      THEN COND #(
        WHEN sy-datum+6(2) LE lv_limit_day " Bugün ayın 24'ünü geçmediyse
        THEN lv_first_day_of_prev_period " Önceki dönemin ilk gününü al
        ELSE lv_first_day_of_curr_period " 24'ünü geçtik -> Bulunduğumuz dönemin ilk gününü al
      )
    ) ##NUMBER_OK.

    IF iv_latest_gi_date IS SUPPLIED AND
       iv_latest_gi_date IS NOT INITIAL AND
       lv_calculated_keydate LT iv_latest_gi_date. " Bulduğumuz tarih mal girişinden eskiyse
      lv_calculated_keydate = iv_latest_gi_date. " Mal giriş tarihini al
    ENDIF.

    rv_date = COND #(
      WHEN
        sy-batch EQ abap_true OR
        iv_enable_popup EQ abap_false
      THEN
        lv_calculated_keydate
      ELSE
        popup_date_req( lv_calculated_keydate )
    ).

    IF rv_date IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_date
        EXPORTING
          textid = zcx_bc_date=>cant_determine_post_date.
    ENDIF.

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

  METHOD get_minutes_between_times.

    cl_abap_tstmp=>td_subtract(
      EXPORTING
        date1    = iv_to_date
        time1    = iv_to_time
        date2    = iv_from_date
        time2    = iv_from_time
      IMPORTING
        res_secs = rv_minutes
    ).

    DIVIDE rv_minutes BY c_seconds_per_minute.

  ENDMETHOD.

  METHOD get_mutual_checked_days.

    CHECK it_day IS NOT INITIAL.

    rs_day = VALUE #(
      monda = abap_true
      tuesd = abap_true
      wedne = abap_true
      thurs = abap_true
      frida = abap_true
      satur = abap_true
      sunda = abap_true
    ).

    LOOP AT it_day ASSIGNING FIELD-SYMBOL(<ls_day>).

      IF <ls_day>-monda EQ abap_false.
        rs_day-monda = abap_false.
      ENDIF.

      IF <ls_day>-tuesd EQ abap_false.
        rs_day-tuesd = abap_false.
      ENDIF.

      IF <ls_day>-wedne EQ abap_false.
        rs_day-wedne = abap_false.
      ENDIF.

      IF <ls_day>-thurs EQ abap_false.
        rs_day-thurs = abap_false.
      ENDIF.

      IF <ls_day>-frida EQ abap_false.
        rs_day-frida = abap_false.
      ENDIF.

      IF <ls_day>-satur EQ abap_false.
        rs_day-satur = abap_false.
      ENDIF.

      IF <ls_day>-sunda EQ abap_false.
        rs_day-sunda = abap_false.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_factory_workday.

    ASSIGN gt_fwd_cache[
        KEY primary_key COMPONENTS
        datum = iv_datum
        calid = iv_calid
      ] TO FIELD-SYMBOL(<ls_fwd>).

    IF sy-subrc NE 0.

      DATA(ls_fwd) = VALUE t_fwd_cache(
        datum = iv_datum
        calid = iv_calid
      ).

      TRY.

          CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
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
          ls_fwd-cx = lo_fun.
      ENDTRY.

      INSERT ls_fwd
        INTO TABLE gt_fwd_cache
        ASSIGNING <ls_fwd>.

    ENDIF.

    IF <ls_fwd>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_fwd>-cx.
    ENDIF.

    rv_workday = <ls_fwd>-workday.

  ENDMETHOD.


  METHOD is_weekday_checked.

    DATA(lv_day_in_week) = get_day_in_week( iv_datum ).

    rv_checked = xsdbool(
      ( lv_day_in_week EQ 1 AND is_wdays-monda EQ abap_true ) OR
      ( lv_day_in_week EQ 2 AND is_wdays-tuesd EQ abap_true ) OR
      ( lv_day_in_week EQ 3 AND is_wdays-wedne EQ abap_true ) OR
      ( lv_day_in_week EQ 4 AND is_wdays-thurs EQ abap_true ) OR
      ( lv_day_in_week EQ 5 AND is_wdays-frida EQ abap_true ) OR
      ( lv_day_in_week EQ 6 AND is_wdays-satur EQ abap_true ) OR
      ( lv_day_in_week EQ 7 AND is_wdays-sunda EQ abap_true )
    ).

  ENDMETHOD.


  METHOD last_day.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = iv_day
      IMPORTING
        e_date = rv_day.
  ENDMETHOD.

  METHOD popup_date_req.

    DATA:
      lv_title(30) TYPE c,
      lv_retcode   TYPE c ##NEEDED,
      lt_fields    TYPE TABLE OF sval.

    lv_title = TEXT-002.

    CLEAR rv_date.

    APPEND VALUE #(
        tabname   = 'SYST'
        fieldname = 'DATUM'
        fieldtext = TEXT-003
        value     = iv_date
      ) TO lt_fields.


    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = lv_title
      IMPORTING
        returncode  = lv_retcode
      TABLES
        fields      = lt_fields
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                      DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF lv_retcode EQ 'A'.
      RETURN.
    ENDIF.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF <ls_field>-fieldname EQ 'DATUM' AND
        <ls_field>-value IS NOT INITIAL.
        rv_date = <ls_field>-value.
      ENDIF.
    ENDLOOP.

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

  METHOD subtract_days_from_month_end.

    DATA(lv_year)  = iv_jahrper+0(4).
    DATA(lv_month) = CONV t009b-poper( iv_jahrper+5(2) ).

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr        = lv_year
        i_periv        = c_default_periv
        i_poper        = lv_month
      IMPORTING
        e_date         = rv_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        OTHERS         = 4
        ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_IN_PERIOD_GET' ).

    SUBTRACT iv_days FROM rv_date.

  ENDMETHOD.

  METHOD subtract_durat_from_date_time.

    DATA:
      lv_end_date TYPE sydatum,
      lv_end_time TYPE syuzeit.

    DATA(lv_start_date) = cv_date.
    DATA(lv_start_time) = cv_time.
    DATA(lv_negative_duration) = iv_duration * -1.

    CALL FUNCTION 'END_TIME_DETERMINE'
      EXPORTING
        duration                   = lv_negative_duration
        unit                       = iv_unit
      IMPORTING
        end_date                   = lv_end_date
        end_time                   = lv_end_time
      CHANGING
        start_date                 = lv_start_date
        start_time                 = lv_start_time
      EXCEPTIONS
        factory_calendar_not_found = 1
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

    DATA:
      lv_year  TYPE numc4,
      lv_month TYPE numc2.

    lv_year  = iv_jahrper+0(4).
    lv_month = iv_jahrper+5(2).

    DO iv_month TIMES.
      SUBTRACT 1 FROM lv_month.
      IF lv_month IS INITIAL.
        SUBTRACT 1 FROM lv_year.
        lv_month = 12 ##NUMBER_OK.
      ENDIF.
    ENDDO.

    rv_jahrper = |{ lv_year }0{ lv_month }|.

  ENDMETHOD.

ENDCLASS.