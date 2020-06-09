CLASS zcl_bc_as4text_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_as4text_rng TYPE RANGE OF e07t-as4text .
    TYPES:
      BEGIN OF t_gekod,
        gekod TYPE zbcd_jira_gekod,
      END OF t_gekod .
    TYPES:
      tt_gekod TYPE STANDARD TABLE OF t_gekod WITH DEFAULT KEY .

    CLASS-METHODS build_as4text_from_gekod_list
      IMPORTING
        !it_gekod         TYPE tt_gekod
        !iv_default       TYPE as4text
      RETURNING
        VALUE(rv_as4text) TYPE as4text .
    CLASS-METHODS build_as4text_slrng_from_gekod
      IMPORTING
        !it_gekod     TYPE tt_gekod
      RETURNING
        VALUE(rt_rng) TYPE tt_as4text_rng .
    CLASS-METHODS extract_gekod_from_as4text
      IMPORTING
        !iv_as4text          TYPE as4text
        !iv_validate_gekod   TYPE abap_bool OPTIONAL
        !iv_validate_as4text TYPE abap_bool OPTIONAL
        !iv_valid_if_auth    TYPE abap_bool OPTIONAL
      EXPORTING
        !ev_gekod            TYPE zbcd_jira_gekod
        !ev_gesum            TYPE zbcd_jira_gesum
      RAISING
        zcx_bc_as4text .
    CLASS-METHODS get_gekod_of_trkorr_safe
      IMPORTING
        !iv_trkorr      TYPE trkorr
      RETURNING
        VALUE(rv_gekod) TYPE zbcd_jira_gekod .
    CLASS-METHODS purify_as4text
      CHANGING
        !cv_as4text TYPE as4text .

    CLASS-METHODS get_sample_valid_as4text RETURNING VALUE(rv_text) TYPE as4text.

    CLASS-METHODS user_has_free_format_tr_auth
      IMPORTING
        !iv_background TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_has)  TYPE abap_bool .
    CLASS-METHODS validate_as4text_jira
      IMPORTING
        !iv_as4text       TYPE as4text
        !iv_valid_if_auth TYPE abap_bool OPTIONAL
      RAISING
        zcx_bc_as4text .
    CLASS-METHODS validate_gekod
      IMPORTING
        !iv_gekod         TYPE zbcd_jira_gekod
        !iv_valid_if_auth TYPE abap_bool OPTIONAL
      RAISING
        zcx_bc_jira_gekod_format .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_string,
        value TYPE string,
      END OF t_string,

      tt_string TYPE STANDARD TABLE OF t_string WITH DEFAULT KEY,

      BEGIN OF t_trkorr_gekod_cache,
        trkorr TYPE trkorr,
        gekod  TYPE zbcd_jira_gekod,
      END OF t_trkorr_gekod_cache,

      tt_trkorr_gekod_cache
        TYPE HASHED TABLE OF t_trkorr_gekod_cache
        WITH UNIQUE KEY primary_key COMPONENTS trkorr.

    CONSTANTS:
      c_option_cp     TYPE ddoption VALUE 'CP',
      c_separ         TYPE char1    VALUE '-',
      c_sign_i        TYPE ddsign   VALUE 'I',
      c_summary_regex TYPE string   VALUE `[a-z A-Z 0-9 "!'^%&/()=?_*~,;.:ığüşöçĞÜŞİÖÇ\-]*` ##NO_TEXT.

    CLASS-DATA:
      gt_as4text_regex      TYPE tt_string,
      gt_gekod_regex        TYPE tt_string,
      gt_trkorr_gekod_cache TYPE tt_trkorr_gekod_cache.

    CLASS-METHODS:
      build_as4text_regex_table,
      build_gekod_regex_table,

      validate_summary
        IMPORTING !iv_text TYPE clike
        RAISING   zcx_bc_data_format,

      validate_text_with_regex_table
        IMPORTING
          !iv_text          TYPE clike
          !it_regex         TYPE tt_string
          !iv_valid_if_auth TYPE abap_bool
        RAISING
          zcx_bc_data_format.
ENDCLASS.



CLASS zcl_bc_as4text_toolkit IMPLEMENTATION.


  METHOD build_as4text_from_gekod_list.

    CASE lines( it_gekod ).
      WHEN 0.
        rv_as4text = iv_default.

      WHEN 1.
        rv_as4text = zcl_bc_tr_text_creator=>get_gesum( VALUE #( it_gekod[ 1 ]-gekod ) ).

      WHEN OTHERS.

        rv_as4text = REDUCE #(
          INIT ret = |{ iv_default }|
          FOR ls_gekod IN it_gekod
          NEXT ret = |{ ret } { ls_gekod-gekod }|
        ).

    ENDCASE.

    IF rv_as4text IS INITIAL.
      rv_as4text = iv_default.
    ENDIF.

  ENDMETHOD.


  METHOD build_as4text_regex_table.

    CHECK gt_as4text_regex IS INITIAL.
    build_gekod_regex_table( ).

    gt_as4text_regex = VALUE #(
      FOR ls_gr IN gt_gekod_regex (
        value = |{ ls_gr-value } - { c_summary_regex }|
    ) ).

  ENDMETHOD.


  METHOD build_as4text_slrng_from_gekod.

    rt_rng = VALUE #( FOR ls_gekod IN it_gekod (
      option = c_option_cp
      sign   = c_sign_i
      low    = |{ ls_gekod-gekod } - *|
    ) ).

  ENDMETHOD.


  METHOD build_gekod_regex_table.

    CHECK gt_gekod_regex IS INITIAL.

    LOOP AT zcl_bc_jira_project=>get_integrable_projects( ) ASSIGNING FIELD-SYMBOL(<ls_proj>).

      APPEND LINES OF VALUE tt_string(
        ( value = |{ <ls_proj>-project_key }-[0-9]| )
        ( value = |{ <ls_proj>-project_key }-[0-9][0-9]| )
        ( value = |{ <ls_proj>-project_key }-[0-9][0-9][0-9]| )
        ( value = |{ <ls_proj>-project_key }-[0-9][0-9][0-9][0-9]| )
        ( value = |{ <ls_proj>-project_key }-[0-9][0-9][0-9][0-9][0-9]| )
        ( value = |{ <ls_proj>-project_key }-[0-9][0-9][0-9][0-9][0-9][0-9]| )
        ( value = |{ <ls_proj>-project_key }-[0-9][0-9][0-9][0-9][0-9][0-9][0-9]| )
      ) TO gt_gekod_regex.

    ENDLOOP.

    ASSERT gt_gekod_regex IS NOT INITIAL.

  ENDMETHOD.


  METHOD extract_gekod_from_as4text.

    DATA lt_split TYPE STANDARD TABLE OF as4text.

    CLEAR: ev_gekod, ev_gesum.

    IF iv_validate_as4text EQ abap_true.
      validate_as4text_jira(
          iv_as4text       = iv_as4text
          iv_valid_if_auth = iv_valid_if_auth
      ).
    ENDIF.

    SPLIT iv_as4text AT space INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<lv_split>).

      CASE sy-tabix.
        WHEN 1.

          IF zcl_bc_jira_project=>is_proj_integrable( <lv_split>+0(3) ) EQ abap_false.
            EXIT.
          ENDIF.

          ev_gekod = <lv_split>.

        WHEN OTHERS.

          CHECK <lv_split> NE c_separ.

          ev_gesum =
            |{ ev_gesum }| &&
            |{ COND #( WHEN ev_gesum IS NOT INITIAL THEN ` ` ) }| &&
            |{ <lv_split> }|.

      ENDCASE.

    ENDLOOP.

    IF iv_validate_gekod EQ abap_true.

      TRY.

          validate_gekod(
            iv_gekod         = ev_gekod
            iv_valid_if_auth = iv_valid_if_auth
          ).

        CATCH cx_root INTO DATA(lo_cx_root).

          RAISE EXCEPTION TYPE zcx_bc_as4text
            EXPORTING
              textid   = zcx_bc_as4text=>invalid_format
              previous = lo_cx_root
              as4text  = iv_as4text.

      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD get_gekod_of_trkorr_safe.

    ASSIGN gt_trkorr_gekod_cache[
      KEY primary_key
      COMPONENTS trkorr = iv_trkorr
    ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_trkorr_gekod_cache( trkorr = iv_trkorr ).

      TRY.

          extract_gekod_from_as4text(
            EXPORTING
              iv_as4text          = zcl_bc_transport_request=>get_as4text_safe( ls_cache-trkorr )
              iv_validate_gekod   = abap_false
              iv_validate_as4text = abap_false
              iv_valid_if_auth    = abap_false
            IMPORTING
              ev_gekod            = ls_cache-gekod
          ).

        CATCH cx_root ##no_handler .
      ENDTRY.

      INSERT ls_cache INTO TABLE gt_trkorr_gekod_cache ASSIGNING <ls_cache>.

    ENDIF.

    rv_gekod = <ls_cache>-gekod.

  ENDMETHOD.

  METHOD get_sample_valid_as4text.
    rv_text = |{ zcl_bc_jira_project=>get_default_integrable_proj( ) }-12345 - Madde aciklamasi|.
  ENDMETHOD.


  METHOD purify_as4text.

    DATA:
      lv_gesum_pure TYPE as4text,
      lv_prev_space TYPE abap_bool.

    CHECK cv_as4text IS NOT INITIAL.

    TRY.

        extract_gekod_from_as4text(
          EXPORTING
            iv_as4text          = cv_as4text
            iv_validate_gekod   = abap_false
            iv_validate_as4text = abap_false
            iv_valid_if_auth    = abap_false
          IMPORTING
            ev_gekod            = DATA(lv_gekod)
            ev_gesum            = DATA(lv_gesum)
        ).

        DATA(lv_len) = strlen( lv_gesum ).
        DATA(lv_pos) = 0.

        DO lv_len TIMES.

          DATA(lv_char) = lv_gesum+lv_pos(1).

          TRY.
              validate_summary( lv_char ).
              lv_gesum_pure = |{ lv_gesum_pure }{ SWITCH #( lv_prev_space WHEN abap_true THEN ` ` ) }{ lv_char }|.
            CATCH cx_root ##no_Handler.
              lv_gesum_pure = |{ lv_gesum_pure } |.
          ENDTRY.

          lv_prev_space = boolc(
            lv_char IS INITIAL OR
            lv_char EQ space
          ).

          ADD 1 TO lv_pos.
        ENDDO.

        cv_as4text = |{ lv_gekod } - { lv_gesum_pure }|.

      CATCH cx_root ##no_Handler .
    ENDTRY.


  ENDMETHOD.


  METHOD user_has_free_format_tr_auth.

    DATA(lv_actvt) = CONV char2( SWITCH #(
        iv_background
        WHEN abap_true THEN 'W2'
        ELSE '01'
    ) ).

    AUTHORITY-CHECK
      OBJECT 'ZBCAOFTR'
      ID 'ACTVT' FIELD lv_actvt.

    rv_has = boolc( sy-subrc EQ 0 ).

  ENDMETHOD.


  METHOD validate_as4text_jira.

    TRY.
        build_as4text_regex_table( ).

        validate_text_with_regex_table(
          iv_text          = iv_as4text
          it_regex         = gt_as4text_regex
          iv_valid_if_auth = iv_valid_if_auth
        ).

      CATCH cx_root INTO DATA(lo_cx_root).

        RAISE EXCEPTION TYPE zcx_bc_as4text
          EXPORTING
            textid   = zcx_bc_as4text=>invalid_format
            previous = lo_cx_root
            as4text  = iv_as4text.

    ENDTRY.

  ENDMETHOD.


  METHOD validate_gekod.

    TRY.
        build_gekod_regex_table( ).

        validate_text_with_regex_table(
          iv_text          = iv_gekod
          it_regex         = gt_gekod_regex
          iv_valid_if_auth = iv_valid_if_auth
        ).

      CATCH cx_root INTO DATA(lo_cx_root).

        RAISE EXCEPTION TYPE zcx_bc_jira_gekod_format
          EXPORTING
            textid   = zcx_bc_jira_gekod_format=>invalid_format
            gekod    = iv_gekod
            previous = lo_cx_root.

    ENDTRY.

  ENDMETHOD.


  METHOD validate_summary.

    validate_text_with_regex_table(
      iv_text            = iv_text
      it_regex           = VALUE #( ( value = c_summary_regex ) )
      iv_valid_if_auth   = abap_false
    ).

  ENDMETHOD.


  METHOD validate_text_with_regex_table.

    TRY.

        LOOP AT it_regex ASSIGNING FIELD-SYMBOL(<ls_regex>).
          CHECK cl_abap_matcher=>matches(
            pattern = <ls_regex>-value
            text    = iv_text
          ).

          RETURN.
        ENDLOOP.

      CATCH cx_root ##no_Handler .

    ENDTRY.

    IF iv_valid_if_auth EQ abap_true.
      IF user_has_free_format_tr_auth( ) EQ abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_bc_data_format
      EXPORTING
        textid = zcx_bc_data_format=>value_format_invalid
        value  = iv_text.

  ENDMETHOD.
ENDCLASS.