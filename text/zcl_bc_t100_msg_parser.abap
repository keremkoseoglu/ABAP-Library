CLASS zcl_bc_t100_msg_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_output,
        msgid TYPE symsgid,
        msgno TYPE symsgno,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF t_output,

      tt_output
        TYPE STANDARD TABLE OF t_output
        WITH DEFAULT KEY,

      BEGIN OF t_search_scope,
        arbgb TYPE t100-arbgb,
        msgnr TYPE t100-msgnr,
      END OF t_search_scope,

      tt_search_scope
        TYPE STANDARD TABLE OF t_search_scope
        WITH DEFAULT KEY.

    METHODS:
      execute
        IMPORTING
          !iv_message      TYPE clike
          !iv_spras        TYPE sylangu DEFAULT sy-langu
          !it_search_scope TYPE tt_search_scope
        RETURNING
          VALUE(rt_output) TYPE tt_output
        RAISING
          cx_scan_iterator_reached_end
          zcx_bc_method_parameter.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_candidate,
        arbgb TYPE t100-arbgb,
        msgnr TYPE t100-msgnr,
        text  TYPE t100-text,
      END OF t_candidate,

      tt_candidate
        TYPE STANDARD TABLE OF t_candidate
        WITH DEFAULT KEY,

      BEGIN OF t_state,
        candidate    TYPE tt_candidate,
        message      TYPE string,
        output       TYPE tt_output,
        search_scope TYPE tt_search_scope,
        spras        TYPE sylangu,
      END OF t_state,

      tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CONSTANTS:
      c_max_do             TYPE i          VALUE 30,
      c_meth_execute       TYPE seocpdname VALUE 'EXECUTE',
      c_msgv_placeholder   TYPE char1      VALUE '&',
      c_msgv1_placeholder  TYPE char2      VALUE '&1',
      c_msgv2_placeholder  TYPE char2      VALUE '&2',
      c_msgv3_placeholder  TYPE char2      VALUE '&3',
      c_msgv4_placeholder  TYPE char2      VALUE '&4',
      c_param_search_scope TYPE seocpdname VALUE 'IT_SEARCH_SCOPE',
      c_star               TYPE char1      VALUE '*'.

    DATA:
      gs_state TYPE t_state.

    METHODS:
      determine_candidates RAISING zcx_bc_method_parameter,
      evaluate_candidates RAISING cx_scan_iterator_reached_end.

ENDCLASS.



CLASS zcl_bc_t100_msg_parser IMPLEMENTATION.

  METHOD determine_candidates.

    CHECK gs_state-search_scope IS NOT INITIAL. " Paranoya

    SELECT arbgb, msgnr, text
      FROM t100
      FOR ALL ENTRIES IN @gs_state-search_scope
      WHERE
        sprsl EQ @gs_state-spras              AND
        arbgb EQ @gs_state-search_scope-arbgb AND
        msgnr EQ @gs_state-search_scope-msgnr
      INTO CORRESPONDING FIELDS OF TABLE @gs_state-candidate.

    IF gs_state-candidate IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_value_invalid
          class_name  = CONV #( cl_abap_classdescr=>get_class_name( me ) )
          method_name = c_meth_execute
          param_name  = c_param_search_scope.
    ENDIF.

  ENDMETHOD.

  METHOD evaluate_candidates.

    DATA:
      lt_candidate_split TYPE tt_string,
      lt_message_split   TYPE tt_string,

      lv_candidate_regex TYPE string.

    LOOP AT gs_state-candidate ASSIGNING FIELD-SYMBOL(<ls_candidate>).

      lv_candidate_regex = <ls_candidate>-text.

      REPLACE ALL OCCURRENCES OF:
        c_msgv1_placeholder IN lv_candidate_regex WITH c_star,
        c_msgv2_placeholder IN lv_candidate_regex WITH c_star,
        c_msgv3_placeholder IN lv_candidate_regex WITH c_star,
        c_msgv4_placeholder IN lv_candidate_regex WITH c_star,
        c_msgv_placeholder  IN lv_candidate_regex WITH c_star.

      CHECK gs_state-message CP lv_candidate_regex.

      DATA(ls_output) = VALUE t_output(
        msgid = <ls_candidate>-arbgb
        msgno = <ls_candidate>-msgnr
      ).

      SPLIT:
        lv_candidate_regex AT space INTO TABLE lt_candidate_split,
        gs_state-message   AT space INTO TABLE lt_message_split.

      DATA(lv_candidate_index) = 1.
      DATA(lv_current_msgv)    = 0.
      DATA(lv_message_index)   = 1.
      DATA(lv_do_count)        = 0.

      DO.

        IF lv_do_count GT c_max_do.
          RAISE EXCEPTION TYPE cx_scan_iterator_reached_end.
        ENDIF.

        ASSIGN lt_candidate_split[
            lv_candidate_index
          ] TO FIELD-SYMBOL(<lv_candidate_word>).

        IF sy-subrc NE 0.
          EXIT.
        ENDIF.

        IF <lv_candidate_word> EQ c_star.

          ASSIGN lt_message_split[
              lv_message_index
            ] TO FIELD-SYMBOL(<lv_message_word>).

          IF sy-subrc NE 0.
            EXIT.
          ENDIF.

          ADD 1 TO lv_current_msgv.

          CASE lv_current_msgv.
            WHEN 1. ls_output-msgv1 = <lv_message_word>.
            WHEN 2. ls_output-msgv2 = <lv_message_word>.
            WHEN 3. ls_output-msgv3 = <lv_message_word>.
            WHEN 4. ls_output-msgv4 = <lv_message_word>.
            WHEN OTHERS. EXIT.
          ENDCASE.

        ENDIF.

        ADD 1 TO:
          lv_candidate_index,
          lv_message_index,
          lv_do_count.

      ENDDO.

      APPEND ls_output TO gs_state-output.

    ENDLOOP.

  ENDMETHOD.

  METHOD execute.

    CLEAR gs_state.

    IF it_search_scope IS INITIAL.

      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_value_initial
          class_name  = CONV #( cl_abap_classdescr=>get_class_name( me ) )
          method_name = c_meth_execute
          param_name  = c_param_search_scope.

    ENDIF.

    gs_state-message      = iv_message.
    gs_state-search_scope = it_search_scope.
    gs_state-spras        = iv_spras.

    determine_candidates( ).
    evaluate_candidates( ).

    rt_output = gs_state-output.

  ENDMETHOD.

ENDCLASS.