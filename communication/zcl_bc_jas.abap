CLASS zcl_bc_jas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_param,
        carrier_clsname TYPE seoclsname,
        jobname         TYPE tbtco-jobname,
        to              TYPE rke_userid,
      END OF t_param.

    METHODS:
      execute
        IMPORTING !is_param TYPE t_param
        RAISING
          zcx_bc_class_method
          zcx_bc_jas_job
          zcx_bc_method_parameter.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_state,
        carrier  TYPE REF TO zif_bc_jas_carrier,
        jobcount TYPE tbtco-jobcount,
        param    TYPE t_param,
      END OF t_state.

    CONSTANTS:
      BEGIN OF c_job_status,
        active     TYPE btcstatus VALUE 'R',
        cancelled  TYPE btcstatus VALUE 'A',
        completed  TYPE btcstatus VALUE 'F',
        put_active TYPE btcstatus VALUE 'Z',
        ready      TYPE btcstatus VALUE 'Y',
        released   TYPE btcstatus VALUE 'S',
        scheduled  TYPE btcstatus VALUE 'P',
        unknown    TYPE btcstatus VALUE 'X',
      END OF c_job_status,

      BEGIN OF c_method,
        cco TYPE seocpdname VALUE 'CREATE_CARRIER_OBJ',
      END OF c_method,

      BEGIN OF c_param,
        ccls TYPE seocpdname VALUE 'CARRIER_CLSNAME',
      END OF c_param,

      c_monitor_wait TYPE i       VALUE 10,
      c_msgid        TYPE symsgid VALUE 'ZBC'.

    DATA:
      gs_state TYPE t_state.

    METHODS:
      create_carrier_obj RAISING zcx_bc_method_parameter,
      find_active_job RAISING zcx_bc_jas_job,

      monitor_job_error
        RAISING
          zcx_bc_class_method
          zcx_bc_jas_job,

      send_notification
        IMPORTING !iv_body TYPE clike
        RAISING   zcx_bc_class_method,

      send_t100_notification
        IMPORTING
          !iv_msgno TYPE symsgno
          !iv_msgv1 TYPE clike OPTIONAL
          !iv_msgv2 TYPE clike OPTIONAL
        RAISING
          zcx_bc_class_method.

ENDCLASS.



CLASS zcl_bc_jas IMPLEMENTATION.

  METHOD create_carrier_obj.

    DATA lo_obj TYPE REF TO object.

    CHECK gs_state-carrier IS INITIAL.

    TRY.
        CREATE OBJECT lo_obj TYPE (gs_state-param-carrier_clsname).
        gs_state-carrier ?= lo_obj.
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING
            textid      = zcx_bc_method_parameter=>param_value_invalid
            previous    = lo_diaper
            class_name  = CONV #( cl_abap_classdescr=>get_class_name( me ) )
            method_name = c_method-cco
            param_name  = c_param-ccls.
    ENDTRY.

  ENDMETHOD.

  METHOD execute.

    DATA lo_diaper TYPE REF TO cx_root.

    TRY.

        CLEAR gs_state.
        gs_state-param = is_param.

        create_carrier_obj( ).
        find_active_job( ).
        monitor_job_error( ).

      CLEANUP INTO lo_diaper.

        TRY.
            send_t100_notification(
              iv_msgno = '404'
              iv_msgv1 = gs_state-param-jobname
              iv_msgv2 = lo_diaper->get_text( )
            ).
          CATCH cx_root ##no_handler .
        ENDTRY.

    ENDTRY.

  ENDMETHOD.

  METHOD find_active_job.

    SELECT SINGLE jobcount
      FROM tbtco
      WHERE
        jobname EQ @gs_state-param-jobname AND
        (
          status EQ @c_job_status-active OR
          status EQ @c_job_status-put_active OR
          status EQ @c_job_status-ready OR
          status EQ @c_job_status-released
        ) AND
        (
          sdlstrtdt LT @sy-datum OR
          (
            sdlstrtdt EQ @sy-datum AND
            sdlstrttm LT @sy-uzeit
          )
        )
      INTO @gs_state-jobcount. "#EC CI_NOORDER

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_jas_job
        EXPORTING
          textid  = zcx_bc_jas_job=>job_not_found
          jobname = gs_state-param-jobname.
    ENDIF.

  ENDMETHOD.

  METHOD monitor_job_error.

    DO.

      SELECT SINGLE status
        FROM tbtco
        WHERE
          jobname  EQ @gs_state-param-jobname AND
          jobcount EQ @gs_state-jobcount
        INTO @DATA(lv_status).

      ASSERT sy-subrc EQ 0.

      CASE lv_status.
        WHEN c_job_status-cancelled OR
             c_job_status-unknown.

          send_t100_notification(
            iv_msgno = '405'
            iv_msgv1 = gs_state-param-jobname
          ).

          RETURN.
        WHEN c_job_status-completed.
          RETURN.
        WHEN OTHERS.
          WAIT UP TO 10 SECONDS.
      ENDCASE.

    ENDDO.

  ENDMETHOD.

  METHOD send_notification.

    CHECK gs_state-carrier IS NOT INITIAL.

    gs_state-carrier->send_message(
      iv_subject = TEXT-129
      iv_body    = iv_body
      it_to      = gs_state-param-to
    ).

  ENDMETHOD.

  METHOD send_t100_notification.

    DATA:
      lv_message TYPE string.

    MESSAGE
      ID c_msgid
      TYPE zcl_bc_applog_facade=>c_msgty_s
      NUMBER iv_msgno
      WITH iv_msgv1 iv_msgv2
      INTO lv_message.

    send_notification( lv_message ).

  ENDMETHOD.

ENDCLASS.