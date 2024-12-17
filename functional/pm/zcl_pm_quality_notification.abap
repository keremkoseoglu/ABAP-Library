CLASS zcl_pm_quality_notification DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_pm_quality_notification.

    CLASS-METHODS get_instance
      IMPORTING qmnum         TYPE qmnum
      RETURNING VALUE(result) TYPE REF TO zcl_pm_quality_notification.

  PRIVATE SECTION.
    CONSTANTS max_wait TYPE i VALUE 30.

    DATA: qmnum      TYPE qmnum,
          enqueued   TYPE abap_bool,
          status_obj TYPE REF TO zif_bc_status_obj.

    METHODS does_bapi_ret_have_error
      IMPORTING bapi_return   TYPE bapirettab
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS wait_until_notif_in_db RAISING zcx_bc_commit_wait_expire.

    METHODS enqueue
      IMPORTING wait_until TYPE abap_bool DEFAULT abap_true
      RAISING   zcx_bc_lock.

    METHODS dequeue.
ENDCLASS.


CLASS zcl_pm_quality_notification IMPLEMENTATION.
  METHOD get_instance.
    result = NEW #( ).
    result->qmnum = qmnum.
  ENDMETHOD.

  METHOD does_bapi_ret_have_error.
    DATA(critical_msgty_rng) = ycl_simbal=>get_crit_msgty_range( ).

    LOOP AT bapi_return TRANSPORTING NO FIELDS WHERE type IN critical_msgty_rng.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD wait_until_notif_in_db.
    DO zcl_pm_quality_notification=>max_wait TIMES.
      SELECT SINGLE FROM qmel
             FIELDS @abap_true
             WHERE qmnum = @me->qmnum
             INTO @DATA(entry_found).

      IF entry_found = abap_true.
        RETURN.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    RAISE EXCEPTION NEW zcx_bc_commit_wait_expire( textid  = zcx_bc_commit_wait_expire=>entry_db_wait_expired
                                                   doc_key = CONV #( me->qmnum ) ).
  ENDMETHOD.

  METHOD enqueue.
    CHECK me->enqueued = abap_false.

    DATA(attempt_count) = SWITCH i( wait_until WHEN abap_true THEN zcl_pm_quality_notification=>max_wait ELSE 1 ).

    DO attempt_count TIMES.

      CALL FUNCTION 'ENQUEUE_EIQMEL'
        EXPORTING  qmnum          = me->qmnum
        EXCEPTIONS foreign_lock   = 1
                   system_failure = 2
                   OTHERS         = 3.

      CHECK sy-subrc = 0.
      me->enqueued = abap_true.
      RETURN.
    ENDDO.

    RAISE EXCEPTION NEW zcx_bc_lock( textid   = zcx_bc_lock=>item_locked
                                     objectid = CONV #( me->qmnum ) ).
  ENDMETHOD.

  METHOD dequeue.
    CHECK me->enqueued = abap_true.

    CALL FUNCTION 'DEQUEUE_EIQMEL'
      EXPORTING qmnum = me->qmnum.

    me->enqueued = abap_false.
  ENDMETHOD.

  METHOD zif_pm_quality_notification~get_qmnum.
    result = me->qmnum.
  ENDMETHOD.

  METHOD zif_pm_quality_notification~close.
    CLEAR messages.

    IF with_email = abap_true.
      zcl_pm_enh_notif_imp04=>enable_email_on_next_close( ).
    ENDIF.

    CALL FUNCTION 'ALM_PM_NOTIFICATION_CLOSE'
      EXPORTING number   = qmnum
                syststat = VALUE bapi2080_notsti( refdate = sy-datum
                                                  reftime = sy-uzeit
                                                  langu   = sy-langu )
      TABLES    return   = messages.

    CHECK does_bapi_ret_have_error( messages ).

    RAISE EXCEPTION NEW zcx_pm_qual_notif_close( qmnum       = me->qmnum
                                                 bapi_return = messages ).
  ENDMETHOD.

  METHOD zif_pm_quality_notification~save.
    CLEAR messages.

    DATA(bapi_return) = VALUE bapirettab( ).

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING number = me->qmnum
      TABLES    return = messages.

    CHECK does_bapi_ret_have_error( messages ).

    RAISE EXCEPTION NEW zcx_pm_qual_notif_save( qmnum       = me->qmnum
                                                bapi_return = messages ).
  ENDMETHOD.

  METHOD zif_pm_quality_notification~close_and_save.
    CLEAR messages.

    zif_pm_quality_notification~close( EXPORTING with_email = with_email
                                       IMPORTING messages   = DATA(close_messages) ).

    APPEND LINES OF close_messages TO messages.

    zif_pm_quality_notification~save( IMPORTING messages = DATA(save_messages) ).
    APPEND LINES OF save_messages TO messages.
  ENDMETHOD.

  METHOD zif_pm_quality_notification~update_custom_fields.
    TRY.
        wait_until_notif_in_db( ).

        DATA(was_enqueued) = me->enqueued.

        IF was_enqueued = abap_false.
          enqueue( ).
        ENDIF.

        IF zzadd_fiori IS SUPPLIED.
          UPDATE qmel SET zzadd_fiori = zzadd_fiori WHERE qmnum = me->qmnum.
        ENDIF.

        IF zzsahakontrol IS SUPPLIED.
          UPDATE qmel SET zzsahakontrol = zzsahakontrol WHERE qmnum = me->qmnum.
        ENDIF.

        IF zzcompletion_text IS SUPPLIED.
          UPDATE qmel SET zzcompletion_text = zzcompletion_text WHERE qmnum = me->qmnum.
        ENDIF.

        COMMIT WORK AND WAIT.

        IF was_enqueued = abap_false.
          dequeue( ).
        ENDIF.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_pm_qual_notif_save( textid   = zcx_pm_qual_notif_save=>custom_field_write_fail
                                                    previous = diaper
                                                    qmnum    = me->qmnum ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_pm_quality_notification~set_breakdown_duration.
    TRY.
        wait_until_notif_in_db( ).

        DATA(was_enqueued) = me->enqueued.

        IF was_enqueued = abap_false.
          enqueue( ).
        ENDIF.

        DATA(str_val) = CONV string( auszt ).
        DATA(flt_val) = CONV float( str_val ).

        UPDATE qmih SET auszt = flt_val WHERE qmnum = me->qmnum.
        COMMIT WORK AND WAIT.

        IF was_enqueued = abap_false.
          dequeue( ).
        ENDIF.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_pm_qual_notif_save( textid   = zcx_pm_qual_notif_save=>breakdown_durat_upd_fail
                                                    previous = diaper
                                                    qmnum    = me->qmnum ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_pm_quality_notification~get_status_object.
    IF me->status_obj IS INITIAL.
      me->status_obj = CAST #( zcl_pm_qual_notif_status=>get_instance_by_qmnum( me->qmnum ) ).
    ENDIF.

    result = me->status_obj.
  ENDMETHOD.

  METHOD zif_pm_quality_notification~update_pm_work_center.
    DATA(notif_header)   = VALUE bapi2080_nothdri( pm_wkctr = pm_wkctr ).
    DATA(notif_header_x) = VALUE bapi2080_nothdri_x( pm_wkctr = abap_true ).
    DATA(bapi_return)    = VALUE bapiret2_tab( ).

    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
      EXPORTING number        = me->qmnum
                notifheader   = notif_header
                notifheader_x = notif_header_x
      TABLES    return        = bapi_return.

    TRY.
        IF does_bapi_ret_have_error( bapi_return ).
          RAISE EXCEPTION NEW zcx_pm_qual_notif_save( qmnum       = me->qmnum
                                                      bapi_return = bapi_return ).
        ENDIF.

        zif_pm_quality_notification~save( ).

      CLEANUP.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDTRY.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = abap_true.
  ENDMETHOD.
ENDCLASS.