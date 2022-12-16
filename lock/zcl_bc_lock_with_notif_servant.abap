CLASS zcl_bc_lock_with_notif_servant DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES lock_action_type TYPE char1.

    CONSTANTS: BEGIN OF lock_action_enum,
                 give_up          TYPE lock_action_type VALUE 'A',
                 notify_and_retry TYPE lock_action_type VALUE 'B',
               END OF lock_action_enum.

    CLASS-METHODS get_instance
      RETURNING VALUE(result) TYPE REF TO zcl_bc_lock_with_notif_servant.

    CLASS-METHODS clear_sy_msg.

    METHODS enqueue
      IMPORTING !imp             TYPE REF TO zif_bc_lock_with_notif_imp
                !lock_action     TYPE lock_action_type
                !ignore_sy_batch TYPE abap_bool DEFAULT abap_false
      RAISING   ycx_addict_class_method
                cx_rs_foreign_lock.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF state_dict,
             imp             TYPE REF TO zif_bc_lock_with_notif_imp,
             lock_action     TYPE REF TO lock_action_type,
             ignore_sy_batch TYPE REF TO abap_bool,
             locking_user    TYPE xubname,
           END OF state_dict.

    CONSTANTS lock_retry_limit TYPE i VALUE 1000.

    CLASS-DATA singleton TYPE REF TO zcl_bc_lock_with_notif_servant.
    DATA state TYPE state_dict.

    METHODS attempt_enqueue
      EXPORTING !success TYPE abap_bool
      RAISING   ycx_addict_class_method.

    METHODS handle_enqueue_failure
      RAISING cx_rs_foreign_lock.

    METHODS notify_locking_user_and_retry
      RAISING
        cx_rs_foreign_lock
        ycx_addict_class_method.

    METHODS show_popup_to_locking_user.
ENDCLASS.



CLASS zcl_bc_lock_with_notif_servant IMPLEMENTATION.
  METHOD get_instance.
    IF singleton IS INITIAL.
      singleton = NEW #( ).
    ENDIF.

    result = singleton.
  ENDMETHOD.


  METHOD enqueue.
    me->state = VALUE #( imp             = imp
                         lock_action     = REF #( lock_action )
                         ignore_sy_batch = REF #( ignore_sy_batch ) ).

    attempt_enqueue( IMPORTING success = DATA(success) ).

    IF success = abap_true.
      RETURN.
    ENDIF.

    handle_enqueue_failure( ).
  ENDMETHOD.


  METHOD clear_sy_msg.
    CLEAR: sy-msgid, sy-msgno, sy-msgty,
           sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.
  ENDMETHOD.


  METHOD attempt_enqueue.
    CLEAR success.
    clear_sy_msg( ).

    DO 3 TIMES.
      me->state-imp->attempt_enqueue(
        IMPORTING success      = success
                  locking_user = me->state-locking_user ).

      IF success = abap_true.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    CASE success.
      WHEN abap_true.
        me->state-imp->handle_attempt_success( ).
      WHEN abap_false.
        me->state-imp->handle_attempt_failure( me->state-locking_user ).
    ENDCASE.
  ENDMETHOD.


  METHOD handle_enqueue_failure.
    CASE me->state-lock_action->*.
      WHEN me->lock_action_enum-give_up.
        me->state-imp->raise_lock_error( locking_user = me->state-locking_user ).

      WHEN me->lock_action_enum-notify_and_retry.
        TRY.
            notify_locking_user_and_retry( ).

          CATCH cx_rs_foreign_lock INTO DATA(lock_error).
            RAISE EXCEPTION lock_error.

          CATCH cx_root INTO DATA(diaper).
            me->state-imp->raise_lock_error(
              locking_user = me->state-locking_user
              previous     = diaper ).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


  METHOD notify_locking_user_and_retry.
    DATA last_locking_user TYPE xubname.

    LOG-POINT ID     zbccg_lock_notif_and_retry
              FIELDS sy-batch
                     me->state-locking_user.

    " Arka planda doğrudan hata """""""""""""""""""""""""""""""""""""
    IF me->state-ignore_sy_batch->* = abap_false.
      DATA(sy_batch) = sy-batch. " Debug'da değiştirebilmek için

      IF sy_batch = abap_false.
        me->state-imp->raise_lock_error( locking_user = me->state-locking_user ).
      ENDIF.
    ENDIF.

    " Popup ver ve bekleyip tekrar dene """""""""""""""""""""""""""""
    DO me->lock_retry_limit TIMES.
      IF last_locking_user <> me->state-locking_user.
        show_popup_to_locking_user( ).
        last_locking_user = me->state-locking_user.
      ENDIF.

      WAIT UP TO 10 SECONDS.

      me->state-imp->attempt_enqueue(
        IMPORTING success      = DATA(success)
                  locking_user = me->state-locking_user ).

      IF success = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Pes et """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->state-imp->raise_lock_error(
      EXPORTING locking_user = me->state-locking_user ).
  ENDMETHOD.


  METHOD show_popup_to_locking_user.
    DATA lv_msg TYPE th_popup.

    me->state-imp->get_user_popup_msg(
      EXPORTING locking_user  = me->state-locking_user
      IMPORTING user_msg      = DATA(popup_msg)
                friend_msg    = DATA(friend_msg)
                friends       = DATA(friends) ).

    TRY.
        IF zcl_bc_sap_user=>get_instance( me->state-locking_user )->is_dialog_user( ).
          CALL FUNCTION 'TH_POPUP' ##FM_SUBRC_OK
            EXPORTING
              client         = sy-mandt
              user           = me->state-locking_user
              message        = popup_msg
            EXCEPTIONS
              user_not_found = 1
              OTHERS         = 2.
        ENDIF.
      CATCH cx_root ##no_handler.
    ENDTRY.

    IF friend_msg IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT friends REFERENCE INTO DATA(friend)
                    WHERE table_line <> me->state-locking_user.

      TRY.
          IF zcl_bc_sap_user=>get_instance( friend->* )->is_dialog_user( ).
            CALL FUNCTION 'TH_POPUP' ##FM_SUBRC_OK
              EXPORTING
                client         = sy-mandt
                user           = friend->*
                message        = friend_msg
              EXCEPTIONS
                user_not_found = 1
                OTHERS         = 2.
          ENDIF.
        CATCH cx_root ##no_handler.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.