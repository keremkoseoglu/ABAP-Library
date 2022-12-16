INTERFACE zif_bc_lock_with_notif_imp
  PUBLIC .

  CONSTANTS: BEGIN OF method,
               attempt_enqueue        TYPE seocpdname VALUE 'ATTEMPT_ENQUEUE',
               handle_attempt_success TYPE seocpdname VALUE 'HANDLE_ATTEMPT_SUCCESS',
               handle_attempt_failure TYPE seocpdname VALUE 'HANDLE_ATTEMPT_FAILURE',
               raise_lock_error       TYPE seocpdname VALUE 'RAISE_LOCK_ERROR',
               get_user_popup_msg     TYPE seocpdname VALUE 'GET_USER_POPUP_MSG',
             END OF method.

  METHODS attempt_enqueue
    EXPORTING !success      TYPE abap_bool
              !locking_user TYPE xubname
    RAISING   ycx_addict_class_method.

  METHODS handle_attempt_success
    RAISING ycx_addict_class_method.

  METHODS handle_attempt_failure
    IMPORTING !locking_user TYPE xubname
    RAISING   ycx_addict_class_method.

  METHODS raise_lock_error
    IMPORTING !locking_user TYPE xubname
              !previous     TYPE REF TO cx_root OPTIONAL
    RAISING   cx_rs_foreign_lock.

  METHODS get_user_popup_msg
    IMPORTING !locking_user TYPE xubname
    EXPORTING !user_msg     TYPE th_popup
              !friend_msg   TYPE th_popup
              !friends      TYPE rke_userid.

ENDINTERFACE.