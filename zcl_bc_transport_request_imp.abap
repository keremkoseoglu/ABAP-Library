CLASS zcl_bc_transport_request_imp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_imp_param,
        sysnam     TYPE tmssysnam,
        mandt      TYPE symandt,
        trkorr     TYPE zbctt_trkorr_det,
        show_popup TYPE abap_bool,
      END OF t_imp_param.

    METHODS:
      execute
        IMPORTING
          !is_param TYPE t_imp_param
        RAISING
          zcx_bc_function_subrc
          zcx_bc_method_parameter
          zcx_bc_tpalog_read
          zcx_bc_tr_not_in_stms.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_state,
        param TYPE t_imp_param,
      END OF t_state.

    CONSTANTS:
      c_clsname_me   TYPE seoclsname VALUE 'ZCL_BC_TRANSPORT_REQUEST_IMP',
      c_meth_execute TYPE seocpdname VALUE 'EXECUTE',
      c_param_sysnam TYPE seocpdname VALUE 'SYSNAM',
      c_param_trkorr TYPE seocpdname VALUE 'TRKORR',
      c_trkorr_some  TYPE trkorr     VALUE 'SOME'.

    DATA:
        gs_state TYPE t_state.

    METHODS:
      import_requests RAISING zcx_bc_function_subrc,

      validate_param
        RAISING
          zcx_bc_method_parameter.

ENDCLASS.



CLASS zcl_bc_transport_request_imp IMPLEMENTATION.


  METHOD execute.

    " ______________________________
    " Parametre hazırlığı

    CLEAR gs_state.
    gs_state-param = is_param.
    validate_param( ).

    " ______________________________
    " Import işlemi

    import_requests( ).

  ENDMETHOD.


  METHOD import_requests.

    " Transmit Queue """"""""""""""""""""""""""""""""""""""""""""""""

    ##todo. " burası tamamlanacak

    IF gs_state-param-show_popup EQ abap_true.

      CALL FUNCTION 'TMS_UI_TRANSMIT_TR_QUEUE'
        EXPORTING
          iv_system             = gs_state-param-sysnam
        EXCEPTIONS
          cancelled_by_user     = 1
          without_refresh       = 2
          transmit_queue_failed = 3
          OTHERS                = 4
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_UI_TRANSMIT_TR_QUEUE' ).

    ELSE.

      data(ls_tp_trque) = value STMS_TP_TRQUE( ).

      CALL FUNCTION 'TMS_MGR_TRANSMIT_TR_QUEUE'
        EXPORTING
          iv_tar_sys                = gs_state-param-sysnam
          iv_tar_dom                = zcl_bc_sap_System=>c_domain
          iv_src_sys                = space
          iv_src_dom                = zcl_bc_sap_System=>c_domain
          iv_loc_grp                = abap_true
          iv_ext_grp                = abap_True
          iv_read_only              = abap_true
          iv_monitor                = abap_true
          iv_verbose                = abap_False
        changing
          cs_tp_trque               = ls_tp_Trque
        EXCEPTIONS
          read_config_failed        = 1
          system_not_found          = 2
          group_not_found           = 3
          no_source_systems_found   = 4
          feature_not_available     = 5
          identical_groups          = 6
          check_group_config_failed = 7
          invalid_group_config      = 8
          OTHERS                    = 9
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_TRANSMIT_TR_QUEUE' ).

      CALL FUNCTION 'TMS_MGR_TRANSMIT_TR_QUEUE'
        EXPORTING
          iv_tar_sys                = gs_state-param-sysnam
          iv_tar_dom                = zcl_bc_sap_System=>c_domain
          iv_read_only              = abap_False
          iv_use_list               = abap_true
          iv_without_ftp            = abap_false
          iv_monitor                = abap_true
          iv_Verbose                = abap_False
        changing
          cs_tp_trque               = ls_tp_trque
        EXCEPTIONS
          read_config_failed        = 1
          system_not_found          = 2
          group_not_found           = 3
          no_source_systems_found   = 4
          feature_not_available     = 5
          identical_groups          = 6
          check_group_config_failed = 7
          invalid_group_config      = 8
          OTHERS                    = 9
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_TRANSMIT_TR_QUEUE' ).

    ENDIF.

    " Import """"""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA(lt_request) = CORRESPONDING stms_tr_requests( gs_state-param-trkorr ).

    IF gs_state-param-show_popup EQ abap_true.

      CALL FUNCTION 'TMS_UI_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system             = gs_state-param-sysnam
          iv_request            = c_trkorr_some
          iv_tarcli             = gs_state-param-mandt
          it_requests           = lt_request
        EXCEPTIONS
          cancelled_by_user     = 1
          import_request_denied = 2
          import_request_failed = 3
          OTHERS                = 4
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_UI_IMPORT_TR_REQUEST' ).

    ELSE.

      DATA(lv_retcode) = CONV stpa-retcode( space ).
      DATA(ls_exception) = VALUE stmscalert( ).

      CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system                  = gs_state-param-sysnam
          iv_request                 = c_trkorr_some
          iv_client                  = gs_state-param-mandt
          iv_overtake                = abap_true  " Leave transport request in queue
          iv_import_again            = abap_true  " Import transport request again
          iv_ignore_originality      = abap_true  " Overwrite originals
          iv_ignore_repairs          = abap_true  " Overwrite objects in unconfirmed repairs
          iv_ignore_transtype        = abap_false " Ignore invalid transport type
          iv_ignore_tabletype        = abap_false " Ignore invalid table class
          iv_ignore_qaflag           = abap_false " ?
          iv_ignore_predec           = abap_false " Skip predecessior relationships
          iv_ignore_cvers            = abap_true  " Ignore invalid component version
          iv_ignore_spam             = abap_false " ?
*         iv_cmd_import              =
*         iv_no_delivery             =
*         iv_subset                  =
*         iv_offline                 =
*         iv_feedback                =     " ID for feedback job (Workflow, UserExit,...)
*         iv_monitor                 = 'X'
*         iv_force                   =
*         iv_verbose                 =
*         is_batch                   =
          it_requests                = lt_request
*         IT_CLIENTS                 =
        IMPORTING
          ev_tp_ret_code             = lv_retcode
*         ev_tp_alog                 =
*         ev_tp_slog                 =
*         ev_tp_pid                  =
*         ev_tpstat_key              =     " TMS: tp Key in TPSTAT and ALOG
          es_exception               = ls_exception
*         et_tp_imports              =
*        TABLES
*         tt_logptr                  =
*         tt_stdout                  =
        EXCEPTIONS
          read_config_failed         = 1
          table_of_requests_is_empty = 2
          OTHERS                     = 3
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_IMPORT_TR_REQUEST' ).

      IF ls_exception IS NOT INITIAL AND ls_exception-msgty IN zcl_bc_applog_facade=>get_crit_msgty_range( ).
        RAISE EXCEPTION TYPE zcx_bc_function_subrc
          EXPORTING
            textid     = zcx_bc_function_subrc=>function_returned_error_txt
            funcname   = 'TMS_MGR_IMPORT_TR_REQUEST'
            error_text = CONV #( ls_exception-text ).
      ENDIF.

      IF NOT (
        lv_retcode EQ '0000' OR lv_retcode EQ '0' OR
        lv_retcode EQ '0004' OR lv_retcode EQ '4'
      ).
        RAISE EXCEPTION TYPE zcx_bc_function_subrc
          EXPORTING
            textid   = zcx_bc_function_subrc=>function_returned_error
            funcname = 'TMS_MGR_IMPORT_TR_REQUEST'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD validate_param.

    " ______________________________
    " Zorunlu değerler

    IF gs_state-param-sysnam IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_missing
          class_name  = c_clsname_me
          method_name = c_meth_execute
          param_name  = c_param_sysnam.
    ENDIF.

    IF gs_state-param-trkorr IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_missing
          class_name  = c_clsname_me
          method_name = c_meth_execute
          param_name  = c_param_trkorr.
    ENDIF.

  ENDMETHOD.
ENDCLASS.