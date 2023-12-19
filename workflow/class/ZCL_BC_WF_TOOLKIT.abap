CLASS zcl_bc_wf_toolkit DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    TYPES tt_status_range TYPE RANGE OF sww_wistat.

    TYPES: wi_rh_task_range TYPE RANGE OF swwwihead-wi_rh_task,
           catid_range      TYPE RANGE OF sww_wi2obj-catid,
           typeid_range     TYPE RANGE OF sww_wi2obj-typeid,
           instid_range     TYPE RANGE OF sww_wi2obj-instid.

    CONSTANTS c_wi_type_w TYPE sww_witype VALUE 'W'.

    CONSTANTS: BEGIN OF c_otype,
                 user TYPE otype VALUE 'US',
               END OF c_otype.

    CONSTANTS: BEGIN OF c_status,
                 waiting    TYPE sww_wistat VALUE 'WAITING',
                 ready      TYPE sww_wistat VALUE 'READY',
                 selected   TYPE sww_wistat VALUE 'SELECTED',
                 started    TYPE sww_wistat VALUE 'STARTED',
                 error      TYPE sww_wistat VALUE 'ERROR',
                 committed  TYPE sww_wistat VALUE 'COMMITTED',
                 completed  TYPE sww_wistat VALUE 'COMPLETED',
                 cancelled  TYPE sww_wistat VALUE 'CANCELLED',
                 checked    TYPE sww_wistat VALUE 'CHECKED',
                 excpcaught TYPE sww_wistat VALUE 'EXCPCAUGHT',
                 excphandlr TYPE sww_wistat VALUE 'EXCPHANDLR',
               END OF c_status.

    CLASS-METHODS:
      cancel_old_active_workflows
        IMPORTING iv_catid  TYPE sww_wi2obj-catid
                  iv_instid TYPE sww_wi2obj-instid
                  iv_typeid TYPE sww_wi2obj-typeid,

      get_active_status_range   RETURNING VALUE(rt_range) TYPE tt_status_range,
      get_inactive_status_range RETURNING VALUE(rt_range) TYPE tt_status_range,

      get_first_agent_from_history
        IMPORTING iv_task         TYPE sww_wi2obj-wi_rh_task
                  iv_instid       TYPE sww_wi2obj-instid
                  iv_typeid       TYPE sww_wi2obj-typeid
        RETURNING VALUE(rv_agent) TYPE sww_aagent
        RAISING   zcx_bc_wf_approver,

      is_user_workitem_recipient
        IMPORTING iv_workitem_id      TYPE swr_struct-workitemid
                  iv_user             TYPE syuname DEFAULT sy-uname
        RETURNING VALUE(rv_recipient) TYPE abap_bool,

      refresh_buffer EXPORTING et_msg TYPE tab_bdcmsgcoll.

    CLASS-METHODS get_active_dialog_workitem
      IMPORTING wf_wi_id      TYPE sww_wiid
      RETURNING VALUE(result) TYPE swwwihead
      RAISING   zcx_bc_wf_workitem.

    CLASS-METHODS cancel_workflows
      IMPORTING wi_ids    TYPE usmd_t_wi
                do_commit TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS get_container_value
      IMPORTING wi_id        TYPE sww_wiid
                element_name TYPE swfdname
      EXPORTING !value       TYPE any
      RAISING   cx_swf_cnt_container.

    CLASS-METHODS is_wf_user_logged_in RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS get_wf_initiator
      IMPORTING wi_id         TYPE sww_wiid
      RETURNING VALUE(result) TYPE xubname
      RAISING   cx_swf_cnt_container.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS c_tcode_buff_refr TYPE sytcode VALUE 'SWU_OBUF'.

    CONSTANTS: c_wf_user_r3 TYPE syuname VALUE 'WF-BATCH',
               c_wf_user_s4 TYPE syuname VALUE 'SAP_WFRT'.

    CONSTANTS: BEGIN OF field,
                 wi_id      TYPE fieldname VALUE 'WI_ID',
                 wi_rh_task TYPE fieldname VALUE 'WI_RH_TASK',
                 catid      TYPE fieldname VALUE 'CATID',
                 instid     TYPE fieldname VALUE 'INSTID',
                 typeid     TYPE fieldname VALUE 'TYPEID',
               END OF field.

    CONSTANTS: BEGIN OF element,
                 initiator TYPE swfdname VALUE '_WF_INITIATOR',
               END OF element.

    CLASS-METHODS get_active_wiids_of_sap_object
      IMPORTING iv_catid        TYPE sww_wi2obj-catid
                iv_instid       TYPE sww_wi2obj-instid
                iv_typeid       TYPE sww_wi2obj-typeid
      RETURNING VALUE(rt_wiids) TYPE usmd_t_wi.

ENDCLASS.


CLASS zcl_bc_wf_toolkit IMPLEMENTATION.
  METHOD cancel_old_active_workflows.
    DATA(lt_active_wiids) = get_active_wiids_of_sap_object( iv_catid  = iv_catid
                                                            iv_instid = iv_instid
                                                            iv_typeid = iv_typeid ).

    IF lines( lt_active_wiids ) <= 1.
      RETURN.
    ENDIF.

    SORT lt_active_wiids DESCENDING.
    DELETE lt_active_wiids INDEX 1.

    cancel_workflows( lt_active_wiids ).
  ENDMETHOD.

  METHOD get_active_status_range.
    rt_range = VALUE #( sign   = zcl_bc_ddic_toolkit=>c_sign_e
                        option = zcl_bc_ddic_toolkit=>c_option_eq
                        ( low = c_status-cancelled )
                        ( low = c_status-completed ) ).
  ENDMETHOD.

  METHOD get_inactive_status_range.
    rt_range = VALUE #( sign   = zcl_bc_ddic_toolkit=>c_sign_i
                        option = zcl_bc_ddic_toolkit=>c_option_eq
                        ( low = c_status-cancelled )
                        ( low = c_status-completed ) ).
  ENDMETHOD.

  METHOD get_active_wiids_of_sap_object.
    DATA(lt_active_status_range) = get_active_status_range( ).

    SELECT _obj~wi_id
           FROM sww_wi2obj AS _obj
                INNER JOIN swwwihead AS _head ON _head~wi_id = _obj~wi_id
           WHERE _obj~catid    = @iv_catid  AND
                 _obj~instid   = @iv_instid AND
                 _obj~typeid   = @iv_typeid AND
                 _head~wi_stat IN @lt_active_status_range
           INTO TABLE @rt_wiids.
  ENDMETHOD.

  METHOD get_first_agent_from_history.
    SELECT DISTINCT wi_id
           FROM sww_wi2obj
           WHERE wi_rh_task = @iv_task   AND
                 instid     = @iv_instid AND
                 typeid     = @iv_typeid
           INTO TABLE @DATA(lt_tasks).

    IF lt_tasks IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_wf_approver( textid   = zcx_bc_wf_approver=>cant_find_any_approver
                                              objectid = CONV #( iv_instid ) ).
    ENDIF.

    SELECT DISTINCT _outbox~wi_id, _outbox~wi_cd, _outbox~wi_ct, _outbox~wi_aagent, _head~top_wi_id "#EC CI_NO_TRANSFORM
           FROM sww_outbox           AS _outbox
                INNER JOIN swwwihead AS _head ON _head~wi_id = _outbox~wi_id
           FOR ALL ENTRIES IN @lt_tasks
           WHERE _outbox~wi_id   = @lt_tasks-wi_id AND
                 _outbox~wi_stat = @c_status-completed
           INTO TABLE @DATA(lt_approvers).

    IF lt_approvers IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_wf_approver( textid   = zcx_bc_wf_approver=>cant_find_any_approver
                                              objectid = CONV #( iv_instid ) ).
    ENDIF.

    SORT lt_approvers BY top_wi_id DESCENDING
                         wi_cd     ASCENDING
                         wi_ct     ASCENDING.

    rv_agent = lt_approvers[ 1 ]-wi_aagent.
  ENDMETHOD.

  METHOD is_user_workitem_recipient.
    DATA(lt_recipients) = VALUE swrtagent( ).

    CALL FUNCTION 'SAP_WAPI_WORKITEM_RECIPIENTS'
      EXPORTING workitem_id = iv_workitem_id
      TABLES    recipients  = lt_recipients.

    rv_recipient = xsdbool( line_exists( lt_recipients[ otype = c_otype-user
                                                        objid = iv_user ] ) ).
  ENDMETHOD.

  METHOD refresh_buffer.
    CLEAR et_msg.

    DATA(lo_bdc) = NEW zcl_bc_bdc( ).
    lo_bdc->add_scr( iv_prg = 'SAPLSWUO' iv_dyn = '0100' ).
    lo_bdc->add_fld( iv_nam = 'BDC_OKCODE' iv_val = '=REFR' ).

    lo_bdc->submit( EXPORTING iv_tcode  = c_tcode_buff_refr
                              is_option = VALUE #( dismode = 'N'
                                                   updmode = 'S' )
                    IMPORTING et_msg    = et_msg ).
  ENDMETHOD.

  METHOD get_active_dialog_workitem.
    DATA(related_wis) = VALUE tswwwihead( ).

    CALL FUNCTION 'SWI_GET_RELATED_WORKITEMS'
      EXPORTING wi_id       = wf_wi_id
      TABLES    related_wis = related_wis.

    DELETE related_wis WHERE NOT (
                                   wi_type = c_wi_type_w
                             AND (    wi_stat = c_status-ready
                                   OR wi_stat = c_status-started ) ).

    SORT related_wis BY wi_stat. " 1) Ready 2) Started

    TRY.
        result = related_wis[ 1 ].

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION NEW zcx_bc_wf_workitem( textid   = zcx_bc_wf_workitem=>no_open_dialog_item
                                                wf_wi_id = wf_wi_id ).
    ENDTRY.
  ENDMETHOD.

  METHOD cancel_workflows.
    LOOP AT wi_ids REFERENCE INTO DATA(wi_id).
      CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
        EXPORTING workitem_id = wi_id->*
                  do_commit   = do_commit.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_container_value.
    CLEAR value.

    TRY.
        DATA(context)   = cl_swf_run_workitem_context=>get_instance( wi_id ).
        DATA(container) = context->if_wapi_workitem_context~get_wf_container( ).

        container->get( EXPORTING name  = element_name
                        IMPORTING value = value ).

      CATCH cx_swf_cnt_container INTO DATA(container_error).
        RAISE EXCEPTION container_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW cx_swf_cnt_container( previous = diaper ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_wf_user_logged_in.
    result = xsdbool(    sy-uname = c_wf_user_r3
                      OR sy-uname = c_wf_user_s4 ).
  ENDMETHOD.

  METHOD get_wf_initiator.
    DATA container_value TYPE char20.

    get_container_value( EXPORTING wi_id        = wi_id
                                   element_name = zcl_bc_wf_toolkit=>element-initiator
                         IMPORTING value        = container_value ).

    SHIFT container_value LEFT BY 2 PLACES.
    result = container_value.
  ENDMETHOD.
ENDCLASS.