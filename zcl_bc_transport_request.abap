CLASS zcl_bc_transport_request DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_as4text_rng  TYPE RANGE OF e07t-as4text .
    TYPES:
      tt_obj_name_rng TYPE RANGE OF e071-obj_name .
    TYPES:
      tt_object_rng   TYPE RANGE OF e071-object .
    TYPES:
      BEGIN OF t_trkorr,
        trkorr TYPE trkorr,
      END OF t_trkorr .
    TYPES:
      tt_trkorr    TYPE STANDARD TABLE OF t_trkorr WITH DEFAULT KEY .
    TYPES:
      tt_pgmid_rng TYPE RANGE OF e071-pgmid .

    TYPES:
      tt_request_object TYPE STANDARD TABLE OF e071      WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_request_and_object,
        trkorr   TYPE e070-trkorr,
        as4text  TYPE e07t-as4text,
        strkorr  TYPE e070-strkorr,
        pgmid    TYPE e071-pgmid,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
      END OF t_request_and_object .
    TYPES:
      tt_request_and_object TYPE STANDARD TABLE OF t_request_and_object WITH DEFAULT KEY .
    TYPES:
      tt_request_attribute  TYPE STANDARD TABLE OF e070a WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_request_object_tag,
        pgmid    TYPE e071-pgmid,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
      END OF t_request_object_tag .
    TYPES:
      tt_request_object_tag TYPE STANDARD TABLE OF t_request_object_tag WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_request_object_type,
        pgmid  TYPE e071-pgmid,
        object TYPE e071-object,
      END OF t_request_object_type .
    TYPES:
      tt_request_key     TYPE STANDARD TABLE OF e071k     WITH DEFAULT KEY .
    TYPES:
      tt_request_nametab TYPE STANDARD TABLE OF e071kf    WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_content,
        object    TYPE tt_request_object,
        key       TYPE tt_request_key,
        nametab   TYPE tt_request_nametab,
        attribute TYPE tt_request_attribute,
      END OF t_content .
    TYPES:
      BEGIN OF t_request_obj,
        trkorr TYPE trkorr,
        obj    TYPE REF TO zcl_bc_transport_request,
      END OF t_request_obj .
    TYPES:
      tt_request_obj    TYPE STANDARD TABLE OF t_request_obj WITH DEFAULT KEY .

    TYPES:
      tt_trstatus_rng   TYPE RANGE OF e070-trstatus .
    TYPES:
      BEGIN OF t_request_param,
        s_trkorr            TYPE zbctt_trkorr_rng,
        s_trfunction        TYPE zbctt_trfunction_rng,
        s_trstatus          TYPE tt_trstatus_rng,
        s_as4text           TYPE tt_as4text_rng,
        s_as4date           TYPE date_t_range,
        p_must_have_subtask TYPE abap_bool,
        p_srch_strkorr      TYPE abap_bool,
        p_ignore_trkorr     TYPE abap_bool,
      END OF t_request_param .
    TYPES:
      BEGIN OF t_user,
        bname TYPE xubname,
      END OF t_user .
    TYPES:
      tt_user TYPE STANDARD TABLE OF t_user WITH DEFAULT KEY .

    CONSTANTS c_domain_trf TYPE dd07t-domname VALUE 'TRFUNCTION' ##NO_TEXT.
    CONSTANTS c_object_cinc TYPE e071-object VALUE 'CINC' ##NO_TEXT.
    CONSTANTS c_object_clas TYPE e071-object VALUE 'CLAS' ##NO_TEXT.
    CONSTANTS c_object_clsd TYPE e071-object VALUE 'CLSD' ##NO_TEXT.
    CONSTANTS c_object_cpub TYPE e071-object VALUE 'CPUB' ##NO_TEXT.
    CONSTANTS c_object_cpri TYPE e071-object VALUE 'CPRI' ##NO_TEXT.
    CONSTANTS c_object_doma TYPE e071-object VALUE 'DOMA' ##NO_TEXT.
    CONSTANTS c_object_domd TYPE e071-object VALUE 'DOMD' ##NO_TEXT.
    CONSTANTS c_object_dted TYPE e071-object VALUE 'DTED' ##NO_TEXT.
    CONSTANTS c_object_dtel TYPE e071-object VALUE 'DTEL' ##NO_TEXT.
    CONSTANTS c_object_tabd TYPE e071-object VALUE 'TABD' ##NO_TEXT.
    CONSTANTS c_object_tabl TYPE e071-object VALUE 'TABL' ##NO_TEXT.
    CONSTANTS c_object_intf TYPE e071-object VALUE 'INTF' ##NO_TEXT.
    CONSTANTS c_object_meth TYPE e071-object VALUE 'METH' ##NO_TEXT.
    CONSTANTS c_object_prog TYPE e071-object VALUE 'PROG' ##NO_TEXT.
    CONSTANTS c_pgmid_corr TYPE e071-pgmid VALUE 'CORR' ##NO_TEXT.
    CONSTANTS c_pgmid_limu TYPE e071-pgmid VALUE 'LIMU' ##NO_TEXT.
    CONSTANTS c_pgmid_r3tr TYPE e071-pgmid VALUE 'R3TR' ##NO_TEXT.
    CONSTANTS c_trfunction_cust TYPE trfunction VALUE 'W' ##NO_TEXT.
    CONSTANTS c_trfunction_cust_task TYPE trfunction VALUE 'Q' ##NO_TEXT.
    CONSTANTS c_trfunction_toc TYPE trfunction VALUE 'T' ##NO_TEXT.
    CONSTANTS c_trfunction_unclass TYPE trfunction VALUE 'X' ##NO_TEXT.
    CONSTANTS c_trfunction_wb TYPE trfunction VALUE 'K' ##NO_TEXT.
    CONSTANTS c_trstatus_modif TYPE trstatus VALUE 'D' ##NO_TEXT.
    CONSTANTS c_trstatus_modif_prot TYPE trstatus VALUE 'L' ##NO_TEXT.
    DATA gv_trkorr TYPE trkorr READ-ONLY .

    CLASS-METHODS create_new_request
      IMPORTING
        !iv_trfunction TYPE trfunction
        !iv_as4text    TYPE as4text OPTIONAL
        !it_user       TYPE tt_user OPTIONAL
        !iv_target     TYPE tr_target OPTIONAL
      RETURNING
        VALUE(ro_req)  TYPE REF TO zcl_bc_transport_request
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    CLASS-METHODS get_as4text_safe
      IMPORTING
        !iv_trkorr        TYPE trkorr
      RETURNING
        VALUE(rv_as4text) TYPE as4text .
    CLASS-METHODS get_empty_open_requests
      IMPORTING
        !it_trkorr_rng TYPE ZBCTT_TRKORR_RNG
      RETURNING
        VALUE(rt_list) TYPE zbctt_trkorr_det
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_trkorr    TYPE trkorr
        !iv_top       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_transport_request
      RAISING
        zcx_bc_table_content .

    CLASS-METHODS get_modified_objects
      IMPORTING
        !it_obj       TYPE zbctt_e071_obj_key
      RETURNING
        VALUE(rt_obj) TYPE zbctt_e071_obj_key.

    CLASS-METHODS get_open_status_rng
      RETURNING
        VALUE(rt_stat) TYPE tt_trstatus_rng .
    CLASS-METHODS get_open_requests
      IMPORTING
        !it_as4text_rng       TYPE tt_as4text_rng OPTIONAL
        !it_trfunction_rng    TYPE zbctt_trfunction_rng OPTIONAL
        !iv_must_have_subtask TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_list)        TYPE zbctt_trkorr_det .
    CLASS-METHODS get_request_list
      IMPORTING
        !is_param      TYPE t_request_param
      RETURNING
        VALUE(rt_list) TYPE zbctt_trkorr_det .
    CLASS-METHODS get_request_and_objects
      IMPORTING
        !it_tag          TYPE tt_request_object_tag
      RETURNING
        VALUE(rt_object) TYPE tt_request_and_object .
    CLASS-METHODS get_request_objects
      IMPORTING
        !it_trkorr_rng TYPE ZBCTT_TRKORR_RNG
        !it_pgmid_rng  TYPE tt_pgmid_rng OPTIONAL
      EXPORTING
        !et_list       TYPE zcl_bc_dol_model=>tt_dol_list
        !et_list_wr    TYPE zcl_bc_dol_model=>tt_dol_list_wr .
    CLASS-METHODS get_requests_containing_obj
      IMPORTING
        !it_obj            TYPE tt_request_object_tag
        !iv_top            TYPE abap_bool DEFAULT abap_true
        !iv_holistic_cls   TYPE abap_bool DEFAULT abap_false
        !it_trfunction_rng TYPE zbctt_trfunction_rng OPTIONAL
      RETURNING
        VALUE(rt_req)      TYPE tt_request_and_object .
    CLASS-METHODS get_source_client_safe
      IMPORTING
        !iv_trkorr       TYPE trkorr
      RETURNING
        VALUE(rv_client) TYPE e070c-client .
    CLASS-METHODS get_toc_safety_safe
      IMPORTING
        !iv_trkorr     TYPE trkorr
      RETURNING
        VALUE(rv_safe) TYPE abap_bool .
    CLASS-METHODS get_user_creatable_trf_rng
      RETURNING
        VALUE(rt_func) TYPE zbctt_trfunction_rng .

    CLASS-METHODS is_obj_type_class_related
      IMPORTING
        !is_obj_type      TYPE t_request_object_type
      RETURNING
        VALUE(rv_related) TYPE abap_bool .

    METHODS add_objects
      IMPORTING
        !it_obj               TYPE tt_request_object_tag
        !iv_sort_and_compress TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_bc_function_subrc
        RESUMABLE(zcx_bc_tr_sort_and_compress).

    METHODS add_objects_from_request
      IMPORTING
        !it_from              TYPE tt_trkorr
        !iv_wait              TYPE abap_bool DEFAULT abap_false
        !iv_sort_and_compress TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_bc_function_subrc
        RESUMABLE(zcx_bc_tr_sort_and_compress)
        zcx_bc_table_content .

    METHODS complete_shi_piece_list RAISING zcx_bc_sh_piece_list_compl.

    METHODS create_subtask
      IMPORTING
        !it_user TYPE tt_user .
    METHODS delete
      RAISING
        zcx_bc_function_subrc .
    METHODS delete_empty_subtasks
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    METHODS delete_object
      IMPORTING
        !is_obj TYPE t_request_object_tag
      RAISING
        zcx_bc_function_subrc .
    METHODS get_content
      RETURNING
        VALUE(rs_content) TYPE t_content
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    METHODS get_objects
      IMPORTING
        !it_pgmid_rng    TYPE tt_pgmid_rng OPTIONAL
        !it_object_rng   TYPE tt_object_rng OPTIONAL
        !it_obj_name_rng TYPE tt_obj_name_rng OPTIONAL
        !it_devclass_rng TYPE zcl_bc_abap_package=>tt_package_rng OPTIONAL
      RETURNING
        VALUE(rt_object) TYPE tt_request_object
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    METHODS get_header
      RETURNING
        VALUE(rs_header) TYPE e070 .
    METHODS get_obj_related_requests
      IMPORTING
        !iv_include_self   TYPE abap_bool DEFAULT abap_false
        !iv_top            TYPE abap_bool DEFAULT abap_true
        !it_obj_name_rng   TYPE tt_obj_name_rng OPTIONAL
        !iv_holistic_cls   TYPE abap_bool DEFAULT abap_false
        !it_trfunction_rng TYPE zbctt_trfunction_rng OPTIONAL
        !it_devclass_rng   TYPE zcl_bc_abap_package=>tt_package_rng OPTIONAL
      RETURNING
        VALUE(rt_list)     TYPE tt_request_and_object
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    METHODS get_source_client
      RETURNING
        VALUE(rv_client) TYPE e070c-client .
    METHODS get_subtasks
      IMPORTING
        !iv_only_empty    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_subtask) TYPE tt_request_obj
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    METHODS get_text
      RETURNING
        VALUE(rv_as4text) TYPE as4text .

    METHODS has_locked_object RETURNING VALUE(rv_has) TYPE abap_bool.

    METHODS is_empty
      RETURNING
        VALUE(rv_empty) TYPE abap_bool
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content .
    METHODS is_toc_safe
      RETURNING
        VALUE(rv_safe) TYPE abap_bool .

    METHODS release
      IMPORTING
        !iv_rel_subtasks_too    TYPE abap_bool
        !iv_del_empty_subtasks  TYPE abap_bool DEFAULT abap_true
        !iv_wait_until_released TYPE abap_bool DEFAULT abap_false
        !iv_max_rel_wait        TYPE i DEFAULT 30
        !iv_compl_sh_piece_list TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !ev_rel_wait_success    TYPE abap_bool
      RAISING
        zcx_bc_function_subrc
        zcx_bc_table_content.

    METHODS sort_and_compress RAISING zcx_bc_tr_sort_and_compress.





  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_abap_bool           TYPE STANDARD TABLE OF abap_bool WITH DEFAULT KEY,
      tt_request_object_type TYPE HASHED TABLE OF t_request_object_type WITH UNIQUE KEY primary_key COMPONENTS pgmid object,

      BEGIN OF t_clazy_flag,
        class_related_obj_tags TYPE abap_bool,
      END OF t_clazy_flag,

      BEGIN OF t_clazy_val,
        class_related_obj_tag TYPE tt_request_object_type,
      END OF t_clazy_val,

      BEGIN OF t_lazy_flag,
        as4text       TYPE abap_bool,
        content       TYPE abap_bool,
        e070          TYPE abap_bool,
        empty         TYPE abap_bool,
        source_client TYPE abap_bool,
        toc_safe      TYPE abap_bool,
      END OF t_lazy_flag,

      BEGIN OF t_lazy_val,
        as4text       TYPE as4text,
        content       TYPE t_content,
        e070          TYPE e070,
        empty         TYPE abap_bool,
        source_client TYPE e070c-client,
        toc_safe      TYPE abap_bool,
      END OF t_lazy_val,

      BEGIN OF t_obj_name,
        obj_name TYPE e071-obj_name,
      END OF t_obj_name,

      tt_obj_name TYPE STANDARD TABLE OF t_obj_name WITH DEFAULT KEY.

    CONSTANTS:
      c_tabname_e070         TYPE tabname VALUE 'E070',
      c_tcode_shi_piece_list TYPE sytcode VALUE 'ZBC901'.

    CLASS-DATA:
      gs_clazy_flag TYPE t_clazy_flag,
      gs_clazy_val  TYPE t_clazy_val.

    DATA:
      gs_lazy_flag TYPE t_lazy_flag,
      gs_lazy_val  TYPE t_lazy_val.

    CLASS-METHODS:
      release_single
        IMPORTING
          !iv_trkorr              TYPE trkorr
          !io_req                 TYPE REF TO zcl_bc_transport_request OPTIONAL
          !iv_wait_until_released TYPE abap_bool DEFAULT abap_false
          !iv_max_rel_wait        TYPE i DEFAULT 30
          !iv_compl_sh_piece_list TYPE abap_bool DEFAULT abap_true
        EXPORTING
          !ev_rel_wait_success    TYPE abap_bool
        RAISING
          zcx_bc_function_subrc
          zcx_bc_table_content.

ENDCLASS.



CLASS zcl_bc_transport_request IMPLEMENTATION.


  METHOD add_objects.

    DATA lt_t071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.

    " ______________________________
    " Nesneleri ekle

    CHECK it_obj IS NOT INITIAL.

    lt_t071 = CORRESPONDING #( it_obj ).

    CALL FUNCTION 'TRINT_APPEND_TO_COMM_ARRAYS'
      EXPORTING
        wi_error_table            = abap_true
        wi_trkorr                 = gv_trkorr
        iv_append_at_order        = abap_true
      TABLES
        wt_e071                   = lt_t071
      EXCEPTIONS
        key_check_keysyntax_error = 1
        ob_check_obj_error        = 2
        tr_lockmod_failed         = 3
        tr_lock_enqueue_failed    = 4
        tr_wrong_order_type       = 5
        tr_order_update_error     = 6
        file_access_error         = 7
        ob_no_systemname          = 8
        OTHERS                    = 9
        ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_APPEND_TO_COMM_ARRAYS' ).

    " ______________________________
    " Sort & Compress

    IF iv_sort_and_compress EQ abap_true.
      TRY.
          sort_and_compress( ).
        CATCH zcx_bc_tr_sort_and_compress INTO DATA(lo_sac).
          RAISE RESUMABLE EXCEPTION lo_sac.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD add_objects_from_request.

    " ______________________________
    " Request'ler ve Task'lerin listesini oluştur

    DATA(lt_trkorr_from) = it_from.

    LOOP AT it_from ASSIGNING FIELD-SYMBOL(<ls_from>).

      APPEND LINES OF CORRESPONDING tt_trkorr(
          get_instance(
              iv_trkorr = <ls_from>-trkorr
              iv_top = abap_true
          )->get_subtasks( )
      ) TO lt_trkorr_from.

    ENDLOOP.

    SORT lt_trkorr_from BY trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr_from COMPARING trkorr.

    " ______________________________
    " Ekleme yap

    LOOP AT lt_trkorr_from ASSIGNING <ls_from>.

      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = abap_false
          wi_trkorr_from           = <ls_from>-trkorr
          wi_trkorr_to             = gv_trkorr
          wi_without_documentation = abap_true
        EXCEPTIONS
          db_access_error          = 1
          trkorr_from_not_exist    = 2
          trkorr_to_is_repair      = 3
          trkorr_to_locked         = 4
          trkorr_to_not_exist      = 5
          trkorr_to_released       = 6
          user_not_owner           = 7
          no_authorization         = 8
          wrong_client             = 9
          wrong_category           = 10
          object_not_patchable     = 11
          OTHERS                   = 12
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TR_COPY_COMM' ).

      CHECK iv_wait EQ abap_true.
      WAIT UP TO 1 SECONDS.

    ENDLOOP.

    " ______________________________
    " Sort & Compress

    IF iv_sort_and_compress EQ abap_true.
      TRY.
          sort_and_compress( ).
        CATCH zcx_bc_tr_sort_and_compress INTO DATA(lo_sac).
          RAISE RESUMABLE EXCEPTION lo_sac.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD complete_shi_piece_list.

    TRY.
        DATA(lo_bdc) = NEW zcl_bc_bdc( ).

        lo_bdc->add_scr(
          iv_prg = 'RS_STREE_OBJECTS_TO_REQ_GET'
          iv_dyn = '1000'
        ).

        lo_bdc->add_fld(:
          iv_nam = 'BDC_OKCODE' iv_val = '=ONLI' ),
          iv_nam = 'P_TRKORR'   iv_val = CONV #( gv_trkorr ) ).

        lo_bdc->add_scr(
          iv_prg = 'RS_STREE_OBJECTS_TO_REQ_GET'
          iv_dyn = '0100'
        ).

        lo_bdc->add_fld(
          iv_nam = 'BDC_OKCODE'
          iv_val = '=ENTER'
        ).

        lo_bdc->add_scr(
          iv_prg = 'RS_STREE_OBJECTS_TO_REQ_GET'
          iv_dyn = '1000'
        ).

        lo_bdc->add_fld(
          iv_nam = 'BDC_OKCODE'
          iv_val = '/EE'
        ).

        lo_bdc->submit(
          EXPORTING
            iv_tcode  = c_tcode_shi_piece_list
            is_option = VALUE #( dismode = zcl_bc_bdc=>c_dismode_none )
          IMPORTING
            et_msg    = DATA(lt_msg)
        ).

        LOOP AT lt_msg
            TRANSPORTING NO FIELDS
            WHERE msgtyp IN zcl_bc_applog_facade=>get_crit_msgty_range( ).

          RAISE EXCEPTION TYPE zcx_bc_bdc EXPORTING bdcmsgcoll = lt_msg.

        ENDLOOP.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_sh_piece_list_compl
          EXPORTING
            previous = lo_diaper
            trkorr   = gv_trkorr.

    ENDTRY.

  ENDMETHOD.

  METHOD create_new_request.

    DATA:
      ls_head    TYPE trwbo_request_header,
      lv_as4text TYPE as4text.


    IF it_user IS SUPPLIED.
      DATA(lt_user) = VALUE scts_users( FOR ls_user IN it_user (
        user = ls_user-bname
        type = c_trfunction_unclass
      ) ).
    ENDIF.

    lv_as4text = COND #(
      WHEN iv_as4text IS SUPPLIED THEN iv_as4text
      ELSE TEXT-321
    ).

    CLEAR ls_head.

    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = iv_trfunction
        iv_text           = lv_as4text
        it_users          = lt_user
        iv_target         = iv_target
      IMPORTING
        es_request_header = ls_head
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2
        OTHERS            = 3
        ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TR_INSERT_REQUEST_WITH_TASKS' ).

    ro_req = get_instance( ls_head-trkorr ).

  ENDMETHOD.


  METHOD create_subtask.

    DATA:
      lt_msg   TYPE STANDARD TABLE OF tr004,
      lt_tr005 TYPE STANDARD TABLE OF tr005.

    CLEAR: lt_tr005[], lt_msg[].

    lt_tr005 = VALUE #( FOR ls_user IN it_user ( as4user = ls_user-bname ) ).

    CALL FUNCTION 'TR40_TASK_ADD'
      EXPORTING
        iv_trkorr   = gv_trkorr
      TABLES
        tt_userlist = lt_tr005
        tt_msg      = lt_msg.

  ENDMETHOD.


  METHOD delete.

    CALL FUNCTION 'TRINT_DELETE_COMM'
      EXPORTING
        wi_dialog                     = abap_false
        wi_trkorr                     = gv_trkorr
        iv_without_any_checks         = abap_true
        iv_without_user_check         = abap_true
        iv_without_ctsproject_check   = abap_true
      EXCEPTIONS
        file_access_error             = 1
        order_already_released        = 2
        order_contains_c_member       = 3
        order_contains_locked_entries = 4
        order_is_refered              = 5
        repair_order                  = 6
        user_not_owner                = 7
        delete_was_cancelled          = 8
        objects_free_but_still_locks  = 9
        order_lock_failed             = 10
        wrong_client                  = 11
        project_still_referenced      = 12
        successors_already_released   = 13
        OTHERS                        = 14
        ##fm_Subrc_ok.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_DELETE_COMM' ).

  ENDMETHOD.


  METHOD delete_empty_subtasks.

    LOOP AT get_subtasks( iv_only_empty = abap_true ) ASSIGNING FIELD-SYMBOL(<ls_sub>).
      <ls_sub>-obj->delete( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_object.

    DATA(ls_e071) = CORRESPONDING e071( is_obj ).
    ls_e071-trkorr = gv_trkorr.

    DATA(ls_req) = VALUE trwbo_request( ).

    CALL FUNCTION 'TRINT_DELETE_COMM_OBJECT_KEYS'
      EXPORTING
        is_e071_delete              = ls_e071
        iv_dialog_flag              = abap_false
      CHANGING
        cs_request                  = ls_req
      EXCEPTIONS
        e_bad_target_request        = 1
        e_database_access_error     = 2
        e_empty_lockkey             = 3
        e_wrong_source_client       = 4
        n_no_deletion_of_c_objects  = 5
        n_no_deletion_of_corr_entry = 6
        n_object_entry_doesnt_exist = 7
        n_request_already_released  = 8
        n_request_from_other_system = 9
        r_user_cancelled            = 10
        r_user_didnt_confirm        = 11
        r_foreign_lock              = 12
        w_bigger_lock_in_same_order = 13
        w_duplicate_entry           = 14
        w_no_authorization          = 15
        w_user_not_owner            = 16
        OTHERS                      = 17
        ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_DELETE_COMM_OBJECT_KEYS' ).

  ENDMETHOD.


  METHOD get_as4text_safe.

    TRY.
        rv_as4text = get_instance( iv_trkorr )->get_text( ).
      CATCH cx_root ##no_handler .
    ENDTRY.

  ENDMETHOD.


  METHOD get_content.

    IF gs_lazy_flag-content IS INITIAL.

      CALL FUNCTION 'TR_READ_COMM'
        EXPORTING
          wi_trkorr        = gv_trkorr
          wi_dialog        = abap_false
          wi_langu         = sy-langu
          wi_sel_e071      = abap_true
          wi_sel_e071k     = abap_true
          iv_sel_e071kf    = abap_true
          iv_sel_e070a     = abap_true
        TABLES
          wt_e071          = gs_lazy_val-content-object
          wt_e071k         = gs_lazy_val-content-key
          et_e071kf        = gs_lazy_val-content-nametab
          et_e070a         = gs_lazy_val-content-attribute
        EXCEPTIONS
          not_exist_e070   = 1
          no_authorization = 2
          OTHERS           = 3
          ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TR_READ_COMM' ).

      LOOP AT get_subtasks( ) ASSIGNING FIELD-SYMBOL(<ls_sub>).
        DATA(ls_sub_content) = <ls_sub>-obj->get_content( ).

        APPEND LINES OF:
          ls_sub_content-object    TO gs_lazy_val-content-object,
          ls_sub_content-key       TO gs_lazy_val-content-key,
          ls_sub_content-nametab   TO gs_lazy_val-content-nametab,
          ls_sub_content-attribute TO gs_lazy_val-content-attribute.

      ENDLOOP.

      gs_lazy_flag-content = abap_true.
    ENDIF.

    rs_content = gs_lazy_val-content.

  ENDMETHOD.


  METHOD get_empty_open_requests.

    SELECT trkorr
      INTO TABLE @DATA(lt_trkorr)
      FROM e070 AS request
      WHERE trkorr IN @it_trkorr_rng AND
            (
              strkorr EQ @space OR
              strkorr IS NULL
            )

            AND

            ( NOT EXISTS (
              SELECT trkorr FROM e071
              WHERE trkorr EQ request~trkorr
            ) )

            AND

            (
              NOT EXISTS
              (
                SELECT trkorr FROM e070 AS task
                WHERE strkorr EQ request~trkorr AND
                      EXISTS ( SELECT trkorr FROM e071
                               WHERE trkorr EQ task~trkorr
                             )
              )
            ).

    LOOP AT lt_trkorr ASSIGNING FIELD-SYMBOL(<ls_trkorr>).
      CHECK get_instance( <ls_trkorr>-trkorr )->is_empty( ) EQ abap_false.
      DELETE lt_trkorr.
      CONTINUE.
    ENDLOOP.

    CHECK lt_trkorr IS NOT INITIAL.

    rt_list = get_request_list( VALUE #(
      s_trkorr        = VALUE #( FOR ls_trkorr IN lt_trkorr (
                          option = zcl_bc_ddic_toolkit=>c_option_eq
                          sign   = zcl_bc_ddic_toolkit=>c_sign_i
                          low    = ls_trkorr-trkorr
                        ) )
      s_trstatus      = get_open_status_rng( )
      p_srch_strkorr  = abap_false
      p_ignore_trkorr = abap_false
    ) ).

  ENDMETHOD.


  METHOD get_header.

    IF gs_lazy_flag-e070 IS INITIAL.

      SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_lazy_val-e070
        FROM e070
        WHERE trkorr EQ gv_trkorr.

      gs_lazy_flag-e070 = abap_true.
    ENDIF.

    rs_header = gs_lazy_val-e070.

  ENDMETHOD.


  METHOD get_instance.

    SELECT SINGLE trkorr, strkorr
      INTO @DATA(ls_e070)
      FROM e070
      WHERE trkorr EQ @iv_trkorr.

    IF sy-subrc NE 0.

      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_trkorr )
          tabname  = c_tabname_e070.

    ENDIF.

    ro_obj = NEW #( ).
    ro_obj->gv_trkorr = SWITCH #( iv_top
      WHEN abap_false THEN ls_e070-trkorr
      WHEN abap_true THEN COND #(
        WHEN ls_e070-strkorr IS NOT INITIAL THEN ls_e070-strkorr
        ELSE ls_e070-trkorr
      )
    ).

  ENDMETHOD.


  METHOD get_modified_objects.

    DATA(lt_req) = get_requests_containing_obj(
        it_obj          = it_obj
        iv_top          = abap_true
        iv_holistic_cls = abap_true
    ).

    DELETE lt_req WHERE trkorr+0(3) NE zcl_bc_sap_system=>c_sysid_dev.
    SORT lt_req BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_req COMPARING pgmid object obj_name. " Binary Search var

    LOOP AT it_obj ASSIGNING FIELD-SYMBOL(<ls_obj>).

      READ TABLE lt_req ASSIGNING FIELD-SYMBOL(<ls_req>)
          WITH KEY
              pgmid = <ls_obj>-pgmid
              object = <ls_obj>-object
              obj_name = <ls_obj>-obj_name
          BINARY SEARCH.

      IF sy-subrc EQ 0.
        APPEND <ls_obj> TO rt_obj.
        CONTINUE.
      ENDIF.

      CHECK is_obj_type_class_related( CORRESPONDING #( <ls_obj> ) ) EQ abap_true.

      LOOP AT lt_req ASSIGNING <ls_req> WHERE obj_name+0(30) EQ <ls_obj>-obj_name+0(30).
        APPEND CORRESPONDING #( <ls_req> ) TO rt_obj.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      APPEND <ls_obj> TO rt_obj.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_objects.

    " ______________________________
    " Nesneleri tespit edip basit filtrelerden geçir

    rt_object = get_content( )-object.

    DELETE rt_object WHERE NOT (
      pgmid    IN it_pgmid_rng AND
      object   IN it_object_rng AND
      obj_name IN it_obj_name_rng
    ).

    " ______________________________
    " Geliştirme sınıfı filtresi verildiyse, filtreye uymayanları ele

    CHECK
        it_devclass_rng IS NOT INITIAL AND
        rt_object IS NOT INITIAL.

    DATA(lt_dc) = zcl_bc_abap_package=>get_package_of_objects( CORRESPONDING #( rt_object ) ).

    LOOP AT rt_object ASSIGNING FIELD-SYMBOL(<ls_object>).

      ASSIGN lt_dc[
        KEY primary_key COMPONENTS key = VALUE #(
          pgmid = <ls_object>-pgmid
          object = <ls_object>-object
          obj_name = <ls_object>-obj_name
      ) ] TO FIELD-SYMBOL(<ls_dc>).

      CHECK
        sy-subrc EQ 0 AND
        <ls_dc>-devclass NOT IN it_devclass_rng.

      DELETE rt_object.
      CONTINUE.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_obj_related_requests.

    rt_list = get_requests_containing_obj(
      it_obj            = CORRESPONDING #( get_objects(
        it_obj_name_rng = it_obj_name_rng
        it_devclass_rng = it_devclass_rng
      ) )
      iv_top            = iv_top
      iv_holistic_cls   = iv_holistic_cls
      it_trfunction_rng = it_trfunction_rng
    ).

    CHECK iv_include_self EQ abap_false.

    TRY.
        DATA(ls_head) = get_header( ).

        DELETE rt_list WHERE
          trkorr  EQ ls_head-trkorr  OR
          trkorr  EQ ls_head-strkorr OR
          (
            strkorr IS NOT INITIAL AND
            (
              strkorr EQ ls_head-trkorr  OR
              strkorr EQ ls_head-strkorr
            )
          ).

        IF ls_head-strkorr IS NOT INITIAL.
          DATA(ls_shead) = get_instance( ls_head-strkorr )->get_header( ).

          DELETE rt_list WHERE
            trkorr  EQ ls_shead-trkorr OR
            strkorr EQ ls_shead-trkorr.

        ENDIF.

      CATCH cx_root ##no_handler .
    ENDTRY.

  ENDMETHOD.


  METHOD get_open_requests.

    rt_list = get_request_list( VALUE #(
      s_trfunction        = COND #(
                              WHEN it_trfunction_rng IS NOT INITIAL THEN it_trfunction_rng
                              ELSE get_user_creatable_trf_rng( )
                            )
      s_trstatus          = get_open_status_rng( )
      s_as4text           = it_as4text_rng
      p_must_have_subtask = iv_must_have_subtask
    ) ).

  ENDMETHOD.


  METHOD get_open_status_rng.

    rt_stat = VALUE #(
      option = zcl_bc_ddic_toolkit=>c_option_eq
      sign   = zcl_bc_ddic_toolkit=>c_sign_i
      ( low = c_trstatus_modif )
      ( low = c_trstatus_modif_prot )
    ).

  ENDMETHOD.


  METHOD get_requests_containing_obj.

    DATA:
      lt_obj_name_rng TYPE RANGE OF e071-obj_name,
      lt_obj_name_tmp TYPE tt_obj_name.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Eğer parametre gönderilmediyse, burada işimiz yok.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CHECK it_obj IS NOT INITIAL.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Elimizdeki nesne listesiyle, bu nesnelerin geçtiği Request'leri bul
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT
        trkorr, pgmid, object, obj_name
      INTO TABLE @DATA(lt_e071)
      FROM e071
      FOR ALL ENTRIES IN @it_obj
      WHERE
        pgmid    EQ @it_obj-pgmid AND
        object   EQ @it_obj-object AND
        obj_name EQ @it_obj-obj_name.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Eğer istendiyse, Holistic sınıf okuması da yapalım.
    "
    " Holistic şu demek:
    " Mesela bir sınıfın bir Methodu Request'e girdiyse,
    " o sınıfın herhangi bir başka parçasının bulunduğu bir başka Request
    " bizimle bağlantılıdır. Bu durumdaki bağlantılı Request'leri bulalım.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF iv_holistic_cls EQ abap_true.

      LOOP AT it_obj ASSIGNING FIELD-SYMBOL(<ls_obj>).

        CHECK is_obj_type_class_related( CORRESPONDING #( <ls_obj> ) ) EQ abap_true.

        CLEAR lt_obj_name_tmp.
        SPLIT <ls_obj>-obj_name AT space INTO TABLE lt_obj_name_tmp.

        APPEND VALUE #(
          option = zcl_bc_ddic_toolkit=>c_option_cp
          sign   = zcl_bc_ddic_toolkit=>c_sign_i
          low    = |{ COND #( WHEN lt_obj_name_tmp IS INITIAL THEN <ls_obj>-obj_name ELSE lt_obj_name_tmp[ 1 ]-obj_name ) }*|
        ) TO lt_obj_name_rng.

      ENDLOOP.

      SORT lt_obj_name_rng BY option sign low high.
      DELETE ADJACENT DUPLICATES FROM lt_obj_name_rng COMPARING option sign low high.

      IF lt_obj_name_rng IS NOT INITIAL.

        SELECT
            trkorr, pgmid, object, obj_name
          APPENDING CORRESPONDING FIELDS OF TABLE @lt_e071
          FROM e071
          WHERE obj_name IN @lt_obj_name_rng. "#EC CI_NOFIRST

      ENDIF.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Bulduğun Request'lerden liste oluştur
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SORT lt_e071 BY trkorr pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_e071 COMPARING trkorr pgmid object obj_name.

    LOOP AT lt_e071 ASSIGNING FIELD-SYMBOL(<ls_e071>).

      TRY.
          DATA(lo_req) = get_instance(
            iv_trkorr = <ls_e071>-trkorr
            iv_top    = iv_top
          ).

          DATA(ls_head) = lo_req->get_header( ).

          APPEND VALUE #(
            trkorr   = ls_head-trkorr
            as4text  = lo_req->get_text( )
            strkorr  = ls_head-strkorr
            pgmid    = <ls_e071>-pgmid
            object   = <ls_e071>-object
            obj_name = <ls_e071>-obj_name
          ) TO rt_req.

        CATCH cx_root ##no_Handler .
      ENDTRY.

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " İstendiyse, Request türüne göre filtreleme yapalım
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF it_trfunction_rng IS NOT INITIAL.

      LOOP AT rt_req ASSIGNING FIELD-SYMBOL(<ls_req>).

        TRY.

            CHECK get_instance(
              iv_trkorr = <ls_req>-trkorr
              iv_top    = abap_true
            )->get_header( )-trfunction NOT IN it_trfunction_rng.

            DELETE rt_req.
            CONTINUE.

          CATCH cx_root ##no_handler.
            " Filtreleyemezsek, ortalığı ayağa kaldırmayalım
        ENDTRY.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_request_and_objects.

    CHECK it_tag IS NOT INITIAL.

    SELECT e070~trkorr e07t~as4text e070~strkorr e071~obj_name
      INTO CORRESPONDING FIELDS OF TABLE rt_object
      FROM
        e071
        INNER JOIN e070 ON e070~trkorr EQ e071~trkorr
        LEFT JOIN e07t ON e07t~trkorr EQ e071~trkorr
                      AND e07t~langu EQ sy-langu
      FOR ALL ENTRIES IN it_tag
      WHERE
        pgmid    EQ it_tag-pgmid AND
        object   EQ it_tag-object AND
        obj_name EQ it_tag-obj_name
      ##TOO_MANY_ITAB_FIELDS .

  ENDMETHOD.


  METHOD get_request_list.

*   Ham liste

    IF is_param-p_ignore_trkorr EQ abap_false.

      SELECT
            e070~trkorr, e070~trfunction, e070~trstatus,
            e07t~as4text, e070~as4user, e070~strkorr,
            e070~as4date, e070~as4time,
            e070~tarsystem
        APPENDING CORRESPONDING FIELDS OF TABLE @rt_list
        FROM
          e070
          LEFT JOIN e07t ON e07t~trkorr EQ e070~trkorr
        WHERE (
          e070~trkorr     IN @is_param-s_trkorr AND
          e070~trfunction IN @is_param-s_trfunction AND
          e070~trstatus   IN @is_param-s_trstatus AND
          e070~as4date    IN @is_param-s_as4date
        )
        ##TOO_MANY_ITAB_FIELDS.

    ENDIF.

    IF is_param-p_srch_strkorr EQ abap_true.

      SELECT
            e070~trkorr, e070~trfunction, e070~trstatus, e07t~as4text,
            e070~as4user, e070~strkorr, e070~as4date, e070~as4time,
            e070~tarsystem
        APPENDING CORRESPONDING FIELDS OF TABLE @rt_list
        FROM
          e070
          LEFT JOIN e07t ON e07t~trkorr EQ e070~strkorr
        WHERE (
          e070~strkorr    IN @is_param-s_trkorr AND
          e070~trfunction IN @is_param-s_trfunction AND
          e070~trstatus   IN @is_param-s_trstatus AND
          e070~as4date    IN @is_param-s_as4date
        )
        ##TOO_MANY_ITAB_FIELDS.

    ENDIF.

    DELETE rt_list WHERE as4text NOT IN is_param-s_as4text.

    SORT rt_list BY trkorr.
    DELETE ADJACENT DUPLICATES FROM rt_list COMPARING trkorr.

*   Altında task olmalı?

    IF is_param-p_must_have_subtask EQ abap_true AND
       rt_list IS NOT INITIAL.

      SELECT
        trkorr, strkorr
        INTO TABLE @DATA(lt_task)
        FROM e070
        FOR ALL ENTRIES IN @rt_list
        WHERE
          strkorr EQ @rt_list-trkorr AND
          trstatus IN (@c_trstatus_modif, @c_trstatus_modif_prot).

      SORT lt_task BY strkorr.

      LOOP AT rt_list ASSIGNING FIELD-SYMBOL(<ls_list>).

        READ TABLE lt_task
           TRANSPORTING NO FIELDS
           WITH KEY strkorr = <ls_list>-trkorr
           BINARY SEARCH.

        CHECK sy-subrc NE 0.

        DELETE rt_list.
        CONTINUE.

      ENDLOOP.

    ENDIF.

*   Domain metinleri

    TRY.

        DATA(lo_domain_trf) = zcl_bc_mdf_domain_line=>get_instance( c_domain_trf ).

        LOOP AT rt_list ASSIGNING <ls_list>.

          TRY.
              <ls_list>-trftxt = lo_domain_trf->go_domain->get_value_text( CONV #( <ls_list>-trfunction ) ).
            CATCH cx_root ##no_Handler .
          ENDTRY.

        ENDLOOP.

      CATCH cx_root ##no_Handler.
    ENDTRY.

  ENDMETHOD.


  METHOD get_request_objects.

    DATA lt_trkorr    TYPE RANGE OF e070-trkorr.

    CLEAR: et_list,
           et_list_wr.

    SELECT
        @zcl_bc_ddic_toolkit=>c_option_eq AS option,
        @zcl_bc_ddic_toolkit=>c_sign_i    AS sign,
        trkorr       AS low
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_trkorr
      FROM e070
      WHERE trkorr  IN @it_trkorr_rng OR
            strkorr IN @it_trkorr_rng
      ##TOO_MANY_ITAB_FIELDS.

    SELECT
        @zcl_bc_ddic_toolkit=>c_option_eq AS option,
        @zcl_bc_ddic_toolkit=>c_sign_i    AS sign,
        strkorr      AS low
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_trkorr
      FROM e070
      WHERE trkorr  IN @it_trkorr_rng OR
            strkorr IN @it_trkorr_rng
      ##TOO_MANY_ITAB_FIELDS .

    SORT lt_trkorr BY low.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr COMPARING low.
    CHECK lt_trkorr IS NOT INITIAL.

    DATA(lt_pgmid_rng) = COND #(
      WHEN it_pgmid_rng IS SUPPLIED THEN it_pgmid_rng
      ELSE VALUE #( ( option = zcl_bc_ddic_toolkit=>c_option_eq
                      sign   = zcl_bc_ddic_toolkit=>c_sign_i
                      low    = zif_bc_dol_obj=>c_pgmid_r3tr ) )
    ).

    SELECT DISTINCT
      e071~trkorr, e071~pgmid, e071~object, e071~obj_name,
      e07t~as4text
      INTO CORRESPONDING FIELDS OF TABLE @et_list_wr
      FROM e071
           LEFT JOIN e07t ON e07t~trkorr EQ e071~trkorr AND
                             e07t~langu EQ @sy-langu
      WHERE
        e071~trkorr IN @lt_trkorr AND
        e071~pgmid  IN @lt_pgmid_rng ##too_many_itab_fields
      ORDER BY e071~pgmid, e071~object, e071~obj_name.

    DELETE ADJACENT DUPLICATES FROM et_list_wr COMPARING trkorr pgmid object obj_name.

    LOOP AT et_list_wr ASSIGNING FIELD-SYMBOL(<ls_list>).

      TRY.
          zcl_bc_dol_model=>get_dol_obj( EXPORTING iv_object = <ls_list>-object
                                         IMPORTING eo_dol    = DATA(lo_dol) ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      IF lo_dol->is_deleted( iv_pgmid    = <ls_list>-pgmid
                             iv_object   = <ls_list>-object
                             iv_obj_name = <ls_list>-obj_name
                           ) EQ abap_true.

        DELETE et_list_wr.
        CONTINUE.
      ENDIF.

      <ls_list>-object_txt = lo_dol->get_object_txt( iv_pgmid  = <ls_list>-pgmid
                                                     iv_object = <ls_list>-object ).

      <ls_list>-ddtext = lo_dol->get_ddtext( iv_pgmid    = <ls_list>-pgmid
                                             iv_object   = <ls_list>-object
                                             iv_obj_name = <ls_list>-obj_name ).
    ENDLOOP.

    et_list = CORRESPONDING #( et_list_wr ).
    SORT et_list BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM et_list COMPARING pgmid object obj_name.

  ENDMETHOD.


  METHOD get_source_client.

    IF gs_lazy_flag-source_client EQ abap_false.
      SELECT SINGLE client INTO gs_lazy_val-source_client FROM e070c WHERE trkorr EQ gv_trkorr.
      gs_lazy_flag-source_client = abap_true.
    ENDIF.

    rv_client = gs_lazy_val-source_client.

  ENDMETHOD.


  METHOD get_source_client_safe.

    TRY.
        rv_client = zcl_bc_transport_request=>get_instance( iv_trkorr )->get_source_client( ).
      CATCH cx_root ##no_handler .
    ENDTRY.

  ENDMETHOD.


  METHOD get_subtasks.

    SELECT trkorr
      INTO CORRESPONDING FIELDS OF TABLE @rt_subtask
      FROM e070
      WHERE strkorr EQ @gv_trkorr
      ##TOO_MANY_ITAB_FIELDS .

    LOOP AT rt_subtask ASSIGNING FIELD-SYMBOL(<ls_subtask>).
      <ls_subtask>-obj = zcl_bc_transport_request=>get_instance( <ls_subtask>-trkorr ).

      IF iv_only_empty EQ abap_true AND
         <ls_subtask>-obj->is_empty( ) EQ abap_false.

        DELETE rt_subtask.
        CONTINUE.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_text.

    IF gs_lazy_flag-as4text EQ abap_false.

      SELECT SINGLE as4text
        INTO @gs_lazy_val-as4text
        FROM e07t
        WHERE
          trkorr EQ @gv_trkorr AND
          langu  EQ @sy-langu.

      IF sy-subrc NE 0.

        SELECT SINGLE as4text
          INTO @gs_lazy_val-as4text
          FROM e07t
          WHERE trkorr EQ @gv_trkorr ##WARN_OK.

      ENDIF.

      gs_lazy_flag-as4text = abap_true.
    ENDIF.

    rv_as4text = gs_lazy_val-as4text.

  ENDMETHOD.


  METHOD get_toc_safety_safe.

    TRY.
        rv_safe = get_instance( iv_trkorr )->is_toc_safe( ).
      CATCH cx_root ##no_Handler .
    ENDTRY.

  ENDMETHOD.


  METHOD get_user_creatable_trf_rng.

    rt_func = VALUE #(
      option = zcl_bc_ddic_toolkit=>c_option_eq
      sign   = zcl_bc_ddic_toolkit=>c_sign_i
      ( low = c_trfunction_cust )
      ( low = c_trfunction_toc )
      ( low = c_trfunction_wb )
    ).

  ENDMETHOD.

  METHOD has_locked_object.

    SELECT SINGLE lockflag
        INTO @DATA(lv_dummy)
        FROM e071
        WHERE
            trkorr EQ @gv_trkorr AND
            lockflag EQ @abap_true
        ##WARN_OK.

    rv_has = xsdbool( sy-subrc EQ 0 ).

  ENDMETHOD.


  METHOD is_empty.

    IF gs_lazy_flag-empty EQ abap_false.
      gs_lazy_val-empty = boolc( get_content( ) IS INITIAL ).
      gs_lazy_flag-empty = abap_true.
    ENDIF.

    rv_empty = gs_lazy_val-empty.

  ENDMETHOD.


  METHOD is_obj_type_class_related.

    IF gs_clazy_flag-class_related_obj_tags EQ abap_false.

      gs_clazy_val-class_related_obj_tag = VALUE #(
        ( pgmid = c_pgmid_limu object = c_object_cinc )
        ( pgmid = c_pgmid_limu object = c_object_clsd )
        ( pgmid = c_pgmid_limu object = c_object_cpri )
        ( pgmid = c_pgmid_limu object = c_object_cpub )
        ( pgmid = c_pgmid_limu object = c_object_meth )
        ( pgmid = c_pgmid_r3tr object = c_object_clas )
        ( pgmid = c_pgmid_r3tr object = c_object_intf )
      ).

      gs_clazy_flag-class_related_obj_tags = abap_true.
    ENDIF.

    rv_related = xsdbool( line_exists( gs_clazy_val-class_related_obj_tag[ KEY primary_key COMPONENTS
      pgmid  = is_obj_type-pgmid
      object = is_obj_type-object
    ] ) ).

  ENDMETHOD.


  METHOD is_toc_safe.

    IF gs_lazy_flag-toc_safe EQ abap_false.

      DATA(lv_trf) = get_header( )-trfunction.

      gs_lazy_val-toc_safe = xsdbool( NOT (
        ( lv_trf EQ c_trfunction_cust OR lv_trf EQ c_trfunction_cust_task ) AND
        ( get_source_client( ) NE sy-mandt )
      ) ).

      gs_lazy_flag-toc_safe = abap_true.
    ENDIF.

    rv_safe = gs_lazy_val-toc_safe.

  ENDMETHOD.


  METHOD release.

    " ______________________________
    " Hazırlık

    CLEAR ev_rel_wait_success.

    DATA(lt_wait_success) = VALUE tt_abap_bool( ).

    IF iv_del_empty_subtasks EQ abap_true.
      delete_empty_subtasks( ).
    ENDIF.

    " ______________________________
    " Talep edildiyse, Subtask'leri Release et

    IF iv_rel_subtasks_too EQ abap_true.

      DATA(lt_sub) = get_subtasks( ).

      LOOP AT lt_sub ASSIGNING FIELD-SYMBOL(<ls_sub>).

        release_single(
          EXPORTING
            iv_trkorr              = <ls_sub>-trkorr
            io_req                 = <ls_sub>-obj
            iv_wait_until_released = iv_wait_until_released
            iv_max_rel_wait        = iv_max_rel_wait
            iv_compl_sh_piece_list = iv_compl_sh_piece_list
          IMPORTING
            ev_rel_wait_success    = DATA(lv_wait_success)
        ).

        COLLECT lv_wait_success INTO lt_wait_success.
        CLEAR lv_wait_success.

      ENDLOOP.

    ENDIF.

    " ______________________________
    " Request'in kendisini Release et

    release_single(
      EXPORTING
        iv_trkorr              = gv_trkorr
        io_req                 = me
        iv_wait_until_released = iv_wait_until_released
        iv_max_rel_wait        = iv_max_rel_wait
        iv_compl_sh_piece_list = iv_compl_sh_piece_list
      IMPORTING
        ev_rel_wait_success    = lv_wait_success
    ).

    COLLECT lv_wait_success INTO lt_wait_success.
    CLEAR lv_wait_success.

    " ______________________________
    " Bekleme işlemi başarılı oldu mu?

    ev_rel_wait_success = xsdbool(
        iv_wait_until_released EQ abap_true AND
        lt_wait_success IS NOT INITIAL AND
        ( NOT line_exists( lt_wait_success[ table_line = abap_false ] ) )
    ).

  ENDMETHOD.


  METHOD release_single.

    DATA lo_req TYPE REF TO zcl_bc_transport_request.

    " ______________________________
    " Hazırlık

    CLEAR ev_rel_wait_success.

    " ______________________________
    " Request tespiti & kontrolleri

    lo_req = COND #(
      WHEN io_req IS INITIAL THEN get_instance( iv_trkorr )
      ELSE io_req
    ).

    CHECK lo_req->get_header( )-trstatus IN get_open_status_rng( ).

    " ______________________________
    " Piece List tamamlama

    IF iv_compl_sh_piece_list EQ abap_true.

      TRY.
          lo_req->complete_shi_piece_list( ).
        CATCH cx_root ##no_handler.
      ENDTRY.

    ENDIF.

    " ______________________________
    " Release et

    CALL FUNCTION 'TR_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                  = iv_trkorr
        iv_dialog                  = abap_true
        iv_as_background_job       = abap_false
        iv_success_message         = abap_false
        iv_display_export_log      = abap_false
        iv_simulation              = abap_false
      EXCEPTIONS
        cts_initialization_failure = 1
        enqueue_failed             = 2
        no_authorization           = 3
        invalid_request            = 4
        request_already_released   = 5
        repeat_too_early           = 6
        error_in_export_methods    = 7
        object_check_error         = 8
        docu_missing               = 9
        db_access_error            = 10
        action_aborted_by_user     = 11
        export_failed              = 12
        OTHERS                     = 13
        ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TR_RELEASE_REQUEST' ).

    " ______________________________
    " Talep edildiyse, Release edilene kadar bekle

    IF iv_wait_until_released EQ abap_true.

      DATA(lv_total_wait) = 0.

      DO.
        IF lo_req->has_locked_object( ) EQ abap_false.
          ev_rel_wait_success = abap_true.
          EXIT.
        ENDIF.

        WAIT UP TO 1 SECONDS.

        ADD 1 TO lv_total_wait.
        CHECK lv_total_wait GT iv_max_rel_wait.
        ev_rel_wait_success = abap_false.
        EXIT.
      ENDDO.

    ENDIF.

  ENDMETHOD.

  METHOD sort_and_compress.

    TRY.

        CALL FUNCTION 'TRINT_SORT_AND_COMPRESS_COMM'
          EXPORTING
            iv_trkorr            = gv_trkorr
          EXCEPTIONS
            request_doesnt_exist = 1
            request_released     = 2
            no_authorization     = 3
            update_error         = 4
            OTHERS               = 5
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_SORT_AND_COMPRESS_COMM' ).

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_tr_sort_and_compress
          EXPORTING
            textid   = zcx_bc_tr_sort_and_compress=>soc_function_error
            previous = lo_diaper
            trkorr   = gv_trkorr.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.