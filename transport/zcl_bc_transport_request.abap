CLASS zcl_bc_transport_request DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES tt_as4text_rng  TYPE RANGE OF e07t-as4text.
    TYPES tt_obj_name_rng TYPE RANGE OF e071-obj_name.
    TYPES tt_object_rng   TYPE RANGE OF e071-object.

    TYPES: BEGIN OF t_trkorr,
             trkorr TYPE trkorr,
           END OF t_trkorr,

           tt_trkorr      TYPE STANDARD TABLE OF t_trkorr WITH DEFAULT KEY,

           tt_trkorr_hash TYPE HASHED TABLE OF t_trkorr
                          WITH UNIQUE KEY primary_key COMPONENTS trkorr.

    TYPES tt_pgmid_rng      TYPE RANGE OF e071-pgmid.

    TYPES tt_request_object TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.
    TYPES:
      BEGIN OF t_request_and_object,
        trkorr   TYPE e070-trkorr,
        as4text  TYPE e07t-as4text,
        strkorr  TYPE e070-strkorr,
        pgmid    TYPE e071-pgmid,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
      END OF t_request_and_object.
    TYPES tt_request_and_object TYPE STANDARD TABLE OF t_request_and_object WITH DEFAULT KEY.
    TYPES tt_request_attribute  TYPE STANDARD TABLE OF e070a WITH DEFAULT KEY.
    TYPES:
      BEGIN OF t_request_object_tag,
        pgmid    TYPE e071-pgmid,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
      END OF t_request_object_tag.
    TYPES tt_request_object_tag TYPE STANDARD TABLE OF t_request_object_tag WITH DEFAULT KEY.
    TYPES:
      BEGIN OF t_request_object_type,
        pgmid  TYPE e071-pgmid,
        object TYPE e071-object,
      END OF t_request_object_type.
    TYPES tt_request_key     TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.
    TYPES tt_request_nametab TYPE STANDARD TABLE OF e071kf WITH DEFAULT KEY.
    TYPES:
      BEGIN OF t_content,
        object    TYPE tt_request_object,
        key       TYPE tt_request_key,
        nametab   TYPE tt_request_nametab,
        attribute TYPE tt_request_attribute,
      END OF t_content.
    TYPES:
      BEGIN OF t_request_obj,
        trkorr TYPE trkorr,
        obj    TYPE REF TO zcl_bc_transport_request,
      END OF t_request_obj.
    TYPES tt_request_obj  TYPE STANDARD TABLE OF t_request_obj WITH DEFAULT KEY.

    TYPES tt_trstatus_rng TYPE RANGE OF e070-trstatus.

    TYPES tt_as4user_rng  TYPE RANGE OF e070-as4user.

    TYPES:
      BEGIN OF t_request_param,
        s_trkorr            TYPE zbctt_trkorr_rng,
        s_trfunction        TYPE zbctt_trfunction_rng,
        s_trstatus          TYPE tt_trstatus_rng,
        s_as4user           TYPE tt_as4user_rng,
        s_as4text           TYPE tt_as4text_rng,
        s_as4date           TYPE date_t_range,
        p_must_have_subtask TYPE abap_bool,
        p_srch_strkorr      TYPE abap_bool,
        p_ignore_trkorr     TYPE abap_bool,
      END OF t_request_param.
    TYPES:
      BEGIN OF t_user,
        bname TYPE xubname,
      END OF t_user.
    TYPES tt_user TYPE STANDARD TABLE OF t_user WITH DEFAULT KEY.

    CONSTANTS c_domain_trf           TYPE dd07t-domname VALUE 'TRFUNCTION' ##NO_TEXT.
    CONSTANTS c_object_cinc          TYPE e071-object   VALUE 'CINC' ##NO_TEXT.
    CONSTANTS c_object_clas          TYPE e071-object   VALUE 'CLAS' ##NO_TEXT.
    CONSTANTS c_object_clsd          TYPE e071-object   VALUE 'CLSD' ##NO_TEXT.
    CONSTANTS c_object_cpub          TYPE e071-object   VALUE 'CPUB' ##NO_TEXT. "#EC CI_USAGE_OK[2270335]
    CONSTANTS c_object_cpri          TYPE e071-object   VALUE 'CPRI' ##NO_TEXT.
    CONSTANTS c_object_doma          TYPE e071-object   VALUE 'DOMA' ##NO_TEXT.
    CONSTANTS c_object_domd          TYPE e071-object   VALUE 'DOMD' ##NO_TEXT.
    CONSTANTS c_object_dted          TYPE e071-object   VALUE 'DTED' ##NO_TEXT.
    CONSTANTS c_object_dtel          TYPE e071-object   VALUE 'DTEL' ##NO_TEXT.
    CONSTANTS c_object_tabd          TYPE e071-object   VALUE 'TABD' ##NO_TEXT.
    CONSTANTS c_object_tabl          TYPE e071-object   VALUE 'TABL' ##NO_TEXT.
    CONSTANTS c_object_tdat          TYPE e071-object   VALUE 'TDAT' ##NO_TEXT.
    CONSTANTS c_object_intf          TYPE e071-object   VALUE 'INTF' ##NO_TEXT.
    CONSTANTS c_object_meth          TYPE e071-object   VALUE 'METH' ##NO_TEXT.
    CONSTANTS c_object_prog          TYPE e071-object   VALUE 'PROG' ##NO_TEXT.
    CONSTANTS c_pgmid_corr           TYPE e071-pgmid    VALUE 'CORR' ##NO_TEXT.
    CONSTANTS c_pgmid_limu           TYPE e071-pgmid    VALUE 'LIMU' ##NO_TEXT.
    CONSTANTS c_pgmid_r3tr           TYPE e071-pgmid    VALUE 'R3TR' ##NO_TEXT.
    CONSTANTS c_trfunction_cust      TYPE trfunction    VALUE 'W' ##NO_TEXT.
    CONSTANTS c_trfunction_cust_task TYPE trfunction    VALUE 'Q' ##NO_TEXT.
    CONSTANTS c_trfunction_toc       TYPE trfunction    VALUE 'T' ##NO_TEXT.
    CONSTANTS c_trfunction_unclass   TYPE trfunction    VALUE 'X' ##NO_TEXT.
    CONSTANTS c_trfunction_wb        TYPE trfunction    VALUE 'K' ##NO_TEXT.
    CONSTANTS c_trstatus_modif       TYPE trstatus      VALUE 'D' ##NO_TEXT.
    CONSTANTS c_trstatus_modif_prot  TYPE trstatus      VALUE 'L' ##NO_TEXT.
    CONSTANTS c_max_wait             TYPE i             VALUE 30.
    CONSTANTS c_auto_prefix          TYPE text8         VALUE 'Üretilen' ##NO_TEXT. " Değiştirirseniz, zbcv_tcr_no_transport da değişmelidir

    DATA gv_trkorr TYPE trkorr READ-ONLY.

    CLASS-METHODS create_new_request
      IMPORTING iv_trfunction TYPE trfunction
                iv_as4text    TYPE as4text   OPTIONAL
                it_user       TYPE tt_user   OPTIONAL
                iv_target     TYPE tr_target OPTIONAL
      RETURNING VALUE(ro_req) TYPE REF TO zcl_bc_transport_request
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    CLASS-METHODS get_as4text_safe
      IMPORTING iv_trkorr         TYPE trkorr
      RETURNING VALUE(rv_as4text) TYPE as4text.

    CLASS-METHODS get_empty_open_requests
      IMPORTING it_trkorr_rng  TYPE zbctt_trkorr_rng
      RETURNING VALUE(rt_list) TYPE zbctt_trkorr_det
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    CLASS-METHODS get_instance
      IMPORTING iv_trkorr     TYPE trkorr
                iv_top        TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_transport_request
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_modified_objects
      IMPORTING it_obj        TYPE zbctt_e071_obj_key
      RETURNING VALUE(rt_obj) TYPE zbctt_e071_obj_key.

    CLASS-METHODS get_open_status_rng
      RETURNING VALUE(rt_stat) TYPE tt_trstatus_rng.

    CLASS-METHODS get_open_requests
      IMPORTING it_as4text_rng       TYPE tt_as4text_rng       OPTIONAL
                it_trfunction_rng    TYPE zbctt_trfunction_rng OPTIONAL
                iv_must_have_subtask TYPE abap_bool            OPTIONAL
      RETURNING VALUE(rt_list)       TYPE zbctt_trkorr_det.

    CLASS-METHODS get_request_list
      IMPORTING is_param       TYPE t_request_param
      RETURNING VALUE(rt_list) TYPE zbctt_trkorr_det.

    CLASS-METHODS get_request_and_objects
      IMPORTING it_tag           TYPE tt_request_object_tag
      RETURNING VALUE(rt_object) TYPE tt_request_and_object.

    CLASS-METHODS get_request_objects
      IMPORTING it_trkorr_rng      TYPE zbctt_trkorr_rng
                it_pgmid_rng       TYPE tt_pgmid_rng OPTIONAL
                iv_read_creation   TYPE abap_bool    DEFAULT abap_false
                iv_include_deleted TYPE abap_bool    DEFAULT abap_false
      EXPORTING et_list            TYPE zcl_bc_dol_model=>tt_dol_list
                et_list_wr         TYPE zcl_bc_dol_model=>tt_dol_list_wr.

    CLASS-METHODS get_requests_containing_obj
      IMPORTING it_obj            TYPE tt_request_object_tag
                iv_holistic_cls   TYPE abap_bool            DEFAULT abap_false
                it_trfunction_rng TYPE zbctt_trfunction_rng OPTIONAL
      RETURNING VALUE(rt_req)     TYPE tt_request_and_object.

    CLASS-METHODS get_source_client_safe
      IMPORTING iv_trkorr        TYPE trkorr
      RETURNING VALUE(rv_client) TYPE e070c-client.

    CLASS-METHODS get_toc_safety_safe
      IMPORTING iv_trkorr      TYPE trkorr
      RETURNING VALUE(rv_safe) TYPE abap_bool.

    CLASS-METHODS get_user_creatable_trf_rng
      RETURNING VALUE(rt_func) TYPE zbctt_trfunction_rng.

    CLASS-METHODS is_obj_type_class_related
      IMPORTING is_obj_type       TYPE t_request_object_type
      RETURNING VALUE(rv_related) TYPE abap_bool.

    METHODS add_objects
      IMPORTING it_obj               TYPE tt_request_object_tag
                iv_sort_and_compress TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_bc_function_subrc
        RESUMABLE(zcx_bc_tr_sort_and_compress).

    METHODS add_objects_from_request
      IMPORTING it_from              TYPE tt_trkorr
                iv_wait              TYPE abap_bool DEFAULT abap_false
                iv_sort_and_compress TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_bc_function_subrc
        RESUMABLE(zcx_bc_tr_sort_and_compress)
        zcx_bc_table_content.

    METHODS complete_shi_piece_list RAISING zcx_bc_sh_piece_list_compl.

    METHODS create_subtask
      IMPORTING it_user TYPE tt_user.

    METHODS delete
      RAISING ycx_addict_trans_req_delete.

    METHODS delete_empty_subtasks
      RAISING ycx_addict_trans_req_delete.

    METHODS delete_object
      IMPORTING is_obj TYPE t_request_object_tag
      RAISING   zcx_bc_function_subrc.

    METHODS get_content
      RETURNING VALUE(rs_content) TYPE t_content
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    METHODS get_objects
      IMPORTING it_pgmid_rng     TYPE tt_pgmid_rng                        OPTIONAL
                it_object_rng    TYPE tt_object_rng                       OPTIONAL
                it_obj_name_rng  TYPE tt_obj_name_rng                     OPTIONAL
                it_devclass_rng  TYPE zcl_bc_abap_package=>tt_package_rng OPTIONAL
      RETURNING VALUE(rt_object) TYPE tt_request_object
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    METHODS get_header
      RETURNING VALUE(rs_header) TYPE e070.

    METHODS get_obj_related_requests
      IMPORTING iv_include_self   TYPE abap_bool                           DEFAULT abap_false
                it_obj_name_rng   TYPE tt_obj_name_rng                     OPTIONAL
                iv_holistic_cls   TYPE abap_bool                           DEFAULT abap_false
                it_trfunction_rng TYPE zbctt_trfunction_rng                OPTIONAL
                it_devclass_rng   TYPE zcl_bc_abap_package=>tt_package_rng OPTIONAL
      RETURNING VALUE(rt_list)    TYPE tt_request_and_object
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    METHODS get_source_client
      RETURNING VALUE(rv_client) TYPE e070c-client.

    METHODS get_subtasks
      IMPORTING iv_only_empty     TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rt_subtask) TYPE tt_request_obj
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    METHODS get_text
      RETURNING VALUE(rv_as4text) TYPE as4text.

    METHODS has_locked_object RETURNING VALUE(rv_has) TYPE abap_bool.

    METHODS has_merge
      RETURNING VALUE(rv_has) TYPE abap_bool
      RAISING   zcx_bc_int_data_read.

    METHODS is_empty
      RETURNING VALUE(rv_empty) TYPE abap_bool
      RAISING   zcx_bc_function_subrc
                zcx_bc_table_content.

    METHODS is_toc_safe
      RETURNING VALUE(rv_safe) TYPE abap_bool
      RAISING   ycx_addict_class_method.

    METHODS release
      IMPORTING iv_rel_subtasks_too    TYPE abap_bool
                iv_del_empty_subtasks  TYPE abap_bool DEFAULT abap_true
                iv_wait_until_released TYPE abap_bool DEFAULT abap_false
                iv_max_rel_wait        TYPE i         DEFAULT c_max_wait
                iv_compl_sh_piece_list TYPE abap_bool DEFAULT abap_true
      EXPORTING ev_rel_wait_success    TYPE abap_bool
      RAISING   ycx_addict_trans_req_release.

    METHODS sort_and_compress RAISING zcx_bc_tr_sort_and_compress.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_multiton,
        trkorr TYPE trkorr,
        top    TYPE abap_bool,
        obj    TYPE REF TO zcl_bc_transport_request,
      END OF t_multiton.
    TYPES tt_multiton TYPE HASHED TABLE OF t_multiton
                             WITH UNIQUE KEY primary_key COMPONENTS trkorr top.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA core TYPE REF TO ycl_addict_transport_request.

    CLASS-METHODS get_instance_from_addict
      IMPORTING addict        TYPE REF TO ycl_addict_transport_request
      RETURNING VALUE(output) TYPE REF TO zcl_bc_transport_request.
ENDCLASS.


CLASS zcl_bc_transport_request IMPLEMENTATION.
  METHOD add_objects.
    TRY.
        me->core->add_objects( obj               = CORRESPONDING #( it_obj )
                               sort_and_compress = iv_sort_and_compress ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_sort_and_compress INTO DATA(sort_error).
        DATA(zsort_error) = zcx_bc_tr_sort_and_compress=>create_from_addict( sort_error ).
        RAISE RESUMABLE EXCEPTION zsort_error.
    ENDTRY.
  ENDMETHOD.

  METHOD add_objects_from_request.
    TRY.
        me->core->add_objects_from_request( from              = VALUE #( FOR _from IN it_from
                                                                         ( _from-trkorr ) )
                                            wait              = iv_wait
                                            sort_and_compress = iv_sort_and_compress ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_sort_and_compress INTO DATA(sort_error).
        DATA(zsort_error) = zcx_bc_tr_sort_and_compress=>create_from_addict( sort_error ).
        RAISE RESUMABLE EXCEPTION zsort_error.
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_new_request.
    TRY.
        DATA(core) = ycl_addict_transport_request=>create_new_request( trfunction = iv_trfunction
                                                                       as4text    = iv_as4text
                                                                       users      = VALUE #( FOR _user IN it_user
                                                                                             ( _user-bname ) )
                                                                       target     = iv_target ).

      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
    ENDTRY.

    ro_req = get_instance_from_addict( core ).
  ENDMETHOD.

  METHOD create_subtask.
    me->core->create_subtask( user = VALUE #( FOR _user IN it_user
                                              ( _user-bname ) ) ).
  ENDMETHOD.

  METHOD get_empty_open_requests.
    TRY.
        rt_list = CORRESPONDING #( ycl_addict_transport_request=>get_empty_open_requests(
                                       CORRESPONDING #( it_trkorr_rng ) ) ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_content.
    TRY.
        rs_content = CORRESPONDING #( me->core->get_content( ) ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD complete_shi_piece_list.
    TRY.
        me->core->complete_shi_piece_list( ).
      CATCH ycx_addict_sh_piece_list_comp INTO DATA(spl).
        zcx_bc_sh_piece_list_compl=>raise_from_addict( spl ).
    ENDTRY.
  ENDMETHOD.

  METHOD delete.
    me->core->delete( ).
  ENDMETHOD.

  METHOD get_request_and_objects.
    rt_object = CORRESPONDING #(
        ycl_addict_transport_request=>get_request_and_objects( CORRESPONDING #( it_tag ) ) ).
  ENDMETHOD.

  METHOD is_empty.
    TRY.
        rv_empty = me->core->is_empty( ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_header.
    rs_header = me->core->get_header( ).
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS trkorr = iv_trkorr
                                                   top    = iv_top ]
           TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      TRY.
          DATA(addict) = ycl_addict_transport_request=>get_instance( trkorr = iv_trkorr
                                                                     top    = iv_top ).

        CATCH ycx_addict_table_content INTO DATA(table_content_error).
          zcx_bc_table_content=>raise_from_addict( table_content_error ).
      ENDTRY.

      DATA(ls_mt) = VALUE t_multiton( trkorr = iv_trkorr
                                      top    = iv_top
                                      obj    = get_instance_from_addict( addict ) ).

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.
    ENDIF.

    ro_obj = <ls_mt>-obj.
  ENDMETHOD.

  METHOD has_merge.
    TRY.
        rv_has = me->core->has_merge( ).

      CATCH ycx_addict_data_read INTO DATA(read_error).
        zcx_bc_int_data_read=>raise_from_addict( read_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_source_client.
    rv_client = me->core->get_source_client( ).
  ENDMETHOD.

  METHOD get_as4text_safe.
    rv_as4text = ycl_addict_transport_request=>get_as4text_safe( iv_trkorr ).
  ENDMETHOD.

  METHOD get_open_requests.
    rt_list = CORRESPONDING #(
        ycl_addict_transport_request=>get_open_requests( as4text_rng       = CORRESPONDING #( it_as4text_rng )
                                                         trfunction_rng    = CORRESPONDING #( it_trfunction_rng )
                                                         must_have_subtask = iv_must_have_subtask ) ).
  ENDMETHOD.

  METHOD get_open_status_rng.
    rt_stat = CORRESPONDING #( ycl_addict_transport_request=>get_open_status_rng( ) ).
  ENDMETHOD.

  METHOD get_text.
    rv_as4text = me->core->get_text( ).
  ENDMETHOD.

  METHOD get_request_objects.
    ycl_addict_transport_request=>get_request_objects( EXPORTING trkorr_rng      = CORRESPONDING #( it_trkorr_rng )
                                                                 pgmid_rng       = CORRESPONDING #( it_pgmid_rng )
                                                                 read_creation   = iv_read_creation
                                                                 include_deleted = iv_include_deleted
                                                       IMPORTING list            = DATA(list)
                                                                 list_wr         = DATA(list_wr) ).

    et_list    = CORRESPONDING #( list ).
    et_list_wr = CORRESPONDING #( list_wr ).
  ENDMETHOD.

  METHOD get_request_list.
    rt_list = CORRESPONDING #(
        ycl_addict_transport_request=>get_request_list( VALUE #(
                                                            trkorr_rng        = CORRESPONDING #( is_param-s_trkorr )
                                                            trfunction_rng    = CORRESPONDING #( is_param-s_trfunction )
                                                            trstatus_rng      = CORRESPONDING #( is_param-s_trstatus )
                                                            as4user_rng       = CORRESPONDING #( is_param-s_as4user )
                                                            as4text_rng       = CORRESPONDING #( is_param-s_as4text )
                                                            as4date_rng       = CORRESPONDING #( is_param-s_as4date )
                                                            must_have_subtask = is_param-p_must_have_subtask
                                                            srch_strkorr      = is_param-p_srch_strkorr
                                                            ignore_trkorr     = is_param-p_ignore_trkorr ) ) ).
  ENDMETHOD.

  METHOD get_objects.
    TRY.
        rt_object = CORRESPONDING #(
            me->core->get_objects( pgmid_rng    = CORRESPONDING #( it_pgmid_rng )
                                   object_rng   = CORRESPONDING #( it_object_rng )
                                   obj_name_rng = CORRESPONDING #( it_obj_name_rng )
                                   devclass_rng = CORRESPONDING #( it_devclass_rng ) ) ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_source_client_safe.
    rv_client = ycl_addict_transport_request=>get_source_client_safe( iv_trkorr ).
  ENDMETHOD.

  METHOD is_toc_safe.
    rv_safe = me->core->is_toc_safe( ).
  ENDMETHOD.

  METHOD get_subtasks.
    TRY.
        rt_subtask = VALUE #( FOR _st IN me->core->get_subtasks( iv_only_empty )
                              ( trkorr = _st-trkorr
                                obj    = zcl_bc_transport_request=>get_instance_from_addict( _st-obj ) ) ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_obj_related_requests.
    TRY.
        rt_list = CORRESPONDING #(
            me->core->get_obj_related_requests( include_self   = iv_include_self
                                                obj_name_rng   = CORRESPONDING #( it_obj_name_rng )
                                                holistic_cls   = iv_holistic_cls
                                                trfunction_rng = CORRESPONDING #( it_trfunction_rng )
                                                devclass_rng   = CORRESPONDING #( it_devclass_rng ) ) ).

      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
      CATCH ycx_addict_table_content INTO DATA(table_error).
        zcx_bc_table_content=>raise_from_addict( table_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD is_obj_type_class_related.
    rv_related = ycl_addict_transport_request=>is_obj_type_class_related( CORRESPONDING #( is_obj_type ) ).
  ENDMETHOD.

  METHOD get_user_creatable_trf_rng.
    rt_func = CORRESPONDING #( ycl_addict_transport_request=>get_user_creatable_trf_rng( ) ).
  ENDMETHOD.

  METHOD delete_empty_subtasks.
    me->core->delete_empty_subtasks( ).
  ENDMETHOD.

  METHOD delete_object.
    TRY.
        me->core->delete_object( CORRESPONDING #( is_obj ) ).
      CATCH ycx_addict_function_subrc INTO DATA(function_error).
        zcx_bc_function_subrc=>raise_from_addict( function_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_toc_safety_safe.
    rv_safe = ycl_addict_transport_request=>get_toc_safety_safe( iv_trkorr ).
  ENDMETHOD.

  METHOD release.
    me->core->release( EXPORTING rel_subtasks_too    = iv_rel_subtasks_too
                                 del_empty_subtasks  = iv_del_empty_subtasks
                                 wait_until_released = iv_wait_until_released
                                 max_rel_wait        = iv_max_rel_wait
                                 compl_sh_piece_list = iv_compl_sh_piece_list
                       IMPORTING rel_wait_success    = ev_rel_wait_success ).
  ENDMETHOD.

  METHOD get_instance_from_addict.
    output = NEW #( ).
    output->core      = addict.
    output->gv_trkorr = addict->trkorr.
  ENDMETHOD.

  METHOD get_modified_objects.
    rt_obj = CORRESPONDING #(
        ycl_addict_transport_request=>get_modified_objects( CORRESPONDING #( it_obj ) ) ).
  ENDMETHOD.

  METHOD get_requests_containing_obj.
    rt_req = CORRESPONDING #(
        ycl_addict_transport_request=>get_requests_containing_obj(
            obj            = CORRESPONDING #( it_obj )
            holistic_cls   = iv_holistic_cls
            trfunction_rng = CORRESPONDING #( it_trfunction_rng ) ) ).
  ENDMETHOD.

  METHOD has_locked_object.
    rv_has = me->core->has_locked_object( ).
  ENDMETHOD.

  METHOD sort_and_compress.
    TRY.
        me->core->sort_and_compress( ).
      CATCH ycx_addict_sort_and_compress INTO DATA(sac).
        zcx_bc_tr_sort_and_compress=>raise_from_addict( sac ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.