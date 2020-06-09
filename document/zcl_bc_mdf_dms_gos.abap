CLASS zcl_bc_mdf_dms_gos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bc_mdf_dms.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_clsname_me     TYPE seoclsname       VALUE 'ZCL_BC_MDF_DMS_GOS',
               c_objtype        TYPE borident-objtype VALUE 'ZBCBO_MDFD',
               c_rw_mode_r      TYPE sgs_rwmod        VALUE 'D',
               c_rw_mode_w      TYPE sgs_rwmod        VALUE 'E',
               c_service_add    TYPE sgs_srvnam       VALUE 'PCATTA_CREA',
               c_service_manage TYPE sgs_srvnam       VALUE 'VIEW_ATTA'.

    CLASS-DATA go_manager TYPE REF TO cl_gos_manager.

    METHODS build_borident
      IMPORTING
        !iv_objkey         TYPE borident-objkey
      RETURNING
        VALUE(rs_borident) TYPE borident.

    METHODS call_service
      IMPORTING
        !iv_objkey  TYPE borident-objkey
        !iv_service TYPE sgs_srvnam
      EXPORTING
        !ev_error   TYPE abap_bool.

ENDCLASS.



CLASS ZCL_BC_MDF_DMS_GOS IMPLEMENTATION.


  METHOD build_borident.
    rs_borident = VALUE #( objkey  = iv_objkey
                           objtype = c_objtype
                           logsys  = space ).
  ENDMETHOD.


  METHOD call_service.

    CLEAR ev_error.

    go_manager->start_service_direct( EXPORTING  ip_service       = iv_service
                                                 is_object        = build_borident( iv_objkey )
                                      EXCEPTIONS no_object        = 1
                                                 object_invalid   = 2
                                                 execution_failed = 3
                                                 OTHERS           = 4 ).

    ev_error = boolc( sy-subrc NE 0 ).

  ENDMETHOD.


  METHOD class_constructor.
    go_manager = NEW #( ip_no_commit = 'R' ).
  ENDMETHOD.


  METHOD zif_bc_mdf_dms~add_doc_via_gui.

    go_manager->set_rw_mode( c_rw_mode_w ).

    call_service( EXPORTING iv_objkey  = CONV #( iv_park_guid )
                            iv_service = c_service_add
                  IMPORTING ev_error   = DATA(lv_error) ).

    CHECK lv_error EQ abap_true.

    RAISE EXCEPTION TYPE zcx_bc_class_method
      EXPORTING
        textid = zcx_bc_class_method=>unexpected_error
        class  = c_clsname_me
        method = zif_bc_mdf_dms=>c_meth_advg.

  ENDMETHOD.


  METHOD zif_bc_mdf_dms~get_doc_count.
    rv_count = lines( zif_bc_mdf_dms~get_doc_list( it_park_guid = value #( ( park_guid = iv_park_guid ) ) ) ).
  ENDMETHOD.


  METHOD zif_bc_mdf_dms~get_doc_list.

    DATA: lt_con     TYPE STANDARD TABLE OF bdn_con,
          lv_Clsname type BAPIBDS01-CLASSNAME,
          lv_objkey  TYPE swotobjid-objkey.

    TRY.

        loop at it_park_guid assigning field-symbol(<ls_park_guid>).

          lv_clsname = conv #( c_objtype ).
          lv_objkey  = CONV #( <ls_park_guid>-park_guid ).
          clear lt_con[].

          CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
            EXPORTING
              classname          = lv_clsname
              objkey             = lv_objkey
            TABLES
              gos_connections    = lt_con
            EXCEPTIONS
              no_objects_found   = 1
              internal_error     = 2
              internal_gos_error = 3
              OTHERS             = 4 ##fm_subrc_ok.

          IF sy-subrc GT 1.
            zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BDS_GOS_CONNECTIONS_GET' ).
          ENDIF.

          append lines of value zbctt_mdf_doc_list( for ls_con in lt_con ( PARK_GUID = <ls_park_guid>-park_guid
                                                                           DESCRIPT  = ls_con-descript
                                                                           docuclass = ls_con-docuclass )
                                                  ) to rt_list.

        endloop.

      CATCH cx_root INTO DATA(lo_cx_root).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_cx_root
            class    = c_clsname_me
            method   = zif_bc_mdf_dms=>c_meth_gdc.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_bc_mdf_dms~manage_docs_via_gui.

    go_manager->set_rw_mode( SWITCH #( iv_read_only WHEN abap_true THEN c_rw_mode_r ELSE c_rw_mode_w ) ).

    call_service( EXPORTING iv_objkey  = CONV #( iv_park_guid )
                            iv_service = c_service_manage
                  IMPORTING ev_error   = DATA(lv_error) ).

  ENDMETHOD.
ENDCLASS.