CLASS zcl_bc_distribution_list DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    TYPES: tt_soid    TYPE STANDARD TABLE OF soid WITH EMPTY KEY,
           tt_members TYPE STANDARD TABLE OF sodm1 WITH EMPTY KEY.

    DATA gs_definition TYPE soid READ-ONLY.

    CLASS-METHODS:
      cache_all_distribution_lists,

      get_instance
        IMPORTING iv_objnam     TYPE soid-objnam
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_distribution_list
        RAISING   cx_no_entry_in_table,

      remove_mail_from_all_lists
        IMPORTING iv_email        TYPE ad_smtpadr
        EXPORTING et_removed_from TYPE tt_soid
        RAISING   RESUMABLE(zcx_bc_dist_list_remove_mail).

    METHODS remove_mail
      IMPORTING iv_email   TYPE ad_smtpadr
      EXPORTING ev_removed TYPE abap_bool
      RAISING   RESUMABLE(zcx_bc_dist_list_remove_mail).

    METHODS get_members
      RETURNING VALUE(result) TYPE tt_members
      RAISING   zcx_bc_function_subrc.

  PROTECTED SECTION.
    METHODS constructor
      IMPORTING iv_objnam TYPE soid-objnam
      RAISING   cx_no_entry_in_table.

    METHODS is_email_in_list
      IMPORTING email         TYPE so_memadr
                alt_emails    TYPE bcsy_smtpa
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_multiton,
             objnam TYPE soid-objnam,
             dlist  TYPE REF TO zcl_bc_distribution_list,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton
                       WITH UNIQUE KEY primary_key COMPONENTS objnam,

           tt_sodm2    TYPE STANDARD TABLE OF sodm2 WITH EMPTY KEY.

    CONSTANTS: BEGIN OF c_table,
                 definition TYPE tabname VALUE 'SOID',
               END OF c_table.

    CLASS-DATA: gt_multiton              TYPE tt_multiton,
                gv_all_dist_lists_cached TYPE abap_bool.

    DATA: gt_members      TYPE tt_members,
          gv_members_read TYPE abap_bool.

    METHODS read_members_lazy RAISING zcx_bc_function_subrc.

ENDCLASS.


CLASS zcl_bc_distribution_list IMPLEMENTATION.
  METHOD cache_all_distribution_lists.
    CHECK gv_all_dist_lists_cached = abap_false.
    SELECT objnam FROM soid INTO TABLE @DATA(lt_soid).  "#EC CI_NOWHERE

    LOOP AT lt_soid ASSIGNING FIELD-SYMBOL(<ls_soid>).
      TRY.
          DATA(lo_distribution_list) = get_instance( <ls_soid>-objnam ).
        CATCH cx_root INTO DATA(lo_diaper).
          MESSAGE lo_diaper TYPE zcl_bc_applog_facade=>c_msgty_a.
      ENDTRY.

      TRY.
          lo_distribution_list->read_members_lazy( ).
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    gv_all_dist_lists_cached = abap_true.
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS objnam = iv_objnam ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      INSERT VALUE #( objnam = iv_objnam
                      dlist  = NEW #( iv_objnam ) )
             INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    ro_obj = <ls_multiton>-dlist.
  ENDMETHOD.

  METHOD remove_mail.
    CLEAR ev_removed.

    DATA(alt_emails) = zcl_bc_mail_facade=>get_alt_domain_email_addresses( iv_email ).

    LOOP AT gt_members ASSIGNING FIELD-SYMBOL(<ls_member>).
      CHECK is_email_in_list( email      = <ls_member>-address
                              alt_emails = alt_emails ).

      TRY.
          DATA(ls_entry)    = CORRESPONDING sodm2( <ls_member> ).
          DATA(lt_dli_tab)  = VALUE tt_sodm2( ).
          DATA(lv_dli_type) = CONV so_escape( gs_definition-dlitp ).

          CALL FUNCTION 'SO_DLI_DELETE_ENTRY'
            ##FM_SUBRC_OK
            EXPORTING  dli_type        = lv_dli_type
                       entry           = ls_entry
            TABLES     dli_tab         = lt_dli_tab
            EXCEPTIONS dl_not_exist    = 1
                       entry_not_exist = 2
                       owner_not_exist = 3
                       OTHERS          = 4.

          zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_DLI_DELETE_ENTRY' ).
          COMMIT WORK AND WAIT.
          ev_removed = abap_true.
          DELETE gt_members.
          CONTINUE.

        CATCH cx_root INTO DATA(lo_diaper).
          RAISE RESUMABLE EXCEPTION NEW zcx_bc_dist_list_remove_mail( previous       = lo_diaper
                                                                      dist_list_name = gs_definition-objnam
                                                                      email_address  = iv_email ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_members.
    read_members_lazy( ).
    result = gt_members.
  ENDMETHOD.

  METHOD constructor.
    SELECT * FROM soid
           WHERE objnam = @iv_objnam
           ORDER BY PRIMARY KEY
           INTO CORRESPONDING FIELDS OF @gs_definition
           UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( c_table-definition )
                                                entry_name = CONV #( iv_objnam ) ).
    ENDIF.
  ENDMETHOD.

  METHOD is_email_in_list.
    LOOP AT alt_emails REFERENCE INTO DATA(alt_email).
      CHECK zcl_bc_text_toolkit=>are_texts_same_ignoring_case( iv_text1 = alt_email->*
                                                               iv_text2 = email ).

      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_members_lazy.
    CHECK gv_members_read = abap_false.

    DATA(lt_objpara) = VALUE selc_tab( ).
    DATA(lt_objparb) = VALUE soop1_tab( ).

    CALL FUNCTION 'SO_DLI_READ'
      ##FM_SUBRC_OK ##NUMBER_OK
      EXPORTING  distributionlist           = gs_definition-objnam
                 system_dli                 = abap_true
      TABLES     member                     = gt_members
                 objpara                    = lt_objpara
                 objparb                    = lt_objparb
      EXCEPTIONS active_user_not_exist      = 1
                 communication_failure      = 2
                 component_not_available    = 3
                 dl_name_not_exist          = 4
                 folder_not_exist           = 5
                 folder_no_authorization    = 6
                 forwarder_not_exist        = 7
                 object_not_exist           = 8
                 object_no_authorization    = 9
                 operation_no_authorization = 10
                 owner_not_exist            = 11
                 parameter_error            = 12
                 substitute_not_active      = 13
                 substitute_not_defined     = 14
                 system_failure             = 15
                 user_not_exist             = 16
                 x_error                    = 17
                 OTHERS                     = 18.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_DLI_READ' ).
    gv_members_read = abap_true.
  ENDMETHOD.

  METHOD remove_mail_from_all_lists.
    CLEAR et_removed_from.
    cache_all_distribution_lists( ).

    LOOP AT gt_multiton ASSIGNING FIELD-SYMBOL(<ls_multiton>).
      TRY.
          <ls_multiton>-dlist->remove_mail( EXPORTING iv_email   = iv_email
                                            IMPORTING ev_removed = DATA(lv_removed) ).

          IF lv_removed = abap_true.
            APPEND <ls_multiton>-dlist->gs_definition TO et_removed_from.
          ENDIF.

        CATCH zcx_bc_dist_list_remove_mail INTO DATA(lo_remove_error).
          RAISE RESUMABLE EXCEPTION lo_remove_error.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.