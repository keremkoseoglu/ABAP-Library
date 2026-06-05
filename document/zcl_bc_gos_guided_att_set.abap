CLASS zcl_bc_gos_guided_att_set DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_bc_email_bin_file_src.

    TYPES: BEGIN OF att_val_map,
             att_guid  TYPE recaguid,
             file_name TYPE zcl_bc_bin_file=>name_type,
           END OF att_val_map,

           att_val_set TYPE HASHED TABLE OF att_val_map WITH UNIQUE KEY att_guid.

    METHODS constructor IMPORTING db        TYPE REF TO zif_bc_gos_guided_att_db
                                  att_guids TYPE re_t_guid OPTIONAL.

    METHODS append IMPORTING attachment TYPE REF TO zcl_bc_gos_guided_attachment.

    METHODS append_bulk IMPORTING append_attachments TYPE REF TO zcl_bc_gos_guided_att_set
                        RAISING   RESUMABLE(zcx_bc_gos_guided_attachment).

    METHODS remove    IMPORTING att_guid      TYPE recaguid.

    METHODS get_count RETURNING VALUE(result) TYPE i.

    METHODS get_guids RETURNING VALUE(result) TYPE re_t_guid.

    METHODS get_values RETURNING VALUE(result) TYPE att_val_set
                       RAISING   RESUMABLE(zcx_bc_gos_guided_attachment).

    METHODS get_attachment
      IMPORTING att_guid      TYPE recaguid
      RETURNING VALUE(result) TYPE REF TO zcl_bc_gos_guided_attachment
      RAISING   zcx_bc_gos_guided_attachment.

  PRIVATE SECTION.
    TYPES: BEGIN OF attachment_map,
             guid      TYPE recaguid,
             obj_cache TYPE REF TO zcl_bc_gos_guided_attachment,
           END OF attachment_map,

           attachment_set TYPE HASHED TABLE OF attachment_map
                          WITH UNIQUE KEY primary_key COMPONENTS guid.

    DATA: db          TYPE REF TO zif_bc_gos_guided_att_db,
          attachments TYPE attachment_set.
ENDCLASS.


CLASS zcl_bc_gos_guided_att_set IMPLEMENTATION.
  METHOD constructor.
    me->db          = db.

    me->attachments = VALUE #( FOR GROUPS _grp OF _guid IN att_guids
                               GROUP BY _guid
                               ( guid = _grp ) ).
  ENDMETHOD.

  METHOD append.
    CHECK NOT line_exists( me->attachments[ guid = attachment->guid ] ).

    INSERT VALUE #( guid      = attachment->guid
                    obj_cache = attachment ) INTO TABLE me->attachments.
  ENDMETHOD.

  METHOD append_bulk.
    LOOP AT append_attachments->attachments REFERENCE INTO DATA(app_att).
      TRY.
          DATA(app_att_obj) = append_attachments->get_attachment( app_att->guid ).
          append( app_att_obj ).

        CATCH zcx_bc_gos_guided_attachment INTO DATA(att_error).
          RAISE RESUMABLE EXCEPTION att_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD remove.
    DELETE me->attachments WHERE guid = att_guid.
  ENDMETHOD.

  METHOD get_count.
    result = lines( me->attachments ).
  ENDMETHOD.

  METHOD get_guids.
    result = VALUE #( FOR _att IN me->attachments ( _att-guid ) ).
  ENDMETHOD.

  METHOD get_values.
    LOOP AT me->attachments REFERENCE INTO DATA(att).
      TRY.
          INSERT VALUE #( att_guid  = att->guid
                          file_name = get_attachment( att->guid )->get_file_name( ) )
                 INTO TABLE result.

        CATCH zcx_bc_gos_guided_attachment INTO DATA(att_error).
          RAISE RESUMABLE EXCEPTION att_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_attachment.
    IF att_guid IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_gos_guided_attachment( textid = zcx_bc_gos_guided_attachment=>empty_id ).
    ENDIF.

    TRY.
        DATA(att) = REF #( me->attachments[ KEY primary_key
                                            guid = att_guid ] ).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION NEW zcx_bc_gos_guided_attachment( textid   = zcx_bc_gos_guided_attachment=>att_not_in_memory
                                                          att_guid = att_guid ).
    ENDTRY.

    IF att->obj_cache IS INITIAL.
      att->obj_cache = zcl_bc_gos_guided_attachment=>get_instance( att_guid = att->guid
                                                                   db       = me->db ).
    ENDIF.

    result = att->obj_cache.
  ENDMETHOD.

  METHOD zif_bc_email_bin_file_src~get_mail_att_bins.
    TRY.
        LOOP AT me->attachments REFERENCE INTO DATA(att_row).
          DATA(att)      = get_attachment( att_row->guid ).
          DATA(att_file) = att->get_file( ).

          APPEND VALUE #( att_type    = att_file->get_extension( )
                          att_subject = att_file->get_name_wo_extension( )
                          att_content = att_file->convert_to_solix_tab( ) ) TO result.
        ENDLOOP.

      CATCH cx_root INTO DATA(att_error).
        RAISE EXCEPTION NEW zcx_bc_mail_bin_attachment( textid   = zcx_bc_mail_bin_attachment=>get_bin_files_failed
                                                        previous = att_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.