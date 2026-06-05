CLASS zcl_bc_email DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF html_table_map,
             structure_name TYPE tabname,
             itab           TYPE REF TO data,
             langu          TYPE sylangu,
           END OF html_table_map.

    DATA: to_emails    TYPE REF TO zcl_bc_email_address_set,
          to_users     TYPE rke_userid,
          cc_dist_list TYPE so_obj_nam,
          subject      TYPE so_obj_des,
          body         TYPE bcsy_text,
          html_table   TYPE html_table_map.

    METHODS attach_bin_files IMPORTING bin_files TYPE zcl_bc_mail_facade=>tt_attachment_bin.

    METHODS attach_bin_files_from_src IMPORTING src TYPE REF TO zif_bc_email_bin_file_src
                                      RAISING   zcx_bc_mail_bin_attachment.

    METHODS send RAISING zcx_bc_mail_send.

  PRIVATE SECTION.
    DATA binary_attachments TYPE zcl_bc_mail_facade=>tt_attachment_bin.
ENDCLASS.


CLASS zcl_bc_email IMPLEMENTATION.
  METHOD attach_bin_files.
    APPEND LINES OF bin_files TO me->binary_attachments.
  ENDMETHOD.

  METHOD attach_bin_files_from_src.
    DATA(bin_files) = src->get_mail_att_bins( ).
    attach_bin_files( bin_files ).
  ENDMETHOD.

  METHOD send.
    TRY.
        DATA(to_emails) = COND zcl_bc_mail_facade=>tt_rlist( WHEN me->to_emails IS NOT INITIAL
                                                             THEN VALUE #( FOR _to IN me->to_emails->rows
                                                                           ( smtpadr = _to  ) ) ).

        DATA(cc_dist_lists) = VALUE zcl_bc_mail_facade=>tt_dlist( ( dname = me->cc_dist_list ) ).

        IF html_table IS INITIAL.
          zcl_bc_mail_facade=>send_email( it_to       = me->to_users
                                          it_rlist    = to_emails
                                          it_dlist_cc = cc_dist_lists
                                          iv_subject  = me->subject
                                          it_body     = me->body
                                          it_att_bin  = me->binary_attachments ).

        ELSE.
          zcl_bc_mail_facade=>send_html_table_email_with_str(
              it_to             = me->to_users
              col_structure     = me->html_table-structure_name
              t_data            = me->html_table-itab->*
              t_rcvlist         = VALUE #( FOR _te IN to_emails ( receiver = _te-smtpadr ) )
              iv_subject        = me->subject
              t_body            = VALUE #( FOR _bo IN me->body ( tdline = _bo-line ) )
              it_dlist_cc       = cc_dist_lists
              it_att_bin        = me->binary_attachments
              html_column_langu = COND #( WHEN me->html_table-langu IS NOT INITIAL
                                          THEN me->html_table-langu
                                          ELSE sy-langu ) ).
        ENDIF.

      CATCH zcx_bc_mail_send INTO DATA(send_error).
        RAISE EXCEPTION send_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_bc_mail_send( textid   = zcx_bc_mail_send=>cant_send
                                              previous = diaper ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.