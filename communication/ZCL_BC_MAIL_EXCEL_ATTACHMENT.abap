CLASS zcl_bc_mail_excel_attachment DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_excel_columns_of_fcat
      IMPORTING fcat          TYPE slis_t_fieldcat_alv
      RETURNING VALUE(result) TYPE zsdtt_column.

    CLASS-METHODS get_excel_columns_of_table
      IMPORTING tabname       TYPE tabname
      RETURNING VALUE(result) TYPE zsdtt_column.

    CLASS-METHODS create
      IMPORTING xls           TYPE zbcs_mail_excel_attachment
      RETURNING VALUE(result) TYPE REF TO  zcl_bc_mail_excel_attachment.

    CLASS-METHODS create_via_fcat
      IMPORTING base_xls      TYPE zbcs_mail_excel_att_base
                fcat          TYPE slis_t_fieldcat_alv
      RETURNING VALUE(result) TYPE REF TO  zcl_bc_mail_excel_attachment.

    CLASS-METHODS create_via_table
      IMPORTING base_xls      TYPE zbcs_mail_excel_att_base
                tabname       TYPE tabname
      RETURNING VALUE(result) TYPE REF TO  zcl_bc_mail_excel_attachment.

    METHODS convert_to_bin_attachment
      RETURNING VALUE(result) TYPE zbcs_mail_attachment_bin
      RAISING   zcx_bc_mail_bin_attachment.

  PRIVATE SECTION.
    DATA: xls            TYPE zbcs_mail_excel_attachment,
          bin_attachment TYPE zbcs_mail_attachment_bin.

    METHODS constructor IMPORTING xls TYPE zbcs_mail_excel_attachment.
ENDCLASS.


CLASS zcl_bc_mail_excel_attachment IMPLEMENTATION.
  METHOD get_excel_columns_of_fcat.
    LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      DATA(desc) = COND zsds_column( WHEN <fcat>-seltext_l IS NOT INITIAL THEN
                                       <fcat>-seltext_l

                                     WHEN <fcat>-rollname IS NOT INITIAL THEN
                                       ycl_addict_data_element=>get_text_safe( <fcat>-rollname )

                                     ELSE
                                       <fcat>-fieldname ).

      APPEND VALUE #( zcolumn     = sy-tabix
                      zcolumn_txt = desc )
             TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_excel_columns_of_table.
    DATA fcat TYPE slis_t_fieldcat_alv.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name       = tabname
      CHANGING   ct_fieldcat            = fcat
      EXCEPTIONS inconsistent_interface = 1
                 program_error          = 2
                 OTHERS                 = 3.

    CHECK sy-subrc = 0.

    result = get_excel_columns_of_fcat( fcat ).
  ENDMETHOD.

  METHOD create.
    result = NEW #( xls ).
  ENDMETHOD.

  METHOD create_via_fcat.
    DATA(xls)   = CORRESPONDING zbcs_mail_excel_attachment( base_xls ).
    xls-columns = get_excel_columns_of_fcat( fcat ).
    result      = NEW #( xls ).
  ENDMETHOD.

  METHOD create_via_table.
    DATA(xls)   = CORRESPONDING zbcs_mail_excel_attachment( base_xls ).
    xls-columns = get_excel_columns_of_table( tabname ).
    result      = NEW #( xls ).
  ENDMETHOD.

  METHOD convert_to_bin_attachment.
    DATA: zcolumn      TYPE column_z5_z5a,
          bin_text     TYPE string,
          element_type TYPE REF TO cl_abap_elemdescr,
          bin_text_tmp TYPE text100,
          binary_texts TYPE solix_tab.

    FIELD-SYMBOLS <xls_rows> TYPE ANY TABLE.

    IF me->bin_attachment IS INITIAL.
      ASSIGN me->xls-itab_ref->* TO <xls_rows>.
      DATA(xls_cols) = zcl_bc_itab_toolkit=>get_itab_components( <xls_rows> ).

      LOOP AT me->xls-exclude_columns INTO DATA(exclude_column).
        DELETE xls_cols WHERE name = exclude_column-zcolumn_txt.
      ENDLOOP.

      LOOP AT xls_cols ASSIGNING FIELD-SYMBOL(<xls_col>).
        zcolumn = sy-tabix.

        IF sy-tabix > 1.
          CONCATENATE bin_text cl_bcs_convert=>gc_tab INTO bin_text.
        ENDIF.

        element_type ?= <xls_col>-type.
        DATA(ddic_field) = element_type->get_ddic_field( ).

        READ TABLE me->xls-columns
             ASSIGNING FIELD-SYMBOL(<ls_columns>)
             WITH KEY zcolumn = zcolumn.

        IF sy-subrc = 0.
          ddic_field-scrtext_l = <ls_columns>-zcolumn_txt.
        ENDIF.

        CONCATENATE bin_text ddic_field-scrtext_l INTO bin_text.
      ENDLOOP.

      CONCATENATE bin_text cl_bcs_convert=>gc_crlf INTO bin_text.

      LOOP AT <xls_rows> ASSIGNING FIELD-SYMBOL(<fs_row>).
        LOOP AT xls_cols ASSIGNING <xls_col>.
          IF sy-tabix > 1.
            CONCATENATE bin_text cl_bcs_convert=>gc_tab INTO bin_text.
          ENDIF.

          ASSIGN COMPONENT <xls_col>-name OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_field>).
          WRITE <fs_field> TO bin_text_tmp.
          CONDENSE bin_text_tmp.
          CONCATENATE bin_text bin_text_tmp INTO bin_text.
        ENDLOOP.

        CONCATENATE bin_text cl_bcs_convert=>gc_crlf INTO bin_text.
      ENDLOOP.

      TRY.
          cl_bcs_convert=>string_to_solix( EXPORTING iv_string   = bin_text
                                                     iv_codepage = '4103' " suitable for MS Excel, leave empty"
                                                     iv_add_bom  = abap_true
                                           IMPORTING et_solix    = binary_texts ).

        CATCH cx_root INTO DATA(solix_error).
          RAISE EXCEPTION NEW zcx_bc_mail_bin_attachment( textid   = zcx_bc_mail_bin_attachment=>att_to_solix_failed
                                                          previous = solix_error ).
      ENDTRY.

      IF binary_texts IS INITIAL.
        RAISE EXCEPTION NEW zcx_bc_mail_bin_attachment( textid = zcx_bc_mail_bin_attachment=>empty_content_no_bin_att ).
      ENDIF.

      me->bin_attachment = VALUE #( att_type    = 'XLS'
                                    att_subject = me->xls-filename
                                    att_content = binary_texts ).
    ENDIF.

    result = me->bin_attachment.
  ENDMETHOD.

  METHOD constructor.
    me->xls = xls.
  ENDMETHOD.
ENDCLASS.