class ZCL_BC_MAIL_FACADE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_attachment_bin,
        att_type    TYPE soodk-objtp,
        att_subject TYPE sood-objdes,
        att_content TYPE solix_tab,
      END OF t_attachment_bin .
  types T_RLIST type ZBCS_REC_LIST .
  types TT_RLIST type ZBCTT_REC_LIST .
  types:
    BEGIN OF t_dlist,
        dname TYPE so_obj_nam,
      END OF t_dlist .
  types:
    tt_dlist TYPE STANDARD TABLE OF t_dlist WITH DEFAULT KEY .
  types:
    BEGIN OF t_attachment_spool,
        att_subject TYPE sood-objdes,
        spoolid     TYPE rspoid,
        partnum     TYPE adsnum,
      END OF t_attachment_spool .
  types:
    BEGIN OF t_attachment_txt,
        att_type    TYPE soodk-objtp,
        att_subject TYPE sood-objdes,
        att_content TYPE soli_tab,
      END OF t_attachment_txt .
  types:
    tt_attachment_bin   TYPE STANDARD TABLE OF t_attachment_bin WITH DEFAULT KEY .
  types:
    tt_attachment_spool TYPE STANDARD TABLE OF t_attachment_spool WITH DEFAULT KEY .
  types:
    tt_attachment_txt   TYPE STANDARD TABLE OF t_attachment_txt WITH DEFAULT KEY .
  types:
    BEGIN OF t_excel_attachment,
        itab_ref        TYPE REF TO data,
        columns         TYPE zsdtt_column,
        exclude_columns TYPE zsdtt_column,
        filename        TYPE sood-objdes,
      END OF t_excel_attachment .
  types:
    tt_excel_attachment TYPE STANDARD TABLE OF t_excel_attachment WITH DEFAULT KEY .

  constants:
    c_int(3)          TYPE c value 'INT' ##NO_TEXT ##NEEDED.
  constants:
    c_doc_type_htm(3) TYPE c value 'HTM' ##NO_TEXT ##NEEDED.
  constants C_REC_TYPE type C value 'U' ##NO_TEXT ##NEEDED.
  constants C_EXPRESS type C value 'X' ##NO_TEXT.

  class-methods CONV_SYMSG_TO_BODY
    importing
      !IS_SYMSG type SYMSG optional
    returning
      value(RT_BODY) type BCSY_TEXT .
  class-methods GET_EMAIL_OF_USER
    importing
      !IV_UNAME type SYUNAME
      !IV_CHECK type FLAG default ABAP_TRUE
    returning
      value(RV_EMAIL) type ADR6-SMTP_ADDR
    raising
      ZCX_BC_USER_MASTER_DATA .
  class-methods GET_EXCEL_COLUMNS_OF_FCAT
    importing
      !IT_FCAT type SLIS_T_FIELDCAT_ALV
    returning
      value(RT_COL) type ZSDTT_COLUMN .
  class-methods GET_EXCEL_COLUMNS_OF_TABLE
    importing
      !IV_TABNAME type TABNAME
    returning
      value(RT_COL) type ZSDTT_COLUMN .
  class-methods SEND_EMAIL
    importing
      !IV_FROM type SYUNAME default SY-UNAME
      !IT_TO type RKE_USERID optional
      !IT_RLIST type TT_RLIST optional
      !IT_DLIST type TT_DLIST optional
      !IV_SUBJECT type SO_OBJ_DES
      !IV_TOLERATE_NO_ADDR type ABAP_BOOL default ABAP_FALSE
      !IT_BODY type BCSY_TEXT
      !IT_BODY_HTML type BCSY_TEXT optional
      !IT_ATT_BIN type TT_ATTACHMENT_BIN optional
      !IT_ATT_TXT type TT_ATTACHMENT_TXT optional
      !IT_ATT_SPOOL type TT_ATTACHMENT_SPOOL optional
      !IV_REQUESTED_STATUS type BCS_RQST default 'E'
      value(IV_COMMIT) type CHAR1 default 'X'
      !IV_LONG_SUBJECT type STRING optional
      !IV_SENSITIVITY type SO_OBJ_SNS optional
    raising
      ZCX_BC_MAIL_SEND .
  class-methods SEND_EMAIL_WITH_SAP_LINK
    importing
      !IV_RECIPIENT type SYUNAME
      !IV_SUBJECT type SO_OBJ_DES
      !IV_TOLERATE_NO_ADDR type ABAP_BOOL default ABAP_FALSE
      !IT_BODY type BCSY_TEXT
      !IV_COMMAND type CLIKE
      value(IV_COMMIT) type CHAR1 default 'X'
    raising
      ZCX_BC_MAIL_SEND .
  class-methods SEND_SYMSG_AS_EMAIL
    importing
      !IV_FROM type SYUNAME default SY-UNAME
      !IT_TO type RKE_USERID
      !IV_SUBJECT type SO_OBJ_DES
      !IS_SYMSG type SYMSG optional
    raising
      ZCX_BC_MAIL_SEND .
  class-methods SEND_HTML_TABLE_EMAIL
    importing
      !T_DATA type ANY TABLE optional
      !T_RCVLIST type ZSDTT_MAIL_RECEIVER optional
      !T_COLUMNS type ZSDTT_COLUMN optional
      !IV_SO10_OBJECT type TDOBNAME optional
      !IV_SUBJECT type SO_OBJ_DES optional
      !T_BODY type TLINET optional
    raising
      ZCX_BC_MAIL_SEND .
  class-methods GET_USER_OF_EMAIL
    importing
      !IV_SMTP type T024-SMTP_ADDR
    returning
      value(RV_BNAME) type USR21-BNAME
    raising
      ZCX_BC_USER_MASTER_DATA .
  class-methods SEND_EXCEL_TABLE_EMAIL
    importing
      !T_DATA type ANY TABLE optional
      !T_COLUMNS type ZSDTT_COLUMN optional
      !IV_SUBJECT type SO_OBJ_DES
      !IT_BODY type BCSY_TEXT
      !IT_TO type RKE_USERID optional
      !IV_FILENAME type SOOD-OBJDES
      !IT_RLIST type TT_RLIST optional
      !IT_DLIST type TT_DLIST optional
      !IT_EXCLUDE_COLUMNS type ZSDTT_COLUMN optional
    raising
      ZCX_BC_MAIL_SEND .
  class-methods SEND_EXCEL_TABLES_EMAIL
    importing
      !IV_SUBJECT type SO_OBJ_DES
      !IT_BODY type BCSY_TEXT
      !IT_EXCEL_ATT type TT_EXCEL_ATTACHMENT
      !IT_TO type RKE_USERID optional
      !IT_RLIST type TT_RLIST optional
      !IT_DLIST type TT_DLIST optional
    raising
      ZCX_BC_MAIL_SEND .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_att_type_pdf TYPE soodk-objtp VALUE 'PDF' ##NO_TEXT.
    CONSTANTS c_linsz TYPE i VALUE 255.
    CONSTANTS c_obj_tp_raw TYPE so_obj_tp VALUE 'RAW' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_BC_MAIL_FACADE IMPLEMENTATION.


  METHOD conv_symsg_to_body.

    DATA ls_symsg TYPE symsg.

*   Mesaj içeriğini oluştur

    IF is_symsg IS SUPPLIED.
      ls_symsg = is_symsg.
    ELSE.
      MOVE-CORRESPONDING sy TO ls_symsg.
    ENDIF.

    IF ls_symsg-msgty IS INITIAL.
      ls_symsg-msgty = zcl_bc_applog_facade=>c_msgty_s.
    ENDIF.

    APPEND INITIAL LINE TO rt_body ASSIGNING FIELD-SYMBOL(<lv_body>).

    MESSAGE ID     ls_symsg-msgid
            TYPE   ls_symsg-msgty
            NUMBER ls_symsg-msgno
            WITH   ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
            INTO   <lv_body>.

  ENDMETHOD.


  METHOD get_email_of_user.

    SELECT SINGLE smtp_addr INTO rv_email
      FROM adr6
      WHERE
        addrnumber EQ ( SELECT addrnumber
                        FROM usr21
                        WHERE bname EQ iv_uname
                      ) AND
        persnumber EQ ( SELECT persnumber
                        FROM usr21
                        WHERE bname EQ iv_uname
                      ) ##WARN_OK .                     "#EC CI_NOORDER

    IF rv_email IS INITIAL AND iv_check EQ abap_true.
      RAISE EXCEPTION TYPE zcx_bc_user_master_data
        EXPORTING
          textid = zcx_bc_user_master_data=>email_missing
          uname  = iv_uname.
    ENDIF.

  ENDMETHOD.


  method get_Excel_columns_of_fcat.

    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

      APPEND VALUE #(
          zcolumn     = sy-tabix
          zcolumn_txt = <ls_fcat>-seltext_l
        ) TO rt_col.

    ENDLOOP.

  endmethod.


  METHOD get_excel_columns_of_table.

    DATA lt_fcat TYPE slis_t_fieldcat_alv.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = iv_tabname
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    rt_col = get_excel_columns_of_fcat( lt_fcat ).

  ENDMETHOD.


  METHOD get_user_of_email.

    SELECT SINGLE u~bname
      INTO rv_bname
      FROM
        usr21 AS u
        INNER JOIN adr6 AS a ON
          a~persnumber = u~persnumber AND
          a~addrnumber = u~addrnumber
      WHERE smtp_addr = iv_smtp ##WARN_OK.              "#EC CI_NOORDER

    IF rv_bname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_user_master_data
        EXPORTING
          textid = zcx_bc_user_master_data=>user_email_nomatch
          email  = iv_smtp.
    ENDIF.

  ENDMETHOD.


  method send_email.

    try.
*       Nesne
        data(lo_send_request) = cl_bcs=>create_persistent( ).
*{  EDIT  Berrin Ulus 13.04.2016 15:46:16
        lo_send_request->set_status_attributes( exporting i_requested_status = iv_requested_status    " N No Status, E Only Error Statuses, A Return All Statuses
                                                          i_status_mail      = 'E' ). " N No Status, E Only Error Statuses, A All Statuses
*}  EDIT  Berrin Ulus 13.04.2016 15:46:16



*       Gönderen
        lo_send_request->set_sender( cl_sapuser_bcs=>create( iv_from ) ).

*       Alıcılar (USER NAME)
        loop at it_to assigning field-symbol(<lv_to>).
          try.
              lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( get_email_of_user( <lv_to> ) )
                                              i_express   = abap_true ).

            catch zcx_bc_user_master_data into data(lo_cx_bumd).
              if iv_tolerate_no_addr eq abap_false.
                raise exception lo_cx_bumd.
              endif.
          endtry.

        endloop.

*       Alıcılar (Mail Adres)
        loop at it_rlist assigning field-symbol(<ls_rlist>).
          try.
              lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( <ls_rlist>-smtpadr )
                                              i_express   = abap_true
                                              i_copy       = <ls_rlist>-sndcp
                                              i_blind_copy = <ls_rlist>-sndbc
                                            ).

            catch zcx_bc_user_master_data into lo_cx_bumd.
              if iv_tolerate_no_addr eq abap_false.
                raise exception lo_cx_bumd.
              endif.
          endtry.

        endloop.

*       Dağıtım listesi
        if it_dlist is supplied.
          loop at it_dlist assigning field-symbol(<ls_list>).
            try.

                lo_send_request->add_recipient(
                    i_recipient  = cl_distributionlist_bcs=>getu_persistent( i_dliname = <ls_list>-dname
                                                                             i_private = '' )
                    i_express    =  abap_true ).

              catch zcx_bc_user_master_data into lo_cx_bumd.
                if iv_tolerate_no_addr eq abap_false.
                  raise exception lo_cx_bumd.
                endif.
            endtry.
          endloop.
        endif.

*       kullanıcı adı ya da dağıtım listesi tanımlı olmalı

        if it_to[] is initial and
           it_dlist[] is initial and
           it_rlist[] is initial.

          if iv_tolerate_no_addr eq abap_false.
            raise exception type zcx_bc_method_parameter
              exporting
                class_name  = 'ZCL_BC_MAIL_FACADE'
                method_name = 'SEND_EMAIL'
                textid      = zcx_bc_method_parameter=>param_error.
          else.
            return.
          endif.

        endif.


*       Metin
        data(lo_doc) = cl_document_bcs=>create_document( i_type    = c_obj_tp_raw
                                                         i_text    = it_body
                                                         i_subject = iv_subject ).
        if iv_sensitivity is not initial.
          lo_doc->set_sensitivity( iv_sensitivity ).
        endif.

*       html
        if it_body_html is not initial.
          data(lo_doc_html) = cl_document_bcs=>create_document( i_type  = c_doc_type_htm
                                                           i_text    = it_body_html
                                                           i_subject = iv_subject ).
          if iv_sensitivity is not initial.
            lo_doc_html->set_sensitivity( iv_sensitivity ).
          endif.
        endif.

        if iv_long_subject is not initial.
          lo_send_request->set_message_subject( iv_long_subject ).
        endif.
*       Attachment

        if it_att_bin is supplied.
          loop at it_att_bin assigning field-symbol(<ls_att_bin>).
            lo_doc->add_attachment( i_attachment_type    = <ls_att_bin>-att_type
                                    i_attachment_subject = <ls_att_bin>-att_subject
                                    i_att_content_hex    = <ls_att_bin>-att_content ).
          endloop.
        endif.

        if it_att_txt is supplied.
          loop at it_att_txt assigning field-symbol(<ls_att_txt>).
            lo_doc->add_attachment( i_attachment_type    = <ls_att_txt>-att_type
                                    i_attachment_subject = <ls_att_txt>-att_subject
                                    i_att_content_text   = <ls_att_txt>-att_content ).
          endloop.
        endif.

        if it_att_spool is supplied.
          loop at it_att_spool assigning field-symbol(<ls_att_spool>).

            zcl_bc_spool_toolkit=>conv_spool_to_pdf(
                exporting
                    iv_spoolid = <ls_att_spool>-spoolid
                    iv_partnum = <ls_att_spool>-partnum
                 importing
                    et_solix   = data(lt_pdf_solix)
            ).

            lo_doc->add_attachment( i_attachment_type    = c_att_type_pdf
                                    i_attachment_subject = <ls_att_spool>-att_subject
                                    i_att_content_hex    = lt_pdf_solix ).
          endloop.
        endif.

*       Gönder
        lo_send_request->set_document( lo_doc ).
        if it_body_html is not initial.
          lo_send_request->set_document( lo_doc_html ).
        endif.


        if lo_send_request->send( ) ne abap_true.
          raise exception type zcx_bc_mail_send
            exporting
              textid = zcx_bc_mail_send=>cant_send.
        else.
          if iv_commit eq abap_true.
            commit work.
          endif.

        endif.

      catch zcx_bc_mail_send into data(lo_cx_ms).
        raise exception lo_cx_ms.

      catch cx_root into data(lo_cx_root).
        raise exception type zcx_bc_mail_send
          exporting
            textid   = zcx_bc_mail_send=>cant_send
            previous = lo_cx_root.

    endtry.

  endmethod.


  METHOD send_email_with_sap_link.

    send_email(
        it_to               = VALUE #( ( iv_recipient ) )
        iv_subject          = iv_subject
        iv_tolerate_no_addr = iv_tolerate_no_addr
        it_body             = it_body
        it_att_txt          = VALUE #( (
          att_type    = 'SAP'
          att_subject = 'LINK'
          att_content = VALUE #(
            ( line = |[System]{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Name={ sy-sysid }{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Description={ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Client={ sy-mandt }{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |[User]{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Name={ iv_recipient }{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Language={ sy-langu }{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |[Function]{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Title=={ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Command={ iv_command }{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |Type=Transaction{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |[Configuration]{ cl_abap_char_utilities=>cr_lf }| )
            ( line = |GuiSize=Maximized{ cl_abap_char_utilities=>cr_lf }| )
          )
        ) )
        iv_commit           = iv_commit
    ).

  ENDMETHOD.


  METHOD send_excel_tables_email.

    DATA:
      lo_table        TYPE REF TO cl_abap_tabledescr,
      lo_str          TYPE REF TO cl_abap_structdescr,
      lo_element_type TYPE REF TO cl_abap_elemdescr,
      ls_field        TYPE dfies,
      lt_att_bin      TYPE tt_attachment_bin,
      lt_components   TYPE cl_abap_structdescr=>component_table,
      lt_binary_text  TYPE solix_tab,
      lv_sent_all     TYPE char1,
      lv_text         TYPE string,
      lv_text_tmp     TYPE text100,
      lv_zcolumn      TYPE column_z5_z5a.

    FIELD-SYMBOLS:
      <lt_excel_itab> TYPE ANY TABLE.

    TRY.

        "---Attachment hazırlığı-----

        LOOP AT it_excel_att ASSIGNING FIELD-SYMBOL(<ls_excel_att>).

          CLEAR lt_binary_text.

          ASSIGN <ls_excel_att>-itab_ref->* TO <lt_excel_itab>.

          lo_table ?= cl_abap_typedescr=>describe_by_data( <lt_excel_itab> ).
          lo_str   ?= lo_table->get_table_line_type( ).
          lt_components = lo_str->get_components( ).

          LOOP AT <ls_excel_att>-exclude_columns INTO DATA(ls_excl).
            DELETE lt_components WHERE name = ls_excl-zcolumn_txt.
          ENDLOOP.

          LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_component>).
            lv_zcolumn = sy-tabix.
            IF sy-tabix GT 1.
              CONCATENATE lv_text cl_bcs_convert=>gc_tab INTO lv_text.
            ENDIF.
            lo_element_type ?= <fs_component>-type.
            ls_field         = lo_element_type->get_ddic_field( ).

            READ TABLE <ls_excel_att>-columns
              ASSIGNING FIELD-SYMBOL(<ls_columns>)
              WITH KEY zcolumn = lv_zcolumn.
            IF sy-subrc EQ 0.
              ls_field-scrtext_l = <ls_columns>-zcolumn_txt.
            ENDIF.

            CONCATENATE lv_text ls_field-scrtext_l INTO lv_text.
          ENDLOOP.
          CONCATENATE lv_text cl_bcs_convert=>gc_crlf INTO lv_text.

          LOOP AT <lt_excel_itab> ASSIGNING FIELD-SYMBOL(<fs_row>).
            LOOP AT lt_components ASSIGNING <fs_component>.
              IF sy-tabix GT 1.
                CONCATENATE lv_text cl_bcs_convert=>gc_tab INTO lv_text.
              ENDIF.
              ASSIGN COMPONENT <fs_component>-name OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_field>).
              WRITE <fs_field> TO lv_text_tmp.
              CONDENSE lv_text_tmp.
              CONCATENATE lv_text lv_text_tmp INTO lv_text.
            ENDLOOP.
            CONCATENATE lv_text cl_bcs_convert=>gc_crlf INTO lv_text.
          ENDLOOP.
          TRY .
              cl_bcs_convert=>string_to_solix(
                EXPORTING
                  iv_string   = lv_text
                  iv_codepage = '4103' "suitable for MS Excel, leave empty"
                  iv_add_bom  = abap_true
                IMPORTING
                  et_solix    = lt_binary_text
              ).
            CATCH cx_bcs.
              CONTINUE.
          ENDTRY.

          CHECK lt_binary_text IS NOT INITIAL.

          APPEND VALUE #(
              att_type    = 'XLS'
              att_subject = <ls_excel_att>-filename
              att_content = lt_binary_text
            ) TO lt_att_bin.

        ENDLOOP.

        "---Gönder-----

        send_email(
          it_to      = it_to
          it_dlist   = it_dlist
          it_rlist   = it_rlist
          iv_subject = iv_subject
          it_body    = it_body[]
          it_att_bin = lt_att_bin
        ).

      CATCH cx_root INTO DATA(lo_cx_root)  ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_bc_mail_send
          EXPORTING
            previous = lo_cx_root
            textid   = zcx_bc_mail_send=>cant_send.

    ENDTRY.


  ENDMETHOD.


  METHOD send_excel_table_email.

    send_excel_tables_email(
        iv_subject   = iv_subject
        it_body      = it_body
        it_to        = it_to
        it_rlist     = it_rlist
        it_dlist     = it_dlist
        it_excel_att = VALUE #( (
          itab_ref        = REF #( t_data )
          columns         = t_columns
          exclude_columns = it_exclude_columns
          filename        = iv_filename
        ) )
    ).

  ENDMETHOD.


  METHOD send_html_table_email.

    DATA : lo_table  TYPE REF TO cl_abap_tabledescr,
           lo_str    TYPE REF TO cl_abap_structdescr,
           lt_fields TYPE abap_compdescr_tab,
           ls_fields TYPE abap_compdescr,
           lt_lines  TYPE TABLE OF tline.

    DATA : lt_output_soli TYPE TABLE OF soli,
           ls_output_soli TYPE          soli,
           lt_objpack     TYPE TABLE OF sopcklsti1,
           ls_objpack     TYPE          sopcklsti1,
           lt_objhead     TYPE TABLE OF solisti1,
           lt_objtxt      TYPE TABLE OF solisti1,
           ls_objtxt      TYPE          solisti1,
           lt_reclist     TYPE TABLE OF somlreci1,
           ls_reclist     TYPE          somlreci1,
           ls_doc_chng    TYPE          sodocchgi1.

    DATA : lv_lines       TYPE sy-tabix,
           lv_msg_lines   TYPE sy-tabix,
           lv_sent_all(1) TYPE c        ##NEEDED.

    DATA : lv_line_data    TYPE i,
           lv_line_columns TYPE i.


    TRY.

*       T_DATA kolon sayısı / T_COLUMNS kolon sayısı UYUMLU MU?
        lo_table ?= cl_abap_typedescr=>describe_by_data( t_data ).
        lo_str   ?= lo_table->get_table_line_type( ).
        APPEND LINES OF lo_str->components TO lt_fields.

        DESCRIBE TABLE lt_fields  LINES lv_line_data.
        DESCRIBE TABLE t_columns LINES lv_line_columns.

        IF lv_line_data NE lv_line_columns.
          RAISE EXCEPTION TYPE zcx_bc_mail_send
            EXPORTING
              textid = zcx_bc_mail_send=>column_number_not_valid.
        ENDIF.


*       Mail başlık
        IF iv_subject IS INITIAL.
          ls_doc_chng-obj_name  = TEXT-001.
          ls_doc_chng-obj_descr = TEXT-001.
        ELSE.
          ls_doc_chng-obj_name  = iv_subject.
          ls_doc_chng-obj_descr = iv_subject.
        ENDIF.


*       Mail HTML Body
        CLEAR ls_objtxt.
        ls_objtxt-line = '<body bgcolor = "#FFFFFF">'.
        APPEND ls_objtxt TO lt_objtxt.

        CLEAR ls_objtxt.
        CONCATENATE '<FONT COLOR = "#000000" face="Garamond">' '<b>'
               INTO ls_objtxt-line.
        APPEND ls_objtxt TO lt_objtxt.


        IF t_body IS INITIAL.
*       Mail body text / SO10
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = sy-langu
              name                    = iv_so10_object
              object                  = 'TEXT'
            TABLES
              lines                   = lt_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

          IF sy-subrc = 0.
            LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<s_lines>).
              html_body_txt  : <s_lines>-tdline.
            ENDLOOP.
          ENDIF.
        ELSE.
          LOOP AT t_body ASSIGNING FIELD-SYMBOL(<s_body>).
            html_body_txt  : <s_body>-tdline.
          ENDLOOP.
        ENDIF.


        CLEAR ls_objtxt.
        ls_objtxt-line = '<center>'.
        APPEND ls_objtxt TO lt_objtxt.

        CLEAR  ls_objtxt.
        ls_objtxt-line = '<TABLE  width= "100%" border="1">'.
        APPEND ls_objtxt TO lt_objtxt.

        LOOP AT t_columns ASSIGNING FIELD-SYMBOL(<s_columns>).
          IF sy-tabix = 1.
            html_hdr  : '<TR>' <s_columns>-zcolumn_txt.
          ELSE.
            html_hdr  : ''     <s_columns>-zcolumn_txt.
          ENDIF.
        ENDLOOP.

        LOOP AT t_data ASSIGNING FIELD-SYMBOL(<fs>).

          LOOP AT lt_fields INTO ls_fields.
            ASSIGN COMPONENT ls_fields-name OF STRUCTURE <fs> TO FIELD-SYMBOL(<fs_value>).

            IF sy-tabix = 1.
              html_itm : '<TR>' <fs_value>.
            ELSE.
              html_itm : ''    <fs_value>.
            ENDIF.
          ENDLOOP.

        ENDLOOP.


        ls_objtxt-line = '</TABLE>'.
        APPEND ls_objtxt TO lt_objtxt.
        CLEAR  ls_objtxt.

        ls_objtxt-line = '</center>'.
        APPEND ls_objtxt TO lt_objtxt.
        CLEAR ls_objtxt.

        ls_objtxt-line = '</FONT></body>'.
        APPEND ls_objtxt TO lt_objtxt.
        CLEAR ls_objtxt.


*       Packing
        DESCRIBE TABLE lt_objtxt LINES lv_msg_lines.
        READ TABLE lt_objtxt INTO ls_objtxt INDEX lv_msg_lines.

        ls_doc_chng-doc_size  = ( lv_msg_lines - 1 ) * c_linsz + strlen( ls_objtxt ).
        ls_objpack-transf_bin = ' '.
        ls_objpack-head_start = 1.
        ls_objpack-head_num   = 0.
        ls_objpack-body_start = 1.
        ls_objpack-body_num   = lv_msg_lines.
        ls_objpack-doc_type   = c_doc_type_htm.
        APPEND ls_objpack TO lt_objpack.
        CLEAR ls_objpack.

        DESCRIBE TABLE lt_output_soli LINES lv_lines.

        IF lv_lines <> 0.
          LOOP AT lt_output_soli INTO ls_output_soli.
            ls_objtxt = ls_output_soli.
            APPEND ls_objtxt TO lt_objtxt.
            CLEAR ls_objtxt.
          ENDLOOP.
        ENDIF.


*       Mail receivers
        LOOP AT t_rcvlist ASSIGNING FIELD-SYMBOL(<s_rcvlist>).
          ls_reclist-receiver = <s_rcvlist>-receiver.
          ls_reclist-rec_type = c_rec_type.
          ls_reclist-express  = c_express.
          ls_reclist-copy = abap_true.
          APPEND ls_reclist TO lt_reclist.
          FREE ls_reclist.
        ENDLOOP.


*       Send mail
        CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
          EXPORTING
            document_data              = ls_doc_chng
            put_in_outbox              = 'X'
            commit_work                = 'X'
          IMPORTING
            sent_to_all                = lv_sent_all
          TABLES
            packing_list               = lt_objpack
            object_header              = lt_objhead
            contents_txt               = lt_objtxt
            receivers                  = lt_reclist
          EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.

        IF sy-subrc = 0.
          cl_os_transaction_end_notifier=>raise_commit_requested( ).
          CALL FUNCTION 'DB_COMMIT'.
          cl_os_transaction_end_notifier=>raise_commit_finished( ).
        ENDIF.

      CATCH zcx_bc_mail_send INTO DATA(lo_cx_html).
        RAISE EXCEPTION lo_cx_html.

      CATCH cx_root INTO DATA(lo_cx_root)  ##CATCH_ALL.
        RAISE EXCEPTION TYPE zcx_bc_mail_send
          EXPORTING
            previous = lo_cx_root
            textid   = zcx_bc_mail_send=>cant_send.

    ENDTRY.

  ENDMETHOD.


  METHOD send_symsg_as_email.

    send_email( it_body    = conv_symsg_to_body( is_symsg )
                it_to      = it_to
                iv_from    = iv_from
                iv_subject = iv_subject ).

  ENDMETHOD.
ENDCLASS.