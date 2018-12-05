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
    constants c_linsz type i value 255.
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


  METHOD send_email.

    TRY.
*       Nesne
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).
*{  EDIT  Berrin Ulus 13.04.2016 15:46:16
        lo_send_request->set_status_attributes( EXPORTING i_requested_status = iv_requested_status    " N No Status, E Only Error Statuses, A Return All Statuses
                                                          i_status_mail      = 'E' ). " N No Status, E Only Error Statuses, A All Statuses
*}  EDIT  Berrin Ulus 13.04.2016 15:46:16



*       Gönderen
        lo_send_request->set_sender( cl_sapuser_bcs=>create( iv_from ) ).

*       Alıcılar (USER NAME)
        LOOP AT it_to ASSIGNING FIELD-SYMBOL(<lv_to>).
          TRY.
              lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( get_email_of_user( <lv_to> ) )
                                              i_express   = abap_true ).

            CATCH zcx_bc_user_master_data INTO DATA(lo_cx_bumd).
              IF iv_tolerate_no_addr EQ abap_false.
                RAISE EXCEPTION lo_cx_bumd.
              ENDIF.
          ENDTRY.

        ENDLOOP.

*       Alıcılar (Mail Adres)
        LOOP AT it_rlist ASSIGNING FIELD-SYMBOL(<ls_rlist>).
          TRY.
              lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( <ls_rlist>-smtpadr )
                                              i_express   = abap_true
                                              i_copy       = <ls_rlist>-sndcp
                                              i_blind_copy = <ls_rlist>-sndbc
                                            ).

            CATCH zcx_bc_user_master_data INTO lo_cx_bumd.
              IF iv_tolerate_no_addr EQ abap_false.
                RAISE EXCEPTION lo_cx_bumd.
              ENDIF.
          ENDTRY.

        ENDLOOP.

*       Dağıtım listesi
        IF it_dlist IS SUPPLIED.
          LOOP AT it_dlist ASSIGNING FIELD-SYMBOL(<ls_list>).
            TRY.

                lo_send_request->add_recipient(
                    i_recipient  = cl_distributionlist_bcs=>getu_persistent( i_dliname = <ls_list>-dname
                                                                             i_private = '' )
                    i_express    =  abap_true ).

              CATCH zcx_bc_user_master_data INTO lo_cx_bumd.
                IF iv_tolerate_no_addr EQ abap_false.
                  RAISE EXCEPTION lo_cx_bumd.
                ENDIF.
            ENDTRY.
          ENDLOOP.
        ENDIF.

*       kullanıcı adı ya da dağıtım listesi tanımlı olmalı

        IF it_to[] IS INITIAL AND
           it_dlist[] IS INITIAL AND
           it_rlist[] IS INITIAL.

          IF iv_tolerate_no_addr EQ abap_false.
            RAISE EXCEPTION TYPE zcx_bc_method_parameter
              EXPORTING
                class_name  = 'ZCL_BC_MAIL_FACADE'
                method_name = 'SEND_EMAIL'
                textid      = zcx_bc_method_parameter=>param_error.
          else.
            return.
          ENDIF.

        ENDIF.


*       Metin
        DATA(lo_doc) = cl_document_bcs=>create_document( i_type    = c_obj_tp_raw
                                                         i_text    = it_body
                                                         i_subject = iv_subject ).

*       html
        IF it_body_html IS NOT INITIAL.
          DATA(lo_doc_html) = cl_document_bcs=>create_document( i_type  = c_doc_type_htm
                                                           i_text    = it_body_html
                                                           i_subject = iv_subject ).
        ENDIF.
        IF iv_long_subject IS NOT INITIAL.
          lo_send_request->set_message_subject( iv_long_subject ).
        ENDIF.

*       Attachment

        IF it_att_bin IS SUPPLIED.
          LOOP AT it_att_bin ASSIGNING FIELD-SYMBOL(<ls_att_bin>).
            lo_doc->add_attachment( i_attachment_type    = <ls_att_bin>-att_type
                                    i_attachment_subject = <ls_att_bin>-att_subject
                                    i_att_content_hex    = <ls_att_bin>-att_content ).
          ENDLOOP.
        ENDIF.

        IF it_att_txt IS SUPPLIED.
          LOOP AT it_att_txt ASSIGNING FIELD-SYMBOL(<ls_att_txt>).
            lo_doc->add_attachment( i_attachment_type    = <ls_att_txt>-att_type
                                    i_attachment_subject = <ls_att_txt>-att_subject
                                    i_att_content_text   = <ls_att_txt>-att_content ).
          ENDLOOP.
        ENDIF.

        IF it_att_spool IS SUPPLIED.
          LOOP AT it_att_spool ASSIGNING FIELD-SYMBOL(<ls_att_spool>).

            zcl_bc_spool_toolkit=>conv_spool_to_pdf(
                EXPORTING
                    iv_spoolid = <ls_att_spool>-spoolid
                    iv_partnum = <ls_att_spool>-partnum
                 IMPORTING
                    et_solix   = DATA(lt_pdf_solix)
            ).

            lo_doc->add_attachment( i_attachment_type    = c_att_type_pdf
                                    i_attachment_subject = <ls_att_spool>-att_subject
                                    i_att_content_hex    = lt_pdf_solix ).
          ENDLOOP.
        ENDIF.

*       Gönder
        lo_send_request->set_document( lo_doc ).
        IF it_body_html IS NOT INITIAL.
          lo_send_request->set_document( lo_doc_html ).
        ENDIF.


        IF lo_send_request->send( ) NE abap_true.
          RAISE EXCEPTION TYPE zcx_bc_mail_send
            EXPORTING
              textid = zcx_bc_mail_send=>cant_send.
        ELSE.
          IF iv_commit EQ abap_true.
            COMMIT WORK.
          ENDIF.

        ENDIF.

      CATCH zcx_bc_mail_send INTO DATA(lo_cx_ms).
        RAISE EXCEPTION lo_cx_ms.

      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_bc_mail_send
          EXPORTING
            textid   = zcx_bc_mail_send=>cant_send
            previous = lo_cx_root.

    ENDTRY.

  ENDMETHOD.


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


  method send_html_table_email.

    data : lo_table  type ref to cl_abap_tabledescr,
           lo_str    type ref to cl_abap_structdescr,
           lt_fields type abap_compdescr_tab,
           ls_fields type abap_compdescr,
           lt_lines  type table of tline.

    data : lt_output_soli type table of soli,
           ls_output_soli type          soli,
           lt_objpack     type table of sopcklsti1,
           ls_objpack     type          sopcklsti1,
           lt_objhead     type table of solisti1,
           lt_objtxt      type table of solisti1,
           ls_objtxt      type          solisti1,
           lt_reclist     type table of somlreci1,
           ls_reclist     type          somlreci1,
           ls_doc_chng    type          sodocchgi1.

    data : lv_lines       type sy-tabix,
           lv_msg_lines   type sy-tabix,
           lv_sent_all(1) type c        ##NEEDED.

    data : lv_line_data    type i,
           lv_line_columns type i.


    try.

*       T_DATA kolon sayısı / T_COLUMNS kolon sayısı UYUMLU MU?
        lo_table ?= cl_abap_typedescr=>describe_by_data( t_data ).
        lo_str   ?= lo_table->get_table_line_type( ).
        append lines of lo_str->components to lt_fields.

        describe table lt_fields  lines lv_line_data.
        describe table t_columns lines lv_line_columns.

        if lv_line_data ne lv_line_columns.
          raise exception type zcx_bc_mail_send
            exporting
              textid = zcx_bc_mail_send=>column_number_not_valid.
        endif.


*       Mail başlık
        if iv_subject is initial.
          ls_doc_chng-obj_name  = text-001.
          ls_doc_chng-obj_descr = text-001.
        else.
          ls_doc_chng-obj_name  = iv_subject.
          ls_doc_chng-obj_descr = iv_subject.
        endif.


*       Mail HTML Body
        clear ls_objtxt.
        ls_objtxt-line = '<body bgcolor = "#FFFFFF">'.
        append ls_objtxt to lt_objtxt.

        clear ls_objtxt.
        concatenate '<FONT COLOR = "#000000" face="Garamond">' '<b>'
               into ls_objtxt-line.
        append ls_objtxt to lt_objtxt.


        if t_body is initial.
*       Mail body text / SO10
          call function 'READ_TEXT'
            exporting
              id                      = 'ST'
              language                = sy-langu
              name                    = iv_so10_object
              object                  = 'TEXT'
            tables
              lines                   = lt_lines
            exceptions
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              others                  = 8.

          if sy-subrc = 0.
            loop at lt_lines assigning field-symbol(<s_lines>).
              html_body_txt  : <s_lines>-tdline.
            endloop.
          endif.
        else.
          loop at t_body assigning field-symbol(<s_body>).
            html_body_txt  : <s_body>-tdline.
          endloop.
        endif.


        clear ls_objtxt.
        ls_objtxt-line = '<center>'.
        append ls_objtxt to lt_objtxt.

        clear  ls_objtxt.
        ls_objtxt-line = '<TABLE  width= "100%" border="1">'.
        append ls_objtxt to lt_objtxt.

        loop at t_columns assigning field-symbol(<s_columns>).
          if sy-tabix = 1.
            html_hdr  : '<TR>' <s_columns>-zcolumn_txt.
          else.
            html_hdr  : ''     <s_columns>-zcolumn_txt.
          endif.
        endloop.

        loop at t_data assigning field-symbol(<fs>).

          loop at lt_fields into ls_fields.
            assign component ls_fields-name of structure <fs> to field-symbol(<fs_value>).

            if sy-tabix = 1.
              html_itm : '<TR>' <fs_value>.
            else.
              html_itm : ''    <fs_value>.
            endif.
          endloop.

        endloop.


        ls_objtxt-line = '</TABLE>'.
        append ls_objtxt to lt_objtxt.
        clear  ls_objtxt.

        ls_objtxt-line = '</center>'.
        append ls_objtxt to lt_objtxt.
        clear ls_objtxt.

        ls_objtxt-line = '</FONT></body>'.
        append ls_objtxt to lt_objtxt.
        clear ls_objtxt.


*       Packing
        describe table lt_objtxt lines lv_msg_lines.
        read table lt_objtxt into ls_objtxt index lv_msg_lines.

        ls_doc_chng-doc_size  = ( lv_msg_lines - 1 ) * c_linsz + strlen( ls_objtxt ).
        ls_objpack-transf_bin = ' '.
        ls_objpack-head_start = 1.
        ls_objpack-head_num   = 0.
        ls_objpack-body_start = 1.
        ls_objpack-body_num   = lv_msg_lines.
        ls_objpack-doc_type   = c_doc_type_htm.
        append ls_objpack to lt_objpack.
        clear ls_objpack.

        describe table lt_output_soli lines lv_lines.

        if lv_lines <> 0.
          loop at lt_output_soli into ls_output_soli.
            ls_objtxt = ls_output_soli.
            append ls_objtxt to lt_objtxt.
            clear ls_objtxt.
          endloop.
        endif.


*       Mail receivers
        loop at t_rcvlist assigning field-symbol(<s_rcvlist>).
          ls_reclist-receiver = <s_rcvlist>-receiver.
          ls_reclist-rec_type = c_rec_type.
          ls_reclist-express  = c_express.
          ls_reclist-copy = abap_true.
          append ls_reclist to lt_reclist.
          free ls_reclist.
        endloop.


*       Send mail
        call function 'SO_DOCUMENT_SEND_API1'
          exporting
            document_data              = ls_doc_chng
            put_in_outbox              = 'X'
            commit_work                = 'X'
          importing
            sent_to_all                = lv_sent_all
          tables
            packing_list               = lt_objpack
            object_header              = lt_objhead
            contents_txt               = lt_objtxt
            receivers                  = lt_reclist
          exceptions
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            others                     = 8.

        if sy-subrc = 0.
          cl_os_transaction_end_notifier=>raise_commit_requested( ).
          call function 'DB_COMMIT'.
          cl_os_transaction_end_notifier=>raise_commit_finished( ).
        endif.

      catch zcx_bc_mail_send into data(lo_cx_html).
        raise exception lo_cx_html.

      catch cx_root into data(lo_cx_root)  ##CATCH_ALL.
        raise exception type zcx_bc_mail_send
          exporting
            previous = lo_cx_root
            textid   = zcx_bc_mail_send=>cant_send.

    endtry.

  endmethod.


  METHOD send_symsg_as_email.

    send_email( it_body    = conv_symsg_to_body( is_symsg )
                it_to      = it_to
                iv_from    = iv_from
                iv_subject = iv_subject ).

  ENDMETHOD.
ENDCLASS.