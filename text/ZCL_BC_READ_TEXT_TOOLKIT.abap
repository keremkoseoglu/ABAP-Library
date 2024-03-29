CLASS zcl_bc_read_text_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS does_memory_txt_differ_from_db
      IMPORTING iv_id               TYPE thead-tdid
                iv_name             TYPE thead-tdname
                iv_object           TYPE thead-tdobject
      EXPORTING es_memory_head      TYPE thead
                et_memory_text      TYPE tline_tab
                et_db_text          TYPE tline_tab
      RETURNING VALUE(rv_different) TYPE abap_bool
      RAISING   RESUMABLE(zcx_bc_no_text_in_memory)
                RESUMABLE(zcx_bc_no_text_in_database).

    CLASS-METHODS read_text_avoiding_memory
      IMPORTING iv_id           TYPE thead-tdid
                iv_language     TYPE thead-tdspras DEFAULT sy-langu
                iv_name         TYPE thead-tdname
                iv_object       TYPE thead-tdobject
      RETURNING VALUE(rt_tline) TYPE tline_tab
      RAISING   zcx_bc_commit_wait_expire.

    CLASS-METHODS read_text_into_tmp01
      IMPORTING iv_guid     TYPE recaguid
                iv_id       TYPE thead-tdid
                iv_language TYPE thead-tdspras DEFAULT sy-langu
                iv_name     TYPE thead-tdname
                iv_object   TYPE thead-tdobject.

  PRIVATE SECTION.
    TYPES tt_01 TYPE STANDARD TABLE OF zbct_rtt_tmp01 WITH DEFAULT KEY.

    CONSTANTS c_max_try TYPE i VALUE 10.

    CLASS-METHODS self_maintenance.

ENDCLASS.


CLASS zcl_bc_read_text_toolkit IMPLEMENTATION.
  METHOD does_memory_txt_differ_from_db.
    CLEAR: es_memory_head,
           et_db_text,
           et_memory_text.

    " Hafızadaki metin """"""""""""""""""""""""""""""""""""""""""""""

    TRY.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING  id                      = iv_id
                     language                = sy-langu
                     name                    = iv_name
                     object                  = iv_object
          IMPORTING  header                  = es_memory_head
          TABLES     lines                   = et_memory_text
          EXCEPTIONS id                      = 1
                     language                = 2
                     name                    = 3
                     not_found               = 4
                     object                  = 5
                     reference_check         = 6
                     wrong_access_to_archive = 7
                     OTHERS                  = 8 ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'READ_TEXT' ).

      CATCH cx_root INTO DATA(lo_diaper). " Belki hafızada metin yoktur
        RAISE RESUMABLE EXCEPTION TYPE zcx_bc_no_text_in_memory
          EXPORTING previous = lo_diaper
                    text_key = |{ iv_id } { sy-langu } { iv_name } { iv_object }|.
    ENDTRY.

    " Veritabanındaki metin """""""""""""""""""""""""""""""""""""""""

    TRY.
        et_db_text = read_text_avoiding_memory( iv_id     = iv_id
                                                iv_name   = iv_name
                                                iv_object = iv_object ).

      CATCH cx_root INTO lo_diaper. " Belki veritabanında metin yoktur
        RAISE RESUMABLE EXCEPTION TYPE zcx_bc_no_text_in_database
          EXPORTING previous = lo_diaper
                    text_key = |{ iv_id } { sy-langu } { iv_name } { iv_object }|.
    ENDTRY.

    " Karşılaştır """""""""""""""""""""""""""""""""""""""""""""""""""

    rv_different = xsdbool( et_memory_text <> et_db_text ).
  ENDMETHOD.

  METHOD read_text_avoiding_memory.
    DATA(lv_guid) = cl_reca_guid=>get_new_guid( ).

    CALL FUNCTION 'ZBCF_READ_TEXT_INTO_TMP01' STARTING NEW TASK 'ZREADTEXT'
      EXPORTING iv_guid     = lv_guid
                iv_id       = iv_id
                iv_language = iv_language
                iv_name     = iv_name
                iv_object   = iv_object.

    DO c_max_try TIMES.

      SELECT SINGLE mandt INTO @sy-mandt
        FROM zbct_rtt_tmp02
        WHERE guid = @lv_guid AND
              completed = @abap_true
        ##WRITE_OK.

      IF sy-subrc = 0.
        DATA(lv_completed) = abap_true.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.

    ENDDO.

    IF lv_completed = abap_false.
      RAISE EXCEPTION TYPE zcx_bc_commit_wait_expire
        EXPORTING textid = zcx_bc_commit_wait_expire=>read_text_expired.
    ENDIF.

    SELECT tdformat, tdline
      INTO CORRESPONDING FIELDS OF TABLE @rt_tline
      FROM zbct_rtt_tmp01
      WHERE guid = @lv_guid.

    DELETE FROM: zbct_rtt_tmp01 WHERE guid = lv_guid,
                 zbct_rtt_tmp02 WHERE guid = lv_guid.
  ENDMETHOD.

  METHOD read_text_into_tmp01.
    DATA: lt_tline TYPE tline_tab,
          lr_prev  TYPE REF TO zbct_rtt_tmp01.

    self_maintenance( ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING  id                      = iv_id
                 language                = iv_language
                 name                    = iv_name
                 object                  = iv_object
      TABLES     lines                   = lt_tline
      EXCEPTIONS id                      = 1
                 language                = 2
                 name                    = 3
                 not_found               = 4
                 object                  = 5
                 reference_check         = 6
                 wrong_access_to_archive = 7
                 OTHERS                  = 8
      ##FM_SUBRC_OK.

    DATA(ls_02) = VALUE zbct_rtt_tmp02( guid      = iv_guid
                                        completed = abap_true
                                        ernam     = sy-uname
                                        erdat     = sy-datum
                                        erzet     = sy-uzeit ).

    DATA(lt_01) = CORRESPONDING tt_01( lt_tline ).

    CLEAR lr_prev.

    LOOP AT lt_01 ASSIGNING FIELD-SYMBOL(<ls_t01>).
      <ls_t01>-guid  = iv_guid.
      <ls_t01>-posnr = COND #( WHEN lr_prev IS INITIAL
                               THEN 1
                               ELSE lr_prev->posnr + 1 ).
      <ls_t01>-ernam = sy-uname.
      <ls_t01>-erdat = sy-datum.
      <ls_t01>-erzet = sy-uzeit.

      lr_prev = REF #( <ls_t01> ).
    ENDLOOP.

    INSERT: zbct_rtt_tmp01 FROM TABLE lt_01,
            zbct_rtt_tmp02 FROM ls_02.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD self_maintenance.
    DATA(lv_datum) = sy-datum - 2.
    DELETE FROM: zbct_rtt_tmp01 WHERE erdat <= lv_datum,
                 zbct_rtt_tmp02 WHERE erdat <= lv_datum.
  ENDMETHOD.
ENDCLASS.