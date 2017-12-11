CLASS zcl_bc_read_text_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:

      read_text_avoiding_memory
        IMPORTING
          !iv_id          TYPE thead-tdid
          !iv_language    TYPE thead-tdspras DEFAULT sy-langu
          !iv_name        TYPE thead-tdname
          !iv_object      TYPE thead-tdobject
        RETURNING
          VALUE(rt_tline) TYPE tline_tab
        raising
          ZCX_BC_COMMIT_WAIT_EXPIRE,

      read_text_into_tmp01
        IMPORTING
          !iv_guid     TYPE recaguid
          !iv_id       TYPE thead-tdid
          !iv_language TYPE thead-tdspras DEFAULT sy-langu
          !iv_name     TYPE thead-tdname
          !iv_object   TYPE thead-tdobject.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES tt_01 TYPE STANDARD TABLE OF zbct_rtt_tmp01 WITH DEFAULT KEY.

    constants c_max_try type i value 10.

    CLASS-METHODS self_maintenance.

ENDCLASS.



CLASS ZCL_BC_READ_TEXT_TOOLKIT IMPLEMENTATION.


  METHOD read_text_avoiding_memory.

    DATA(lv_guid) = cl_reca_guid=>get_new_guid( ).

    CALL FUNCTION 'ZBCF_READ_TEXT_INTO_TMP01' starting new task 'ZREADTEXT'
      EXPORTING
        iv_guid     = lv_guid
        iv_id       = iv_id
        iv_language = iv_language
        iv_name     = iv_name
        iv_object   = iv_object.

    do c_max_try times.

      select single mandt into @sy-mandt
        from ZBCT_RTT_TMP02
        where guid eq @lv_guid and
              completed eq @abap_true
        ##write_ok.

      if sy-subrc eq 0.
        data(lv_completed) = abap_True.
        exit.
      endif.

      wait up to 1 seconds.

    enddo.

    if lv_completed eq abap_False.
      raise exception type zcx_bc_commit_wait_expire
        EXPORTING
          textid   = ZCX_BC_COMMIT_WAIT_EXPIRE=>read_text_expired.
    endif.

    select tdformat, tdline
      into corresponding fields of table @rt_tline
      from ZBCT_RTT_TMP01
      where guid eq @lv_guid.

    DELETE FROM: zbct_rtt_tmp01 WHERE guid eq lv_guid,
                 zbct_rtt_tmp02 WHERE guid eq lv_guid.

  ENDMETHOD.


  METHOD read_text_into_tmp01.

    DATA: lr_prev  TYPE REF TO zbct_rtt_tmp01,
          lt_tline TYPE tline_tab.

    self_maintenance( ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = iv_id
        language                = iv_language
        name                    = iv_name
        object                  = iv_object
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8
        ##FM_SUBRC_OK.

    DATA(ls_02) = VALUE zbct_rtt_tmp02(
        guid      = iv_guid
        completed = abap_true
        ernam     = sy-uname
        erdat     = sy-datum
        erzet     = sy-uzeit
      ).

    DATA(lt_01) = CORRESPONDING tt_01( lt_tline ).

    CLEAR lr_prev.

    LOOP AT lt_01 ASSIGNING FIELD-SYMBOL(<ls_t01>).
      <ls_t01>-guid  = iv_guid.
      <ls_t01>-posnr = COND #( WHEN lr_prev IS INITIAL THEN 1
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
    DELETE FROM: zbct_rtt_tmp01 WHERE erdat LE lv_datum,
                 zbct_rtt_tmp02 WHERE erdat LE lv_datum.
  ENDMETHOD.
ENDCLASS.