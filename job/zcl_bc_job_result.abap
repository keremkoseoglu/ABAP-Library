CLASS zcl_bc_job_result DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF input_dict,
             jobname  TYPE tbtcp-jobname,
             jobcount TYPE tbtcp-jobcount,
           END OF input_dict.

    TYPES spool_list TYPE STANDARD TABLE OF bapixmspow WITH EMPTY KEY.

    CONSTANTS max_job_wait TYPE i VALUE 72000.

    METHODS constructor IMPORTING !input TYPE input_dict.

    METHODS wait_until_job_done
      IMPORTING !max_wait   TYPE i DEFAULT max_job_wait
      EXPORTING !job_status TYPE btcstatus
      RAISING   zcx_bc_job_wait.

    METHODS get_spool_output RETURNING VALUE(result) TYPE spool_list.

    METHODS get_job_log
      RETURNING VALUE(result) TYPE btc_t_job_log
      RAISING   ycx_addict_function_subrc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF job_status,
                 running    TYPE btcstatus VALUE 'R',
                 ready      TYPE btcstatus VALUE 'Y',
                 scheduled  TYPE btcstatus VALUE 'P',
                 released   TYPE btcstatus VALUE 'S',
                 aborted    TYPE btcstatus VALUE 'A',
                 finished   TYPE btcstatus VALUE 'F',
                 put_active TYPE btcstatus VALUE 'Z',
                 unknown    TYPE btcstatus VALUE 'X',
               END OF job_status.

    DATA input TYPE input_dict.
ENDCLASS.



CLASS zcl_bc_job_result IMPLEMENTATION.
  METHOD constructor.
    me->input = input.
  ENDMETHOD.


  METHOD wait_until_job_done.
    " JOB'un bitmesini bekler """""""""""""""""""""""""""""""""""""""
    " En fazla 20 saat bekliyoruz. Bu kadar uzun sürmeyecektir normalde,
    " ancak teknik bir hatadan ötürü Job sonucunu alamıyorsak,
    " sonsuza kadar asılı kalmasın.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR job_status.

    TRY.
        DO max_wait TIMES.
          ##FM_SUBRC_OK
          CALL FUNCTION 'BDL_READ_JOB_STATUS'
            EXPORTING
              jobname       = zcl_sd_otfat_scre_helper=>job_name
              jobnumber     = me->input-jobcount
            IMPORTING
              jobstatus     = job_status
            EXCEPTIONS
              job_not_found = 1
              OTHERS        = 2.

          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'BDL_READ_JOB_STATUS' ).

          CASE job_status.
            WHEN me->job_status-aborted OR me->job_status-finished.
              RETURN.
            WHEN OTHERS.
              WAIT UP TO 1 SECONDS.
          ENDCASE.
        ENDDO.

        RAISE EXCEPTION TYPE zcx_bc_job_wait_expire
          EXPORTING
            job_name = zcl_sd_otfat_scre_helper=>job_name.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_job_wait
          EXPORTING
            previous = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD get_spool_output.
    " LISTIDENT tespiti """""""""""""""""""""""""""""""""""""""""""""
    " Job bittiğinde, Job adı & numarası ile TBTCP tablosuna gidip
    " LISTIDENT'i alırız. Örnek Job adı: ZSD_OTFAT, numarası 09053800
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT DISTINCT listident FROM tbtcp
           WHERE jobname  = @me->input-jobname AND
                 jobcount = @me->input-jobcount
           INTO TABLE @DATA(listidents).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Çıktının alınması """""""""""""""""""""""""""""""""""""""""""""
    " RSPO_RETURN_ABAP_SPOOLJOB fonksiyonuna LISTIDENT'i vererek,
    " Spool'daki mesajlara erişiriz
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT listidents ASSIGNING FIELD-SYMBOL(<li>).
      DATA(local_result) = VALUE spool_list( ).

      CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
        EXPORTING
          rqident              = <li>
        TABLES
          buffer               = local_result
        EXCEPTIONS
          no_such_job          = 1
          not_abap_list        = 2
          job_contains_no_data = 3
          selection_empty      = 4
          no_permission        = 5
          can_not_access       = 6
          read_error           = 7
          OTHERS               = 8.

      CHECK sy-subrc = 0.
      APPEND LINES OF local_result TO result.
    ENDLOOP.
  ENDMETHOD.


  method get_job_log.
    ##FM_SUBRC_OK
    call function 'BP_JOBLOG_READ'
      EXPORTING
        jobcount              = me->input-jobcount
        jobname               = me->input-jobname
      TABLES
        joblogtbl             = result
      EXCEPTIONS
        cant_read_joblog      = 1
        jobcount_missing      = 2
        joblog_does_not_exist = 3
        joblog_is_empty       = 4
        joblog_name_missing   = 5
        jobname_missing       = 6
        job_does_not_exist    = 7
        others                = 8.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'BP_JOBLOG_READ' ).
  endmethod.
ENDCLASS.