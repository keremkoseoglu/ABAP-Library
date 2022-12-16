CLASS zcl_bc_namesake_job_guard DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO zcl_bc_namesake_job_guard.

    METHODS execute RAISING zcx_bc_job_singularity.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES jobname_range TYPE RANGE OF tbtcp-jobname.

    CONSTANTS: BEGIN OF job_status,
                 active     TYPE btcstatus VALUE 'R',
                 put_active TYPE btcstatus VALUE 'Z',
               END OF job_status.

    CLASS-DATA singleton TYPE REF TO zcl_bc_namesake_job_guard.

    DATA namesake_jobname_rng TYPE jobname_range.

    METHODS constructor.
ENDCLASS.



CLASS zcl_bc_namesake_job_guard IMPLEMENTATION.
  METHOD get_instance.
    IF singleton IS INITIAL.
      singleton = NEW #( ).
    ENDIF.

    result = singleton.
  ENDMETHOD.


  METHOD execute.
    " Benzer isme sahip Job'lar paralel çalışamasın """""""""""""""""
    " Örnek : ZBCT_JOB_SINGN tablosuna ZSD_OGS* yazıyor diyelim.
    "         Biz şu anda ZSD_OGS_002 Job'undayız.
    "         ZSD_OGS_001 diye etkin bir Job varsa, bizimki çalışmamalı.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->namesake_jobname_rng IS NOT INITIAL.

    zcl_bc_job_toolkit=>am_i_in_job(
      IMPORTING in_job   = DATA(in_job)
                jobname  = DATA(jobname) ).

    IF in_job = abap_false.
      RETURN.
    ENDIF.

    IF jobname NOT IN me->namesake_jobname_rng.
      RETURN.
    ENDIF.

    SELECT  SINGLE  FROM tbtco FIELDS jobname
            WHERE   jobname    IN @me->namesake_jobname_rng     AND
                    jobname    <> @jobname                      AND
                    ( status    = @me->job_status-active  OR
                      status    = @me->job_status-put_active )  AND
                    ( sdlstrtdt < @sy-datum OR ( sdlstrtdt = @sy-datum AND
                                                 sdlstrttm < @sy-uzeit ) )
            INTO    @DATA(active_namesake_jobname).

    IF active_namesake_jobname IS INITIAL.
      RETURN.
    ENDIF.

    RAISE EXCEPTION NEW zcx_bc_job_singularity( textid  = zcx_bc_job_singularity=>active_namesake_job
                                                jobname = active_namesake_jobname ).
  ENDMETHOD.


  METHOD constructor.
    SELECT FROM   zbct_job_singn
           FIELDS @ycl_addict_toolkit=>sign-include AS sign   ,
                  ddoption                          AS option ,
                  btcjob                            AS low
           INTO   CORRESPONDING FIELDS OF TABLE @me->namesake_jobname_rng.
  ENDMETHOD.
ENDCLASS.