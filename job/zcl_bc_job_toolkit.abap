CLASS zcl_bc_job_toolkit DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS am_i_in_job
      EXPORTING !in_job   TYPE abap_bool
                !jobcount TYPE btcjobcnt
                !jobname  TYPE btcjob.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_job_toolkit IMPLEMENTATION.
  METHOD am_i_in_job.
    CLEAR: in_job, jobcount, jobname.

    IF sy-batch = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobcount        = jobcount
        jobname         = jobname
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.

    in_job = xsdbool( sy-subrc = 0 AND
                      jobname IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.