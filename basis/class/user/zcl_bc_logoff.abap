CLASS zcl_bc_logoff DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS logoff_user
      IMPORTING !user   TYPE syuname
                !client TYPE symandt DEFAULT sy-mandt
      RAISING   ycx_addict_function_subrc.

    CLASS-METHODS logoff_user_in_job
      IMPORTING !user   TYPE syuname
                !client TYPE symandt DEFAULT sy-mandt
      RAISING   ycx_addict_function_subrc.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: job_class TYPE btcjobclas VALUE 'A',
               job_name  TYPE btcjob VALUE 'ZLOGOFF',
               job_user  TYPE syuname VALUE 'BATCHUSER'.
ENDCLASS.



CLASS zcl_bc_logoff IMPLEMENTATION.
  METHOD logoff_user.
    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _synchron = abap_true.

    ##FM_SUBRC_OK
    CALL FUNCTION 'TH_DELETE_USER'
      EXPORTING
        user            = sy-uname
        client          = sy-mandt
      EXCEPTIONS
        authority_error = 1
        OTHERS          = 2.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TH_DELETE_USER' ).
  ENDMETHOD.


  METHOD logoff_user_in_job.
    DATA job_number TYPE btcjobcnt.

    ##FM_SUBRC_OK
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = zcl_sd_otfat_scre_helper=>job_name
        jobclass         = zcl_bc_logoff=>job_class
      IMPORTING
        jobcount         = job_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_OPEN' ).

    SUBMIT zbcp_logoff                                   "#EC CI_SUBMIT
           USER zcl_bc_logoff=>job_user
           WITH p_mandt = client
           WITH p_uname = user
           VIA JOB zcl_bc_logoff=>job_name NUMBER job_number
           AND RETURN.

    ##FM_SUBRC_OK
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = job_number
        jobname              = zcl_bc_logoff=>job_name
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        OTHERS               = 9.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_CLOSE' ).
  ENDMETHOD.
ENDCLASS.