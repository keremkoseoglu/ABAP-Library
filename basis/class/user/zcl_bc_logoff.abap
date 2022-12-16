CLASS zcl_bc_logoff DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS logoff_user
      IMPORTING !user         TYPE syuname
                !client       TYPE symandt    DEFAULT sy-mandt
                !logoff_class TYPE seocpdname DEFAULT zcl_bc_logoff_user_sm04=>class-me
      RAISING   ycx_addict_class_method.

    CLASS-METHODS logoff_user_in_job
      IMPORTING !user   TYPE syuname
                !client TYPE symandt DEFAULT sy-mandt
      RAISING   ycx_addict_function_subrc.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'ZCL_BC_LOGOFF',
               END OF class.

    CONSTANTS: BEGIN OF method,
                 logoff_user TYPE seocpdname VALUE 'LOGOFF_USER',
               END OF method.

    CONSTANTS: BEGIN OF job,
                 class TYPE btcjobclas VALUE 'A',
                 name  TYPE btcjob VALUE 'ZLOGOFF',
                 user  TYPE syuname VALUE 'BATCHUSER',
               END OF job.
ENDCLASS.



CLASS zcl_bc_logoff IMPLEMENTATION.
  METHOD logoff_user.
    DATA obj TYPE REF TO object.

    TRY.
        CREATE OBJECT obj TYPE (logoff_class).
        DATA(logoff_obj) = CAST zif_bc_logoff_user( obj ).

        logoff_obj->logoff_user( user   = user
                                 client = client ).

      CATCH ycx_addict_class_method INTO DATA(method_error).
        RAISE EXCEPTION method_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW ycx_addict_class_method( textid   = ycx_addict_class_method=>unexpected_error
                                                     previous = diaper
                                                     class    = zcl_bc_logoff=>class-me
                                                     method   = zcl_bc_logoff=>method-logoff_user ).
    ENDTRY.
  ENDMETHOD.


  METHOD logoff_user_in_job.
    DATA job_number TYPE btcjobcnt.

    ##FM_SUBRC_OK
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = zcl_bc_logoff=>job-name
        jobclass         = zcl_bc_logoff=>job-class
      IMPORTING
        jobcount         = job_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_OPEN' ).

    SUBMIT zbcp_logoff                                   "#EC CI_SUBMIT
           USER zcl_bc_logoff=>job-user
           WITH p_mandt = client
           WITH p_uname = user
           VIA JOB zcl_bc_logoff=>job-name NUMBER job_number
           AND RETURN.

    ##FM_SUBRC_OK
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = job_number
        jobname              = zcl_bc_logoff=>job-name
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