CLASS zcl_bc_single_job_enforcer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: i_am_in_job    TYPE abap_bool READ-ONLY,
          my_jobname     TYPE tbtcp-jobname READ-ONLY,
          my_jobcount    TYPE tbtcp-jobcount READ-ONLY,
          sinkey         TYPE zbct_job_singa-sinkey READ-ONLY,
          job_simulation TYPE abap_bool READ-ONLY.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING !sinkey         TYPE zbct_job_singa-sinkey
                !job_simulation TYPE abap_bool DEFAULT abap_false
                !assume_no_job  TYPE abap_bool DEFAULT abap_false.

    METHODS job_started RAISING zcx_bc_job_singularity.
    METHODS job_ended.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: preserve_days TYPE i VALUE 60,
               max_random    TYPE i VALUE 99999999.


    CLASS-DATA: random TYPE REF TO cl_abap_random_int.


    DATA: committer     TYPE REF TO zcl_bc_job_singa_commit,
          sje_badi      TYPE REF TO zbc_job_sje,
          assume_no_job TYPE abap_bool.

    METHODS am_i_in_job.
    METHODS delete_completed_jobs RAISING zcx_bc_job_singularity.
    METHODS ensure_no_active_job_exists RAISING zcx_bc_job_singularity.
    METHODS register_current_job RAISING zcx_bc_job_singularity.
    METHODS mark_job_as_ended IMPORTING !key TYPE zbcs_job_singa_key.
ENDCLASS.



CLASS zcl_bc_single_job_enforcer IMPLEMENTATION.


  METHOD job_ended.
    CHECK me->i_am_in_job = abap_true. " Paranoya

    mark_job_as_ended( VALUE #( jobname  = me->my_jobname
                                jobcount = me->my_jobcount ) ).

    TRY.
        me->committer->commit( ).
      CATCH cx_root ##no_handler.
        " En kötü bir sonraki Job'da silinir
    ENDTRY.

    CALL BADI me->sje_badi->job_ended
      EXPORTING
        enforcer = me.
  ENDMETHOD.


  METHOD class_constructor.
    DATA erdat TYPE erdat.
    erdat = sy-datum - zcl_bc_single_job_enforcer=>preserve_days.

    DELETE FROM zbct_job_singa
           WHERE lvorm = @abap_true AND
                 erdat < @erdat.
  ENDMETHOD.


  METHOD am_i_in_job.
    IF me->job_simulation = abap_true.
      IF me->random IS INITIAL.
        me->random = cl_abap_random_int=>create( min = 1
                                                 max = me->max_random ).
      ENDIF.

      me->my_jobname  = cl_reca_guid=>get_new_guid( ).
      me->my_jobcount = me->random->get_next( ).
      me->i_am_in_job = abap_true.

      RETURN.
    ENDIF.

    IF me->assume_no_job = abap_true.
      RETURN.
    ENDIF.

    zcl_bc_job_toolkit=>am_i_in_job(
      IMPORTING in_job   = me->i_am_in_job
                jobcount = me->my_jobcount
                jobname  = me->my_jobname ).
  ENDMETHOD.


  METHOD job_started.
    CHECK me->i_am_in_job = abap_true. " Paranoya

    CALL BADI me->sje_badi->job_started_begin
      EXPORTING
        enforcer = me.

    CALL FUNCTION 'ENQUEUE_EZBC_JOB_SINKEY'
      EXPORTING
        sinkey         = me->sinkey
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      DATA(enqueue_error) = NEW zcx_bc_job_singularity(
                                    textid = zcx_bc_job_singularity=>another_job_starting
                                    sinkey = me->sinkey ).

      CALL BADI me->sje_badi->job_started_error
        EXPORTING
          enforcer = me
          error    = enqueue_error.

      RAISE EXCEPTION enqueue_error.
    ENDIF.

    TRY.
        delete_completed_jobs( ).
        ensure_no_active_job_exists( ).
        zcl_bc_namesake_job_guard=>get_instance( )->execute( ).
        register_current_job( ).
      CLEANUP INTO DATA(registration_error).
        CALL FUNCTION 'DEQUEUE_EZBC_JOB_SINKEY'
          EXPORTING
            sinkey = me->sinkey.

        CALL BADI me->sje_badi->job_started_error
          EXPORTING
            enforcer = me
            error    = registration_error.
    ENDTRY.

    CALL FUNCTION 'DEQUEUE_EZBC_JOB_SINKEY'
      EXPORTING
        sinkey = me->sinkey.

    CALL BADI me->sje_badi->job_started_end
      EXPORTING
        enforcer = me.
  ENDMETHOD.


  METHOD delete_completed_jobs.
    TRY.
        CALL BADI me->sje_badi->del_completed_jobs_begin
          EXPORTING
            enforcer = me.

        SELECT jobname, jobcount FROM zi_bc_single_job_active
               WHERE sinkey = @me->sinkey
               INTO TABLE @DATA(actives).

        IF actives IS INITIAL.
          RETURN.
        ENDIF.

        LOOP AT actives ASSIGNING FIELD-SYMBOL(<active>).
          DATA(job_key) = CORRESPONDING zbcs_job_singa_key( <active> ).

          CALL BADI me->sje_badi->active_job_check_begin
            EXPORTING
              enforcer = me
              job_key  = job_key.

          DATA(job_result) = NEW zcl_bc_job_result( CORRESPONDING #( <active> ) ).

          TRY.
              job_result->is_job_done( IMPORTING done       = DATA(done)
                                                 job_status = DATA(job_status) ).

            CATCH cx_root INTO DATA(job_error).
              CALL BADI me->sje_badi->active_job_check_error
                EXPORTING
                  enforcer = me
                  job_key  = job_key
                  error    = job_error.

              CONTINUE.
          ENDTRY.

          CALL BADI me->sje_badi->active_job_check_end
            EXPORTING
              enforcer   = me
              job_key    = job_key
              done       = done
              job_status = job_status.

          CHECK done = abap_true.

          mark_job_as_ended( CORRESPONDING #( <active> ) ).
        ENDLOOP.

        me->committer->commit( ).

        CALL BADI me->sje_badi->del_completed_jobs_end
          EXPORTING
            enforcer = me.

      CATCH zcx_bc_job_singularity INTO DATA(singu_error).
        CALL BADI me->sje_badi->del_completed_jobs_error
          EXPORTING
            enforcer = me
            error    = singu_error.

        RAISE EXCEPTION singu_error.

      CATCH cx_root INTO DATA(diaper).
        CALL BADI me->sje_badi->del_completed_jobs_error
          EXPORTING
            enforcer = me
            error    = diaper.

        RAISE EXCEPTION TYPE zcx_bc_job_singularity
          EXPORTING
            textid   = zcx_bc_job_singularity=>del_comp_job_error
            previous = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD ensure_no_active_job_exists.
    SELECT SINGLE @abap_true FROM zi_bc_single_job_active
           WHERE sinkey = @me->sinkey AND
                 ( NOT ( jobname  = @me->my_jobname AND
                         jobcount = @me->my_jobcount ) )
           INTO @DATA(another_job_active).

    IF another_job_active = abap_true.
      RAISE EXCEPTION TYPE zcx_bc_job_singularity
        EXPORTING
          textid = zcx_bc_job_singularity=>active_job_exists
          sinkey = me->sinkey.
    ENDIF.
  ENDMETHOD.


  METHOD register_current_job.
    TRY.
        SELECT SINGLE @abap_true FROM zbct_job_singa
               WHERE jobname  = @me->my_jobname AND
                     jobcount = @me->my_jobcount
               INTO @DATA(job_already_registered).

        IF job_already_registered = abap_true.
          RETURN.
        ENDIF.

        DATA(db_entry) =
          VALUE zbct_job_singa(
                  jobname  = me->my_jobname
                  jobcount = me->my_jobcount
                  sinkey   = me->sinkey
                  ernam    = sy-uname
                  erdat    = sy-datum
                  erzet    = sy-uzeit
                  aenam    = sy-uname
                  aedat    = sy-datum
                  aezet    = sy-uzeit
                  simul    = me->job_simulation ).

        INSERT zbct_job_singa FROM @db_entry.

        me->committer->queue_insert( CORRESPONDING #( db_entry ) ).
        me->committer->commit( ).

      CATCH zcx_bc_job_singularity INTO DATA(singu_error).
        RAISE EXCEPTION singu_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_job_singularity
          EXPORTING
            textid   = zcx_bc_job_singularity=>cant_save_new_job
            previous = diaper
            sinkey   = |{ me->my_jobname } { me->my_jobcount }|.
    ENDTRY.
  ENDMETHOD.


  METHOD mark_job_as_ended.
    UPDATE zbct_job_singa
           SET   lvorm    = @abap_true,
                 aenam    = @sy-uname,
                 aedat    = @sy-datum,
                 aezet    = @sy-uzeit
           WHERE jobname  = @key-jobname AND
                 jobcount = @key-jobcount.

    me->committer->queue_delete( key ).
  ENDMETHOD.


  METHOD constructor.
    GET BADI me->sje_badi.

    me->sinkey          = sinkey.
    me->job_simulation  = job_simulation.
    me->assume_no_job   = assume_no_job.
    me->committer       = NEW #( ).

    am_i_in_job( ).

    CALL BADI me->sje_badi->enforcer_constructor
      EXPORTING
        enforcer = me.
  ENDMETHOD.
ENDCLASS.