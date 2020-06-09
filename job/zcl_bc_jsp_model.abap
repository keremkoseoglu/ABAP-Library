CLASS zcl_bc_jsp_model DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES: tt_erdat_rng TYPE RANGE OF erdat,
           tt_item      TYPE STANDARD TABLE OF zbct_jsp_jpitm WITH DEFAULT KEY,
           tt_mile      TYPE STANDARD TABLE OF zbct_jsp_jpmil WITH DEFAULT KEY,
           tt_jphid_rng TYPE RANGE OF zbcd_jsp_jphid,
           tt_jppos_rng TYPE RANGE OF zbcd_jsp_jppos,
           tt_milst_rng TYPE RANGE OF zbcd_jsp_milst.

    CONSTANTS: c_milst_begin TYPE zbcd_jsp_milst VALUE 'BEGIN',
               c_milst_end   TYPE zbcd_jsp_milst VALUE 'END'.

    DATA: go_imp  TYPE REF TO zcl_bc_jsp_imp,
          gt_item TYPE tt_item READ-ONLY,
          gt_mile TYPE tt_mile READ-ONLY,
          gs_head TYPE zbct_jsp_jphdr READ-ONLY.

    CLASS-METHODS delete
      IMPORTING
        !it_jphid_rng TYPE tt_jphid_rng
        !it_erdat_rng TYPE tt_erdat_rng
        !iv_impid     TYPE zbcd_jsp_impid
      RAISING
        zcx_bc_jsp_deletion
        zcx_bc_table_content.

    CLASS-METHODS create_instance
      IMPORTING
        !is_fld       TYPE zbcs_jsp_jphdr_fld
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_jsp_model
      RAISING
        zcx_bc_jsp_job_size
        zcx_bc_nr_next
        zcx_bc_nr_not_available
        zcx_bc_table_content.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_jphid     TYPE zbcd_jsp_jphid
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_jsp_model
      RAISING
        zcx_bc_table_content.

    METHODS create_jobs
      RAISING
        zcx_bc_function_subrc
        zcx_bc_class_method.

    METHODS execute
      IMPORTING
        !iv_milst TYPE zbcd_jsp_milst OPTIONAL
        !iv_jppos TYPE zbcd_jsp_jppos OPTIONAL
      RAISING
        zcx_bc_class_method
        zcx_bc_method_parameter.

    METHODS has_incomplete_item_in_db
      IMPORTING
        !it_jppos_rng TYPE tt_jppos_rng OPTIONAL
      RETURNING
        VALUE(rv_has) TYPE abap_bool.

    METHODS has_incomplete_milestone_in_db
      IMPORTING
        !it_milst_rng TYPE tt_milst_rng OPTIONAL
      RETURNING
        VALUE(rv_has) TYPE abap_bool.

    METHODS is_protected RETURNING VALUE(rv_prot) TYPE abap_bool.

    METHODS save_to_db.

    METHODS split_job_package
      RAISING
        zcx_bc_class_method
        zcx_bc_method_parameter.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_jobchain,
             jppos    TYPE zbcd_jsp_jppos,
             jobname  TYPE tbtcjob-jobname,
             jobcount TYPE tbtcjob-jobcount,
           END OF t_jobchain,

           tt_jobchain TYPE STANDARD TABLE OF t_jobchain WITH DEFAULT KEY,

           BEGIN OF t_jobparal,
             index TYPE i,
             chain TYPE tt_jobchain,
           END OF t_jobparal,

           tt_jobparal TYPE HASHED TABLE OF t_jobparal WITH UNIQUE KEY primary_key COMPONENTS index.

    CONSTANTS: c_clsname_me   TYPE seoclsname VALUE 'ZCL_BC_JSP_MODEL',
               c_method_cj    TYPE seocpdname VALUE 'CREATE_JOBS',
               c_method_exe   TYPE seocpdname VALUE 'EXECUTE',
               c_method_sjp   TYPE seocpdname VALUE 'SPLIT_JOB_PACKAGE',
               c_option_eq    TYPE ddoption   VALUE 'EQ',
               c_option_lt    TYPE ddoption   VALUE 'LT',
               c_sign_i       TYPE ddsign     VALUE 'I',
               c_tabname_head TYPE tabname    VALUE 'ZBCT_JSP_JPHDR',
               c_wait         TYPE i          VALUE 5.

    METHODS create_job_for_items
      RAISING
        zcx_bc_function_subrc.

    METHODS create_job_for_milestone
      IMPORTING
        !iv_milst TYPE zbcd_jsp_milst
      RAISING
        zcx_bc_function_subrc
        zcx_bc_class_method.

    METHODS wait_until_items_complete
      IMPORTING
        !it_jppos_rng TYPE tt_jppos_rng OPTIONAL.

    METHODS wait_until_milestones_complete
      IMPORTING
        !it_milst_rng TYPE tt_milst_rng.

ENDCLASS.



CLASS ZCL_BC_JSP_MODEL IMPLEMENTATION.


  METHOD create_instance.

*   Nesne
    ro_obj = NEW #( ).

*   Başlık
    ro_obj->gs_head = VALUE #( ernam = sy-uname
                               erdat = sy-datum
                               erzet = sy-uzeit
                               aenam = sy-uname
                               aedat = sy-datum
                               aezet = sy-uzeit ).

    MOVE-CORRESPONDING is_fld TO ro_obj->gs_head.
    NEW zcl_bc_nr_facade( iv_nr_range = '01' iv_object = 'ZBCNR_JSP1' )->get_next_number( IMPORTING ev_number = ro_obj->gs_head-jphid ).

*   Milestone
    ro_obj->gt_mile = VALUE #( jphid = ro_obj->gs_head-jphid
                               ernam = ro_obj->gs_head-ernam
                               erdat = ro_obj->gs_head-erdat
                               erzet = ro_obj->gs_head-erzet
                               aenam = ro_obj->gs_head-aenam
                               aedat = ro_obj->gs_head-aedat
                               aezet = ro_obj->gs_head-aezet ( milst = c_milst_begin )
                                                             ( milst = c_milst_end ) ).

*   Uygulama
    ro_obj->go_imp = zcl_bc_jsp_imp=>get_instance( ro_obj->gs_head-impid ).
    ro_obj->go_imp->go_imp->set_model( ro_obj ).

*   Uygulama üzerinden parametreleri valide et

    TRY.

        IF ro_obj->gs_head-jimax GT ro_obj->go_imp->gs_sys-jimax.
          RAISE EXCEPTION TYPE zcx_bc_jsp_job_size
            EXPORTING
              textid = zcx_bc_jsp_job_size=>jimax_exceed_customizing
              jimax  = ro_obj->go_imp->gs_sys-jimax.
        ENDIF.

        IF ro_obj->gs_head-pjmax GT ro_obj->go_imp->gs_sys-pjmax.
          RAISE EXCEPTION TYPE zcx_bc_jsp_job_size
            EXPORTING
              textid = zcx_bc_jsp_job_size=>pjmax_exceed_customizing
              pjmax  = ro_obj->go_imp->gs_sys-pjmax.
        ENDIF.

      CATCH cx_root INTO DATA(lo_cx_root).

        RAISE EXCEPTION TYPE zcx_bc_jsp_job_size
          EXPORTING
            textid   = zcx_bc_jsp_job_size=>param_exceed_customizing
            impid    = ro_obj->go_imp->gs_def-impid
            previous = lo_cx_root
            pjmax    = ro_obj->gs_head-pjmax
            jimax    = ro_obj->gs_head-jimax.

    ENDTRY.

  ENDMETHOD.


  METHOD create_jobs.

    IF gt_item[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_class_method
        EXPORTING
          textid = zcx_bc_class_method=>call_before_operation
          class  = c_clsname_me
          method = c_method_cj.
    ENDIF.

    LOOP AT gt_mile TRANSPORTING NO FIELDS WHERE jobcount IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_class_method
        EXPORTING
          textid = zcx_bc_class_method=>call_after_operation
          class  = c_clsname_me
          method = c_method_cj.
    ENDLOOP.

    save_to_db( ).
    create_job_for_milestone( c_milst_begin ).
    create_job_for_items( ).
    create_job_for_milestone( c_milst_end ).

    LOOP AT gt_mile ASSIGNING FIELD-SYMBOL(<ls_mile>).
      UPDATE zbct_jsp_jpmil SET jobcount = <ls_mile>-jobcount
                            WHERE jphid EQ gs_head-jphid
                              AND milst EQ <ls_mile>-milst.
    ENDLOOP.

    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
      UPDATE zbct_jsp_jpitm SET jobcount = <ls_item>-jobcount
                            WHERE jphid EQ gs_head-jphid
                              AND jppos EQ <ls_item>-jppos.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_job_for_items.

    DATA: lt_jp    TYPE tt_jobparal,

          lv_jpidx TYPE i,

          lr_prev  TYPE REF TO t_jobchain.

*   İşleri bölümlendir

    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<ls_item>).

      ADD 1 TO lv_jpidx.
      IF lv_jpidx GT gs_head-pjmax.
        lv_jpidx = 1.
      ENDIF.

      ASSIGN lt_jp[ KEY primary_key COMPONENTS index = lv_jpidx ] TO FIELD-SYMBOL(<ls_jp>).
      IF sy-subrc NE 0.
        INSERT VALUE #( index = lv_jpidx ) INTO TABLE lt_jp ASSIGNING <ls_jp>.
      ENDIF.

      APPEND VALUE #( jppos   = <ls_item>-jppos
                      jobname = gs_head-jobname ) TO <ls_jp>-chain.

    ENDLOOP.

*   Job'ları yarat

    LOOP AT lt_jp ASSIGNING <ls_jp>.

      CLEAR lr_prev.

      LOOP AT <ls_jp>-chain ASSIGNING FIELD-SYMBOL(<ls_chain>).

        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = <ls_chain>-jobname
            jobclass         = 'B'
          IMPORTING
            jobcount         = <ls_chain>-jobcount
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4 ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_OPEN'  ).
        MODIFY gt_item FROM VALUE #( jobcount = <ls_chain>-jobcount ) TRANSPORTING jobcount WHERE jppos = <ls_chain>-jppos.

        SUBMIT zbcp_jsp_execute WITH p_jphid EQ gs_head-jphid
                                WITH p_jppos EQ <ls_chain>-jppos
                                VIA JOB <ls_chain>-jobname NUMBER <ls_chain>-jobcount
                                AND RETURN.

        IF lr_prev IS NOT INITIAL.

          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount             = <ls_chain>-jobcount
              jobname              = <ls_chain>-jobname
              strtimmed            = abap_false
              pred_jobcount        = lr_prev->jobcount
              pred_jobname         = lr_prev->jobname
              predjob_checkstat    = abap_true
            EXCEPTIONS
              cant_start_immediate = 1
              invalid_startdate    = 2
              jobname_missing      = 3
              job_close_failed     = 4
              job_nosteps          = 5
              job_notex            = 6
              lock_failed          = 7
              invalid_target       = 8
              OTHERS               = 9 ##FM_SUBRC_OK.

        ELSE.

          DATA(lr_chain_first) = REF #( <ls_chain> ).

        ENDIF.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_CLOSE'  ).

        lr_prev = REF #( <ls_chain> ).

      ENDLOOP.

      IF lr_chain_first IS NOT INITIAL. "Paranoya

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lr_chain_first->jobcount
            jobname              = lr_chain_first->jobname
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
            OTHERS               = 9 ##FM_SUBRC_OK.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_job_for_milestone.

    DATA lv_jobcount TYPE btcjobcnt.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = gs_head-jobname
        jobclass         = 'B'
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4 ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_OPEN'  ).
    MODIFY gt_mile FROM VALUE #( jobcount = lv_jobcount ) TRANSPORTING jobcount WHERE milst EQ iv_milst.

    SUBMIT zbcp_jsp_execute WITH p_jphid EQ gs_head-jphid
                            WITH p_milst EQ iv_milst
                            VIA JOB gs_head-jobname NUMBER lv_jobcount
                            AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = gs_head-jobname
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
        OTHERS               = 9 ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'JOB_CLOSE'  ).

  ENDMETHOD.


  METHOD delete.

    DATA lt_jphid_rng TYPE tt_jphid_rng.

    TRY.

        DATA(lo_imp) = zcl_bc_jsp_imp=>get_instance( iv_impid ).

        SELECT @c_option_eq AS option,
               @c_sign_i AS sign,
               jphid AS low
               INTO CORRESPONDING FIELDS OF TABLE @lt_jphid_rng ##TOO_MANY_ITAB_FIELDS
               FROM zbct_jsp_jphdr
               WHERE jphid IN @lt_jphid_rng
                 AND erdat IN @it_erdat_rng
                 AND impid EQ @iv_impid.

        CHECK lt_jphid_rng[] IS NOT INITIAL.

        LOOP AT lt_jphid_rng ASSIGNING FIELD-SYMBOL(<ls_jphid_rng>).
          IF get_instance( <ls_jphid_rng>-low )->is_protected( ) EQ abap_true.
            RAISE EXCEPTION TYPE zcx_bc_jsp_deletion
              EXPORTING
                textid = zcx_bc_jsp_deletion=>protected
                jphid  = <ls_jphid_rng>-low.
          ENDIF.
        ENDLOOP.

        DELETE FROM: zbct_jsp_jphdr WHERE jphid IN lt_jphid_rng,
                     zbct_jsp_jpitm WHERE jphid IN lt_jphid_rng,
                     zbct_jsp_jpmil WHERE jphid IN lt_jphid_rng.

        lo_imp->go_imp->delete( lt_jphid_rng ).

      CLEANUP.
        ROLLBACK WORK.
    ENDTRY.

  ENDMETHOD.


  METHOD execute.

*   __________
*   Kontroller

    IF ( iv_milst IS INITIAL AND iv_jppos IS INITIAL ) OR ( iv_milst IS NOT INITIAL AND iv_jppos IS NOT INITIAL ).
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_error
          class_name  = c_clsname_me
          method_name = c_method_exe.
    ENDIF.

*   __________
*   Milestone için çalıştırma

    IF iv_milst IS NOT INITIAL.

      MODIFY gt_mile FROM VALUE #( aenam = sy-uname
                                   aedat = sy-datum
                                   aezet = sy-uzeit
                                   mstar = abap_true
                                   msnam = sy-uname
                                   msdat = sy-datum
                                   mszet = sy-uzeit )
                     TRANSPORTING aenam aedat aezet mstar msnam msdat mszet
                     WHERE milst EQ iv_milst.

      UPDATE zbct_jsp_jpmil SET aenam = sy-uname
                                aedat = sy-datum
                                aezet = sy-uzeit
                                mstar = abap_true
                                msnam = sy-uname
                                msdat = sy-datum
                                mszet = sy-uzeit
                            WHERE jphid EQ gs_head-jphid
                              AND milst EQ iv_milst.

      COMMIT WORK AND WAIT.

      LOG-POINT
        ID zbccg_jsp
        SUBKEY |{ gs_head-jphid }_{ iv_milst }_exe01|
        FIELDS iv_milst.

      CASE iv_milst.
        WHEN c_milst_begin.
          go_imp->go_imp->execute_milestone( iv_milst ).

        WHEN c_milst_end.
          wait_until_items_complete( ).
          go_imp->go_imp->execute_milestone( iv_milst ).

      ENDCASE.

      LOG-POINT
        ID zbccg_jsp
        SUBKEY |{ gs_head-jphid }_{ iv_milst }_exe02|
        FIELDS iv_milst.

      MODIFY gt_mile FROM VALUE #( aenam = sy-uname
                                   aedat = sy-datum
                                   aezet = sy-uzeit
                                   mfini = abap_true
                                   mfnam = sy-uname
                                   mfdat = sy-datum
                                   mfzet = sy-uzeit )
                     TRANSPORTING aenam aedat aezet mfini mfnam mfdat mfzet
                     WHERE milst EQ iv_milst.

      UPDATE zbct_jsp_jpmil SET aenam = sy-uname
                                aedat = sy-datum
                                aezet = sy-uzeit
                                mfini = abap_true
                                mfnam = sy-uname
                                mfdat = sy-datum
                                mfzet = sy-uzeit
                            WHERE jphid EQ gs_head-jphid
                              AND milst EQ iv_milst.

      COMMIT WORK AND WAIT.

    ENDIF.

*   __________
*   Item için çalıştırma

    IF iv_jppos IS NOT INITIAL.

      MODIFY gt_item FROM VALUE #( aenam = sy-uname
                                   aedat = sy-datum
                                   aezet = sy-uzeit
                                   mstar = abap_true
                                   msnam = sy-uname
                                   msdat = sy-datum
                                   mszet = sy-uzeit )
                     TRANSPORTING aenam aedat aezet mstar msnam msdat mszet
                     WHERE jppos EQ iv_jppos.

      UPDATE zbct_jsp_jpitm SET aenam = sy-uname
                                aedat = sy-datum
                                aezet = sy-uzeit
                                mstar = abap_true
                                msnam = sy-uname
                                msdat = sy-datum
                                mszet = sy-uzeit
                            WHERE jphid EQ gs_head-jphid
                              AND jppos EQ iv_jppos.

      COMMIT WORK AND WAIT.

      wait_until_milestones_complete( it_milst_rng = VALUE #( ( option = c_option_eq sign = c_sign_i low = c_milst_begin ) ) ).
      wait_until_items_complete( it_jppos_rng = VALUE #( ( option = c_option_lt sign = c_sign_i low = iv_jppos ) ) ).
      go_imp->go_imp->execute_item( iv_jppos ).

      MODIFY gt_item FROM VALUE #( aenam = sy-uname
                                   aedat = sy-datum
                                   aezet = sy-uzeit
                                   mfini = abap_true
                                   mfnam = sy-uname
                                   mfdat = sy-datum
                                   mfzet = sy-uzeit )
                     TRANSPORTING aenam aedat aezet mfini mfnam mfdat mfzet
                     WHERE jppos EQ iv_jppos.

      UPDATE zbct_jsp_jpitm SET aenam = sy-uname
                                aedat = sy-datum
                                aezet = sy-uzeit
                                mfini = abap_true
                                mfnam = sy-uname
                                mfdat = sy-datum
                                mfzet = sy-uzeit
                            WHERE jphid EQ gs_head-jphid
                              AND jppos EQ iv_jppos.

      COMMIT WORK AND WAIT.

    ENDIF.



  ENDMETHOD.


  METHOD get_instance.

    ro_obj = NEW #( ).

    SELECT SINGLE * INTO ro_obj->gs_head FROM zbct_jsp_jphdr WHERE jphid EQ iv_jphid.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_jphid )
          tabname  = c_tabname_head.
    ENDIF.

    SELECT * INTO TABLE: ro_obj->gt_item FROM zbct_jsp_jpitm WHERE jphid EQ ro_obj->gs_head-jphid,
                         ro_obj->gt_mile FROM zbct_jsp_jpmil WHERE jphid EQ ro_obj->gs_head-jphid.

    ro_obj->go_imp = zcl_bc_jsp_imp=>get_instance( ro_obj->gs_head-impid ).
    ro_obj->go_imp->go_imp->set_model( ro_obj ).

  ENDMETHOD.


  METHOD has_incomplete_item_in_db.

    SELECT SINGLE mandt INTO @DATA(lv_mandt)
           FROM zbct_jsp_jpitm
           WHERE jphid EQ @gs_head-jphid
             AND jppos IN @it_jppos_rng
             AND ( mfini EQ @abap_false OR mfini IS NULL ) ##WARN_OK.

    rv_has = boolc( sy-subrc EQ 0 ).

  ENDMETHOD.


  METHOD has_incomplete_milestone_in_db.

    SELECT SINGLE mandt INTO @DATA(lv_mandt)
           FROM zbct_jsp_jpmil
           WHERE jphid EQ @gs_head-jphid
             AND milst IN @it_milst_rng
             AND ( mfini EQ @abap_false OR mfini IS NULL ) ##WARN_OK.

    rv_has = boolc( sy-subrc EQ 0 ).

  ENDMETHOD.


  METHOD is_protected.
    rv_prot = gs_head-prot.
  ENDMETHOD.


  METHOD save_to_db.

    DELETE FROM: zbct_jsp_jphdr WHERE jphid EQ gs_head-jphid,
                 zbct_jsp_jpitm WHERE jphid EQ gs_head-jphid,
                 zbct_jsp_jpmil WHERE jphid EQ gs_head-jphid.

    INSERT: zbct_jsp_jphdr FROM gs_head,
            zbct_jsp_jpitm FROM TABLE gt_item,
            zbct_jsp_jpmil FROM TABLE gt_mile.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD split_job_package.

    DATA lv_curr_pos TYPE zbcd_jsp_jppos.

    IF gt_item[] IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_class_method
        EXPORTING
          textid = zcx_bc_class_method=>call_after_operation
          class  = c_clsname_me
          method = c_method_sjp.
    ENDIF.

    go_imp->go_imp->read_and_save_split( IMPORTING ev_last_pos = DATA(lv_last_pos) ).

    IF lv_last_pos IS INITIAL.

      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_missing
          class_name  = c_clsname_me
          method_name = c_method_sjp
          param_name  = 'EV_LAST_POS'.

    ENDIF.

    WHILE lv_curr_pos LT lv_last_pos.
      ADD 1 TO lv_curr_pos.

      APPEND VALUE #( jphid = gs_head-jphid
                      jppos = lv_curr_pos
                      ernam = sy-uname
                      erdat = sy-datum
                      erzet = sy-uzeit
                      aenam = sy-uname
                      aedat = sy-datum
                      aezet = sy-uzeit ) TO gt_item.
    ENDWHILE.

  ENDMETHOD.


  METHOD wait_until_items_complete.

    DO.
      IF has_incomplete_item_in_db( it_jppos_rng ) EQ abap_false.
        RETURN.
      ENDIF.
      WAIT UP TO c_wait SECONDS.
    ENDDO.

  ENDMETHOD.


  METHOD wait_until_milestones_complete.

    DO.
      IF has_incomplete_milestone_in_db( it_milst_rng ) EQ abap_false.
        RETURN.
      ENDIF.
      WAIT UP TO c_wait SECONDS.
    ENDDO.

  ENDMETHOD.
ENDCLASS.