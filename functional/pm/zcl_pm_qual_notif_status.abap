CLASS zcl_pm_qual_notif_status DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_status_obj.

    CLASS-METHODS get_instance
      IMPORTING objnr         TYPE qmel-objnr
      RETURNING VALUE(result) TYPE REF TO zcl_pm_qual_notif_status
      RAISING   zcx_pm_qual_notif_status_obj.

    CLASS-METHODS get_instance_by_qmnum
      IMPORTING qmnum         TYPE qmel-qmnum
      RETURNING VALUE(result) TYPE REF TO zcl_pm_qual_notif_status
      RAISING   zcx_pm_qual_notif.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF status,
                 completed TYPE j_status VALUE 'I0072',
               END OF status.

    DATA objnr TYPE qmel-objnr.

    METHODS constructor
      IMPORTING objnr TYPE qmel-objnr
      RAISING   zcx_pm_qual_notif_status_obj.
ENDCLASS.


CLASS zcl_pm_qual_notif_status IMPLEMENTATION.
  METHOD get_instance.
    result = NEW #( objnr ).
  ENDMETHOD.

  METHOD get_instance_by_qmnum.
    SELECT SINGLE FROM qmel
           FIELDS objnr
           WHERE qmnum = @qmnum
           INTO @DATA(objnr).

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_pm_qual_notif( textid = zcx_pm_qual_notif=>notif_not_found
                                             qmnum  = qmnum ).
    ENDIF.

    TRY.
        result = get_instance( objnr ).

      CATCH cx_root INTO DATA(status_obj_error).
        RAISE EXCEPTION NEW zcx_pm_qual_notif( textid   = zcx_pm_qual_notif=>cant_build_status_obj
                                               previous = status_obj_error
                                               qmnum    = qmnum ).
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    IF objnr IS INITIAL.
      RAISE EXCEPTION NEW zcx_pm_qual_notif_status_obj(
                              textid = zcx_pm_qual_notif_status_obj=>empty_objnr_cant_build_obj ).
    ENDIF.

    me->objnr = objnr.
  ENDMETHOD.

  METHOD zif_bc_status_obj~is_completed.
    DATA statuses TYPE tt_jstat.

    TRY.
        ##FM_SUBRC_OK
        CALL FUNCTION 'STATUS_READ'
          EXPORTING  objnr            = me->objnr
                     only_active      = abap_true
          TABLES     status           = statuses
          EXCEPTIONS object_not_found = 1
                     OTHERS           = 2.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'STATUS_READ' ).

      CATCH cx_root INTO DATA(status_func_error).
        RAISE EXCEPTION NEW zcx_bc_obj_status( textid   = zcx_bc_obj_status=>cant_determine_if_completed
                                               previous = status_func_error
                                               objnr    = me->objnr ).
    ENDTRY.

    result = xsdbool( line_exists( statuses[ stat = zcl_pm_qual_notif_status=>status-completed ] ) ).
  ENDMETHOD.

  METHOD zif_bc_status_obj~get_objnr.
    result = me->objnr.
  ENDMETHOD.
ENDCLASS.