CLASS zcl_bc_nr_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    data: gv_nr_range    TYPE inri-nrrangenr read-only,
          gv_object      TYPE inri-object read-only.

    METHODS actualize_reservation
      RAISING
        zcx_bc_method_parameter
        zcx_bc_nr_next .

    METHODS cancel_reservation .

    METHODS constructor
      IMPORTING
        !iv_nr_range TYPE inri-nrrangenr
        !iv_object   TYPE inri-object .

    METHODS get_next_number
      EXPORTING
        !ev_number TYPE data
      RAISING
        zcx_bc_nr_next
        zcx_bc_nr_not_available.

    METHODS reserve_number
      EXPORTING
        !ev_number TYPE data
      RAISING
        zcx_bc_nr_not_available
        zcx_bc_singleton.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_do_max TYPE int4 VALUE 30.

    DATA gv_reserved_no TYPE nrlevel.

    METHODS call_number_get_next
      EXPORTING
        VALUE(ev_number) TYPE data
      RAISING
        zcx_bc_nr_next .

    METHODS enqueue_nr
      RAISING
        zcx_bc_nr_not_available .

    METHODS dequeue_nr .

ENDCLASS.



CLASS zcl_bc_nr_facade IMPLEMENTATION.

  METHOD actualize_reservation.

    DATA lv_number TYPE nrlevel.

*   Ensure that a reservation was carried out

    IF gv_reserved_no IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_method_parameter
        EXPORTING
          textid      = zcx_bc_method_parameter=>param_error
          class_name  = 'ZCL_BC_NR_FACADE'
          method_name = 'ACTUALIZE_RESERVATION'.
    ENDIF.

*   Actualize reserved number

    call_number_get_next( IMPORTING ev_number = lv_number ).

    IF lv_number NE gv_reserved_no.
      RAISE EXCEPTION TYPE zcx_bc_nr_next
        EXPORTING
          textid   = zcx_bc_nr_next=>gen_ne_next
          nr_range = gv_nr_range
          object   = gv_object.
    ENDIF.

*   Cleanup

    CLEAR gv_reserved_no.
    dequeue_nr( ).

  ENDMETHOD.

  METHOD call_number_get_next.

    CLEAR ev_number.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = gv_nr_range
        object                  = gv_object
        ignore_buffer           = abap_true
      IMPORTING
        number                  = ev_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    CHECK sy-subrc NE 0.

    RAISE EXCEPTION TYPE zcx_bc_nr_next
      EXPORTING
        textid   = zcx_bc_nr_next=>cant_get_next
        previous = zcx_bc_symsg=>get_instance( )
        nr_range = gv_nr_range
        object   = gv_object.

  ENDMETHOD.

  METHOD cancel_reservation.
    CLEAR gv_reserved_no.
    dequeue_nr( ).
  ENDMETHOD.

  METHOD constructor.
    gv_nr_range = iv_nr_range.
    gv_object = iv_object.
  ENDMETHOD.

  METHOD dequeue_nr.

    CALL FUNCTION 'DEQUEUE_EZBC_INRI'
      EXPORTING
        object    = gv_object
        nrrangenr = gv_nr_range.

  ENDMETHOD.

  METHOD enqueue_nr.

    DATA: lv_do_cnt   TYPE i.

    DO.

*     Enqueue + return if successful

      CALL FUNCTION 'ENQUEUE_EZBC_INRI'
        EXPORTING
          object         = gv_object
          nrrangenr      = gv_nr_range
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.

*     Retry in 1 second

      ADD 1 TO lv_do_cnt.

      IF lv_do_cnt GT c_do_max.

        RAISE EXCEPTION TYPE zcx_bc_nr_not_available
          EXPORTING
            textid   = zcx_bc_nr_not_available=>locked
            previous = zcx_bc_symsg=>get_instance( )
            nr_range = gv_nr_range
            object   = gv_object.

      ENDIF.

      WAIT UP TO 1 SECONDS.

    ENDDO.

  ENDMETHOD.

  METHOD get_next_number.
    CLEAR ev_number.
    enqueue_nr( ).
    call_number_get_next( IMPORTING ev_number = ev_number ).
    dequeue_nr( ).
  ENDMETHOD.

  METHOD reserve_number.

    DATA lv_nrlevel TYPE nrlevel.

*   Initialize

    CLEAR ev_number.

*   Aynı anda sadece bir numara rezerve edilebilir

    IF gv_reserved_no IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_singleton
        EXPORTING
          textid     = zcx_bc_singleton=>singleton
          error_text = CONV string( text-024 ).
    ENDIF.

*   Sıradaki sayırı rezerve et + döndür

    enqueue_nr( ).

    SELECT SINGLE nrlevel INTO lv_nrlevel
      FROM nriv
      WHERE object    EQ gv_object
        AND nrrangenr EQ gv_nr_range.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_nr_not_available
        EXPORTING
          textid   = zcx_bc_nr_not_available=>undefined
          nr_range = gv_nr_range
          object   = gv_object.
    ENDIF.

    gv_reserved_no = lv_nrlevel + 1.
    MOVE gv_reserved_no TO ev_number.

  ENDMETHOD.

ENDCLASS.