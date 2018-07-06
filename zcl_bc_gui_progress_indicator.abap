CLASS zcl_bc_gui_progress_indicator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING !iv_max_step TYPE i,
      next_step IMPORTING !iv_text TYPE clike OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      gv_curr_step TYPE i,
      gv_max_step  TYPE i.

ENDCLASS.



CLASS zcl_bc_gui_progress_indicator IMPLEMENTATION.

  METHOD constructor.
    gv_max_step = iv_max_step.
    IF gv_max_step IS INITIAL.
      gv_max_step = 1.
    ENDIF.
  ENDMETHOD.

  METHOD next_step.

    TRY.

        ADD 1 TO gv_curr_step.
        IF gv_curr_step GT gv_max_step.
          gv_curr_step = gv_max_step.
        ENDIF.

        DATA(lv_perc) = ( gv_curr_step * 100 ) / gv_max_step.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lv_perc
            text       = iv_text.

      CATCH cx_root ##no_handler .
    ENDTRY.

  ENDMETHOD.

ENDCLASS.