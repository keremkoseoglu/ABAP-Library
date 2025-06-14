CLASS zcl_sd_posnr_counter DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING initial_posnr TYPE vbap-posnr DEFAULT '000010'
                span          TYPE vbap-posnr DEFAULT '000010'
      RAISING   ycx_addict_object.

    METHODS reset.

    METHODS get_current RETURNING VALUE(result) TYPE vbap-posnr.

    METHODS get_next    RETURNING VALUE(result) TYPE vbap-posnr.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF method_name,
                 constructor TYPE seocpdname VALUE 'CONSTRUCTOR',
               END OF method_name.

    CONSTANTS: BEGIN OF param_name,
                 initial_posnr TYPE seocpdname VALUE 'INITIAL_POSNR',
                 span          TYPE seocpdname VALUE 'SPAN',
               END OF param_name.

    DATA: initial_posnr TYPE vbap-posnr,
          span          TYPE vbap-posnr,
          current_posnr TYPE vbap-posnr,
          is_reset      TYPE abap_bool.
ENDCLASS.


CLASS zcl_sd_posnr_counter IMPLEMENTATION.
  METHOD constructor.
    TRY.
        IF initial_posnr IS INITIAL.
          RAISE EXCEPTION NEW ycx_addict_method_parameter(
                                  textid      = ycx_addict_method_parameter=>param_value_initial
                                  class_name  = ycl_addict_class=>get_class_name( me )
                                  method_name = method_name-constructor
                                  param_name  = param_name-initial_posnr ).
        ENDIF.

        IF span IS INITIAL.
          RAISE EXCEPTION NEW ycx_addict_method_parameter(
                                  textid      = ycx_addict_method_parameter=>param_value_initial
                                  class_name  = ycl_addict_class=>get_class_name( me )
                                  method_name = method_name-constructor
                                  param_name  = param_name-span ).
        ENDIF.

      CATCH cx_root INTO DATA(param_validation_error).
        RAISE EXCEPTION NEW ycx_addict_object( textid   = ycx_addict_object=>cant_create_instance
                                               previous = param_validation_error
                                               clsname  = ycl_addict_class=>get_class_name( me ) ).
    ENDTRY.

    me->initial_posnr = initial_posnr.
    me->span          = span.

    reset( ).
  ENDMETHOD.

  METHOD get_next.
    CASE me->is_reset.
      WHEN abap_true.
        me->current_posnr = me->initial_posnr.
        me->is_reset      = abap_false.
      WHEN abap_false.
        me->current_posnr += me->span.
    ENDCASE.

    result = me->current_posnr.
  ENDMETHOD.

  METHOD reset.
    CLEAR me->current_posnr.
    me->is_reset = abap_true.
  ENDMETHOD.

  METHOD get_current.
    result = me->current_posnr.
  ENDMETHOD.
ENDCLASS.