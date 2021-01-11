CLASS zcl_bc_bdc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      c_dismode_all   TYPE ctu_mode VALUE ycl_addict_bdc=>dismode-all,
      c_dismode_error TYPE ctu_mode VALUE ycl_addict_bdc=>dismode-error,
      c_dismode_none  TYPE ctu_mode VALUE ycl_addict_bdc=>dismode-none.

    METHODS constructor.

    METHODS add_fld
      IMPORTING
        !iv_nam TYPE fnam_____4
        !iv_val TYPE bdc_fval.

    METHODS add_scr
      IMPORTING
        !iv_prg TYPE bdc_prog
        !iv_dyn TYPE bdc_dynr.

    METHODS clear.

    METHODS close_group
      RAISING
        zcx_bc_function_subrc.

    METHODS insert_tcode
      IMPORTING
        !iv_tcode TYPE tcode
      RAISING
        zcx_bc_function_subrc.

    METHODS open_group
      IMPORTING
        !iv_group TYPE apqi-groupid
      RAISING
        zcx_bc_function_subrc.

    METHODS submit
      IMPORTING
        !iv_tcode  TYPE sytcode
        !is_option TYPE ctu_params
      EXPORTING
        !et_msg    TYPE tab_bdcmsgcoll.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA core TYPE REF TO ycl_addict_bdc.

ENDCLASS.



CLASS zcl_bc_bdc IMPLEMENTATION.
  METHOD constructor.
    me->core = NEW #( ).
  ENDMETHOD.


  METHOD add_fld.
    me->core->add_fld(
        nam = iv_nam
        val = iv_val ).
  ENDMETHOD.


  METHOD add_scr.
    me->core->add_scr(
        prg = iv_prg
        dyn = iv_dyn ).
  ENDMETHOD.


  METHOD clear.
    me->core->clear( ).
  ENDMETHOD.


  METHOD close_group.
    TRY.
        me->core->close_group( ).
      CATCH ycx_addict_function_subrc INTO DATA(error).
        zcx_bc_function_subrc=>raise_from_addict( error ).
    ENDTRY.
  ENDMETHOD.


  METHOD insert_tcode.
    TRY.
        me->core->insert_tcode( iv_tcode ).
      CATCH ycx_addict_function_subrc INTO DATA(error).
        zcx_bc_function_subrc=>raise_from_addict( error ).
    ENDTRY.
  ENDMETHOD.


  METHOD open_group.
    TRY.
        me->core->open_group( iv_group ).
      CATCH ycx_addict_function_subrc INTO DATA(error).
        zcx_bc_function_subrc=>raise_from_addict( error ).
    ENDTRY.
  ENDMETHOD.


  METHOD submit.
    TRY.
        me->core->submit(
          EXPORTING tcode  = iv_tcode
                    option = is_option
          IMPORTING msg    = et_msg ).

      CATCH ycx_addict_bdc ##no_handler.
        " validate_msg yollamadığımız için, bu hata asla oluşmayacak
    ENDTRY.
  ENDMETHOD.
ENDCLASS.