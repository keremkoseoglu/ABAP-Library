CLASS zcl_bc_bdc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      c_dismode_all   TYPE ctu_mode VALUE 'A',
      c_dismode_error TYPE ctu_mode VALUE 'E',
      c_dismode_none  TYPE ctu_mode VALUE 'N'.

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

    DATA gt_bdcdata TYPE bdcdata_tab.

ENDCLASS.


CLASS zcl_bc_bdc IMPLEMENTATION.

  METHOD add_fld.
    APPEND VALUE #( fnam = iv_nam fval = iv_val ) TO gt_bdcdata.
  ENDMETHOD.

  METHOD add_scr.
    APPEND VALUE #( program = iv_prg dynpro = iv_dyn dynbegin = abap_true ) TO gt_bdcdata.
  ENDMETHOD.

  METHOD clear.
    CLEAR gt_bdcdata[].
  ENDMETHOD.

  METHOD close_group.
    CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
        not_open    = 1
        queue_error = 2
        OTHERS      = 3.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BDC_CLOSE_GROUP' ).

    clear(  ).

  ENDMETHOD.

  METHOD insert_tcode.

    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode            = iv_tcode
      TABLES
        dynprotab        = gt_bdcdata
      EXCEPTIONS
        internal_error   = 1
        not_open         = 2
        queue_error      = 3
        tcode_invalid    = 4
        printing_invalid = 5
        posting_invalid  = 6
        OTHERS           = 7.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BDC_INSERT' ).

    clear( ).

  ENDMETHOD.

  METHOD open_group.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        group               = iv_group
        keep                = abap_true
        user                = sy-uname
      EXCEPTIONS
        client_invalid      = 1
        destination_invalid = 2
        group_invalid       = 3
        group_is_locked     = 4
        holddate_invalid    = 5
        internal_error      = 6
        queue_error         = 7
        running             = 8
        system_lock_error   = 9
        user_invalid        = 10
        OTHERS              = 11.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BDC_OPEN_GROUP' ).

  ENDMETHOD.

  METHOD submit.

    CALL TRANSACTION iv_tcode
         USING gt_bdcdata
         MESSAGES INTO et_msg
         OPTIONS FROM is_option.

  ENDMETHOD.

ENDCLASS.