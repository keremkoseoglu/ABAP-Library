CLASS zcl_bc_print_binary_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_bc_print_binary.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_obj) TYPE REF TO zif_bc_print_binary.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_clsname_me  TYPE seoclsname      VALUE 'ZCL_BC_PRINT_BINARY_PDF',
               c_doctype_pdf TYPE tsp01-rqdoctype VALUE 'PDF',
               c_name        type TSP01-RQ0NAME   value 'BINARY',
               c_suffix_bin  TYPE tsp01-rq1name   VALUE 'BIN'.

    CLASS-DATA: go_singleton    TYPE REF TO zcl_bc_print_binary_pdf,
                go_singleton_if type ref to zif_bc_print_binary.

ENDCLASS.



CLASS ZCL_BC_PRINT_BINARY_PDF IMPLEMENTATION.


  METHOD get_instance.

    IF go_singleton_if IS INITIAL.
      go_singleton = NEW #( ).
      go_singleton_if ?= go_singleton.
    ENDIF.

    ro_obj = go_singleton_if.

  ENDMETHOD.


  METHOD zif_bc_print_binary~print_bin.

    DATA: lv_handle  TYPE sy-tabix,
          lv_spoolid TYPE tsp01-rqident,
          lv_suffix2 type tsp01-rq2name.

    TRY.

        lv_Suffix2 = Sy-uname.

        CALL FUNCTION 'RSPO_SR_OPEN'
          EXPORTING
            dest             = is_param-dest    " Spool: Output Device
*           ldest            =     " Spool: Long device names
*           layout           =
            name             = c_name
            suffix1          = c_suffix_bin
            suffix2          = lv_suffix2
*           copies           =
*           prio             =
            immediate_print  = is_param-immediate_print
            auto_delete      = is_param-auto_delete
*           titleline        =
*           receiver         =
*           division         =
*           authority        =
*           posname          =
*           acttime          =
*           lifetime         = '0'
*           append           =
*           coverpage        =
*           codepage         =
            doctype          = c_doctype_pdf
*           archmode         =     " Archive type (internal, not on screen)
*           archparams       =     " ImageLink structure
*           teleland         =     " Country Indicator
*           telenum          =     " Telecommunications partner
*           telenume         =     " Telecommunications partner
*           sponumiv         = SPACE    " Number range number
*           usespoolid       =
*           printoptions     =
*           printticket      = SPACE
          IMPORTING
            handle           = lv_handle
            spoolid          = lv_spoolid
*          TABLES
*           attributes       =     " Spool: Entry for Object Description
          EXCEPTIONS
            device_missing   = 1
            name_twice       = 2
            no_such_device   = 3
            operation_failed = 4
            OTHERS           = 5
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'RSPO_SR_OPEN' ).

        CALL FUNCTION 'RSPO_SR_TABLE_WRITE_BINARY'
          EXPORTING
            handle           = lv_handle
*           codepage         =
            total            = iv_Filesize
          TABLES
            lines            = it_solix
          EXCEPTIONS
            handle_not_valid = 1
            operation_failed = 2
            OTHERS           = 3
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'RSPO_SR_TABLE_WRITE_BINARY' ).

        CALL FUNCTION 'RSPO_SR_CLOSE'
          EXPORTING
            handle           = lv_handle
*           pages            =
            final            = abap_true
          EXCEPTIONS
            handle_not_valid = 1
            operation_failed = 2
            OTHERS           = 3
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'RSPO_SR_CLOS' ).

      CATCH cx_root INTO DATA(lo_cx_root).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_cx_root
            class    = c_clsname_me
            method   = zif_bc_print_binary=>c_meth_print_bin.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.