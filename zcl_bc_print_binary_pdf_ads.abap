CLASS zcl_bc_print_binary_pdf_ads DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_bc_print_binary.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_obj) TYPE REF TO zif_bc_print_binary.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_clsname_me  TYPE seoclsname      VALUE 'ZCL_BC_PRINT_BINARY_PDF_ADS',
               c_doctype_pdf TYPE tsp01-rqdoctype VALUE 'PDF',
               c_suffix_dmy  TYPE tsp01-rq2name   VALUE 'DMY',
               c_suffix_pdf  TYPE tsp01-rq1name   VALUE 'PDF'.

    CLASS-DATA: go_singleton    TYPE REF TO zcl_bc_print_binary_pdf_ads,
                go_singleton_if TYPE REF TO zif_bc_print_binary.

ENDCLASS.



CLASS ZCL_BC_PRINT_BINARY_PDF_ADS IMPLEMENTATION.


  METHOD get_instance.

    IF go_singleton_if IS INITIAL.
      go_singleton = NEW #( ).
      go_singleton_if ?= go_singleton.
    ENDIF.

    ro_obj = go_singleton_if.

  ENDMETHOD.


  METHOD zif_bc_print_binary~print_bin.

    DATA lv_buffer TYPE xstring.

    TRY.

        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = iv_filesize
          IMPORTING
            buffer       = lv_buffer
          TABLES
            binary_tab   = it_solix
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2
          ##FM_SUBRC_OK .

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SCMS_BINARY_TO_XSTRING' ).

        CALL FUNCTION 'ADS_CREATE_PDF_SPOOLJOB'
          EXPORTING
            dest              = is_param-dest
            pages             = 2 "?
            pdf_data          = lv_buffer
            name              = 'BINARY'
            suffix1           = 'BIN'
            suffix2           = c_suffix_dmy
*           copies            =
*           prio              =
            immediate_print   = is_param-immediate_print
            auto_delete       = is_param-auto_delete
*           titleline         =
*           receiver          =
*           division          =
*           authority         =
*           lifetime          = '0'
*          IMPORTING
*           spoolid           =
          exceptions
            no_data           = 1
            not_pdf           = 2
            wrong_devtype     = 3
            operation_failed  = 4
            cannot_write_file = 5
            device_missing    = 6
            no_such_device    = 7
            others            = 8
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'ADS_CREATE_PDF_SPOOLJOB' ).

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