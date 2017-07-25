CLASS zcl_bc_spool_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS conv_spool_to_pdf
      IMPORTING
        !iv_spoolid TYPE rspoid
        !iv_partnum TYPE adsnum DEFAULT 1
      EXPORTING
        !et_fp      TYPE fpcontent
        !et_solix   TYPE solix_tab
      RAISING
        zcx_bc_spool_to_pdf .

    CLASS-METHODS print_option_get_for_pdf
      EXPORTING
        VALUE(es_control_param)  TYPE ssfctrlop
        VALUE(es_composer_param) TYPE ssfcompop .

    CLASS-METHODS ssf_name
      IMPORTING
        !im_formname    TYPE tdsfname
      RETURNING
        VALUE(rv_fname) TYPE rs38l_fnam .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_spool_toolkit IMPLEMENTATION.

  METHOD conv_spool_to_pdf.

    CALL FUNCTION 'FPCOMP_CREATE_PDF_FROM_SPOOL'
      EXPORTING
        i_spoolid      = iv_spoolid
        i_partnum      = iv_partnum
      IMPORTING
        e_pdf          = et_fp
      EXCEPTIONS
        ads_error      = 1
        usage_error    = 2
        system_error   = 3
        internal_error = 4
        OTHERS         = 5.

    IF sy-subrc NE 0.
      DATA(lo_cx_symsg) = zcx_bc_symsg=>get_instance( ).
      RAISE EXCEPTION TYPE zcx_bc_spool_to_pdf
        EXPORTING
          partnum  = iv_partnum
          previous = lo_cx_symsg
          spoolid  = iv_spoolid
          textid   = zcx_bc_spool_to_pdf=>conv_error.
    ENDIF.

    CHECK et_solix IS REQUESTED.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = et_fp
      TABLES
        binary_tab = et_solix.

  ENDMETHOD.

  METHOD print_option_get_for_pdf.

    es_control_param-getotf     = 'X'.
    es_control_param-no_dialog  = 'X'.
    es_control_param-preview    = 'X'.
    es_control_param-device     = 'PRINTER'.

    es_composer_param-tddest = 'LP01'.
    es_composer_param-tdnoprev  = ''.


  ENDMETHOD.

  METHOD ssf_name.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = im_formname
*       VARIANT            = ' '
*       DIRECT_CALL        = ' '
      IMPORTING
        fm_name            = rv_fname
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3 ##FM_SUBRC_OK.

  ENDMETHOD.

ENDCLASS.