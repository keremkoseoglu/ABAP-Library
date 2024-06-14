CLASS zcl_bc_uom DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    DATA gs_def TYPE t006 READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING iv_msehi      TYPE msehi
      RETURNING VALUE(ro_uom) TYPE REF TO zcl_bc_uom
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_instance_iso
      IMPORTING iv_iso_code   TYPE t006-isocode
      RETURNING VALUE(ro_uom) TYPE REF TO zcl_bc_uom
      RAISING   zcx_bc_table_content.

    CLASS-METHODS convert_unit_with_bapi_msg
      IMPORTING iv_old_amount        TYPE zppd_tsure
                iv_old_unit          TYPE meins
                iv_new_unit          TYPE meins
      CHANGING  cv_mesaj             TYPE bapi_msg
      RETURNING VALUE(rv_new_amount) TYPE zppd_tsure.

    METHODS get_dimension
      RETURNING VALUE(result) TYPE REF TO zif_bc_dimension
      RAISING   zcx_bc_dimension.

    METHODS get_dimension_uom_range RETURNING VALUE(result) TYPE mdg_bs_mat_t_range_meinh.

    METHODS is_weight               RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_multiton,
             msehi TYPE msehi,
             cx    TYPE REF TO zcx_bc_table_content,
             obj   TYPE REF TO zcl_bc_uom,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton
                         WITH UNIQUE KEY primary_key COMPONENTS msehi.

    TYPES: BEGIN OF t_iso_multiton,
             iso_code TYPE t006-isocode,
             cx       TYPE REF TO zcx_bc_table_content,
             obj      TYPE REF TO zcl_bc_uom,
           END OF t_iso_multiton,

           tt_iso_multiton TYPE HASHED TABLE OF t_iso_multiton
                             WITH UNIQUE KEY primary_key COMPONENTS iso_code.

    CONSTANTS c_tabname_def TYPE tabname VALUE 'T006'.

    CONSTANTS: BEGIN OF c_dimid,
                 mass TYPE t006-dimid VALUE 'MASS',
               END OF c_dimid.

    CLASS-DATA: gt_multiton     TYPE tt_multiton,
                gt_iso_multiton TYPE tt_iso_multiton.

    DATA: gv_dimension_read    TYPE abap_bool,
          go_dimension_error   TYPE REF TO zcx_bc_dimension,
          go_dimension         TYPE REF TO zif_bc_dimension,
          gt_dimension_uom_rng TYPE mdg_bs_mat_t_range_meinh.

    METHODS constructor
      IMPORTING iv_msehi TYPE msehi
      RAISING   zcx_bc_table_content.

ENDCLASS.


CLASS zcl_bc_uom IMPLEMENTATION.
  METHOD constructor.
    SELECT SINGLE * FROM t006
           WHERE msehi = @iv_msehi
           INTO @gs_def.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                objectid = CONV #( iv_msehi )
                                                tabname  = c_tabname_def ).
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS msehi = iv_msehi ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      DATA(ls_mt) = VALUE t_multiton( msehi = iv_msehi ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-msehi ).
        CATCH zcx_bc_table_content INTO ls_mt-cx ##NO_HANDLER.
      ENDTRY.

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.
    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_uom = <ls_mt>-obj.
  ENDMETHOD.

  METHOD get_instance_iso.
    DATA lv_msehi TYPE t006-msehi.

    ASSIGN gt_iso_multiton[ KEY primary_key COMPONENTS iso_code = iv_iso_code ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      DATA(ls_mt) = VALUE t_iso_multiton( iso_code = iv_iso_code ).

      CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
        EXPORTING  iso_code  = ls_mt-iso_code
        IMPORTING  sap_code  = lv_msehi
        EXCEPTIONS not_found = 1
                   OTHERS    = 2.

      CASE sy-subrc.
        WHEN 0.
          TRY.
              ls_mt-obj = NEW #( lv_msehi ).
            CATCH zcx_bc_table_content INTO ls_mt-cx ##NO_HANDLER.
          ENDTRY.

        WHEN OTHERS.
          ls_mt-cx = NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>value_missing
                                               objectid = CONV #( iv_iso_code )
                                               tabname  = c_tabname_def ).
      ENDCASE.

      INSERT ls_mt INTO TABLE gt_iso_multiton ASSIGNING <ls_mt>.
    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_uom = <ls_mt>-obj.
  ENDMETHOD.

  METHOD convert_unit_with_bapi_msg.
    CLEAR : rv_new_amount,
            cv_mesaj.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING  input                = iv_old_amount
                 unit_in              = iv_old_unit
                 unit_out             = iv_new_unit
      IMPORTING  output               = rv_new_amount
      EXCEPTIONS conversion_not_found = 1
                 division_by_zero     = 23
                 input_invalid        = 3
                 output_invalid       = 4
                 overflow             = 5
                 type_invalid         = 6
                 units_missing        = 7
                 unit_in_not_found    = 8
                 unit_out_not_found   = 9
                 OTHERS               = 10.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO cv_mesaj.
    ENDIF.
  ENDMETHOD.

  METHOD get_dimension.
    IF gv_dimension_read = abap_false.
      TRY.
          go_dimension = CAST #( zcl_bc_dimension=>get_instance( gs_def-dimid ) ).
        CATCH zcx_bc_dimension INTO go_dimension_error ##NO_HANDLER.
      ENDTRY.

      gv_dimension_read = abap_true.
    ENDIF.

    IF go_dimension_error IS NOT INITIAL.
      RAISE EXCEPTION go_dimension_error.
    ENDIF.

    result = go_dimension.
  ENDMETHOD.

  METHOD get_dimension_uom_range.
    IF gt_dimension_uom_rng IS INITIAL.
      TRY.
          gt_dimension_uom_rng = get_dimension( )->get_uom_range( ).
        CATCH zcx_bc_dimension.
          gt_dimension_uom_rng = VALUE #( ( sign   = ycl_addict_toolkit=>sign-include
                                            option = ycl_addict_toolkit=>option-eq
                                            low    = gs_def-msehi ) ).
      ENDTRY.
    ENDIF.

    result = gt_dimension_uom_rng.
  ENDMETHOD.

  METHOD is_weight.
    result = xsdbool( gs_def-dimid = c_dimid-mass ).
  ENDMETHOD.
ENDCLASS.