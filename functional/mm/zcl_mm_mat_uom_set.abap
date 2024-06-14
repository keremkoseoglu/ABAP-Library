CLASS zcl_mm_mat_uom_set DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mm_mat_uom_set.

    METHODS constructor IMPORTING matnrs TYPE zmmtt_matnr.

  PRIVATE SECTION.
    TYPES: BEGIN OF mat_uom_dict,
             matnr TYPE marm-matnr,
             meinh TYPE marm-meinh,
             umren TYPE float,
             umrez TYPE float,
           END OF mat_uom_dict,

           mat_uom_set TYPE HASHED TABLE OF mat_uom_dict
                       WITH UNIQUE KEY primary_key COMPONENTS matnr meinh.

    TYPES: BEGIN OF mat_uom_cnw_dict, " Custom net weight
             matnr TYPE marm-matnr,
             meinh TYPE marm-meinh,
             ntgew TYPE mara-ntgew,
             gewei TYPE mara-gewei,
             umren TYPE float,
             umrez TYPE float,
           END OF mat_uom_cnw_dict,

           mat_uom_cnw_set TYPE HASHED TABLE OF mat_uom_cnw_dict
                           WITH UNIQUE KEY primary_key COMPONENTS matnr meinh ntgew gewei.

    DATA: mat_uoms     TYPE mat_uom_set,
          mat_uom_cnws TYPE mat_uom_cnw_set.

    METHODS build_new_mat_uom
      IMPORTING matnr         TYPE marm-matnr
                meinh         TYPE marm-meinh
      RETURNING VALUE(result) TYPE mat_uom_dict
      RAISING   zcx_mm_material_unit_conv.

    METHODS build_new_mat_uom_cnw
      IMPORTING matnr         TYPE marm-matnr
                meinh         TYPE marm-meinh
                ntgew         TYPE mara-ntgew
                gewei         TYPE mara-gewei
      RETURNING VALUE(result) TYPE mat_uom_cnw_dict
      RAISING   zcx_mm_material_unit_conv.

    METHODS calc_umrex_over_dimension
      IMPORTING matnr TYPE matnr
                uom   TYPE REF TO zcl_bc_uom
      CHANGING  umren TYPE float
                umrez TYPE float.

    METHODS calc_umrex_over_current_weight
      IMPORTING matnr TYPE matnr
                uom   TYPE REF TO zcl_bc_uom
      CHANGING  umren TYPE float
                umrez TYPE float.

    METHODS calc_umrex_over_weight
      IMPORTING uom   TYPE REF TO zcl_bc_uom
                ntgew TYPE ntgew
                gewei TYPE gewei
      CHANGING  umren TYPE float
                umrez TYPE float.

ENDCLASS.


CLASS zcl_mm_mat_uom_set IMPLEMENTATION.
  METHOD constructor.
    SELECT FROM @matnrs AS _mat
                INNER JOIN marm
                  ON marm~matnr = _mat~table_line
           FIELDS _mat~table_line AS matnr,
                  marm~meinh,
                  marm~umren,
                  marm~umrez
           INTO CORRESPONDING FIELDS OF TABLE @me->mat_uoms.
  ENDMETHOD.

  METHOD build_new_mat_uom.
    result = VALUE mat_uom_dict( matnr = matnr
                                 meinh = meinh ).

    TRY.
        DATA(uom) = zcl_bc_uom=>get_instance( meinh ).

      CATCH cx_root INTO DATA(uom_error).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid   = zcx_mm_material_unit_conv=>uom_def_problem
                                                       previous = uom_error
                                                       matnr    = matnr
                                                       src_uom  = meinh ).
    ENDTRY.

    calc_umrex_over_dimension( EXPORTING matnr = matnr
                                         uom   = uom
                               CHANGING  umren = result-umren
                                         umrez = result-umrez ).

    CHECK result-umren IS INITIAL OR result-umrez IS INITIAL.

    calc_umrex_over_current_weight( EXPORTING matnr = matnr
                                              uom   = uom
                                    CHANGING  umren = result-umren
                                              umrez = result-umrez ).

    CHECK result-umren IS INITIAL OR result-umrez IS INITIAL.

    RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>mat_alt_uom_missing
                                                   matnr   = result-matnr
                                                   src_uom = result-meinh ).
  ENDMETHOD.

  METHOD build_new_mat_uom_cnw.
    result = VALUE mat_uom_cnw_dict( matnr = matnr
                                     meinh = meinh
                                     ntgew = ntgew
                                     gewei = gewei ).

    TRY.
        DATA(uom) = zcl_bc_uom=>get_instance( meinh ).

      CATCH cx_root INTO DATA(uom_error).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid   = zcx_mm_material_unit_conv=>uom_def_problem
                                                       previous = uom_error
                                                       matnr    = matnr
                                                       src_uom  = meinh ).
    ENDTRY.

    calc_umrex_over_dimension( EXPORTING matnr = matnr
                                         uom   = uom
                               CHANGING  umren = result-umren
                                         umrez = result-umrez ).

    CHECK result-umren IS INITIAL OR result-umrez IS INITIAL.

    calc_umrex_over_weight( EXPORTING uom   = uom
                                      ntgew = ntgew
                                      gewei = gewei
                            CHANGING  umren = result-umren
                                      umrez = result-umrez ).

    CHECK result-umren IS INITIAL OR result-umrez IS INITIAL.

    RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>mat_alt_uom_missing
                                                   matnr   = result-matnr
                                                   src_uom = result-meinh ).
  ENDMETHOD.

  METHOD calc_umrex_over_dimension.
    DATA(dimension_uom_rng) = uom->get_dimension_uom_range( ).

    LOOP AT me->mat_uoms REFERENCE INTO DATA(dim_mat_uom)
         WHERE     matnr  = matnr "#EC CI_HASHSEQ
               AND meinh IN dimension_uom_rng.

      TRY.
          ##FM_SUBRC_OK
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING  input                = dim_mat_uom->umrez
                       unit_in              = dim_mat_uom->meinh
                       unit_out             = uom->gs_def-msehi
            IMPORTING  output               = umren
            EXCEPTIONS conversion_not_found = 1
                       division_by_zero     = 2
                       input_invalid        = 3
                       output_invalid       = 4
                       overflow             = 5
                       type_invalid         = 6
                       units_missing        = 7
                       unit_in_not_found    = 8
                       unit_out_not_found   = 9
                       OTHERS               = 10.

          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'UNIT_CONVERSION_SIMPLE' ).

          umrez = dim_mat_uom->umrez.

        CATCH cx_root.
          CLEAR: umren,
                 umrez. " Paranoya
          CONTINUE.
      ENDTRY.

      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD calc_umrex_over_current_weight.
    CHECK uom->is_weight( ).

    DATA(mara) = zcl_mm_material=>get_mara( matnr ).
    CHECK mara-gewei IS NOT INITIAL.

    calc_umrex_over_weight( EXPORTING uom   = uom
                                      ntgew = mara-ntgew
                                      gewei = mara-gewei
                            CHANGING  umren = umren
                                      umrez = umrez ).
  ENDMETHOD.

  METHOD calc_umrex_over_weight.
    CHECK uom->is_weight( ).

    ##FM_SUBRC_OK
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING  input                = ntgew
                 unit_in              = gewei
                 unit_out             = uom->gs_def-msehi
      IMPORTING  output               = umren
      EXCEPTIONS conversion_not_found = 1
                 division_by_zero     = 2
                 input_invalid        = 3
                 output_invalid       = 4
                 overflow             = 5
                 type_invalid         = 6
                 units_missing        = 7
                 unit_in_not_found    = 8
                 unit_out_not_found   = 9
                 OTHERS               = 10.

    CHECK sy-subrc = 0.
    umrez = 1.
  ENDMETHOD.

  METHOD zif_mm_mat_uom_set~get_umrex.
    CLEAR: umren,
           umrez.

    TRY.
        DATA(mat_uom) = REF #( me->mat_uoms[ KEY primary_key
                                             matnr = matnr
                                             meinh = meinh ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT build_new_mat_uom( matnr = matnr
                                  meinh = meinh )
               INTO TABLE me->mat_uoms REFERENCE INTO mat_uom.
    ENDTRY.

    umren = mat_uom->umren.
    umrez = mat_uom->umrez.
  ENDMETHOD.

  METHOD zif_mm_mat_uom_set~get_umrex_w_custom_net_weight.
    CLEAR: umren,
           umrez.

    TRY.
        DATA(mat_uom_cnw) = REF #( me->mat_uom_cnws[ KEY primary_key
                                                     matnr = matnr
                                                     meinh = meinh
                                                     ntgew = ntgew
                                                     gewei = gewei ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT build_new_mat_uom_cnw( matnr = matnr
                                      meinh = meinh
                                      ntgew = ntgew
                                      gewei = gewei )
               INTO TABLE me->mat_uom_cnws REFERENCE INTO mat_uom_cnw.
    ENDTRY.

    umren = mat_uom_cnw->umren.
    umrez = mat_uom_cnw->umrez.
  ENDMETHOD.
ENDCLASS.