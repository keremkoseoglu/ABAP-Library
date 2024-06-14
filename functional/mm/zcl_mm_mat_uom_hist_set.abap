CLASS zcl_mm_mat_uom_hist_set DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mm_mat_uom_hist_set.

    METHODS constructor IMPORTING matnrs TYPE zmmtt_matnr.

  PRIVATE SECTION.
    TYPES: history_key_type     TYPE char8,
           history_key_cat_type TYPE char1.

    TYPES: BEGIN OF material_change_dict,
             matnr     TYPE matnr,
             changenr  TYPE cdhdr-changenr,
             udate     TYPE cdhdr-udate,
             utime     TYPE cdhdr-utime,
             jahrper   TYPE jahrper,
             tabname   TYPE cdpos-tabname,
             tabkey    TYPE cdpos-tabkey,
             fname     TYPE cdpos-fname,
             chngind   TYPE cdpos-chngind,
             value_new TYPE cdpos-value_new,
             value_old TYPE cdpos-value_old,
           END OF material_change_dict,

           material_change_list TYPE STANDARD TABLE OF material_change_dict
                                WITH KEY matnr changenr.

    TYPES: BEGIN OF material_dict,
             matnr TYPE matnr,
             meins TYPE meins,
             ntgew TYPE mara-ntgew,
             gewei TYPE mara-gewei,
           END OF material_dict,

           material_set TYPE HASHED TABLE OF material_dict
                        WITH UNIQUE KEY primary_key COMPONENTS matnr.

    TYPES: BEGIN OF historic_uom_dict,
             matnr       TYPE matnr,
             meinh       TYPE marm-meinh,
             history_key TYPE history_key_type,
             base_uom    TYPE meins,
             umren       TYPE float,
             umrez       TYPE float,
           END OF historic_uom_dict,

           historic_uom_set TYPE HASHED TABLE OF historic_uom_dict
                                  WITH UNIQUE KEY primary_key COMPONENTS matnr meinh history_key.

    TYPES: BEGIN OF historic_net_weight_dict,
             matnr       TYPE matnr,
             history_key TYPE history_key_type,
             to_gewei    TYPE mara-gewei,
             ntgew       TYPE float,
             gewei       TYPE mara-gewei,
           END OF historic_net_weight_dict,

           historic_net_weight_set TYPE HASHED TABLE OF historic_net_weight_dict
                                         WITH UNIQUE KEY primary_key COMPONENTS matnr history_key to_gewei.

    CONSTANTS: BEGIN OF table,
                 mara  TYPE tabname VALUE 'MARA',
                 dmarm TYPE tabname VALUE 'DMARM',
               END OF table.

    CONSTANTS: BEGIN OF field,
                 meins TYPE fieldname VALUE 'MEINS',
                 umren TYPE fieldname VALUE 'UMREN',
                 umrez TYPE fieldname VALUE 'UMREZ',
                 ntgew TYPE fieldname VALUE 'NTGEW',
                 gewei TYPE fieldname VALUE 'GEWEI',
               END OF field.

    CONSTANTS: BEGIN OF history_key_cat,
                 period TYPE history_key_cat_type VALUE 'P',
                 date   TYPE history_key_cat_type VALUE 'D',
               END OF history_key_cat.

    DATA: matnrs                  TYPE zmmtt_matnr,
          materials               TYPE material_set,
          material_changes        TYPE material_change_list,
          historic_uom_cache      TYPE historic_uom_set,
          mat_uoms                TYPE REF TO zif_mm_mat_uom_set,
          historic_net_wght_cache TYPE historic_net_weight_set.

    METHODS get_historic_uom
      IMPORTING matnr       TYPE matnr
                meinh       TYPE meinh
                history_key TYPE history_key_type
                key_cat     TYPE history_key_cat_type
      EXPORTING base_uom    TYPE meins
                umren       TYPE float
                umrez       TYPE float
      RAISING   zcx_mm_material_unit_conv.

    METHODS get_historic_net_weight
      IMPORTING matnr       TYPE matnr
                history_key TYPE history_key_type
                key_cat     TYPE history_key_cat_type
                to_gewei    TYPE gewei OPTIONAL
      EXPORTING ntgew       TYPE float
                gewei       TYPE mara-gewei
      RAISING   zcx_mm_material_unit_conv.

    METHODS convert_to_historic_base_uom
      IMPORTING matnr           TYPE matnr
                from_menge      TYPE rke2_vvmi0
                from_meins      TYPE meins
                history_key     TYPE history_key_type
                key_cat         TYPE history_key_cat_type
      EXPORTING converted_menge TYPE rke2_vvmi2
                converted_meins TYPE ekpo-meins
      RAISING   zcx_mm_material_unit_conv.

ENDCLASS.


CLASS zcl_mm_mat_uom_hist_set IMPLEMENTATION.
  METHOD constructor.
    me->matnrs = matnrs.

    SELECT FROM @me->matnrs AS _mat
                INNER JOIN mara
                  ON mara~matnr = _mat~table_line
           FIELDS _mat~table_line AS matnr,
                  mara~meins,
                  mara~ntgew,
                  mara~gewei
           INTO CORRESPONDING FIELDS OF TABLE @me->materials.

    me->mat_uoms = CAST #( NEW zcl_mm_mat_uom_set( matnrs ) ).

    SELECT FROM @me->matnrs             AS _mat
                INNER JOIN zi_mm_muh_cd AS _cd
                  ON _cd~matnr = _mat~table_line
           FIELDS _cd~*
           ORDER BY _cd~matnr    ASCENDING,
                    _cd~tabname  ASCENDING,
                    _cd~jahrper  DESCENDING,
                    _cd~udate    DESCENDING,
                    _cd~utime    DESCENDING,
                    _cd~changenr DESCENDING
           INTO CORRESPONDING FIELDS OF TABLE @me->material_changes.
  ENDMETHOD.

  METHOD get_historic_uom.
    CLEAR: base_uom,
           umren,
           umrez.

    ASSIGN me->historic_uom_cache[ KEY primary_key
                                   matnr       = matnr
                                   meinh       = meinh
                                   history_key = history_key ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      IF NOT line_exists( me->matnrs[ KEY primary_key
                                      table_line = matnr ] ).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid = zcx_mm_material_unit_conv=>mat_not_cached
                                                       matnr  = matnr ).
      ENDIF.

      DATA(new_cache) = VALUE historic_uom_dict( matnr       = matnr
                                                 meinh       = meinh
                                                 history_key = history_key ).

      DATA(stop_base_uom) = abap_false.
      DATA(stop_umren)    = abap_false.
      DATA(stop_umrez)    = abap_false.

      TRY.
          DATA(uom) = zcl_bc_uom=>get_instance( new_cache-meinh ).
        CATCH cx_root INTO DATA(uom_error).
          RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid   = zcx_mm_material_unit_conv=>uom_read_on_period_failed
                                                         previous = uom_error
                                                         matnr    = matnr
                                                         src_uom  = meinh
                                                         jahrper  = CONV #( new_cache-history_key ) ).
      ENDTRY.

      DATA(dimension_uom_rng) = uom->get_dimension_uom_range( ).

      LOOP AT me->material_changes REFERENCE INTO DATA(mat_chg)
           WHERE     matnr = new_cache-matnr
                 AND (          tabname  = zcl_mm_mat_uom_hist_set=>table-mara
                            AND fname    = zcl_mm_mat_uom_hist_set=>field-meins
                       OR
                          (     tabname  = zcl_mm_mat_uom_hist_set=>table-dmarm
                            AND tabkey  IN dimension_uom_rng ) ).

        IF     mat_chg->tabname = zcl_mm_mat_uom_hist_set=>table-mara
           AND mat_chg->fname   = zcl_mm_mat_uom_hist_set=>field-meins.

          CHECK stop_base_uom = abap_false.

          CASE key_cat.
            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-period.
              IF mat_chg->jahrper > new_cache-history_key.
                new_cache-base_uom = mat_chg->value_old.
              ELSEIF mat_chg->jahrper = new_cache-history_key.
                new_cache-base_uom = mat_chg->value_new.
                stop_base_uom = abap_true.
              ENDIF.

            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-date.
              IF mat_chg->udate > new_cache-history_key.
                new_cache-base_uom = mat_chg->value_old.
              ELSEIF mat_chg->udate = new_cache-history_key.
                new_cache-base_uom = mat_chg->value_new.
                stop_base_uom = abap_true.
              ENDIF.

            WHEN OTHERS.
              ASSERT 1 = 0.
          ENDCASE.

          CONTINUE.
        ENDIF.

        IF     mat_chg->tabname = zcl_mm_mat_uom_hist_set=>table-dmarm
           AND mat_chg->fname   = zcl_mm_mat_uom_hist_set=>field-umren.

          CHECK stop_umren = abap_false.

          CASE key_cat.
            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-period.
              IF mat_chg->jahrper > new_cache-history_key.
                new_cache-umren = mat_chg->value_old.
              ELSEIF mat_chg->jahrper = new_cache-history_key.
                new_cache-umren = mat_chg->value_new.
                stop_umren = abap_true.
              ENDIF.

            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-date.
              IF mat_chg->udate > new_cache-history_key.
                new_cache-umren = mat_chg->value_old.
              ELSEIF mat_chg->udate = new_cache-history_key.
                new_cache-umren = mat_chg->value_new.
                stop_umren = abap_true.
              ENDIF.

            WHEN OTHERS.
              ASSERT 1 = 0.
          ENDCASE.

          IF mat_chg->tabkey <> new_cache-meinh.
            TRY.
                ##FM_SUBRC_OK
                CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
                  EXPORTING  input                = new_cache-umren
                             unit_in              = mat_chg->tabkey
                             unit_out             = new_cache-meinh
                  IMPORTING  output               = new_cache-umren
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

              CATCH cx_root INTO DATA(conv_umren_error).
                RAISE EXCEPTION NEW zcx_mm_material_unit_conv(
                                        textid   = zcx_mm_material_unit_conv=>uom_read_on_period_failed
                                        previous = conv_umren_error
                                        matnr    = matnr
                                        src_uom  = meinh
                                        jahrper  = CONV #( key_cat ) ).
            ENDTRY.
          ENDIF.

          CONTINUE.
        ENDIF.

        IF     mat_chg->tabname = zcl_mm_mat_uom_hist_set=>table-dmarm
           AND mat_chg->fname   = zcl_mm_mat_uom_hist_set=>field-umrez.

          CHECK stop_umrez = abap_false.

          CASE key_cat.
            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-period.
              IF mat_chg->jahrper > new_cache-history_key.
                new_cache-umrez = mat_chg->value_old.
              ELSEIF mat_chg->jahrper = new_cache-history_key.
                new_cache-umrez = mat_chg->value_new.
                stop_umrez = abap_true.
              ENDIF.

            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-date.
              IF mat_chg->udate > new_cache-history_key.
                new_cache-umrez = mat_chg->value_old.
              ELSEIF mat_chg->udate = new_cache-history_key.
                new_cache-umrez = mat_chg->value_new.
                stop_umrez = abap_true.
              ENDIF.

            WHEN OTHERS.
              ASSERT 1 = 0.
          ENDCASE.

          IF mat_chg->tabkey <> new_cache-meinh.
            TRY.
                ##FM_SUBRC_OK
                CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
                  EXPORTING  input                = new_cache-umrez
                             unit_in              = mat_chg->tabkey
                             unit_out             = new_cache-meinh
                  IMPORTING  output               = new_cache-umrez
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

              CATCH cx_root INTO DATA(conv_umrez_error).
                RAISE EXCEPTION NEW zcx_mm_material_unit_conv(
                                        textid   = zcx_mm_material_unit_conv=>uom_read_on_period_failed
                                        previous = conv_umrez_error
                                        matnr    = matnr
                                        src_uom  = meinh
                                        jahrper  = CONV #( new_cache-history_key ) ).
            ENDTRY.
          ENDIF.

          CONTINUE.
        ENDIF.
      ENDLOOP.

      TRY.
          IF new_cache-base_uom IS INITIAL.
            new_cache-base_uom = me->materials[ KEY primary_key
                                                matnr = new_cache-matnr ]-meins.
          ENDIF.

          IF new_cache-umren IS INITIAL. " Hiç değişmedi demektir
            SELECT SINGLE FROM marm
                   FIELDS umren
                   WHERE matnr = @new_cache-matnr
                     AND meinh = @meinh
                   INTO @new_cache-umren.
          ENDIF.

          IF new_cache-umrez IS INITIAL. " Hiç değişmedi demektir
            SELECT SINGLE FROM marm
                   FIELDS umrez
                   WHERE matnr = @new_cache-matnr
                     AND meinh = @meinh
                   INTO @new_cache-umrez.
          ENDIF.

        CATCH cx_root INTO DATA(umrex_error).
          RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid   = zcx_mm_material_unit_conv=>mat_alt_uom_missing
                                                         previous = umrex_error
                                                         matnr    = matnr
                                                         src_uom  = meinh ).
      ENDTRY.

      INSERT new_cache INTO TABLE me->historic_uom_cache ASSIGNING <cache>.
    ENDIF.

    base_uom = <cache>-base_uom.
    umren    = <cache>-umren.
    umrez    = <cache>-umrez.
  ENDMETHOD.

  METHOD get_historic_net_weight.
    CLEAR: ntgew,
           gewei.

    ASSIGN me->historic_net_wght_cache[ KEY primary_key
                                        matnr       = matnr
                                        history_key = history_key
                                        to_gewei    = to_gewei ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      IF NOT line_exists( me->matnrs[ KEY primary_key
                                      table_line = matnr ] ).
        RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid = zcx_mm_material_unit_conv=>mat_not_cached
                                                       matnr  = matnr ).
      ENDIF.

      DATA(new_cache) = VALUE historic_net_weight_dict( matnr       = matnr
                                                        history_key = history_key
                                                        to_gewei    = to_gewei ).

      DATA(stop_ntgew) = abap_false.
      DATA(stop_gewei) = abap_false.

      LOOP AT me->material_changes REFERENCE INTO DATA(mat_chg)
           WHERE     matnr = new_cache-matnr
                 AND (          tabname = zcl_mm_mat_uom_hist_set=>table-mara
                            AND fname   = zcl_mm_mat_uom_hist_set=>field-ntgew
                       OR
                          (     tabname = zcl_mm_mat_uom_hist_set=>table-mara
                            AND fname   = zcl_mm_mat_uom_hist_set=>field-gewei ) ).

        IF     mat_chg->tabname = zcl_mm_mat_uom_hist_set=>table-mara
           AND mat_chg->fname   = zcl_mm_mat_uom_hist_set=>field-ntgew.

          CHECK stop_ntgew = abap_false.

          CASE key_cat.
            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-period.
              IF mat_chg->jahrper > new_cache-history_key.
                new_cache-ntgew = mat_chg->value_old.
              ELSEIF mat_chg->jahrper = new_cache-history_key.
                new_cache-ntgew = mat_chg->value_new.
                stop_ntgew = abap_true.
              ENDIF.

            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-date.
              IF mat_chg->udate > new_cache-history_key.
                new_cache-ntgew = mat_chg->value_old.
              ELSEIF mat_chg->udate = new_cache-history_key.
                new_cache-ntgew = mat_chg->value_new.
                stop_ntgew = abap_true.
              ENDIF.

            WHEN OTHERS.
              ASSERT 1 = 0.
          ENDCASE.

          CONTINUE.
        ENDIF.

        IF     mat_chg->tabname = zcl_mm_mat_uom_hist_set=>table-mara
           AND mat_chg->fname   = zcl_mm_mat_uom_hist_set=>field-gewei.

          CHECK stop_gewei = abap_false.

          CASE key_cat.
            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-period.
              IF mat_chg->jahrper > new_cache-history_key.
                new_cache-gewei = mat_chg->value_old.
              ELSEIF mat_chg->jahrper = new_cache-history_key.
                new_cache-gewei = mat_chg->value_new.
                stop_gewei = abap_true.
              ENDIF.

            WHEN zcl_mm_mat_uom_hist_set=>history_key_cat-date.
              IF mat_chg->udate > new_cache-history_key.
                new_cache-gewei = mat_chg->value_old.
              ELSEIF mat_chg->udate = new_cache-history_key.
                new_cache-gewei = mat_chg->value_new.
                stop_gewei = abap_true.
              ENDIF.

            WHEN OTHERS.
              ASSERT 1 = 0.
          ENDCASE.

          CONTINUE.
        ENDIF.
      ENDLOOP.

      TRY.
          IF new_cache-ntgew IS INITIAL.
            new_cache-ntgew = me->materials[ KEY primary_key
                                             matnr = new_cache-matnr ]-ntgew.
          ENDIF.

          IF new_cache-gewei IS INITIAL.
            new_cache-gewei = me->materials[ KEY primary_key
                                             matnr = new_cache-matnr ]-gewei.
          ENDIF.

        CATCH cx_root INTO DATA(umrex_error).
          RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid   = zcx_mm_material_unit_conv=>mat_alt_uom_missing
                                                         previous = umrex_error
                                                         matnr    = matnr
                                                         src_uom  = gewei ).
      ENDTRY.

      IF new_cache-ntgew IS NOT INITIAL AND new_cache-to_gewei IS NOT INITIAL AND new_cache-to_gewei <> new_cache-gewei.
        TRY.
            DATA(converted_ntgew) = CONV float( 0 ).

            ##FM_SUBRC_OK
            CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
              EXPORTING  input                = new_cache-ntgew
                         unit_in              = new_cache-gewei
                         unit_out             = new_cache-to_gewei
              IMPORTING  output               = converted_ntgew
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
            new_cache-ntgew = converted_ntgew.

          CATCH cx_root INTO DATA(to_gewei_error).
            RAISE EXCEPTION NEW zcx_mm_material_unit_conv( previous = to_gewei_error
                                                           matnr    = new_cache-matnr
                                                           src_uom  = new_cache-gewei
                                                           tar_uom  = new_cache-to_gewei ).
        ENDTRY.
      ENDIF.

      INSERT new_cache INTO TABLE me->historic_net_wght_cache ASSIGNING <cache>.
    ENDIF.

    ntgew = <cache>-ntgew.

    gewei = COND #( WHEN <cache>-to_gewei IS NOT INITIAL
                    THEN <cache>-to_gewei
                    ELSE <cache>-gewei ).
  ENDMETHOD.

  METHOD convert_to_historic_base_uom.
    CLEAR: converted_menge,
           converted_meins.

    CHECK from_menge IS NOT INITIAL.

    get_historic_uom( EXPORTING matnr       = matnr
                                meinh       = from_meins
                                history_key = history_key
                                key_cat     = key_cat
                      IMPORTING base_uom    = DATA(base_uom)
                                umren       = DATA(umren)
                                umrez       = DATA(umrez) ).

    IF base_uom IS INITIAL.
      RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>no_mat_uom_on_period
                                                     matnr   = matnr
                                                     jahrper = CONV #( history_key ) ).
    ENDIF.

    converted_meins = base_uom.

    IF     umren IS NOT INITIAL
       AND umrez IS NOT INITIAL.

      converted_menge = from_menge * umrez / umren.
      RETURN.
    ENDIF.

    zcl_co_exit_zxkkeu03=>uom_conversion( EXPORTING iv_matnr = matnr
                                                    iv_imein = from_meins
                                                    iv_omein = base_uom
                                                    iv_menge = from_menge
                                          CHANGING  cv_menge = converted_menge
                                                    cv_meins = converted_meins ).

    CHECK converted_menge IS INITIAL.

    RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>conv_failed_on_period
                                                   matnr   = matnr
                                                   src_uom = from_meins
                                                   jahrper = CONV #( history_key ) ).
  ENDMETHOD.

  METHOD zif_mm_mat_uom_hist_set~convert_to_base_uom_on_period.
    CLEAR: converted_menge,
           converted_meins.

    convert_to_historic_base_uom( EXPORTING matnr           = matnr
                                            from_menge      = from_menge
                                            from_meins      = from_meins
                                            history_key     = CONV #( jahrper )
                                            key_cat         = zcl_mm_mat_uom_hist_set=>history_key_cat-period
                                  IMPORTING converted_menge = converted_menge
                                            converted_meins = converted_meins ).
  ENDMETHOD.

  METHOD zif_mm_mat_uom_hist_set~convert_to_base_uom_on_date.
    CLEAR: converted_menge,
           converted_meins.

    convert_to_historic_base_uom( EXPORTING matnr           = matnr
                                            from_menge      = from_menge
                                            from_meins      = from_meins
                                            history_key     = CONV #( datum )
                                            key_cat         = zcl_mm_mat_uom_hist_set=>history_key_cat-date
                                  IMPORTING converted_menge = converted_menge
                                            converted_meins = converted_meins ).
  ENDMETHOD.

  METHOD zif_mm_mat_uom_hist_set~convert_fr_base_uom_on_period.
    CLEAR: converted_menge,
           base_uom.

    CHECK from_menge IS NOT INITIAL.

    get_historic_uom( EXPORTING matnr       = matnr
                                meinh       = to_meins
                                history_key = CONV #( jahrper )
                                key_cat     = zcl_mm_mat_uom_hist_set=>history_key_cat-period
                      IMPORTING base_uom    = base_uom
                                umren       = DATA(umren)
                                umrez       = DATA(umrez) ).

    IF base_uom IS INITIAL.
      RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>no_mat_uom_on_period
                                                     matnr   = matnr
                                                     jahrper = jahrper ).
    ENDIF.

    IF     umren IS NOT INITIAL
       AND umrez IS NOT INITIAL.

      converted_menge = from_menge * umren / umrez.
      RETURN.
    ENDIF.

    DATA(converted_meins) = base_uom.

    zcl_co_exit_zxkkeu03=>uom_conversion( EXPORTING iv_matnr = matnr
                                                    iv_imein = base_uom
                                                    iv_omein = to_meins
                                                    iv_menge = from_menge
                                          CHANGING  cv_menge = converted_menge
                                                    cv_meins = converted_meins ).

    CHECK converted_menge IS INITIAL.

    RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>conv_failed_on_period
                                                   matnr   = matnr
                                                   src_uom = to_meins
                                                   jahrper = jahrper ).
  ENDMETHOD.

  METHOD zif_mm_mat_uom_hist_set~get_net_weight_on_period.
    get_historic_net_weight( EXPORTING matnr       = matnr
                                       history_key = CONV #( jahrper )
                                       key_cat     = zcl_mm_mat_uom_hist_set=>history_key_cat-period
                                       to_gewei    = to_gewei
                             IMPORTING ntgew       = ntgew
                                       gewei       = gewei ).
  ENDMETHOD.

  METHOD zif_mm_mat_uom_hist_set~get_net_weight_on_date.
    get_historic_net_weight( EXPORTING matnr       = matnr
                                       history_key = CONV #( datum )
                                       key_cat     = zcl_mm_mat_uom_hist_set=>history_key_cat-date
                                       to_gewei    = to_gewei
                             IMPORTING ntgew       = ntgew
                                       gewei       = gewei ).
  ENDMETHOD.

  METHOD zif_mm_mat_uom_hist_set~convert_fr_base_uom_on_date.
    CLEAR: converted_menge,
           base_uom.

    CHECK from_menge IS NOT INITIAL.

    get_historic_uom( EXPORTING matnr       = matnr
                                meinh       = to_meins
                                history_key = CONV #( datum )
                                key_cat     = zcl_mm_mat_uom_hist_set=>history_key_cat-date
                      IMPORTING base_uom    = base_uom
                                umren       = DATA(umren)
                                umrez       = DATA(umrez) ).

    IF base_uom IS INITIAL.
      RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>no_mat_uom_on_period
                                                     matnr   = matnr
                                                     jahrper = CONV #( datum ) ).
    ENDIF.

    IF     umren IS NOT INITIAL
       AND umrez IS NOT INITIAL.

      converted_menge = from_menge * umren / umrez.
      RETURN.
    ENDIF.

    DATA(converted_meins) = base_uom.

    zcl_co_exit_zxkkeu03=>uom_conversion( EXPORTING iv_matnr = matnr
                                                    iv_imein = base_uom
                                                    iv_omein = to_meins
                                                    iv_menge = from_menge
                                          CHANGING  cv_menge = converted_menge
                                                    cv_meins = converted_meins ).

    CHECK converted_menge IS INITIAL.

    RAISE EXCEPTION NEW zcx_mm_material_unit_conv( textid  = zcx_mm_material_unit_conv=>conv_failed_on_period
                                                   matnr   = matnr
                                                   src_uom = to_meins
                                                   jahrper = CONV #( datum ) ).
  ENDMETHOD.
ENDCLASS.