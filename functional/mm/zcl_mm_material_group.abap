CLASS zcl_mm_material_group DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_fnam_matkl TYPE fieldname VALUE 'MATKL'.

    CLASS-METHODS cache_wgbez
      IMPORTING ir_tab   TYPE REF TO data
                iv_fnam  TYPE fieldname DEFAULT c_fnam_matkl
                iv_spras TYPE sylangu   DEFAULT sy-langu
      RAISING   zcx_bc_class_method.

    CLASS-METHODS get_wgbez
      IMPORTING iv_matkl   TYPE matkl
                iv_spras   TYPE spras DEFAULT sy-langu
      EXPORTING ev_wgbez   TYPE wgbez
                ev_wgbez60 TYPE wgbez60.

    CLASS-METHODS ensure_existence
      IMPORTING iv_matkl TYPE matkl
      RAISING   zcx_mm_material_group.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_existence_cache,
             matkl TYPE matkl,
             error TYPE REF TO zcx_mm_material_group,
           END OF t_existence_cache,

           tt_existence_cache TYPE HASHED TABLE OF t_existence_cache
                              WITH UNIQUE KEY primary_key COMPONENTS matkl.

    CONSTANTS: c_clsname_me       TYPE seoclsname VALUE 'ZCL_MM_MATERIAL_GROUP',
               c_meth_cache_wgbez TYPE seocpdname VALUE 'CACHE_WGBEZ'.

    CLASS-DATA: gt_t023t           TYPE HASHED TABLE OF t023t
                         WITH UNIQUE KEY primary_key COMPONENTS spras matkl,

                gt_existence_cache TYPE tt_existence_cache.

ENDCLASS.


CLASS zcl_mm_material_group IMPLEMENTATION.
  METHOD cache_wgbez.
    DATA lt_matkl_rng TYPE mds_matkl_range_tab.

    FIELD-SYMBOLS: <lt_tab>   TYPE ANY TABLE,
                   <lv_matkl> TYPE matkl.

    TRY.
        CHECK ir_tab IS NOT INITIAL.
        ASSIGN ir_tab->* TO <lt_tab>.
        CHECK <lt_tab> IS ASSIGNED.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).
          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_matkl>.

          CHECK           <lv_matkl> IS ASSIGNED
                AND       <lv_matkl> IS NOT INITIAL
                AND ( NOT line_exists( gt_t023t[ KEY primary_key COMPONENTS spras = iv_spras matkl = <lv_matkl> ] ) ).

          COLLECT VALUE mds_matkl_range_type( option = zcl_bc_ddic_toolkit=>c_option_eq
                                              sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                              low    = <lv_matkl> )
                  INTO lt_matkl_rng.
        ENDLOOP.

        CHECK lt_matkl_rng IS NOT INITIAL.

        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_t023t
                 FROM t023t
                 WHERE spras =  @iv_spras AND
                       matkl IN @lt_matkl_rng.

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION NEW zcx_bc_class_method( textid   = zcx_bc_class_method=>unexpected_error
                                                 previous = lo_diaper
                                                 class    = c_clsname_me
                                                 method   = c_meth_cache_wgbez ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_wgbez.
    DATA ls_t023t TYPE t023t.

    ASSIGN gt_t023t[ KEY        primary_key
                     COMPONENTS spras = iv_spras
                                matkl = iv_matkl ]
           TO FIELD-SYMBOL(<ls_t023t>).

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM t023t INTO ls_t023t
        WHERE spras = iv_spras
          AND matkl = iv_matkl.

      ls_t023t-spras = iv_spras.
      ls_t023t-matkl = iv_matkl.
      INSERT ls_t023t INTO TABLE gt_t023t ASSIGNING <ls_t023t>.
    ENDIF.

    ev_wgbez   = <ls_t023t>-wgbez.
    ev_wgbez60 = <ls_t023t>-wgbez60.
  ENDMETHOD.

  METHOD ensure_existence.
    TRY.
        DATA(lr_existence) = REF #( gt_existence_cache[ KEY         primary_key
                                                        COMPONENTS matkl = iv_matkl ] ).
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( matkl = iv_matkl ) INTO TABLE gt_existence_cache REFERENCE INTO lr_existence.

        SELECT SINGLE FROM t023 FIELDS @abap_true
               WHERE  matkl = @iv_matkl
               INTO   @DATA(lv_exists).

        IF lv_exists = abap_false.
          lr_existence->error = NEW #( textid = zcx_mm_material_group=>not_found
                                       matkl  = iv_matkl ).
        ENDIF.
    ENDTRY.

    CHECK lr_existence->error IS NOT INITIAL.
    RAISE EXCEPTION lr_existence->error.
  ENDMETHOD.
ENDCLASS.