CLASS zcl_mm_material_group DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_fnam_matkl TYPE fieldname VALUE 'MATKL'.

    CLASS-METHODS:
      cache_wgbez
        IMPORTING
          !ir_tab   TYPE REF TO data
          !iv_fnam  TYPE fieldname DEFAULT c_fnam_matkl
          !iv_spras TYPE sylangu DEFAULT sy-langu
        RAISING
          zcx_bc_class_method,

      get_wgbez
        IMPORTING
          !iv_matkl  TYPE matkl
          !iv_spras  TYPE spras DEFAULT sy-langu
        EXPORTING
          ev_wgbez   TYPE wgbez
          ev_wgbez60 TYPE wgbez60 .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_clsname_me       TYPE seoclsname VALUE 'ZCL_MM_MATERIAL_GROUP',
      c_meth_cache_wgbez TYPE seocpdname VALUE 'CACHE_WGBEZ'.

    CLASS-DATA:
      gt_t023t
        TYPE HASHED TABLE OF t023t
        WITH UNIQUE KEY primary_key COMPONENTS spras matkl .

ENDCLASS.



CLASS ZCL_MM_MATERIAL_GROUP IMPLEMENTATION.


  METHOD cache_wgbez.

    DATA:
      lt_matkl_rng TYPE WRBA_RANGE_MATKL_TABLE.

    FIELD-SYMBOLS:
      <lt_tab>   TYPE ANY TABLE,
      <lv_matkl> TYPE matkl.

    TRY.

        CHECK ir_tab IS NOT INITIAL.
        ASSIGN ir_tab->* TO <lt_tab>.
        CHECK <lt_tab> IS ASSIGNED.

        LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_tab>).

          ASSIGN COMPONENT iv_fnam OF STRUCTURE <ls_tab> TO <lv_matkl>.

          CHECK <lv_matkl> IS ASSIGNED AND
                <lv_matkl> IS NOT INITIAL AND
                ( NOT line_exists( gt_t023t[ KEY primary_key COMPONENTS spras = iv_spras matkl = <lv_matkl> ] ) ).

          collect VALUE WRBA_RANGE_MATKL_STRUC(
            option = zcl_bc_ddic_toolkit=>c_option_eq
            sign   = zcl_bc_ddic_toolkit=>c_sign_i
            low    = <lv_matkl>
          ) inTO lt_matkl_rng.

        ENDLOOP.

        CHECK lt_matkl_rng IS NOT INITIAL.

        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @gt_t023t
          FROM t023t
          WHERE spras EQ @iv_spras AND
                matkl IN @lt_matkl_rng.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_diaper
            class    = c_clsname_me
            method   = c_meth_cache_wgbez.

    ENDTRY.

  ENDMETHOD.


  METHOD get_wgbez.

    DATA ls_t023t TYPE t023t.

    ASSIGN gt_t023t[ KEY primary_key COMPONENTS
      spras = iv_spras
      matkl = iv_matkl
    ] TO FIELD-SYMBOL(<ls_t023t>).

    IF sy-subrc ne 0.
      SELECT SINGLE * FROM t023t INTO ls_t023t
        WHERE spras EQ iv_spras
          AND matkl EQ iv_matkl.
      ls_t023t-spras = iv_spras.
      ls_t023t-matkl = iv_matkl.
      INSERT ls_t023t INTO TABLE gt_t023t ASSIGNING <ls_t023t>.
    ENDIF.

    ev_wgbez   = <ls_t023t>-wgbez.
    ev_wgbez60 = <ls_t023t>-wgbez60.

  ENDMETHOD.
ENDCLASS.