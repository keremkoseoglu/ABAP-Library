CLASS zcl_bc_def_alv_variant DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_alv_variant.

    CLASS-METHODS get_instance
      IMPORTING disvariant    TYPE disvariant
      RETURNING VALUE(result) TYPE REF TO zcl_bc_def_alv_variant.

    CLASS-METHODS create_instance_from_gui
      IMPORTING !report        TYPE disvariant-report OPTIONAL
                variant_suffix TYPE zbcd_text11
      RETURNING VALUE(result)  TYPE REF TO zcl_bc_def_alv_variant
      RAISING   zcx_bc_alv_variant.

  PRIVATE SECTION.
    DATA disvariant TYPE disvariant.

    METHODS constructor IMPORTING disvariant TYPE disvariant.
ENDCLASS.


CLASS zcl_bc_def_alv_variant IMPLEMENTATION.
  METHOD constructor.
    me->disvariant = disvariant.
  ENDMETHOD.

  METHOD get_instance.
    result = NEW #( disvariant ).
  ENDMETHOD.

  METHOD create_instance_from_gui.
    TRY.
        DATA(alv_variant) = VALUE disvariant( ).
        DATA(alv_fcat)    = VALUE slis_t_fieldcat_alv( ).
        DATA(alv_sort)    = VALUE slis_t_sortinfo_alv( ).
        DATA(alv_filter)  = VALUE slis_t_filter_alv( ).

        ##FM_SUBRC_OK
        CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
          IMPORTING  es_variant    = alv_variant
                     et_fieldcat   = alv_fcat
                     et_sort       = alv_sort
                     et_filter     = alv_filter
          EXCEPTIONS no_infos      = 1
                     program_error = 2
                     OTHERS        = 3.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'REUSE_ALV_GRID_LAYOUT_INFO_GET' ).

        IF alv_variant-report IS INITIAL.
          alv_variant-report = report.
        ENDIF.

        alv_variant-variant = EXACT #( |/{ variant_suffix }| ). " SNAPSHOT_ID'yi gelecekte uzatırlarsa ve ALV varyantına sığmazsa, patlasın
        CLEAR alv_variant-username.

        ##FM_SUBRC_OK
        CALL FUNCTION 'REUSE_ALV_VARIANT_SAVE'
          EXPORTING  it_fieldcat     = alv_fcat
                     it_sort         = alv_sort
                     it_filter       = alv_filter
                     i_dialog        = abap_false
                     i_overwrite     = abap_true
                     i_user_specific = abap_false
          CHANGING   cs_variant      = alv_variant
          EXCEPTIONS wrong_input     = 1
                     fc_not_complete = 2
                     foreign_lock    = 3
                     variant_exists  = 4
                     name_reserved   = 5
                     program_error   = 6
                     OTHERS          = 7.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'REUSE_ALV_VARIANT_SAVE' ).

        result = NEW #( alv_variant ).

      CATCH zcx_bc_alv_variant INTO DATA(variant_error).
        RAISE EXCEPTION variant_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_bc_alv_variant( textid   = zcx_bc_alv_variant=>creation_failed
                                                previous = diaper ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_bc_alv_variant~get_disvariant.
    result = me->disvariant.
  ENDMETHOD.

  METHOD zif_bc_alv_variant~delete.
    CHECK NOT zif_bc_alv_variant~is_initial( ).

    NEW cl_alv_variant( )->delete_variants( EXPORTING  it_variants    = VALUE #( ( CORRESPONDING #( me->disvariant ) ) )
                                            RECEIVING  boolean        = DATA(deletion_success)
                                            EXCEPTIONS no_authority   = 1
                                                       programm_error = 2
                                                       OTHERS         = 3 ).

    CHECK sy-subrc <> 0 OR deletion_success IS INITIAL.

    RAISE EXCEPTION NEW zcx_bc_alv_variant( textid     = zcx_bc_alv_variant=>deletion_failed
                                            variant_id = |{ me->disvariant-report }-{ me->disvariant-variant }| ).
  ENDMETHOD.

  METHOD zif_bc_alv_variant~is_initial.
    result = xsdbool( me->disvariant-variant IS INITIAL ).
  ENDMETHOD.
ENDCLASS.