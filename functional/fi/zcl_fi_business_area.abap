CLASS zcl_fi_business_area DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_def TYPE tgsb.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_gsber     TYPE gsber
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_fi_business_area
        RAISING   zcx_bc_table_content,

      get_text_Safe
        importing !iv_gsber       type gsber
        returning value(rv_gtext) type tgsbt-gtext.

    METHODS:
      get_business_area_group
        IMPORTING !iv_bukrs     TYPE bukrs
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_fi_business_area_grp
        RAISING   zcx_bc_table_content,

      get_text RETURNING VALUE(rv_gtext) TYPE tgsbt-gtext.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_grgsber
        TYPE HASHED TABLE OF zfit_grgsber
        WITH UNIQUE KEY primary_key COMPONENTS bukrs gsber,

      BEGIN OF t_multiton,
        gsber TYPE gsber,
        obj   TYPE REF TO zcl_fi_business_area,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS gsber,

      tt_tgsb
        TYPE HASHED TABLE OF tgsb
        WITH UNIQUE KEY primary_key COMPONENTS gsber,

      tt_tgsbt
        TYPE HASHED TABLE OF tgsbt
        WITH UNIQUE KEY primary_key COMPONENTS gsber,

      BEGIN OF t_clazy_flg,
        def  TYPE abap_bool,
        grp  TYPE abap_bool,
        text TYPE abap_bool,
      END OF t_clazy_flg,

      BEGIN OF t_clazy_val,
        def  TYPE tt_tgsb,
        grp  TYPE tt_grgsber,
        text TYPE tt_tgsbt,
      END OF t_clazy_val,

      BEGIN OF t_clazy,
        flg TYPE t_clazy_flg,
        val TYPE t_clazy_val,
      END OF t_clazy,

      BEGIN OF t_group,
        bukrs TYPE bukrs,
        obj   TYPE REF TO zcl_fi_business_area_grp,
        cx    TYPE REF TO zcx_bc_table_content,
      END OF t_group,

      tt_group
        TYPE HASHED TABLE OF t_group
        WITH UNIQUE KEY primary_key COMPONENTS bukrs.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'TGSB'.

    CLASS-DATA:
      gs_clazy    TYPE t_clazy,
      gt_multiton TYPE tt_multiton.

    DATA:
      gt_group TYPE tt_group.

    CLASS-METHODS:
      read_def_lazy,
      read_grp_lazy,
      read_text_lazy.

    METHODS:
      constructor
        IMPORTING !iv_gsber TYPE gsber
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_fi_business_area IMPLEMENTATION.

  METHOD constructor.

    read_def_lazy( ).

    TRY.
        gs_def = gs_clazy-val-def[
          KEY primary_key COMPONENTS
          gsber = iv_gsber
        ].

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            previous = lo_silnf
            objectid = |{ iv_gsber }|
            tabname  = c_tabname_def.

    ENDTRY.

  ENDMETHOD.

  METHOD get_business_area_group.

    ASSIGN gt_group[
        KEY primary_key COMPONENTS
        bukrs = iv_bukrs
      ] TO FIELD-SYMBOL(<ls_group>).

    IF sy-subrc NE 0.

      read_grp_lazy( ).

      DATA(ls_group) = VALUE t_group(
        bukrs = iv_bukrs
      ).

      TRY.

          DATA(lv_zgsber) = gs_clazy-val-grp[
              KEY primary_key COMPONENTS
              bukrs = iv_bukrs
              gsber = gs_def-gsber
            ]-zgsber.

          ls_group-obj = zcl_fi_business_area_grp=>get_instance( lv_zgsber ).

        CATCH zcx_bc_table_content INTO DATA(lo_tc).

          ls_group-cx = lo_tc.

        CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

          ls_group-cx = NEW zcx_bc_table_content(
              textid    = zcx_bc_table_content=>entry_missing
              previous  = lo_silnf
              objectid  = |{ iv_bukrs } { gs_def-gsber }|
              tabname   = zcl_fi_business_area_grp=>c_tabname_def
          ).

      ENDTRY.

      INSERT ls_group
        INTO TABLE gt_group
        ASSIGNING <ls_group>.

    ENDIF.

    IF <ls_group>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_group>-cx.
    ENDIF.

    ro_obj = <ls_group>-obj.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        gsber = iv_gsber
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      INSERT
        VALUE #(
          gsber = iv_gsber
          obj   = NEW #( iv_gsber )
        )
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

  METHOD get_text.

    read_text_lazy( ).

    TRY.

        rv_gtext = gs_clazy-val-text[
            KEY primary_key COMPONENTS
            gsber = gs_def-gsber
          ]-gtext.

      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  method get_text_safe.

    TRY.
        rv_gtext = zcl_fi_business_area=>get_instance(
            iv_gsber
          )->get_text( ).
      CATCH cx_root ##no_handler .
    ENDTRY.

  endmethod.

  METHOD read_def_lazy.
    CHECK gs_clazy-flg-def EQ abap_false.
    SELECT * INTO TABLE gs_clazy-val-def FROM tgsb.
    gs_clazy-flg-def = abap_true.
  ENDMETHOD.

  METHOD read_grp_lazy.
    CHECK gs_clazy-flg-grp EQ abap_false.
    SELECT * INTO TABLE gs_clazy-val-grp FROM zfit_grgsber.
    gs_clazy-flg-grp = abap_true.
  ENDMETHOD.


  METHOD read_text_lazy.
    CHECK gs_clazy-flg-text EQ abap_false.
    SELECT * INTO TABLE gs_clazy-val-text FROM tgsbt WHERE spras EQ sy-langu.
    gs_clazy-flg-text = abap_true.
  ENDMETHOD.

ENDCLASS.