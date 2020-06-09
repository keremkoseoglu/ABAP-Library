CLASS zcl_fi_business_area_grp DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_def,
        zgsber TYPE zfit_grgsber-zgsber,
        zgstxt TYPE zfit_igt-zgstxt,
      END OF t_def.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'ZFIT_GRGSBER'.

    DATA:
      gs_def TYPE t_def.

    CLASS-METHODS:
      class_constructor,


      get_instance
        IMPORTING !iv_zgsber    TYPE zfit_grgsber-zgsber
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_fi_business_area_grp
        RAISING   zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_def
        TYPE HASHED TABLE OF t_def
        WITH UNIQUE KEY primary_key COMPONENTS zgsber,

      BEGIN OF t_multiton,
        zgsber TYPE zfit_grgsber-zgsber,
        obj    TYPE REF TO zcl_fi_business_area_grp,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS zgsber.

    CLASS-DATA:
      gt_def      TYPE tt_def,
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !iv_zgsber TYPE zfit_grgsber-zgsber
        RAISING   zcx_bc_table_content.

ENDCLASS.



CLASS zcl_fi_business_area_grp IMPLEMENTATION.

  METHOD class_constructor.

    SELECT DISTINCT
        zfit_grgsber~zgsber,
        zfit_igt~zgstxt
      FROM
        zfit_grgsber
        LEFT JOIN zfit_igt ON
          zfit_igt~zgsber EQ zfit_grgsber~zgsber AND
          zfit_igt~spras  EQ @sy-langu
      WHERE
        zfit_grgsber~zgsber NE @space AND
        zfit_grgsber~zgsber IS NOT NULL
      INTO TABLE @DATA(lt_def).

    SORT lt_def BY zgsber.
    DELETE ADJACENT DUPLICATES FROM lt_def COMPARING zgsber.

    gt_def = CORRESPONDING #( lt_def ).

  ENDMETHOD.

  METHOD constructor.

    TRY.
        gs_def = gt_def[
          KEY primary_key COMPONENTS
          zgsber = iv_zgsber
        ].

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            previous = lo_silnf
            objectid = |{ iv_zgsber }|
            tabname  = c_tabname_def.

    ENDTRY.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        zgsber = iv_zgsber
      ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      INSERT VALUE #(
          zgsber = iv_zgsber
          obj    = NEW #( iv_zgsber )
        )
        INTO TABLE gt_multiton
        ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.

ENDCLASS.