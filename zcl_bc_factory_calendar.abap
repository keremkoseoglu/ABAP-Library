CLASS zcl_bc_factory_calendar DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA gs_def TYPE tfacd.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_fabkl     TYPE fabkl
        RETURNING VALUE(ro_cal) TYPE REF TO zcl_bc_factory_calendar
        RAISING   zcx_bc_factory_calendar_def.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        fabkl TYPE fabkl,
        obj   TYPE REF TO zcl_bc_factory_calendar,
        cx    TYPE REF TO zcx_bc_factory_calendar_def,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS fabkl.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !iv_fabkl TYPE fabkl
        RAISING   zcx_bc_factory_calendar_def.

ENDCLASS.



CLASS zcl_bc_factory_calendar IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      FROM tfacd
      WHERE ident EQ @iv_fabkl
      INTO @gs_def.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_factory_calendar_def
        EXPORTING
          fabkl = iv_fabkl.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        fabkl = iv_fabkl
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( fabkl = iv_fabkl ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-fabkl ).
        CATCH zcx_bc_factory_calendar_def INTO ls_mt-cx ##no_handler.
      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_cal = <ls_mt>-obj.

  ENDMETHOD.

ENDCLASS.