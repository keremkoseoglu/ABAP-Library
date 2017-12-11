CLASS zcl_pp_order_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_def TYPE t003o READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_auart     TYPE aufart
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_pp_order_type
        RAISING   zcx_pp_order_type_undefined.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        auart TYPE aufart,
        obj   TYPE REF TO zcl_pp_order_type,
        cx    TYPE REF TO zcx_pp_order_type_undefined,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS auart.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !iv_auart TYPE aufart
        RAISING   zcx_pp_order_type_undefined.

ENDCLASS.



CLASS zcl_pp_order_type IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      FROM t003o
      WHERE auart EQ @iv_auart
      INTO @gs_def.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_pp_order_type_undefined
        EXPORTING
          auart = iv_auart.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        auart = iv_auart
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( auart = iv_auart ).

      TRY.
          ls_mt-obj = NEW #( ls_mt-auart ).
        CATCH zcx_pp_order_type_undefined INTO DATA(lo_cx).
          ls_mt-cx = lo_cx.
      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

ENDCLASS.