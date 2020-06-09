CLASS zcl_bc_multiton DEFINITION
  PUBLIC
  FINAL
  CREATE public .

  PUBLIC SECTION.

    class-METHODS:
      get_obj
        IMPORTING
          !iv_clsname   TYPE seoclsname
          !iv_objectid  TYPE cdobjectv
        RETURNING
          VALUE(ro_obj) TYPE REF TO zif_bc_multiton
        RAISING
          cx_sy_create_object_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        clsname  TYPE seoclsname,
        objectid TYPE cdobjectv,
        cx       TYPE REF TO cx_sy_create_object_error,
        obj      TYPE REF TO zif_bc_multiton,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS clsname objectid.

    class-DATA:
      gt_multiton TYPE tt_multiton.

ENDCLASS.



CLASS zcl_bc_multiton IMPLEMENTATION.

  METHOD get_obj.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        clsname  = iv_clsname
        objectid = iv_objectid
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton(
        clsname  = iv_clsname
        objectid = iv_objectid
      ).

      TRY.

          CALL METHOD (ls_mt-clsname)=>zif_bc_multiton~get_instance
            EXPORTING
              iv_objectid = ls_mt-objectid
            RECEIVING
              ro_obj      = ls_mt-obj.

        CATCH cx_sy_create_object_error INTO ls_mt-cx ##no_handler.
        CATCH cx_root INTO DATA(lo_diaper).

          ls_mt-cx = NEW #(
            textid    = cx_sy_create_object_error=>cx_sy_create_object_error
            classname = CONV #( ls_mt-clsname )
            previous  = lo_diaper
          ).

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