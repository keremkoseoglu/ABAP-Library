CLASS zcl_fi_currency DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_key,
        waers TYPE waers,
      END OF t_key.

    DATA gs_def TYPE tcurc READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !is_key       TYPE t_key
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_fi_currency
        RAISING   cx_no_entry_in_table.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        key TYPE t_key,
        obj TYPE REF TO zcl_fi_currency,
        cx  TYPE REF TO cx_no_entry_in_table,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS key.

    CONSTANTS:
      BEGIN OF c_tabname,
        def TYPE tabname VALUE 'TCURC',
      END OF c_tabname.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING !is_key TYPE t_key
        RAISING   cx_no_entry_in_table.

ENDCLASS.

CLASS zcl_fi_currency IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      FROM tcurc
      WHERE
        waers EQ @is_key-waers
      INTO CORRESPONDING FIELDS OF @gs_def.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_no_entry_in_table
        EXPORTING
          table_name = CONV #( c_tabname-def )
          entry_name = |{ is_key-waers }|.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS key = is_key
      ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.
      DATA(ls_multiton) = VALUE t_multiton( key = is_key ).

      TRY.
          ls_multiton-obj = NEW #( ls_multiton-key ).
        CATCH cx_no_entry_in_table INTO ls_multiton-cx ##NO_HANDLER.
      ENDTRY.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    IF <ls_multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_multiton>-cx.
    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.

ENDCLASS.