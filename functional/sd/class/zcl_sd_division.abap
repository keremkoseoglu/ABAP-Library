CLASS zcl_sd_division DEFINITION
  PUBLIC
  FINAL
  CREATE private .

  PUBLIC SECTION.
    TYPES: BEGIN OF key_dict,
             spart TYPE spart,
           END OF key_dict.

    DATA def TYPE tspa READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !key TYPE key_dict
      RETURNING VALUE(obj) TYPE REF TO zcl_sd_division
      RAISING   cx_no_entry_in_table.

    CLASS-METHODS get_division_txt
      IMPORTING
        !iv_spart       TYPE tspat-spart
        !iv_spras       TYPE tspat-spras DEFAULT sy-langu
      RETURNING
        VALUE(rv_vtext) TYPE tspat-vtext.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             key TYPE key_dict,
             obj TYPE REF TO zcl_sd_division,
             cx  TYPE REF TO cx_no_entry_in_table,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS key.

    TYPES tt_tspat TYPE HASHED TABLE OF tspat WITH UNIQUE KEY primary_key COMPONENTS spras spart.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'TSPA',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.
    CLASS-DATA gt_tspat TYPE tt_tspat.

    METHODS constructor
      IMPORTING !key TYPE key_dict
      RAISING   cx_no_entry_in_table.
ENDCLASS.



CLASS zcl_sd_division IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key COMPONENTS key = key
                    ] TO FIELD-SYMBOL(<multiton>).
    IF sy-subrc NE 0.
      DATA(multiton) = VALUE multiton_dict( key = key ).

      TRY.
          multiton-obj = NEW #( multiton-key ).
        CATCH cx_no_entry_in_table INTO multiton-cx ##NO_HANDLER.
      ENDTRY.

      INSERT multiton INTO TABLE multitons ASSIGNING <multiton>.
    ENDIF.

    IF <multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <multiton>-cx.
    ENDIF.

    obj = <multiton>-obj.
  ENDMETHOD.


  METHOD get_division_txt.
    IF gt_tspat IS INITIAL.
      SELECT * INTO TABLE gt_tspat FROM tspat.
    ENDIF.

    TRY.
        rv_vtext = gt_tspat[ KEY primary_key COMPONENTS
          spras = iv_spras
          spart = iv_spart
        ]-vtext.

      CATCH cx_sy_itab_line_not_found ##no_Handler .
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    SELECT SINGLE *
           FROM tspa
           WHERE spart = @key-spart
           INTO CORRESPONDING FIELDS OF @me->def.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_no_entry_in_table
        EXPORTING
          table_name = CONV #( table-def )
          entry_name = |{ key-spart }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.