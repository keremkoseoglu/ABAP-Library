CLASS ZCL_PP_PROD_ORDER DEFINITION PUBLIC FINAL CREATE PRIVATE.

    PUBLIC SECTION.
      TYPES: BEGIN OF key_dict,
               aufnr TYPE aufnr,
             END OF key_dict.
  
      DATA def TYPE aufk READ-ONLY.
  
      CLASS-METHODS get_instance
        IMPORTING !key TYPE key_dict
        RETURNING VALUE(obj) TYPE REF TO zcl_pp_prod_order
        RAISING   cx_no_entry_in_table.
  
    PROTECTED SECTION.
  
    PRIVATE SECTION.
      TYPES: BEGIN OF multiton_dict,
               key TYPE key_dict,
               obj TYPE REF TO zcl_pp_prod_order,
               cx  TYPE REF TO cx_no_entry_in_table,
             END OF multiton_dict,
  
             multiton_set TYPE HASHED TABLE OF multiton_dict
                          WITH UNIQUE KEY primary_key COMPONENTS key.
  
      CONSTANTS: BEGIN OF table,
                   def TYPE tabname VALUE 'AUFK',
                 END OF table.
  
      CLASS-DATA multitons TYPE multiton_set.
  
      METHODS constructor
        IMPORTING !key TYPE key_dict
        RAISING   cx_no_entry_in_table.
  ENDCLASS.
  
  CLASS ZCL_PP_PROD_ORDER IMPLEMENTATION.
    METHOD constructor.
      SELECT SINGLE *
             FROM aufk
             WHERE aufnr = @key-aufnr
             INTO CORRESPONDING FIELDS OF @me->def.
  
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_no_entry_in_table
          EXPORTING
            table_name = CONV #( table-def )
            entry_name = |{ key-aufnr }|.
      ENDIF.
    ENDMETHOD.
  
  
    METHOD get_instance.
      ASSIGN multitons[ KEY primary_key COMPONENTS key = key
                      ] TO FIELD-SYMBOL(<multiton>).
      IF sy-subrc <> 0.
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
  ENDCLASS.
  