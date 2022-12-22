CLASS ZCL_SD_DLV_TYPE DEFINITION PUBLIC FINAL CREATE PRIVATE.

    PUBLIC SECTION.
      TYPES: BEGIN OF key_dict,
               lfart TYPE lfart,
             END OF key_dict.
  
      DATA def TYPE tvlk READ-ONLY.
  
      CLASS-METHODS get_instance
        IMPORTING !key TYPE key_dict
        RETURNING VALUE(obj) TYPE REF TO zcl_sd_dlv_type
        RAISING   cx_no_entry_in_table.
  
    PROTECTED SECTION.
  
    PRIVATE SECTION.
      TYPES: BEGIN OF multiton_dict,
               key TYPE key_dict,
               obj TYPE REF TO zcl_sd_dlv_type,
               cx  TYPE REF TO cx_no_entry_in_table,
             END OF multiton_dict,
  
             multiton_set TYPE HASHED TABLE OF multiton_dict
                          WITH UNIQUE KEY primary_key COMPONENTS key.
  
      CONSTANTS: BEGIN OF table,
                   def TYPE tabname VALUE 'TVLK',
                 END OF table.
  
      CLASS-DATA multitons TYPE multiton_set.
  
      METHODS constructor
        IMPORTING !key TYPE key_dict
        RAISING   cx_no_entry_in_table.
  ENDCLASS.
  
  CLASS ZCL_SD_DLV_TYPE IMPLEMENTATION.
    METHOD constructor.
      SELECT SINGLE *
             FROM tvlk
             WHERE lfart = @key-lfart
             INTO CORRESPONDING FIELDS OF @me->def.
  
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_no_entry_in_table
          EXPORTING
            table_name = CONV #( table-def )
            entry_name = |{ key-lfart }|.
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
  