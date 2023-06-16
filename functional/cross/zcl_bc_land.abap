CLASS zcl_bc_land DEFINITION PUBLIC FINAL CREATE PRIVATE.

    PUBLIC SECTION.
      TYPES: BEGIN OF key_dict,
               land1 TYPE land1,
             END OF key_dict.
  
      DATA def TYPE t005 READ-ONLY.
  
      CLASS-METHODS get_instance
        IMPORTING !key       TYPE key_dict
        RETURNING VALUE(obj) TYPE REF TO zcl_bc_land
        RAISING   cx_no_entry_in_table.
  
      CLASS-METHODS put_land_name_into_itab CHANGING itab TYPE STANDARD TABLE.
  
    PRIVATE SECTION.
      TYPES: BEGIN OF multiton_dict,
               key TYPE key_dict,
               obj TYPE REF TO zcl_bc_land,
               cx  TYPE REF TO cx_no_entry_in_table,
             END OF multiton_dict,
  
             multiton_set TYPE HASHED TABLE OF multiton_dict
                          WITH UNIQUE KEY primary_key COMPONENTS key.
  
      TYPES: BEGIN OF land1_dict,
               land1 TYPE land1_gp,
             END OF land1_dict,
  
             land1_list TYPE STANDARD TABLE OF land1_dict WITH KEY land1.
  
      TYPES: BEGIN OF land_name_dict,
               land1 TYPE t005t-land1,
               landx TYPE t005t-landx,
             END OF land_name_dict,
  
             land_name_set TYPE HASHED TABLE OF land_name_dict
                           WITH UNIQUE KEY primary_key COMPONENTS land1.
  
      CONSTANTS: BEGIN OF table,
                   def TYPE tabname VALUE 'T005',
                 END OF table.
  
      CONSTANTS: BEGIN OF field,
                   land1 TYPE fieldname VALUE 'LAND1',
                   landx TYPE fieldname VALUE 'LANDX',
                 END OF field.
  
      CLASS-DATA multitons TYPE multiton_set.
  
      METHODS constructor
        IMPORTING !key TYPE key_dict
        RAISING   cx_no_entry_in_table.
  ENDCLASS.
  
  
  CLASS zcl_bc_land IMPLEMENTATION.
    METHOD constructor.
      SELECT SINGLE * FROM t005
             WHERE land1 = @key-land1
             INTO CORRESPONDING FIELDS OF @me->def.
  
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( me->table-def )
                                                  entry_name = |{ key-land1 }| ).
      ENDIF.
    ENDMETHOD.
  
    METHOD put_land_name_into_itab.
      CHECK itab IS NOT INITIAL.
  
      DATA(land1s) = CORRESPONDING land1_list( itab ).
  
      DELETE land1s WHERE land1 IS INITIAL.
      SORT land1s.
      DELETE ADJACENT DUPLICATES FROM land1s.
  
      IF land1s IS INITIAL.
        RETURN.
      ENDIF.
  
      DATA(land_names) = VALUE land_name_set( ).
  
      SELECT t005t~land1, t005t~landx
             FROM   @land1s AS _lnd
                    INNER JOIN t005t ON t005t~land1 = _lnd~land1 "#EC CI_BUFFJOIN
             WHERE  t005t~spras = @sy-langu
             INTO   CORRESPONDING FIELDS OF TABLE @land_names.
  
      IF land_names IS INITIAL.
        RETURN.
      ENDIF.
  
      LOOP AT itab ASSIGNING FIELD-SYMBOL(<itab_entry>).
        ASSIGN COMPONENT field-land1 OF STRUCTURE <itab_entry> TO FIELD-SYMBOL(<land1>).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
  
        CHECK <land1> IS NOT INITIAL.
  
        ASSIGN COMPONENT field-landx OF STRUCTURE <itab_entry> TO FIELD-SYMBOL(<landx>).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
  
        <landx> = VALUE #( land_names[ KEY        primary_key
                                       COMPONENTS land1 = <land1>
                           ]-landx OPTIONAL ).
      ENDLOOP.
    ENDMETHOD.
  
    METHOD get_instance.
      ASSIGN zcl_bc_land=>multitons[ KEY        primary_key
                                     COMPONENTS key = key ]
             TO FIELD-SYMBOL(<multiton>).
  
      IF sy-subrc <> 0.
        DATA(multiton) = VALUE multiton_dict( key = key ).
  
        TRY.
            multiton-obj = NEW #( multiton-key ).
          CATCH cx_no_entry_in_table INTO multiton-cx ##NO_HANDLER.
        ENDTRY.
  
        INSERT multiton INTO TABLE zcl_bc_land=>multitons ASSIGNING <multiton>.
      ENDIF.
  
      IF <multiton>-cx IS NOT INITIAL.
        RAISE EXCEPTION <multiton>-cx.
      ENDIF.
  
      obj = <multiton>-obj.
    ENDMETHOD.
  ENDCLASS.