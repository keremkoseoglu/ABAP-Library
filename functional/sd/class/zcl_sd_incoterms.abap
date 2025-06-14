CLASS zcl_sd_incoterms DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    DATA inco1 TYPE inco1 READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING inco1         TYPE inco1
      RETURNING VALUE(result) TYPE REF TO zcl_sd_incoterms
      RAISING   ycx_addict_table_content.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             inco1 TYPE inco1,
             obj   TYPE REF TO zcl_sd_incoterms,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS inco1.

    CONSTANTS: BEGIN OF table,
                 tinc TYPE tabname VALUE 'TINC',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    METHODS constructor IMPORTING inco1 TYPE inco1.
ENDCLASS.


CLASS zcl_sd_incoterms IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( multitons[ KEY primary_key
                                     inco1 = inco1 ] ).
      CATCH cx_sy_itab_line_not_found.
        SELECT SINGLE FROM tinc
               FIELDS @abap_true
               WHERE inco1 = @inco1
               INTO @DATA(found_in_db).

        IF found_in_db = abap_false.
          RAISE EXCEPTION NEW ycx_addict_table_content( textid   = ycx_addict_table_content=>no_entry_for_objectid
                                                        tabname  = table-tinc
                                                        objectid = CONV #( inco1 ) ).
        ENDIF.

        INSERT VALUE #( inco1 = inco1
                        obj   = NEW #( inco1 ) ) INTO TABLE multitons REFERENCE INTO mt.

    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD constructor.
    me->inco1 = inco1.
  ENDMETHOD.
ENDCLASS.