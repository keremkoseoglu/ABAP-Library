CLASS zcl_bc_printer DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_printer.

    CLASS-METHODS get_instance
      IMPORTING padest        TYPE rspopname
      RETURNING VALUE(result) TYPE REF TO zcl_bc_printer
      RAISING   ycx_addict_object.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             padest TYPE rspopname,
             obj    TYPE REF TO zcl_bc_printer,
             error  TYPE REF TO ycx_addict_object,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict WITH UNIQUE KEY primary_key COMPONENTS padest.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'TSP03',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    DATA padest TYPE rspopname.

    METHODS constructor IMPORTING padest TYPE rspopname
                        RAISING   ycx_addict_object.
ENDCLASS.


CLASS zcl_bc_printer IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_bc_printer=>multitons[ KEY primary_key
                                                     padest = padest ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE multiton_dict( padest = padest ).

        TRY.
            new_mt-obj = NEW #( new_mt-padest ).
          CATCH ycx_addict_object INTO new_mt-error ##NO_HANDLER.
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_bc_printer=>multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->error IS NOT INITIAL.
      RAISE EXCEPTION mt->error.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM tsp03
           FIELDS @abap_true
           WHERE padest = @padest
           INTO @DATA(printer_exists).

    IF printer_exists = abap_false.
      RAISE EXCEPTION NEW ycx_addict_object( textid   = ycx_addict_object=>cant_create_instance
                                             clsname  = ycl_addict_class=>get_class_name( me )
                                             previous = NEW ycx_addict_table_content(
                                                 textid   = ycx_addict_table_content=>no_entry_for_objectid
                                                 tabname  = zcl_bc_printer=>table-def
                                                 objectid = CONV #( padest ) ) ).
    ENDIF.

    me->padest = padest.
  ENDMETHOD.

  METHOD zif_bc_printer~get_padest.
    result = me->padest.
  ENDMETHOD.
ENDCLASS.