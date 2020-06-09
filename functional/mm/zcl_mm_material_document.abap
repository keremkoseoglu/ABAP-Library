CLASS zcl_mm_material_document DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES: BEGIN OF mat_doc_key_dict,
             mblnr TYPE mkpf-mblnr,
             mjahr TYPE mkpf-mjahr,
           END OF mat_doc_key_dict,

           mat_doc_key_list TYPE STANDARD TABLE OF mat_doc_key_dict WITH EMPTY KEY.

    DATA mat_doc_key TYPE mat_doc_key_dict READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
        !mat_doc_key   TYPE mat_doc_key_dict
        !bypass_buffer TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(output)  TYPE REF TO zcl_mm_material_document
      RAISING
        cx_no_entry_in_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF multiton_dict,
             key TYPE mat_doc_key_dict,
             obj TYPE REF TO zcl_mm_material_document,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS key.

    CONSTANTS: BEGIN OF table,
                 header TYPE tabname VALUE 'MKPF',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    METHODS constructor
      IMPORTING !mat_doc_key TYPE mat_doc_key_dict
      RAISING   cx_no_entry_in_table.

ENDCLASS.



CLASS zcl_mm_material_document IMPLEMENTATION.

  METHOD get_instance.
    IF bypass_buffer = abap_true.
      output = NEW #( mat_doc_key ).
      RETURN.
    ENDIF.

    ASSIGN zcl_mm_material_document=>multitons[
        KEY primary_key COMPONENTS
        key = mat_doc_key ] TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc NE 0.
      INSERT VALUE #(
          key = mat_doc_key
          obj = NEW #( mat_doc_key )
        ) INTO TABLE zcl_mm_material_document=>multitons
          ASSIGNING <multiton>.
    ENDIF.

    output = <multiton>-obj.
  ENDMETHOD.


  METHOD constructor.
    DATA(mblnr) = VALUE mblnr( ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = mat_doc_key-mblnr
      IMPORTING
        output = mblnr.

    SELECT SINGLE mblnr, mjahr
           FROM mkpf
           WHERE mblnr = @mblnr AND
                 mjahr = @mat_doc_key-mjahr
           INTO CORRESPONDING FIELDS OF @me->mat_doc_key.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_no_entry_in_table
        EXPORTING
          table_name = CONV #( table-header )
          entry_name = |{ mat_doc_key-mblnr }{ mat_doc_key-mjahr }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.