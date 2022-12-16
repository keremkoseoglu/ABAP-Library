CLASS zcl_bc_text_pool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING !cprog TYPE sycprog DEFAULT sy-cprog
                !langu TYPE sylangu DEFAULT sy-langu.

    METHODS get_text_symbol
      IMPORTING !key          TYPE textpool-key
      RETURNING VALUE(result) TYPE textpool-entry.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES pool_set TYPE HASHED TABLE OF textpool WITH UNIQUE KEY primary_key COMPONENTS id key.

    CONSTANTS: BEGIN OF id,
                 text_symbol TYPE textpool-id VALUE 'I',
               END OF id.

    DATA pool TYPE pool_set.
ENDCLASS.



CLASS zcl_bc_text_pool IMPLEMENTATION.
  METHOD constructor.
    READ TEXTPOOL cprog LANGUAGE langu INTO me->pool.
  ENDMETHOD.


  METHOD get_text_symbol.
    result = VALUE #( me->pool[ KEY primary_key COMPONENTS
                                id  = me->id-text_symbol
                                key = key
                              ]-entry
                      DEFAULT space ).
  ENDMETHOD.
ENDCLASS.