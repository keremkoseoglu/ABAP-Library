CLASS zcx_bc_singleton DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF singleton,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'ERROR_TEXT',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF singleton .
    DATA error_text TYPE string.

    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !error_text TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_singleton IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->error_text = error_text .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.