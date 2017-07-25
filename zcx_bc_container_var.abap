CLASS zcx_bc_container_var DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

  constants:
    begin of VAR_MISSING,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '078',
      attr1 type scx_attrname value 'CONTEXT',
      attr2 type scx_attrname value 'NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of VAR_MISSING .

    data: context type string,
          name    type string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !context  type string optional
        !name     type string optional.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_container_var IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->context = context.
    me->name    = name.

  ENDMETHOD.
ENDCLASS.