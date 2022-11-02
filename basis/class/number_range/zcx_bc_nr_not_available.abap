CLASS zcx_bc_nr_not_available DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF locked,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'NR_RANGE',
        attr2 TYPE scx_attrname VALUE 'OBJECT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF locked .

    CONSTANTS:
      BEGIN OF undefined,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'NR_RANGE',
        attr2 TYPE scx_attrname VALUE 'OBJECT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF undefined .

    DATA nr_range TYPE inri-nrrangenr .
    DATA object TYPE inri-object .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !nr_range TYPE inri-nrrangenr OPTIONAL
        !object   TYPE inri-object OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_NR_NOT_AVAILABLE IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->nr_range = nr_range .
    me->object = object .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.