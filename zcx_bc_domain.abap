CLASS zcx_bc_domain DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF invalid_value,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '090',
        attr1 TYPE scx_attrname VALUE 'DOMNAME',
        attr2 TYPE scx_attrname VALUE 'DOMVALUE_L',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_value.

    DATA: domname    TYPE domname,
          domvalue_l TYPE domvalue_l.

    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !domname    TYPE domname OPTIONAL
        !domvalue_l TYPE domvalue_l OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_domain IMPLEMENTATION.


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

    me->domname = domname.
    me->domvalue_l = domvalue_l.

  ENDMETHOD.
ENDCLASS.