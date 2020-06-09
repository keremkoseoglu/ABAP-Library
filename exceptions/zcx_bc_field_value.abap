CLASS zcx_bc_field_value DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF missing_value,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '259',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE 'DDTEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_value .
    CONSTANTS:
      BEGIN OF invalid_value,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '258',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE 'VALUE',
        attr3 TYPE scx_attrname VALUE 'DDTEXT',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_value .
    DATA fieldname TYPE fieldname .
    DATA value TYPE string .
    DATA ddtext TYPE ddtext .

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !fieldname TYPE fieldname OPTIONAL
        !value     TYPE string OPTIONAL
        !ddtext    TYPE ddtext OPTIONAL .

    class-METHODS raise_invalid_value
      IMPORTING
        !iv_fieldname TYPE fieldname
        !iv_rollname  TYPE rollname optional
        !iv_value     TYPE string
      RAISING
        zcx_bc_field_value.

    class-METHODS raise_missing_value
      IMPORTING
        !iv_fieldname TYPE fieldname
        !iv_rollname  TYPE rollname optional
      RAISING
        zcx_bc_field_value.

  PROTECTED SECTION.
  PRIVATE SECTION.



ENDCLASS.



CLASS zcx_bc_field_value IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->fieldname = fieldname .
    me->value = value .
    me->ddtext = ddtext .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_invalid_value.

    RAISE EXCEPTION TYPE zcx_bc_field_value
      EXPORTING
        textid    = zcx_bc_field_value=>invalid_value
        fieldname = iv_fieldname
        ddtext    = zcl_bc_data_element=>get_text_safe( iv_rollname )
        value     = iv_value.

  ENDMETHOD.

  METHOD raise_missing_value.

    RAISE EXCEPTION TYPE zcx_bc_field_value
      EXPORTING
        textid    = zcx_bc_field_value=>missing_value
        fieldname = iv_fieldname
        ddtext    = zcl_bc_data_element=>get_text_safe( iv_rollname ).

  ENDMETHOD.

ENDCLASS.