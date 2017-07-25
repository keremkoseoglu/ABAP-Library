class ZCX_BC_DATA_FORMAT definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of CANT_EXTRACT_FIELD,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '063',
      attr1 type scx_attrname value 'FIELDNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_EXTRACT_FIELD .
  constants:
    begin of FIELD_INITIAL,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '093',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'FIELDNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FIELD_INITIAL .
  constants:
    begin of FIELD_MISSING_IN_TAB,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '023',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'FIELDNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FIELD_MISSING_IN_TAB .
  constants:
    begin of VALUE_FORMAT_INVALID,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '236',
      attr1 type scx_attrname value 'VALUE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of VALUE_FORMAT_INVALID .
  data TABNAME type TABNAME .
  data FIELDNAME type FIELDNAME .
  data VALUE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TABNAME type TABNAME optional
      !FIELDNAME type FIELDNAME optional
      !value type string optional.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_DATA_FORMAT IMPLEMENTATION.


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

    me->tabname   = tabname.
    me->fieldname = fieldname.
    me->value     = value.

  ENDMETHOD.
ENDCLASS.