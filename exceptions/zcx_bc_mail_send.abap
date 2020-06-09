class ZCX_BC_MAIL_SEND definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of CANT_SEND,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '020',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_SEND .
    CONSTANTS :
    begin of CANT_SEND_NOITEM,
      msgid type symsgid value 'ZSD',
      msgno type symsgno value '334',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_SEND_NOITEM .

  constants:
    begin of COLUMN_NUMBER_NOT_VALID,
      msgid type symsgid value 'ZSD',
      msgno type symsgno value '052',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of COLUMN_NUMBER_NOT_VALID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_MAIL_SEND IMPLEMENTATION.


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
  ENDMETHOD.
ENDCLASS.