class ZCX_BC_USER_INPUT definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF user_cancelled,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '120',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_cancelled .
  constants:
    begin of NO_ITEM_SELECTED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '163',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_ITEM_SELECTED .
  constants:
    begin of NOT_SAVED_YET,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '203',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_SAVED_YET .
  constants:
    begin of NOT_SAVED_AND_SELECTED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '207',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_SAVED_AND_SELECTED .
  constants:
    begin of INVALID_INPUT,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '244',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_INPUT .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_USER_INPUT IMPLEMENTATION.


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