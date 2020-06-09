class ZCX_BC_CLASS_METHOD definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF call_After_operation,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '147',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF call_after_operation .
  constants:
    BEGIN OF unexpected_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '135',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unexpected_error .
  constants:
    begin of CALL_BEFORE_OPERATION,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '186',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CALL_BEFORE_OPERATION .
  constants:
    begin of ALREADY_CALLED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '247',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ALREADY_CALLED .
  constants:
    begin of INCORRECT_LINES,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '301',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INCORRECT_LINES .
  data CLASS type SEOCLSNAME .
  data METHOD type SEOCPDNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !CLASS type SEOCLSNAME
      !METHOD type SEOCPDNAME .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_CLASS_METHOD IMPLEMENTATION.


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

    me->class  = class.
    me->method = method.

  ENDMETHOD.
ENDCLASS.