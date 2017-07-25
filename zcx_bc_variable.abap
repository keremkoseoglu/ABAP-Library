class ZCX_BC_VARIABLE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF no_eligible_rec_in_itab,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '121',
        attr1 TYPE scx_attrname VALUE 'VARNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_eligible_rec_in_itab .
  constants:
    begin of VALUE_INITIAL,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '320',
      attr1 type scx_attrname value 'VARNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of VALUE_INITIAL .
  data VARNAME type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !VARNAME type STRING optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_VARIABLE IMPLEMENTATION.


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

    me->varname = varname.

  ENDMETHOD.
ENDCLASS.