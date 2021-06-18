class ZCX_BC_NR_NEXT definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF cant_get_next,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'NR_RANGE',
        attr2 TYPE scx_attrname VALUE 'OBJECT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cant_get_next .
  constants:
    BEGIN OF gen_ne_next,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'NR_RANGE',
        attr2 TYPE scx_attrname VALUE 'OBJECT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gen_ne_next .
  constants:
    begin of RELATED_OVERLAP,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '260',
      attr1 type scx_attrname value 'NR_RANGE',
      attr2 type scx_attrname value 'OBJECT',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RELATED_OVERLAP .
  data NR_RANGE type INRI-NRRANGENR .
  data OBJECT type INRI-OBJECT .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !NR_RANGE type INRI-NRRANGENR optional
      !OBJECT type INRI-OBJECT optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_NR_NEXT IMPLEMENTATION.


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