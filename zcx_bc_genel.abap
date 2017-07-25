class ZCX_BC_GENEL definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of INVALID_DATERANGE,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '297',
      attr1 type scx_attrname value 'FIELDTEXT',
      attr2 type scx_attrname value 'LIMIT',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_DATERANGE .
  data FIELDTEXT type SCRTEXT_L .
  data LIMIT type ZSDD_LIMIT_DAY .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !FIELDTEXT type SCRTEXT_L optional
      !LIMIT type ZSDD_LIMIT_DAY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_GENEL IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->FIELDTEXT = FIELDTEXT .
me->LIMIT = LIMIT .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.