class ZCX_BC_WORK_AREA_STRUCTURE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of FIELD_MISSING,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '322',
      attr1 type scx_attrname value 'FIELDNAME',
      attr2 type scx_attrname value 'WORKAREA',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FIELD_MISSING .
  data WORKAREA type FIELDNAME .
  data FIELDNAME type FIELDNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !WORKAREA type FIELDNAME optional
      !FIELDNAME type FIELDNAME optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_WORK_AREA_STRUCTURE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->WORKAREA = WORKAREA .
me->FIELDNAME = FIELDNAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.