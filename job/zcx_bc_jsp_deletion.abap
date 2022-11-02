class ZCX_BC_JSP_DELETION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of PROTECTED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '280',
      attr1 type scx_attrname value 'JPHID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PROTECTED .
  data JPHID type ZBCD_JSP_JPHID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !JPHID type ZBCD_JSP_JPHID optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_JSP_DELETION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->JPHID = JPHID .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.