class ZCX_BC_JSP_JOB_SIZE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of PARAM_EXCEED_CUSTOMIZING,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '211',
      attr1 type scx_attrname value 'PJMAX',
      attr2 type scx_attrname value 'JIMAX',
      attr3 type scx_attrname value 'IMPID',
      attr4 type scx_attrname value '',
    end of PARAM_EXCEED_CUSTOMIZING .
  constants:
    begin of PJMAX_EXCEED_CUSTOMIZING,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '212',
      attr1 type scx_attrname value 'PJMAX',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PJMAX_EXCEED_CUSTOMIZING .
  constants:
    begin of JIMAX_EXCEED_CUSTOMIZING,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '213',
      attr1 type scx_attrname value 'JIMAX',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of JIMAX_EXCEED_CUSTOMIZING .
  data IMPID type ZBCD_JSP_IMPID .
  data PJMAX type ZBCD_JSP_PJMAX .
  data JIMAX type ZBCD_JSP_JIMAX .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !IMPID type ZBCD_JSP_IMPID optional
      !PJMAX type ZBCD_JSP_PJMAX optional
      !JIMAX type ZBCD_JSP_JIMAX optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_JSP_JOB_SIZE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->IMPID = IMPID .
me->PJMAX = PJMAX .
me->JIMAX = JIMAX .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.