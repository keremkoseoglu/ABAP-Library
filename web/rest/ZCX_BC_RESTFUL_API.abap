class ZCX_BC_RESTFUL_API definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of SOLE_OPERATION,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '591',
      attr1 type scx_attrname value 'APIID',
      attr2 type scx_attrname value 'OPERATION',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SOLE_OPERATION .
  data APIID type ZBCD_RESTFUL_APIID .
  data OPERATION type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !APIID type ZBCD_RESTFUL_APIID optional
      !OPERATION type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_RESTFUL_API IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->APIID = APIID .
me->OPERATION = OPERATION .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.