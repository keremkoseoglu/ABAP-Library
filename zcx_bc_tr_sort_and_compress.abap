class ZCX_BC_TR_SORT_AND_COMPRESS definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of SOC_FUNCTION_ERROR,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '316',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SOC_FUNCTION_ERROR .
  data TRKORR type TRKORR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TRKORR type TRKORR optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_TR_SORT_AND_COMPRESS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TRKORR = TRKORR .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.