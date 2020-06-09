class ZCX_MM_MATERIAL_VIEW definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ALL_VIEWS_MISSING,
      msgid type symsgid value 'ZMM',
      msgno type symsgno value '402',
      attr1 type scx_attrname value 'MATNR',
      attr2 type scx_attrname value 'WERKS',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ALL_VIEWS_MISSING .
  constants:
    begin of VIEW_MISSING,
      msgid type symsgid value 'ZMM',
      msgno type symsgno value '403',
      attr1 type scx_attrname value 'MATNR',
      attr2 type scx_attrname value 'WERKS',
      attr3 type scx_attrname value 'VIEW',
      attr4 type scx_attrname value '',
    end of VIEW_MISSING .
  data MATNR type MATNR .
  data WERKS type WERKS_D .
  data VIEW type STTXT .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MATNR type MATNR optional
      !WERKS type WERKS_D optional
      !VIEW type STTXT optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MM_MATERIAL_VIEW IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MATNR = MATNR .
me->WERKS = WERKS .
me->VIEW = VIEW .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.