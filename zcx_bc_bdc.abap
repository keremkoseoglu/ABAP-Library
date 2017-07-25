class ZCX_BC_BDC definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_BC_BDC,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '901',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BC_BDC .
  data BDCMSGCOLL type TAB_BDCMSGCOLL .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !BDCMSGCOLL type TAB_BDCMSGCOLL optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_BDC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->BDCMSGCOLL = BDCMSGCOLL .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_BC_BDC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.