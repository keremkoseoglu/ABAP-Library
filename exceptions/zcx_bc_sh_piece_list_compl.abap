class ZCX_BC_SH_PIECE_LIST_COMPL definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_BC_SH_PIECE_LIST_COMPL,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '902',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_BC_SH_PIECE_LIST_COMPL .
  data TRKORR type TRKORR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TRKORR type TRKORR optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_SH_PIECE_LIST_COMPL IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TRKORR = TRKORR .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_BC_SH_PIECE_LIST_COMPL .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.