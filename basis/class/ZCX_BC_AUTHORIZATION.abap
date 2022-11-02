class ZCX_BC_AUTHORIZATION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF no_auth,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '103',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_auth .
  constants:
    BEGIN OF no_auth_for_objectid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '141',
        attr1 TYPE scx_attrname VALUE 'OBJECTID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_auth_for_objectid .
  constants:
    BEGIN OF no_auth_for_entered_val,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '143',
        attr1 TYPE scx_attrname VALUE 'DATA_TYPE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_auth_for_entered_val .
  constants:
    begin of AUTH_CHECK_FAILED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '491',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of AUTH_CHECK_FAILED .
  constants:
    begin of APPROVAL_AUTH_CHECK_ERROR,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '528',
      attr1 type scx_attrname value 'OBJECTID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of APPROVAL_AUTH_CHECK_ERROR .
  constants:
    begin of NO_APPROVAL_AUTH,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '529',
      attr1 type scx_attrname value 'OBJECTID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_APPROVAL_AUTH .
  constants:
    begin of NO_APPROVAL_NEEDED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '530',
      attr1 type scx_attrname value 'OBJECTID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_APPROVAL_NEEDED .
  data OBJECTID type CDHDR-OBJECTID .
  data DATA_TYPE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !OBJECTID type CDHDR-OBJECTID optional
      !DATA_TYPE type STRING optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_AUTHORIZATION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->OBJECTID = OBJECTID .
me->DATA_TYPE = DATA_TYPE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.