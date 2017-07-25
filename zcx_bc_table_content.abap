class ZCX_BC_TABLE_CONTENT definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF entry_invalid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_invalid .
  constants:
    BEGIN OF entry_missing,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'OBJECTID',
        attr2 TYPE scx_attrname VALUE 'TABNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_missing .
  constants:
    BEGIN OF value_group_invalid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF value_group_invalid .
  constants:
    BEGIN OF value_invalid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF value_invalid .
  constants:
    BEGIN OF value_missing,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF value_missing .
  constants:
    BEGIN OF query_value_not_found,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '122',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF query_value_not_found .
  constants:
    begin of ENTRY_PROCESSED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '145',
      attr1 type scx_attrname value 'FIELDNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ENTRY_PROCESSED .
  constants:
    begin of VALUE_DUPLICATE,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '178',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'FIELDNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of VALUE_DUPLICATE .
  constants:
    begin of TABLE_EMPTY,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '179',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TABLE_EMPTY .
  constants:
    begin of COLUMN_VALUES_INCONSISTENT,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '182',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'FIELDNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of COLUMN_VALUES_INCONSISTENT .
  constants:
    begin of COLUMN_VALUES_DUPLICATE,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '188',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'FIELDNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of COLUMN_VALUES_DUPLICATE .
  constants:
    begin of ENTRY_PRESENT,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '240',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ENTRY_PRESENT .
  constants:
    begin of TABLE_FLD_CROSS_INCONSISTENT,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '261',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'TABNAME2',
      attr3 type scx_attrname value 'FIELDNAME',
      attr4 type scx_attrname value '',
    end of TABLE_FLD_CROSS_INCONSISTENT .
  data OBJECTID type CDHDR-OBJECTID .
  data TABNAME type TABNAME .
  data FIELDNAME type FIELDNAME .
  data TABNAME2 type TABNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !OBJECTID type CDHDR-OBJECTID optional
      !TABNAME type TABNAME optional
      !tabname2 type tabname optional
      !FIELDNAME type FIELDNAME optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_TABLE_CONTENT IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->objectid  = objectid.
    me->tabname   = tabname.
    me->tabname2  = tabname.
    me->fieldname = fieldname.
  ENDMETHOD.
ENDCLASS.