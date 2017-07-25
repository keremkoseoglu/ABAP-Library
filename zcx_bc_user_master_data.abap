class ZCX_BC_USER_MASTER_DATA definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF user_unknown,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '095',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_unknown .
  constants:
    BEGIN OF user_inactive,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '096',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_inactive .
  constants:
    BEGIN OF email_missing,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF email_missing .
  constants:
    BEGIN OF user_email_nomatch,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '125',
        attr1 TYPE scx_attrname VALUE 'EMAIL',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_email_nomatch .
  data UNAME type SYUNAME .
  data EMAIL type AD_SMTPADR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !UNAME type SYUNAME optional
      !email type ad_smtpadr optional.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_USER_MASTER_DATA IMPLEMENTATION.


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

    me->uname = uname.
    me->email = email.

  ENDMETHOD.
ENDCLASS.