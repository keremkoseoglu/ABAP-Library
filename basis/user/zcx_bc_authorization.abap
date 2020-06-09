CLASS zcx_bc_authorization DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF no_auth,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '103',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_auth .
    CONSTANTS:
      BEGIN OF no_auth_for_objectid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '141',
        attr1 TYPE scx_attrname VALUE 'OBJECTID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_auth_for_objectid .
    CONSTANTS:
      BEGIN OF no_auth_for_entered_val,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '143',
        attr1 TYPE scx_attrname VALUE 'DATA_TYPE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_auth_for_entered_val .
    DATA objectid TYPE cdhdr-objectid .
    DATA data_type TYPE string .

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !objectid  TYPE cdhdr-objectid OPTIONAL
        !data_type TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_authorization IMPLEMENTATION.


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
    me->data_type = data_type.

  ENDMETHOD.
ENDCLASS.