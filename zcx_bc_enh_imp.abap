CLASS zcx_bc_enh_imp DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF imp_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '071',
        attr1 TYPE scx_attrname VALUE 'ENHID',
        attr2 TYPE scx_attrname VALUE 'ENIMP',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF imp_error .

    DATA: enhid TYPE zbcd_enhid,
          enimp TYPE zbcd_enimp.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !enhid    TYPE zbcd_enhid
        !enimp    TYPE zbcd_enimp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_enh_imp IMPLEMENTATION.


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

    me->enhid = enhid.
    me->enimp = enimp.

  ENDMETHOD.
ENDCLASS.