CLASS zcx_bc_class_method DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF call_after_operation,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '147',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF call_after_operation .
    CONSTANTS:
      BEGIN OF unexpected_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '135',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unexpected_error .
    CONSTANTS:
      BEGIN OF call_before_operation,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '186',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF call_before_operation .
    CONSTANTS:
      BEGIN OF already_called,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '247',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF already_called .
    CONSTANTS:
      BEGIN OF incorrect_lines,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '301',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF incorrect_lines .
    CONSTANTS:
      BEGIN OF oblig_fields_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '327',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF oblig_fields_initial .
    CONSTANTS:
      BEGIN OF authorization,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '358',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF authorization .
    CONSTANTS:
      BEGIN OF unexpected_field_value,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '387',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE 'FNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF unexpected_field_value .
    CONSTANTS:
      BEGIN OF error_with_text,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '540',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE 'ERROR_TEXT',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_with_text .
    CONSTANTS:
      BEGIN OF missing_field_value,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '586',
        attr1 TYPE scx_attrname VALUE 'CLASS',
        attr2 TYPE scx_attrname VALUE 'METHOD',
        attr3 TYPE scx_attrname VALUE 'FNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_field_value .
    DATA class TYPE seoclsname .
    DATA method TYPE seocpdname .
    DATA fname TYPE fieldname .
    DATA error_text TYPE string .

    CLASS-METHODS raise_from_addict
      IMPORTING !error TYPE REF TO ycx_addict_class_method
      RAISING   zcx_bc_class_method.

    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !class      TYPE seoclsname
        !method     TYPE seocpdname
        !fname      TYPE fieldname OPTIONAL
        !error_text TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_CLASS_METHOD IMPLEMENTATION.


  METHOD raise_from_addict.
    RAISE EXCEPTION TYPE zcx_bc_class_method
      EXPORTING
        previous   = error
        class      = error->class
        method     = error->method
        fname      = error->fname
        error_text = error->error_text.
  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->class = class .
    me->method = method .
    me->fname = fname .
    me->error_text = error_text .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.