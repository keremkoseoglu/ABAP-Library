CLASS zcx_bc_method_parameter DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF param_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_error .
    CONSTANTS:
      BEGIN OF param_missing,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '148',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_missing .
    CONSTANTS:
      BEGIN OF param_value_invalid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '180',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_value_invalid .
    CONSTANTS:
      BEGIN OF param_value_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '321',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_value_initial .
    CONSTANTS:
      BEGIN OF param_pair_inconsistent,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '323',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE 'PARAM_NAME_2',
      END OF param_pair_inconsistent .
    DATA class_name TYPE seoclsname .
    DATA method_name TYPE seocpdname .
    DATA param_name TYPE seocpdname .
    DATA param_name_2 TYPE seocpdname .

    METHODS constructor
      IMPORTING
        !textid       LIKE if_t100_message=>t100key OPTIONAL
        !previous     LIKE previous OPTIONAL
        !class_name   TYPE seoclsname OPTIONAL
        !method_name  TYPE seocpdname OPTIONAL
        !param_name   TYPE seocpdname OPTIONAL
        !param_name_2 TYPE seocpdname OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_method_parameter IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.
    me->class_name  = class_name.
    me->method_name = method_name.
    me->param_name  = param_name.
    me->param_name_2 = param_name_2.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.