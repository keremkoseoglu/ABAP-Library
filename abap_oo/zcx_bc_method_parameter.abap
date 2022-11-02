class ZCX_BC_METHOD_PARAMETER definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF param_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_error .
  constants:
    BEGIN OF param_missing,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '148',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_missing .
  constants:
    BEGIN OF param_value_invalid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '180',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_value_invalid .
  constants:
    BEGIN OF param_value_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '421',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_value_initial .
  constants:
    BEGIN OF param_pair_inconsistent,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '423',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE 'PARAM_NAME_2',
      END OF param_pair_inconsistent .
  constants:
    BEGIN OF cant_call_without_param,
        msgid TYPE symsgid VALUE 'ZMM',
        msgno TYPE symsgno VALUE '327',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cant_call_without_param .
  constants:
    BEGIN OF range_can_only_have_singular,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '385',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE 'PARAM_NAME_2',
      END OF range_can_only_have_singular .
  constants:
    BEGIN OF range_has_i_and_e,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '386',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF range_has_i_and_e .
  constants:
    BEGIN OF field_missing_in_str,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '399',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE 'PARAM_NAME_2',
      END OF field_missing_in_str .
  constants:
    BEGIN OF no_order_enabled_mat,
        msgid TYPE symsgid VALUE 'ZSD',
        msgno TYPE symsgno VALUE '915',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_order_enabled_mat .
  constants:
    BEGIN OF param_not_cached_yet,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '440',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE 'METHOD_NAME',
        attr3 TYPE scx_attrname VALUE 'PARAM_NAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_not_cached_yet .
  constants:
    begin of DUPLICATE_VALUES,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '623',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value '',
    end of DUPLICATE_VALUES .
  constants:
    begin of SIMULTANEOUS_VALUES,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '655',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value 'PARAM_NAME_2',
    end of SIMULTANEOUS_VALUES .
  data CLASS_NAME type SEOCLSNAME .
  data METHOD_NAME type SEOCPDNAME .
  data PARAM_NAME type SEOCPDNAME .
  data PARAM_NAME_2 type SEOCPDNAME .

  class-methods RAISE_FROM_ADDICT
    importing
      !CORE type ref to YCX_ADDICT_METHOD_PARAMETER
    raising
      ZCX_BC_METHOD_PARAMETER .
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !CLASS_NAME type SEOCLSNAME optional
      !METHOD_NAME type SEOCPDNAME optional
      !PARAM_NAME type SEOCPDNAME optional
      !PARAM_NAME_2 type SEOCPDNAME optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_METHOD_PARAMETER IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->class_name = class_name .
    me->method_name = method_name .
    me->param_name = param_name .
    me->param_name_2 = param_name_2 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_from_addict.
    RAISE EXCEPTION TYPE zcx_bc_method_parameter
      EXPORTING
        previous     = core
        class_name   = core->class_name
        method_name  = core->method_name
        param_name   = core->param_name
        param_name_2 = core->param_name_2.
  ENDMETHOD.
ENDCLASS.