class ZCX_BC_USER_INPUT definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF user_cancelled,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '120',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_cancelled .
  constants:
    BEGIN OF no_item_selected,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '163',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_item_selected .
  constants:
    BEGIN OF not_saved_yet,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '203',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_saved_yet .
  constants:
    BEGIN OF not_saved_and_selected,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '207',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_saved_and_selected .
  constants:
    BEGIN OF invalid_input,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '244',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_input .
  constants:
    BEGIN OF duplicate_values,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '433',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_values .
  constants:
    BEGIN OF duplicate_key,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '487',
        attr1 TYPE scx_attrname VALUE 'OBJID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_key .
  constants:
    BEGIN OF no_value_copied,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '505',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_value_copied .
  constants:
    BEGIN OF value_cant_be_less,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '506',
        attr1 TYPE scx_attrname VALUE 'OBJID',
        attr2 TYPE scx_attrname VALUE 'OBJID2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF value_cant_be_less .
  constants:
    BEGIN OF invalid_upload_data,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '512',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_upload_data .
  constants:
    BEGIN OF duplicate_field_values,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '513',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_field_values .
  constants:
    BEGIN OF initial_field_values,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '514',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF initial_field_values .
  constants:
    BEGIN OF multiple_initial_field_values,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '515',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF multiple_initial_field_values .
  constants:
    BEGIN OF no_initial_field_value,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '516',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_initial_field_value .
  constants:
    BEGIN OF key_entry_not_found,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '517',
        attr1 TYPE scx_attrname VALUE 'OBJID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF key_entry_not_found .
  constants:
    BEGIN OF field_is_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '519',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_is_initial .
  constants:
    BEGIN OF field_group_is_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '520',
        attr1 TYPE scx_attrname VALUE 'OBJID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_group_is_initial .
  constants:
    BEGIN OF field_must_be_less_than,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '521',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_must_be_less_than .
  constants:
    BEGIN OF field_value_invalid,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '522',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_value_invalid .
  constants:
    BEGIN OF both_fields_empty,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '523',
        attr1 TYPE scx_attrname VALUE 'OBJID',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME2',
        attr4 TYPE scx_attrname VALUE '',
      END OF both_fields_empty .
  constants:
    BEGIN OF entry_fields_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '524',
        attr1 TYPE scx_attrname VALUE 'OBJID',
        attr2 TYPE scx_attrname VALUE 'OBJID2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_fields_initial .
  constants:
    BEGIN OF canned_data_in_bg,
        msgid TYPE symsgid VALUE 'ZMM',
        msgno TYPE symsgno VALUE '410',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF canned_data_in_bg .
  constants:
    BEGIN OF invalid_selection_screen,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '543',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_selection_screen .
  constants:
    BEGIN OF selection_screen_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '544',
        attr1 TYPE scx_attrname VALUE 'ERROR_DESCRIPTION',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF selection_screen_error .
  constants:
    BEGIN OF field_cant_be_greater_than,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '583',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE 'OBJID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_cant_be_greater_than .
  constants:
    begin of NO_VALUE_CHANGED,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '633',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_VALUE_CHANGED .
  constants:
    begin of MUST_RUN_IN_BG,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '640',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MUST_RUN_IN_BG .
  constants:
    begin of CHAR_OBLIGATORY,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '642',
      attr1 type scx_attrname value 'FIELDNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CHAR_OBLIGATORY .
  constants:
    begin of UPLOAD_EMPTY,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '659',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UPLOAD_EMPTY .
  data OBJID type STRING .
  data OBJID2 type STRING .
  data FIELDNAME type FIELDNAME .
  data FIELDNAME2 type FIELDNAME .
  data ERROR_DESCRIPTION type STRING .

  class-methods RAISE_FROM_ADDICT
    importing
      !ERROR type ref to YCX_ADDICT_USER_INPUT
    raising
      ZCX_BC_USER_INPUT .
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !OBJID type STRING optional
      !OBJID2 type STRING optional
      !FIELDNAME type FIELDNAME optional
      !FIELDNAME2 type FIELDNAME optional
      !ERROR_DESCRIPTION type STRING optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_USER_INPUT IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->objid = objid .
    me->objid2 = objid2 .
    me->fieldname = fieldname .
    me->fieldname2 = fieldname2 .
    me->error_description = error_description .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_from_addict.
    RAISE EXCEPTION TYPE zcx_bc_user_input
      EXPORTING
        textid   = error->if_t100_message~t100key
        previous = error.
  ENDMETHOD.
ENDCLASS.