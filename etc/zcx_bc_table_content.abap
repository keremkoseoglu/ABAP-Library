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
    BEGIN OF entry_processed,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '145',
        attr1 TYPE scx_attrname VALUE 'FIELDNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_processed .
  constants:
    BEGIN OF value_duplicate,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '178',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF value_duplicate .
  constants:
    BEGIN OF table_empty,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '179',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF table_empty .
  constants:
    BEGIN OF column_values_inconsistent,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '182',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF column_values_inconsistent .
  constants:
    BEGIN OF column_values_duplicate,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '188',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF column_values_duplicate .
  constants:
    BEGIN OF entry_present,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '240',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_present .
  constants:
    BEGIN OF table_fld_cross_inconsistent,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '261',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'TABNAME2',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF table_fld_cross_inconsistent .
  constants:
    BEGIN OF no_suitable_entry_found,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '278',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_suitable_entry_found .
  constants:
    BEGIN OF entry_field_initial,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '279',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF entry_field_initial .
  constants:
    BEGIN OF rate_sum_not_100,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '312',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF rate_sum_not_100 .
  constants:
    BEGIN OF table_flds_simult_filled,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '325',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF table_flds_simult_filled .
  constants:
    BEGIN OF no_entry_with_fld_val_found,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '364',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_entry_with_fld_val_found .
  constants:
    BEGIN OF dup_entry_with_fld_val_found,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '365',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF dup_entry_with_fld_val_found .
  constants:
    BEGIN OF value_unexpected,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '380',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE 'FIELD_VALUE',
      END OF value_unexpected .
  constants:
    BEGIN OF multiple_entries_for_key,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '384',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF multiple_entries_for_key .
  constants:
    BEGIN OF field_values_same,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '437',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE 'FIELDNAME2',
      END OF field_values_same .
  constants:
    BEGIN OF empty_column_value_found,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '518',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELDNAME',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF empty_column_value_found .
  constants:
    BEGIN OF fill_only_one_field,
        msgid TYPE symsgid VALUE 'ZCO',
        msgno TYPE symsgno VALUE '146',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'FIELD_LIST',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF fill_only_one_field .
  constants:
    BEGIN OF entrys_need_single_full_field,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '525',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF entrys_need_single_full_field .
  constants:
    BEGIN OF field_values_vary,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '599',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE 'OBJECTID',
        attr3 TYPE scx_attrname VALUE 'FIELDNAME',
        attr4 TYPE scx_attrname VALUE '',
      END OF field_values_vary .
  constants:
    BEGIN OF overlapping_ranges,
        msgid TYPE symsgid VALUE 'ZFI',
        msgno TYPE symsgno VALUE '308',
        attr1 TYPE scx_attrname VALUE 'TABNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF overlapping_ranges .
  constants:
    begin of ENTRYS_NEED_SINGLE_EMPTY_FIELD,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '622',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value 'FIELDNAME',
      attr4 type scx_attrname value '',
    end of ENTRYS_NEED_SINGLE_EMPTY_FIELD .
  constants:
    begin of SOME_FIELDS_EMPTY,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '660',
      attr1 type scx_attrname value 'FIELDNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SOME_FIELDS_EMPTY .
  constants:
    begin of GAP_BETWEEN_FIELDS,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '661',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value 'FIELDNAME',
      attr4 type scx_attrname value 'FIELDNAME2',
    end of GAP_BETWEEN_FIELDS .
  data OBJECTID type CDHDR-OBJECTID .
  data TABNAME type TABNAME .
  data FIELDNAME type FIELDNAME .
  data TABNAME2 type TABNAME .
  data FIELD_VALUE type TEXT40 .
  data FIELDNAME2 type FIELDNAME .
  data FIELD_LIST type STRING .

  class-methods RAISE_FROM_ADDICT
    importing
      !ERROR type ref to YCX_ADDICT_TABLE_CONTENT
    raising
      ZCX_BC_TABLE_CONTENT .
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !OBJECTID type CDHDR-OBJECTID optional
      !TABNAME type TABNAME optional
      !FIELDNAME type FIELDNAME optional
      !TABNAME2 type TABNAME optional
      !FIELD_VALUE type TEXT40 optional
      !FIELDNAME2 type FIELDNAME optional
      !FIELD_LIST type STRING optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_TABLE_CONTENT IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->objectid = objectid .
    me->tabname = tabname .
    me->fieldname = fieldname .
    me->tabname2 = tabname2 .
    me->field_value = field_value .
    me->fieldname2 = fieldname2 .
    me->field_list = field_list .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_from_addict.
    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        previous  = error
        objectid  = error->objectid
        tabname   = error->tabname
        fieldname = error->fieldname.
  ENDMETHOD.
ENDCLASS.