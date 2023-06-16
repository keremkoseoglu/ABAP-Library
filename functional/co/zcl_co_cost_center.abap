CLASS zcl_co_cost_center DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF key_dict,
             kokrs TYPE kokrs,
             kostl TYPE kostl,
           END OF key_dict.

    TYPES: BEGIN OF def_dict,
             kokrs TYPE csks-kokrs,
             kostl TYPE csks-kostl,
             datbi TYPE csks-datbi,
           END OF def_dict.

    CONSTANTS: BEGIN OF field,
                 kokrs TYPE fieldname VALUE 'KOKRS',
                 kostl TYPE fieldname VALUE 'KOSTL',
               END OF field.

    DATA def TYPE def_dict READ-ONLY.

    CLASS-METHODS ensure_exists_for_comp
      IMPORTING !kostl TYPE kostl
                !bukrs TYPE bukrs
      RAISING   cx_no_entry_in_table.

    CLASS-METHODS cache_with_itab
      IMPORTING
        !itab        TYPE REF TO data
        !kokrs_field TYPE fieldname DEFAULT field-kokrs
        !kostl_field TYPE fieldname DEFAULT field-kostl
        !fixed_kokrs TYPE kokrs OPTIONAL.

    CLASS-METHODS get_instance
      IMPORTING !key       TYPE key_dict
      RETURNING VALUE(obj) TYPE REF TO zcl_co_cost_center
      RAISING   cx_no_entry_in_table.

    METHODS get_text RETURNING VALUE(ktext) TYPE cskt-ktext.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             key TYPE key_dict,
             obj TYPE REF TO zcl_co_cost_center,
             cx  TYPE REF TO cx_no_entry_in_table,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS key.

    TYPES key_list TYPE STANDARD TABLE OF key_dict WITH EMPTY KEY.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'CSKS',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: ktext      TYPE cskt-ktext,
          ktext_read TYPE abap_bool.

    METHODS constructor
      IMPORTING
        !key TYPE key_dict
        !def TYPE def_dict OPTIONAL
      RAISING
        cx_no_entry_in_table.
ENDCLASS.



CLASS zcl_co_cost_center IMPLEMENTATION.


  METHOD constructor.
    IF def IS INITIAL.
      SELECT SINGLE kokrs, kostl, datbi
             FROM csks
             WHERE kokrs = @key-kokrs AND
                   kostl = @key-kostl AND
                   datab < @sy-datum  AND
                   datbi > @sy-datum
             INTO CORRESPONDING FIELDS OF @me->def.     "#EC CI_NOORDER

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_no_entry_in_table
          EXPORTING
            table_name = CONV #( me->table-def )
            entry_name = |{ key-kokrs } { key-kostl }|.
      ENDIF.

    ELSE.
      me->def = def.
    ENDIF.
  ENDMETHOD.


  METHOD ensure_exists_for_comp.
    SELECT SINGLE FROM csks                             "#EC CI_GENBUFF
           FIELDS @abap_true
           WHERE  kostl  = @kostl     AND
                  bukrs  = @bukrs     AND
                  datbi >= @sy-datum  AND
                  datab <= @sy-datum
           INTO   @DATA(record_exists).

    IF record_exists = abap_false.
      RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( table-def )
                                                entry_name = |{ kostl } { bukrs }| ).
    ENDIF.
  ENDMETHOD.


  METHOD cache_with_itab.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " İletilen ITAB ile masraf yeri ana verilerini hazırlar
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA entry_kokrs TYPE kokrs.

    FIELD-SYMBOLS <itab> TYPE ANY TABLE.
    FIELD-SYMBOLS <kokrs> TYPE kokrs.
    FIELD-SYMBOLS <kostl> TYPE kostl.

    CHECK itab IS NOT INITIAL.
    ASSIGN itab->* TO <itab>.
    IF <itab> IS INITIAL.
      RETURN.
    ENDIF.

    DATA(keys) = VALUE key_list( ).

    LOOP AT <itab> ASSIGNING FIELD-SYMBOL(<line>).
      ASSIGN COMPONENT kostl_field OF STRUCTURE <line> TO <kostl>.
      CHECK <kostl> IS NOT INITIAL.

      IF fixed_kokrs IS INITIAL.
        ASSIGN COMPONENT kokrs_field OF STRUCTURE <line> TO <kokrs>.
        CHECK <kokrs> IS NOT INITIAL.
        entry_kokrs = <kokrs>.
      ELSE.
        entry_kokrs = fixed_kokrs.
      ENDIF.

      DATA(key) = VALUE key_dict( kokrs = entry_kokrs
                                  kostl = <kostl> ).

      CHECK NOT line_exists( multitons[ key = key ] ).
      APPEND key TO keys.
    ENDLOOP.

    IF keys IS INITIAL.
      RETURN.
    ENDIF.

    SELECT kokrs, kostl, datbi
           FROM csks
           FOR ALL ENTRIES IN @keys
           WHERE kokrs = @keys-kokrs AND
                 kostl = @keys-kostl AND
                 datab < @sy-datum  AND
                 datbi > @sy-datum
           INTO TABLE @DATA(csks).

    LOOP AT csks ASSIGNING FIELD-SYMBOL(<csks>).
      TRY.
          DATA(cost_center) = NEW zcl_co_cost_center( key = CORRESPONDING #( <csks> )
                                                      def = CORRESPONDING #( <csks> ) ).

          INSERT VALUE #( key = CORRESPONDING #( <csks> )
                          obj = cost_center
                        ) INTO TABLE multitons.
        CATCH cx_root ##no_handler .
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_instance.
    ASSIGN zcl_co_cost_center=>multitons[
             KEY primary_key COMPONENTS key = key
           ] TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_dict( key = key ).

      TRY.
          multiton-obj = NEW #( multiton-key ).
        CATCH cx_no_entry_in_table INTO multiton-cx ##NO_HANDLER.
      ENDTRY.

      INSERT multiton INTO TABLE zcl_co_cost_center=>multitons ASSIGNING <multiton>.
    ENDIF.

    IF <multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <multiton>-cx.
    ENDIF.

    obj = <multiton>-obj.
  ENDMETHOD.


  METHOD get_text.
    IF me->ktext_read = abap_false.
      SELECT SINGLE ktext FROM cskt
             WHERE spras = @sy-langu AND
                   kokrs = @me->def-kokrs AND
                   kostl = @me->def-kostl AND
                   datbi > @sy-datum
             INTO @me->ktext.                           "#EC CI_NOORDER

      me->ktext_read = abap_true.
    ENDIF.

    ktext = me->ktext.
  ENDMETHOD.
ENDCLASS.