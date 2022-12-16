CLASS zcl_mm_class_type DEFINITION PUBLIC FINAL CREATE PRIVATE.

    PUBLIC SECTION.
      TYPES class_list TYPE STANDARD TABLE OF klasse_d WITH KEY table_line.
  
      TYPES: BEGIN OF class_with_txt_dict,
               class TYPE klah-class,
               kschl TYPE swor-kschl,
             END OF class_with_txt_dict,
  
             class_with_txt_list TYPE STANDARD TABLE OF class_with_txt_dict WITH KEY class.
  
      DATA def TYPE tcla READ-ONLY.
  
      CLASS-METHODS get_instance
        IMPORTING !klart     TYPE klassenart
        RETURNING VALUE(obj) TYPE REF TO zcl_mm_class_type
        RAISING   cx_no_entry_in_table.
  
      METHODS get_text RETURNING VALUE(result) TYPE tclat-artxt.
      METHODS get_classes RETURNING VALUE(result) TYPE class_list.
      METHODS get_classes_with_txt RETURNING VALUE(result) TYPE class_with_txt_list.
  
    PROTECTED SECTION.
  
    PRIVATE SECTION.
      TYPES: BEGIN OF multiton_dict,
               klart TYPE klassenart,
               obj   TYPE REF TO zcl_mm_class_type,
               cx    TYPE REF TO cx_no_entry_in_table,
             END OF multiton_dict,
  
             multiton_set TYPE HASHED TABLE OF multiton_dict
                          WITH UNIQUE KEY primary_key COMPONENTS klart.
  
      CONSTANTS: BEGIN OF table,
                   def TYPE tabname VALUE 'TCLA',
                 END OF table.
  
      CONSTANTS: BEGIN OF klpos,
                   min TYPE swor-klpos VALUE '01',
                 END OF klpos.
  
      CLASS-DATA multitons TYPE multiton_set.
  
      DATA: artxt                 TYPE tclat-artxt,
            artxt_read            TYPE abap_bool,
            classes               TYPE class_list,
            classes_read          TYPE abap_bool,
            classes_with_txt      TYPE class_with_txt_list,
            classes_with_txt_read TYPE abap_bool.
  
      METHODS constructor
        IMPORTING !klart TYPE klassenart
        RAISING   cx_no_entry_in_table.
  ENDCLASS.
  
  CLASS zcl_mm_class_type IMPLEMENTATION.
    METHOD constructor.
      SELECT SINGLE * FROM tcla
             WHERE  klart = @klart
             INTO   CORRESPONDING FIELDS OF @me->def.
  
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_no_entry_in_table
          EXPORTING
            table_name = CONV #( table-def )
            entry_name = |{ klart }|.
      ENDIF.
    ENDMETHOD.
  
  
    METHOD get_instance.
      ASSIGN multitons[ KEY primary_key COMPONENTS klart = klart
                      ] TO FIELD-SYMBOL(<multiton>).
      IF sy-subrc <> 0.
        DATA(multiton) = VALUE multiton_dict( klart = klart ).
  
        TRY.
            multiton-obj = NEW #( multiton-klart ).
          CATCH cx_no_entry_in_table INTO multiton-cx ##NO_HANDLER.
        ENDTRY.
  
        INSERT multiton INTO TABLE multitons ASSIGNING <multiton>.
      ENDIF.
  
      IF <multiton>-cx IS NOT INITIAL.
        RAISE EXCEPTION <multiton>-cx.
      ENDIF.
  
      obj = <multiton>-obj.
    ENDMETHOD.
  
  
    METHOD get_text.
      IF me->artxt_read = abap_false.
        SELECT SINGLE FROM tclat
               FIELDS artxt
               WHERE  spras = @sy-langu AND
                      klart = @me->def-klart
               INTO   @me->artxt.
  
        me->artxt_read = abap_true.
      ENDIF.
  
      result = me->artxt.
    ENDMETHOD.
  
  
    METHOD get_classes.
      IF me->classes_read = abap_false.
        me->classes       = VALUE #( FOR _cwt IN get_classes_with_txt( ) ( _cwt-class ) ).
        me->classes_read  = abap_true.
      ENDIF.
  
      result = me->classes.
    ENDMETHOD.
  
  
    METHOD get_classes_with_txt.
      IF me->classes_with_txt_read = abap_false.
        SELECT FROM   klah
                      LEFT OUTER JOIN swor ON swor~clint = klah~clint AND
                                              swor~spras = @sy-langu  AND
                                              swor~klpos = @me->klpos-min
               FIELDS DISTINCT klah~class, swor~kschl
               WHERE  klah~klart  = @me->def-klart AND
                      klah~vondt <= @sy-datum      AND
                      klah~bisdt >= @sy-datum
               INTO   CORRESPONDING FIELDS OF TABLE @me->classes_with_txt.
  
        me->classes_with_txt_read = abap_true.
      ENDIF.
  
      result = me->classes_with_txt.
    ENDMETHOD.
  ENDCLASS.
  