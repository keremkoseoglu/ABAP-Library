CLASS zcl_bc_abap_table DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_dd03l TYPE STANDARD TABLE OF dd03l WITH DEFAULT KEY,

      BEGIN OF t_tabfld,
        tabname   TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
        rollname  TYPE dd03l-rollname,
      END OF t_tabfld ,

      tt_tabfld       TYPE STANDARD TABLE OF t_tabfld WITH DEFAULT KEY,
      tt_tabfld_fsort TYPE SORTED TABLE OF t_tabfld WITH UNIQUE KEY primary_key COMPONENTS fieldname,

      BEGIN OF t_tabname,
        tabname TYPE tabname,
      END OF t_tabname,

      tt_tabname     TYPE STANDARD TABLE OF t_tabname WITH DEFAULT KEY,

      tt_tabname_rng TYPE RANGE OF tabname,

      BEGIN OF t_fldroll,
        fieldname TYPE dd03l-fieldname,
        rollname  TYPE dd03l-rollname,
      END OF t_fldroll,

      tt_fldroll TYPE STANDARD TABLE OF t_fldroll WITH DEFAULT KEY.

    DATA gs_def TYPE dd02l READ-ONLY.

    CLASS-METHODS:

      get_dbfield_text
        IMPORTING !iv_dbfield      TYPE clike
        RETURNING VALUE(rv_ddtext) TYPE ddtext,

      get_instance
        IMPORTING !iv_tabname   TYPE tabname
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_abap_table
        RAISING   zcx_bc_table_content,

      get_rollname_pairs
        IMPORTING
          !iv_tabname1  TYPE tabname
          !iv_tabname2  TYPE tabname
        RETURNING
          VALUE(rt_ret) TYPE zbctt_rollname_pair
        RAISING
          zcx_bc_table_content ,

      get_tables_containing_dtel
        IMPORTING !iv_rollname      TYPE rollname
        RETURNING VALUE(rt_tabname) TYPE tt_tabname,

      get_tables_containing_fldroll
        IMPORTING
          !it_tabname_rng   TYPE tt_tabname_rng
          !it_fldroll       TYPE tt_fldroll
        RETURNING
          VALUE(rt_tabname) TYPE tt_tabname.

    METHODS:
      check_table_has_flds_of_tab
        IMPORTING !iv_tabname TYPE tabname
        RAISING   zcx_bc_table_content,


      check_table_has_field
        IMPORTING !iv_fieldname TYPE fieldname
        RAISING   zcx_bc_table_content,

      enqueue
        IMPORTING !iv_key TYPE clike OPTIONAL
        RAISING   cx_rs_foreign_lock ,

      get_field
        IMPORTING !iv_fnam        TYPE fieldname
        RETURNING VALUE(rs_dd03l) TYPE dd03l
        RAISING   zcx_bc_table_content,

      get_field_count RETURNING VALUE(rv_count) TYPE i,

      get_fields RETURNING VALUE(rt_dd03l) TYPE tt_dd03l,

      get_included_tables
        IMPORTING !iv_recursive     TYPE abap_bool
        RETURNING VALUE(rt_tabname) TYPE tt_tabname,

      get_key_fields
        IMPORTING !iv_with_mandt  TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rt_dd03l) TYPE tt_dd03l,

      get_rollname_of_field
        IMPORTING !iv_fnam       TYPE fieldname
        RETURNING VALUE(rv_roll) TYPE rollname
        RAISING   zcx_bc_table_content,

      is_field_key
        IMPORTING !iv_fnam      TYPE fieldname
        RETURNING VALUE(rv_key) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_dtel_tab,
        rollname TYPE rollname,
        tabname  TYPE tt_tabname,
      END OF t_dtel_tab,

      tt_dtel_tab
        TYPE HASHED TABLE OF t_dtel_tab
        WITH UNIQUE KEY primary_key COMPONENTS rollname,

      BEGIN OF t_lazy_flag,
        field   TYPE abap_bool,
        include TYPE abap_bool,
        key     TYPE abap_bool,
      END OF t_lazy_flag,

      BEGIN OF t_lazy_val,
        field   TYPE tt_dd03l,
        include TYPE tt_tabname,
        key     TYPE tt_dd03l,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flag TYPE t_lazy_flag,
        val  TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_mt, " Multiton
        tabname TYPE dd02l,
        obj     TYPE REF TO zcl_bc_abap_table,
      END OF t_mt,

      tt_mt
        TYPE HASHED TABLE OF t_mt
        WITH UNIQUE KEY primary_key COMPONENTS tabname,

      BEGIN OF t_rollname,
        rollname TYPE rollname,
      END OF t_rollname ,

      tt_rollname TYPE STANDARD TABLE OF t_rollname.

    CONSTANTS:
      c_fnam_mandt  TYPE fieldname VALUE 'MANDT',
      c_tabname_def TYPE tabname   VALUE 'DD02L',
      c_tabname_fld TYPE tabname   VALUE 'DD03L'.

    CLASS-DATA:
      gt_dtel_tab TYPE tt_dtel_tab,
      gt_mt       TYPE tt_mt.

    DATA gs_lazy TYPE t_lazy.

    METHODS:
      read_fields_lazy,
      read_includes_lazy,
      read_keys_lazy.

ENDCLASS.



CLASS zcl_bc_abap_table IMPLEMENTATION.


  METHOD check_table_has_field.
    read_fields_lazy( ).
    IF NOT line_exists( gs_lazy-val-field[ fieldname = iv_fieldname ] ).
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          objectid = |{ gs_def-tabname } - { iv_fieldname }|
          tabname  = gs_def-tabname
          textid   = zcx_bc_table_content=>entry_missing.
    ENDIF.

  ENDMETHOD.


  METHOD check_table_has_flds_of_tab.

    LOOP AT get_instance( iv_tabname )->get_fields( ) ASSIGNING FIELD-SYMBOL(<ls_fld>).
      check_table_has_field( <ls_fld>-fieldname ).
    ENDLOOP.

  ENDMETHOD.


  METHOD enqueue.

    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        view_name            = gs_def-tabname
      EXCEPTIONS
        client_reference     = 1
        foreign_lock         = 2
        invalid_action       = 3
        invalid_enqueue_mode = 4
        system_failure       = 5
        table_not_found      = 6
        OTHERS               = 7.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_rs_foreign_lock
        EXPORTING
          key      = CONV #( iv_key )
          object   = CONV #( gs_def-tabname )
          previous = zcx_bc_symsg=>get_instance( )
          textid   = cx_rs_foreign_lock=>cx_rs_foreign_lock
          user     = CONV #( sy-msgv1 ).
    ENDIF.

  ENDMETHOD.


  METHOD get_dbfield_text.
    rv_ddtext = zcl_bc_dbfield_text_abs=>get_text_via_chain( iv_dbfield ).
  ENDMETHOD.


  METHOD get_field.
    read_fields_lazy( ).

    TRY.
        rs_dd03l = gs_lazy-val-field[ fieldname = iv_fnam ].
      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = |{ gs_def-tabname } { iv_fnam }|
            previous = lo_diaper
            tabname  = c_tabname_fld.

    ENDTRY.

  ENDMETHOD.

  METHOD get_field_count.
    read_fields_lazy( ).
    rv_count = lines( gs_lazy-val-field ).
  ENDMETHOD.

  METHOD get_fields.
    read_fields_lazy( ).
    rt_dd03l = gs_lazy-val-field.
  ENDMETHOD.


  METHOD get_included_tables.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Bu tablonun Include'ları
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    read_includes_lazy( ).
    DATA(lt_return) = gs_lazy-val-include.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " İstendiyse, Recursive diğer Include'lar
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF iv_recursive EQ abap_true.

      LOOP AT gs_lazy-val-include ASSIGNING FIELD-SYMBOL(<ls_include>).

        TRY.
            DATA(lo_tab) = zcl_bc_abap_table=>get_instance( <ls_include>-tabname ).
          CATCH cx_root. " Paranoya
            DELETE lt_return WHERE tabname EQ <ls_include>-tabname.
            CONTINUE.
        ENDTRY.

        APPEND LINES OF lo_tab->get_included_tables( abap_true ) TO lt_return.

      ENDLOOP.

      SORT lt_return BY tabname.
      DELETE ADJACENT DUPLICATES FROM lt_return COMPARING tabname.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Değerleri döndür
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    rt_tabname = lt_return.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_mt[ KEY primary_key COMPONENTS tabname = iv_tabname ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_mt( tabname = iv_tabname ).

      ls_mt-obj = NEW #( ).

      SELECT SINGLE * INTO @ls_mt-obj->gs_def
        FROM dd02l
        WHERE tabname EQ @ls_mt-tabname.                "#EC CI_NOORDER

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_mt-tabname )
            tabname  = c_tabname_def.
      ENDIF.

      INSERT ls_mt INTO TABLE gt_mt ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.


  METHOD get_key_fields.
    read_keys_lazy( ).
    rt_dd03l = gs_lazy-val-key.

    IF iv_with_mandt EQ abap_false.
      DELETE rt_dd03l WHERE fieldname EQ c_fnam_mandt.
    ENDIF.

  ENDMETHOD.


  METHOD get_rollname_of_field.
    rv_roll = get_field( iv_fnam )-rollname.
  ENDMETHOD.


  METHOD get_rollname_pairs.

    DATA:
      lt_dd04t TYPE STANDARD TABLE OF dd04t,
      lt_roll  TYPE tt_rollname,
      lt_tab1  TYPE tt_tabfld_fsort,
      lt_tab2  TYPE tt_tabfld_fsort,

      ls_ret   LIKE LINE OF rt_ret.

*   Tablo alanlarını al
    lt_tab1 = CORRESPONDING #( get_instance( iv_tabname1 )->get_fields( ) ).
    lt_tab2 = CORRESPONDING #( get_instance( iv_tabname2 )->get_fields( ) ).

*   Tekrarsız Domain listesi oluştur
    LOOP AT lt_tab1 ASSIGNING FIELD-SYMBOL(<ls_tab1>) WHERE rollname IS NOT INITIAL. "#EC CI_SORTSEQ
      COLLECT VALUE t_rollname( rollname = <ls_tab1>-rollname ) INTO lt_roll.
    ENDLOOP.

    LOOP AT lt_tab2 ASSIGNING FIELD-SYMBOL(<ls_tab2>) WHERE rollname IS NOT INITIAL. "#EC CI_SORTSEQ
      COLLECT VALUE t_rollname( rollname = <ls_tab2>-rollname ) INTO lt_roll.
    ENDLOOP.

    IF lt_roll[] IS INITIAL.
      RETURN.
    ENDIF.

*   Domain metinlerini oku
    SELECT * INTO TABLE lt_dd04t
      FROM dd04t
      FOR ALL ENTRIES IN lt_roll
      WHERE
        rollname   EQ lt_roll-rollname AND
        ddlanguage EQ sy-langu.

    SORT lt_dd04t BY rollname.

*   Tablo alanlarını eşleştir + verileri tamamla
    LOOP AT lt_roll ASSIGNING FIELD-SYMBOL(<ls_roll>).

      CLEAR ls_ret.
      ls_ret-rollname = <ls_roll>-rollname.

      READ TABLE lt_dd04t
        ASSIGNING FIELD-SYMBOL(<ls_dd04t>)
        WITH KEY rollname = <ls_roll>-rollname
        BINARY SEARCH.

      IF sy-subrc EQ 0.
        ls_ret-ddtext = <ls_dd04t>-ddtext.
      ENDIF.

      LOOP AT lt_tab1
        ASSIGNING <ls_tab1>
        USING KEY primary_key
        WHERE rollname EQ <ls_roll>-rollname.           "#EC CI_SORTSEQ

        ls_ret-tabname1   = <ls_tab1>-tabname.
        ls_ret-fieldname1 = <ls_tab1>-fieldname.

        LOOP AT lt_tab2
          ASSIGNING <ls_tab2>
          USING KEY primary_key
          WHERE rollname EQ <ls_roll>-rollname.         "#EC CI_SORTSEQ

          ls_ret-tabname2   = <ls_tab2>-tabname.
          ls_ret-fieldname2 = <ls_tab2>-fieldname.
          APPEND ls_ret TO rt_ret.

        ENDLOOP.

        IF sy-subrc NE 0.
          APPEND ls_ret TO rt_ret.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_tables_containing_dtel.

    ASSIGN gt_dtel_tab[
      rollname = iv_rollname
    ] TO FIELD-SYMBOL(<ls_dtel_tab>).

    IF sy-subrc NE 0.

      DATA(ls_new) = VALUE t_dtel_tab( rollname = iv_rollname ).

      SELECT DISTINCT dd03l~tabname
        INTO CORRESPONDING FIELDS OF TABLE @ls_new-tabname
        FROM dd03l
             INNER JOIN dd02l ON dd02l~tabname EQ dd03l~tabname
        WHERE rollname EQ @ls_new-rollname AND
              dd02l~tabclass NE 'INTTAB' AND
              dd02l~tabclass NE 'VIEW' AND
              dd02l~tabclass NE 'APPEND'.

      SORT ls_new-tabname BY tabname.
      DELETE ADJACENT DUPLICATES FROM ls_new-tabname COMPARING tabname.

      INSERT ls_new INTO TABLE gt_dtel_tab ASSIGNING <ls_dtel_tab>.

    ENDIF.

    rt_tabname = <ls_dtel_tab>-tabname.

  ENDMETHOD.

  METHOD get_tables_containing_fldroll.

    CHECK it_fldroll IS NOT INITIAL.

    SELECT tabname, fieldname, rollname
      FROM dd03l
      FOR ALL ENTRIES IN @it_fldroll
      WHERE
        tabname   IN @it_tabname_rng       AND
        fieldname EQ @it_fldroll-fieldname AND
        rollname  EQ @it_fldroll-rollname
      INTO TABLE @DATA(lt_dd03l).

    rt_tabname = VALUE #(
      FOR GROUPS _tabname OF _dd03l IN lt_dd03l
      GROUP BY _dd03l-tabname
      ( tabname = _tabname )
    ).

    SORT lt_dd03l BY tabname fieldname rollname. " Binary Search var

    LOOP AT rt_tabname ASSIGNING FIELD-SYMBOL(<ls_tabname>).

      DATA(lv_table_ok) = abap_true.

      LOOP AT it_fldroll ASSIGNING FIELD-SYMBOL(<ls_fldroll>).

        READ TABLE lt_dd03l
          TRANSPORTING NO FIELDS
          WITH KEY
            tabname   = <ls_tabname>-tabname
            fieldname = <ls_fldroll>-fieldname
            rollname  = <ls_fldroll>-rollname
          BINARY SEARCH.

        CHECK sy-subrc NE 0.
        lv_table_ok = abap_false.
        EXIT.

      ENDLOOP.

      CHECK lv_table_ok EQ abap_false.
      DELETE rt_tabname.
      CONTINUE.

    ENDLOOP.

  ENDMETHOD.

  METHOD is_field_key.

    read_keys_lazy( ).

    rv_key = xsdbool(
      line_exists(
        gs_lazy-val-key[ fieldname = iv_fnam ]
      )
    ).

  ENDMETHOD.

  METHOD read_fields_lazy.

    CHECK gs_lazy-flag-field IS INITIAL.

    SELECT * INTO TABLE @gs_lazy-val-field
      FROM dd03l
      WHERE tabname EQ @gs_def-tabname AND
            fieldname NOT LIKE '.%'.

    gs_lazy-flag-field = abap_true.

  ENDMETHOD.


  METHOD read_includes_lazy.

    CHECK gs_lazy-flag-include IS INITIAL.

    SELECT precfield AS tabname
      INTO CORRESPONDING FIELDS OF TABLE @gs_lazy-val-include
      FROM dd03l
      WHERE tabname   EQ @gs_def-tabname AND
            fieldname EQ '.INCLUDE'.

    gs_lazy-flag-include = abap_true.

  ENDMETHOD.


  METHOD read_keys_lazy.

    CHECK gs_lazy-flag-key IS INITIAL.

    read_fields_lazy( ).

    gs_lazy-val-key = VALUE #(
      FOR ls_field IN gs_lazy-val-field
      WHERE ( keyflag EQ abap_true )
      ( ls_field )
    ).

    gs_lazy-flag-key = abap_true.

  ENDMETHOD.
ENDCLASS.