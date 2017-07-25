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

      begin of t_Tabname,
        tabname type tabname,
      end of t_tabname,

      tt_tabname type standard table of t_Tabname with default key.

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

      get_Tables_containing_dtel
        importing !iv_rollname type rollname
        returning value(rt_Tabname) type tt_Tabname.

    METHODS:
      check_Table_Has_flds_of_tab
        importing !iv_tabname type tabname
        raising   zcx_bc_table_content,


      check_Table_has_field
        importing !iv_fieldname type fieldname
        raising   zcx_bc_table_content,

      enqueue
        importing !iv_key TYPE clike OPTIONAL
        RAISING   cx_rs_foreign_lock ,

      get_field
        importing !iv_fnam type fieldname
        returning value(rs_dd03l) type dd03l
        raising   zcx_bc_table_content,

      get_Fields RETURNING VALUE(rt_dd03l) TYPE tt_dd03l,

      get_included_Tables
        importing !iv_Recursive type abap_bool
        returning value(rt_tabname) type tt_Tabname,

      get_key_fields
        importing !iv_with_mandt type abap_bool default abap_true
        RETURNING VALUE(rt_dd03l) TYPE tt_dd03l,

      get_rollname_of_field
        importing !iv_fnam type fieldname
        returning value(rv_roll) type rollname
        raising   zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      begin of t_dtel_tab,
        rollname type rollname,
        tabname  type tt_tabname,
      end of t_dtel_Tab,

      tt_dtel_Tab
        type hashed table of t_dtel_Tab
        with unique key primary_key components rollname,

      BEGIN OF t_lazy_flag,
        field   TYPE abap_bool,
        include type abap_bool,
        key     TYPE abap_bool,
      END OF t_lazy_flag,

      BEGIN OF t_lazy_val,
        field   TYPE tt_dd03l,
        include type tt_tabname,
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
      c_fnam_mandt  type fieldname value 'MANDT',
      c_tabname_def TYPE tabname   VALUE 'DD02L',
      c_tabname_fld type tabname   value 'DD03L'.

    CLASS-DATA:
      gt_dtel_Tab type tt_dtel_Tab,
      gt_mt       TYPE tt_mt.

    DATA gs_lazy TYPE t_lazy.

    METHODS:
      read_fields_lazy,
      read_includes_lazy,
      read_keys_lazy.

ENDCLASS.



CLASS zcl_bc_abap_table IMPLEMENTATION.

  method check_Table_has_field.
    read_fields_lazy( ).
    check not line_exists( gs_lazy-val-field[ fieldname = iv_fieldname ] ).

    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        objectid = |{ gs_def-tabname } - { iv_fieldname }|
        tabname  = gs_def-tabname
        textid   = zcx_bc_table_content=>entry_missing.

  endmethod.

  method check_Table_Has_flds_of_tab.

    loop at get_instance( iv_tabname )->get_fields( ) assigning field-symbol(<ls_fld>).
      check_table_has_field( <ls_fld>-fieldname ).
    endloop.

  endmethod.

  method enqueue.

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

    CHECK  sy-subrc NE 0.

    RAISE EXCEPTION TYPE cx_rs_foreign_lock
      EXPORTING
        key      = CONV #( iv_key )
        object   = CONV #( gs_def-tabname )
        previous = zcx_bc_symsg=>get_instance( )
        textid   = cx_rs_foreign_lock=>cx_rs_foreign_lock
        user     = CONV #( sy-msgv1 ).

  endmethod.

  METHOD get_dbfield_text.
    rv_ddtext = zcl_bc_dbfield_text_abs=>get_text_via_chain( iv_dbfield ).
  ENDMETHOD.

  method get_field.
    read_fields_lazy( ).

    try.
        rs_dd03l = gs_lazy-val-field[ fieldname = iv_fnam ].
      catch cx_root into data(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = |{ gs_def-tabname } { iv_fnam }|
            previous = lo_Diaper
            tabname  = c_tabname_fld.

    endtry.

  endmethod.

  method get_Fields.
    read_fields_lazy( ).
    rt_dd03l = gs_lazy-val-field.
  endmethod.

  method get_included_Tables.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Bu tablonun Include'ları
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    read_includes_lazy( ).
    data(lt_return) = gs_lazy-val-include.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " İstendiyse, Recursive diğer Include'lar
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    if iv_recursive eq abap_true.

      loop at gs_lazy-val-include assigning field-symbol(<ls_include>).

        try.
            data(lo_tab) = zcl_bc_abap_Table=>get_instance( <ls_include>-tabname ).
          catch cx_root. " Paranoya
            delete lt_Return where tabname eq <ls_include>-tabname.
            continue.
        endtry.

        append lines of lo_Tab->get_included_tables( abap_true ) to lt_return.

      endloop.

      sort lt_return by tabname.
      delete adjacent duplicates from lt_Return comparing tabname.

    endif.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Değerleri döndür
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    rt_tabname = lt_return.

  endmethod.

  METHOD get_instance.

    ASSIGN gt_mt[ KEY primary_key COMPONENTS tabname = iv_tabname ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_mt( tabname = iv_tabname ).

      ls_mt-obj = NEW #( ).

      SELECT SINGLE * INTO @ls_mt-obj->gs_def
        FROM dd02l
        WHERE tabname EQ @ls_mt-tabname.

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

    if iv_with_mandt eq abap_false.
      delete rt_dd03l where fieldname eq c_fnam_mandt.
    endif.

  ENDMETHOD.

  method get_rollname_of_field.
    rv_roll = get_Field( iv_fnam )-rollname.
  endmethod.

  METHOD get_rollname_pairs.

    DATA:
      lt_dd04t TYPE STANDARD TABLE OF dd04t,
      lt_roll  TYPE tt_rollname,
      lt_tab1  TYPE tt_tabfld_fsort,
      lt_tab2  TYPE tt_tabfld_fsort,

      ls_ret   LIKE LINE OF rt_ret.

*   Tablo alanlarını al
    lt_tab1 = corresponding #( get_instance( iv_Tabname1 )->get_fields( ) ).
    lt_tab2 = corresponding #( get_instance( iv_tabname2 )->get_fields( ) ).

*   Tekrarsız Domain listesi oluştur
    LOOP AT lt_tab1 ASSIGNING FIELD-SYMBOL(<ls_tab1>) WHERE rollname IS NOT INITIAL.
      COLLECT VALUE t_rollname( rollname = <ls_tab1>-rollname ) INTO lt_roll.
    ENDLOOP.

    LOOP AT lt_tab2 ASSIGNING FIELD-SYMBOL(<ls_tab2>) WHERE rollname IS NOT INITIAL.
      COLLECT VALUE t_rollname( rollname = <ls_tab2>-rollname ) INTO lt_roll.
    ENDLOOP.

    CHECK lt_roll[] IS NOT INITIAL.

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
        WHERE rollname EQ <ls_roll>-rollname.

        ls_ret-tabname1   = <ls_tab1>-tabname.
        ls_ret-fieldname1 = <ls_tab1>-fieldname.

        LOOP AT lt_tab2
          ASSIGNING <ls_tab2>
          USING KEY primary_key
          WHERE rollname EQ <ls_roll>-rollname.

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

  method get_Tables_containing_dtel.

    assign gt_dtel_Tab[
      rollname = iv_rollname
    ] to field-symbol(<ls_dtel_Tab>).

    if sy-subrc ne 0.

      data(ls_new) = value t_dtel_Tab( rollname = iv_rollname ).

      select distinct dd03l~tabname
        into corresponding fields of table @ls_new-tabname
        from dd03l
             inner join dd02l on dd02l~tabname eq dd03l~tabname
        where rollname eq @ls_new-rollname and
              dd02l~tabclass ne 'INTTAB' and
              dd02l~tabclass ne 'VIEW' and
              dd02l~tabclass ne 'APPEND'.

      sort ls_new-tabname by tabname.
      delete adjacent duplicates from ls_new-tabname comparing tabname.

      insert ls_new into table gt_dtel_Tab assigning <ls_dtel_tab>.

    endif.

    rt_tabname = <ls_dtel_Tab>-tabname.

  endmethod.

  METHOD read_fields_lazy.

    CHECK gs_lazy-flag-field IS INITIAL.

    SELECT * INTO TABLE @gs_lazy-val-field
      FROM dd03l
      WHERE tabname EQ @gs_def-tabname and
            fieldname NOT LIKE '.%'.

    gs_lazy-flag-field = abap_true.

  ENDMETHOD.

  method read_includes_lazy.

    check gs_lazy-flag-include is initial.

    select precfield as tabname
      into corresponding fields of table @gs_lazy-val-include
      from dd03l
      where tabname   eq @gs_Def-tabname and
            fieldname eq '.INCLUDE'.

    gs_lazy-flag-include = abap_true.

  endmethod.

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