CLASS zcl_bc_dynamic_ss DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF t_so,
             sign   TYPE tvarv-sign,
             option TYPE tvarv-opti,
             low    TYPE tvarv-low,
             high   TYPE tvarv-high,
           END OF t_so,

           tt_so TYPE STANDARD TABLE OF t_so WITH DEFAULT KEY,

           BEGIN OF t_return,
             name      TYPE tvarv-name,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             so        TYPE tt_so,
           END OF t_return,

           tt_return  TYPE STANDARD TABLE OF t_return WITH DEFAULT KEY,
           tt_tabname TYPE STANDARD TABLE OF tabname WITH DEFAULT KEY.

    CLASS-METHODS execute_for_tables
      IMPORTING
        !it_tabname   TYPE tt_tabname
      RETURNING
        VALUE(rt_tab) TYPE tt_return
      RAISING
        zcx_bc_code_generation
        zcx_bc_table_content.

    METHODS add_ss_val IMPORTING is_ss TYPE tvarv.

    METHODS build_query
      IMPORTING
                !iv_main_tab    TYPE tabname
                !iv_key_fld     TYPE fieldname
      RETURNING VALUE(rv_query) TYPE string.

    METHODS constructor.

    METHODS execute
      IMPORTING
        !it_fld       TYPE zcl_bc_ddic_toolkit=>tt_tabfld OPTIONAL
        !it_tab       TYPE tt_tabname OPTIONAL
      RETURNING
        VALUE(rt_tab) TYPE tt_return
      RAISING
        zcx_bc_code_generation
        zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_so_map,
             name   TYPE tvarv-name,
             tabfld TYPE zcl_bc_abap_Table=>t_tabfld,
           END OF t_so_map,

           tt_so_map TYPE HASHED TABLE OF t_so_map WITH UNIQUE KEY primary_key COMPONENTS name,
           tt_tvarv  TYPE STANDARD TABLE OF tvarv WITH DEFAULT KEY.

    CONSTANTS: c_option_bt TYPE ddoption VALUE 'BT',
               c_option_nb TYPE ddoption VALUE 'NB',
               c_sign_e    TYPE ddsign VALUE 'E'.

    DATA: go_dynprg TYPE REF TO zcl_bc_dynamic_prog,
          gt_fld    TYPE zcl_bc_ddic_toolkit=>tt_tabfld,
          gt_ret    TYPE tt_return,
          gt_so_map TYPE tt_so_map,
          gt_src    TYPE zcl_bc_dynamic_prog=>tt_src,
          gt_ssfld  TYPE zcl_bc_ddic_toolkit=>tt_tabfld,
          gt_tvarv  TYPE tt_tvarv,
          gt_txt    TYPE textpool_table.

    METHODS build_return.
    METHODS determine_ssfld RAISING zcx_bc_code_generation.

    METHODS get_query_txt_for_so_line
      IMPORTING
        !iv_fieldname TYPE fieldname
        !is_so        TYPE zcl_bc_dynamic_ss=>t_so
      RETURNING
        VALUE(rv_txt) TYPE string.

    METHODS get_so_name IMPORTING !iv_fieldname TYPE fieldname RETURNING VALUE(rv_so) TYPE string.
    METHODS generate_data.
    METHODS generate_report_src.
    METHODS generate_ss.
    METHODS submit_report.
ENDCLASS.



CLASS ZCL_BC_DYNAMIC_SS IMPLEMENTATION.


  METHOD add_ss_val.
    APPEND is_ss TO gt_tvarv.
  ENDMETHOD.


  METHOD build_query.
    DATA lv_first_condition TYPE abap_bool.

*   Temizlik
    CLEAR rv_query.
    lv_first_condition = abap_true.

*   Ana tablo ile ilgili koşullar
    LOOP AT gt_ret ASSIGNING FIELD-SYMBOL(<ls_opt>) WHERE tabname EQ iv_main_tab.
      LOOP AT <ls_opt>-so ASSIGNING FIELD-SYMBOL(<ls_so>).
        rv_query = | { rv_query } | &&
                   | { SWITCH #( lv_first_condition WHEN abap_false THEN 'AND' ) } | &&
                   | { get_query_txt_for_so_line( iv_fieldname = <ls_opt>-fieldname
                                                  is_so        = <ls_so> ) } |.

        CLEAR lv_first_condition.
      ENDLOOP.


    ENDLOOP.

*   Yardımcı tablo ile ilgili koşullar
    LOOP AT gt_ret ASSIGNING <ls_opt> WHERE tabname NE iv_main_tab.

      CHECK <ls_opt>-so[] IS NOT INITIAL.

      rv_query = | { rv_query } | &&
                 | { SWITCH #( lv_first_condition WHEN abap_false THEN 'AND' ) } | &&
                 | EXISTS ( SELECT { iv_key_fld } FROM { <ls_opt>-tabname } WHERE { iv_key_fld } EQ { iv_main_tab }~{ iv_key_fld }  |.

      CLEAR lv_first_condition.

      LOOP AT <ls_opt>-so ASSIGNING <ls_so>.
        rv_query = | { rv_query } AND | &&
                   | { get_query_txt_for_so_line( iv_fieldname = <ls_opt>-fieldname
                                                  is_so        = <ls_so> ) } |.
      ENDLOOP.

      rv_query = | { rv_query } ) |.

    ENDLOOP.
  ENDMETHOD.


  METHOD build_return.

*   Tablonun ham halini oluştur
    gt_ret = CORRESPONDING #( gt_tvarv ).
    SORT gt_ret BY name.
    DELETE ADJACENT DUPLICATES FROM gt_ret COMPARING name.

*   Tablo ve alan isimlerini ekle
    LOOP AT gt_ret ASSIGNING FIELD-SYMBOL(<ls_ret>).
      ASSIGN gt_so_map[ KEY primary_key name = <ls_ret>-name ] TO FIELD-SYMBOL(<ls_so_map>).
      CHECK sy-subrc EQ 0.
      MOVE-CORRESPONDING <ls_so_map>-tabfld TO <ls_ret>.
    ENDLOOP.

*   Select-Optionları ekle
    LOOP AT gt_tvarv ASSIGNING FIELD-SYMBOL(<ls_var>).

      READ TABLE gt_ret ASSIGNING <ls_ret>
                 WITH KEY name = <ls_var>-name
                 BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      APPEND VALUE #( sign   = <ls_var>-sign
                      option = <ls_var>-opti
                      low    = <ls_var>-low
                      high   = <ls_var>-high ) TO <ls_ret>-so.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    DATA(lv_erdat) = sy-datum - 2.
    DELETE FROM zbct_int_parcels WHERE erdat LE lv_erdat.
  ENDMETHOD.


  METHOD determine_ssfld.

    DATA: lt_fnam  TYPE STANDARD TABLE OF fieldname,
          lv_count TYPE i.

*   Veritabanından alanları çek
    SELECT dd03l~tabname dd03l~fieldname dd03l~rollname
           INTO CORRESPONDING FIELDS OF TABLE gt_ssfld
           FROM dd03l
           FOR ALL ENTRIES IN gt_fld
           WHERE tabname   EQ gt_fld-tabname
             AND fieldname EQ gt_fld-fieldname
             AND rollname  EQ gt_fld-rollname
             AND EXISTS ( SELECT domname FROM dd01l
                                 WHERE domname EQ dd03l~domname
                                   AND datatype IN ('ACCP', 'CHAR', 'DATS', 'TIMS' ) ).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_code_generation
        EXPORTING
          textid = zcx_bc_code_generation=>no_ss_param.
    ENDIF.

*   İsmi aynı olabilecekleri sil
    lt_fnam = VALUE #( FOR ls_ssfld IN gt_ssfld ( ls_ssfld-fieldname+0(6) ) ).
    SORT lt_fnam.
    DELETE ADJACENT DUPLICATES FROM lt_fnam.

    LOOP AT lt_fnam ASSIGNING FIELD-SYMBOL(<lv_fnam>).

      CLEAR lv_count.
      LOOP AT gt_ssfld TRANSPORTING NO FIELDS WHERE fieldname+0(6) EQ <lv_fnam>.
        ADD 1 TO lv_count.
      ENDLOOP.

      CHECK lv_count GT 1.

      DELETE gt_ssfld WHERE fieldname+0(6) EQ <lv_fnam>.

    ENDLOOP.

  ENDMETHOD.


  METHOD execute.

*   Alanların belirlenmesi
    gt_fld[] = it_fld[].

    IF it_tab IS SUPPLIED.

      LOOP AT it_tab ASSIGNING FIELD-SYMBOL(<lv_tabname>).
        APPEND LINES OF zcl_bc_ddic_toolkit=>get_table_fields( <lv_tabname> ) TO gt_fld.
      ENDLOOP.

      SORT gt_fld BY fieldname.
      DELETE ADJACENT DUPLICATES FROM gt_fld COMPARING fieldname.
      SORT gt_fld BY tabname fieldname.
    ENDIF.

    IF gt_fld[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_code_generation
        EXPORTING
          textid = zcx_bc_code_generation=>no_ss_param.
    ENDIF.

    determine_ssfld( ).

*   Yürütme
    go_dynprg = NEW zcl_bc_dynamic_prog( ).
    go_dynprg->generate_name( ).

    generate_report_src( ).

    go_dynprg->execute( it_src = gt_src
                        it_txt = gt_txt ).

    submit_report( ).

    go_dynprg->delete( ).

*   Veri döndürme

    build_return( ).
    rt_tab[] = gt_ret[].

  ENDMETHOD.


  METHOD execute_for_tables.
    rt_tab = NEW zcl_bc_dynamic_ss( )->execute( it_tab = it_tabname ).
  ENDMETHOD.


  METHOD generate_data.
    DATA(lt_tab) = CORRESPONDING zcl_bc_ddic_toolkit=>tt_tabfld( gt_ssfld ).
    SORT lt_tab BY tabname.
    DELETE ADJACENT DUPLICATES FROM lt_tab COMPARING tabname.

    LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
      APPEND |TABLES { <ls_tab>-tabname }.| TO gt_src.
    ENDLOOP.

    APPEND |DATA gt_db TYPE STANDARD TABLE OF ZBCT_INT_PARCELS.| TO gt_src.
    APPEND |DATA gv_posnr TYPE ZBCT_INT_PARCELS-POSNR.| TO gt_src.

  ENDMETHOD.


  METHOD generate_report_src.

*   Temizlik
    CLEAR gt_src[].

*   Rapor başlığı
    APPEND |REPORT { go_dynprg->gs_dynprog-programm }.| TO gt_src.

*   Tablo tanımları + seçim ekranı
    generate_data( ).
    generate_ss( ).

*   Program başlangıcı
    APPEND 'START-OF-SELECTION.' TO gt_src.

*   Export işi
    LOOP AT gt_ssfld ASSIGNING FIELD-SYMBOL(<ls_ssfld>).
      DATA(lv_so) = get_so_name( <ls_ssfld>-fieldname ).
      APPEND |LOOP AT { lv_so }.| TO gt_src.
      APPEND |ADD 1 TO gv_posnr.| TO gt_src.
      APPEND |APPEND VALUE #( guid = p_guid posnr = gv_posnr name = '{ lv_so }' sign = { lv_so }-sign opti = { lv_so }-option low = { lv_so }-low high = { lv_so }-high erdat = sy-datum ) TO GT_DB.| TO gt_src.
      APPEND 'ENDLOOP.' TO gt_src.
    ENDLOOP.

    APPEND |INSERT ZBCT_INT_PARCELS FROM TABLE GT_DB.| TO gt_src.

*   Çıkış
    APPEND 'LEAVE PROGRAM.' TO gt_src.
    APPEND 'END-OF-SELECTION.' TO gt_src.

  ENDMETHOD.


  METHOD generate_ss.

    CLEAR gt_so_map[].

    LOOP AT gt_ssfld ASSIGNING FIELD-SYMBOL(<ls_ssfld>).

      DATA(lv_name) = get_so_name( <ls_ssfld>-fieldname ).
      APPEND: |SELECT-OPTIONS { lv_name } FOR { <ls_ssfld>-tabname }-{ <ls_ssfld>-fieldname }.|  TO gt_src,
              VALUE #( id     = 'S'
                       key    = lv_name
                       entry  = `D       .`
                       length = 9  ) TO gt_txt.

      IF NOT line_exists( gt_so_map[ KEY primary_key COMPONENTS name = conv #( lv_name ) ] ).
        INSERT VALUE #( name = lv_name tabfld = <ls_ssfld> ) INTO TABLE gt_so_map.
      ENDIF.

    ENDLOOP.

    APPEND |PARAMETERS P_GUID TYPE RECAGUID NO-DISPLAY.| TO gt_src.

  ENDMETHOD.


  METHOD get_query_txt_for_so_line.
    rv_txt = | ( | &&
             | { SWITCH #( is_so-sign WHEN c_sign_e THEN 'NOT' ) } | &&
             | { iv_fieldname } |  &&
             | { SWITCH #( is_so-option WHEN c_option_bt THEN | BETWEEN '{ is_so-low }' AND '{ is_so-high }' |
                                        WHEN c_option_nb THEN | NOT BETWEEN '{ is_so-low }' AND '{ is_so-high }' |
                                        ELSE | { is_so-option } '{ is_so-low }' | ) } | &&
             | ) |.
  ENDMETHOD.


  METHOD get_so_name.
    rv_so = |S_{ COND #( WHEN strlen( iv_fieldname ) LE 6 THEN iv_fieldname ELSE iv_fieldname+0(6) ) }|.
  ENDMETHOD.


  METHOD submit_report.

    DATA(lv_guid) = cl_reca_guid=>get_new_guid( ).

    SUBMIT (go_dynprg->gs_dynprog-programm)
           WITH p_guid = lv_guid
           VIA SELECTION-SCREEN AND RETURN.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_tvarv
           FROM zbct_int_parcels
           WHERE guid EQ lv_guid
           ##TOO_MANY_ITAB_FIELDS.

    DELETE FROM zbct_int_parcels WHERE guid EQ lv_guid.

  ENDMETHOD.
ENDCLASS.