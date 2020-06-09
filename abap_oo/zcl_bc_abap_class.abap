CLASS zcl_bc_abap_class DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:

      BEGIN OF t_clsname,
        clsname TYPE seoclsname,
      END OF t_clsname,

      tt_clsname     TYPE STANDARD TABLE OF t_clsname WITH DEFAULT KEY,
      tt_clsname_rng TYPE RANGE OF seoclsname,

      tt_cmpname_rng TYPE RANGE OF seocompo-cmpname,
      tt_cmptype_rng TYPE RANGE OF seocompo-cmptype,

      BEGIN OF t_component,
        cmpname TYPE seocmpname,
        cmptype TYPE seocmptype,
        mtdtype TYPE seomtdtype,
      END OF t_component,

      tt_component
        TYPE HASHED TABLE OF t_component
        WITH UNIQUE KEY primary_key COMPONENTS cmpname,

      tt_component_sort
        TYPE SORTED TABLE OF t_component
        WITH UNIQUE KEY primary_key COMPONENTS cmpname,

      tt_component_std
        TYPE STANDARD TABLE OF t_component
        WITH DEFAULT KEY,

      tt_mtdtype_rng    TYPE RANGE OF seocompo-mtdtype,

      BEGIN OF t_component_param,
        cmpname_rng TYPE tt_cmpname_rng,
        cmptype_rng TYPE tt_cmptype_rng,
        mtdtype_rng TYPE tt_mtdtype_rng,
      END OF t_component_param,

      BEGIN OF t_dok_text,
        dok_text TYPE dok_text,
      END OF t_dok_text,

      tt_dok_text TYPE STANDARD TABLE OF t_dok_text WITH DEFAULT KEY.

    CONSTANTS:
      c_cmptype_method   TYPE seocmptype VALUE '1',
      c_meth_constructor TYPE seocpdname VALUE 'CONSTRUCTOR'.

    DATA:
      gs_def       TYPE seoclass READ-ONLY.

    CLASS-METHODS:

      check_class_existence
        IMPORTING !iv_clsname TYPE seoclsname
        RAISING   zcx_bc_table_content,

      check_class_has_interface
        IMPORTING
          !iv_class     TYPE seoclsname
          !iv_interface TYPE seoclsname
        RAISING
          zcx_bc_table_content,

      convert_prgname_to_clsname
        IMPORTING !iv_prgname       TYPE clike
        RETURNING VALUE(rv_clsname) TYPE seoclsname,

      get_instance
        IMPORTING !iv_clsname   TYPE seoclsname
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_abap_class
        RAISING   zcx_bc_table_content.

    METHODS:

      accept
        IMPORTING !io_visitor TYPE REF TO zif_bc_abap_class_visitor
        RAISING   zcx_bc_class_method,

      dequeue_exec,

      enqueue_exec RAISING zcx_bc_lock,

      get_components
        IMPORTING
          !is_param    TYPE t_component_param OPTIONAL
        EXPORTING
          !et_cmp_hash TYPE tt_component
          !et_cmp_sort TYPE tt_component_sort
          !et_cmp_std  TYPE tt_component_std,

      get_immediate_subclass_names RETURNING VALUE(rt_clsname) TYPE tt_clsname,
      get_instanceable_subclasses RETURNING VALUE(rt_clsname) TYPE tt_clsname,
      get_recursive_subclass_names RETURNING VALUE(rt_clsname) TYPE tt_clsname,
      get_text RETURNING VALUE(rs_txt) TYPE seoclasstx,

      is_in_call_stack RETURNING VALUE(rv_stack) TYPE abap_bool,

      search_class_doc
        IMPORTING !it_word      TYPE tt_dok_text
        RETURNING VALUE(rv_hit) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        clsname TYPE seoclsname,
        obj     TYPE REF TO zcl_bc_abap_class,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS clsname.

    CONSTANTS:
      c_doku_id_class  TYPE dokil-id  VALUE 'CL',
      c_doku_typ_class TYPE dokil-typ VALUE 'E',
      c_option_eq      TYPE ddoption  VALUE 'EQ',
      c_sign_i         TYPE ddsign    VALUE 'I',
      c_tabname_def    TYPE tabname   VALUE 'SEOCLASS',
      c_tabname_rel    TYPE tabname   VALUE 'SEOMETAREL'.

    DATA:
      gs_dokil                 TYPE dokil,
      gs_txt                   TYPE seoclasstx,
      gt_component_std         TYPE tt_component_std,
      gt_immed_subcls_nam      TYPE tt_clsname,
      gt_insta_subcls_nam      TYPE tt_clsname,
      gt_recur_subcls_nam      TYPE tt_clsname,
      gv_dokil_read            TYPE abap_bool,
      gv_txt_read              TYPE abap_bool,
      gv_component_read        TYPE abap_bool,
      gv_immed_subcls_nam_read TYPE abap_bool,
      gv_insta_subcls_nam_read TYPE abap_bool,
      gv_recur_subcls_nam_read TYPE abap_bool.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      get_recursive_subclass_names_p
        IMPORTING
                  !iv_refclsname    TYPE seoclsname
                  !iv_rec           TYPE abap_bool
        RETURNING VALUE(rt_clsname) TYPE tt_clsname,

      read_dokil.

ENDCLASS.



CLASS zcl_bc_abap_class IMPLEMENTATION.


  METHOD accept.
    io_visitor->visit( me ).
  ENDMETHOD.


  METHOD check_class_existence.
    get_instance( iv_clsname ).
  ENDMETHOD.


  METHOD check_class_has_interface.

    DATA(lt_cls) = get_instance( iv_interface )->get_instanceable_subclasses( ).
    CHECK NOT line_exists( lt_cls[ clsname = iv_class ] ).

    RAISE EXCEPTION TYPE zcx_bc_table_content
      EXPORTING
        objectid = CONV #( |{ iv_class } { iv_interface }| )
        tabname  = c_tabname_rel
        textid   = zcx_bc_table_content=>entry_missing.

  ENDMETHOD.


  METHOD convert_prgname_to_clsname.
    rv_clsname = iv_prgname+0(30).
    REPLACE ALL OCCURRENCES OF '=' IN rv_clsname WITH space.
  ENDMETHOD.


  METHOD dequeue_exec.

    CALL FUNCTION 'DEQUEUE_EZBC_CLSNAME'
      EXPORTING
        clsname = gs_def-clsname.

  ENDMETHOD.


  METHOD enqueue_exec.

    TRY.

        CALL FUNCTION 'ENQUEUE_EZBC_CLSNAME'
          EXPORTING
            clsname        = gs_def-clsname
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'ENQUEUE_EZBC_CLSNAME' ).

      CATCH zcx_bc_function_subrc INTO DATA(lo_cx_fsr).

        RAISE EXCEPTION TYPE zcx_bc_lock
          EXPORTING
            textid   = zcx_bc_lock=>locked_by_user
            previous = lo_cx_fsr
            bname    = CONV #( sy-msgv1 ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_components.

    IF gv_component_read EQ abap_false. " Lazy initialization

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE @gt_component_std
        FROM seocompo
        WHERE clsname EQ @gs_def-clsname.

      gv_component_read = abap_true.
    ENDIF.

    et_cmp_std = gt_component_std.

    DELETE et_cmp_std WHERE NOT (
      cmpname IN is_param-cmpname_rng AND
      cmptype IN is_param-cmptype_rng AND
      mtdtype IN is_param-mtdtype_rng
    ).

    IF et_cmp_hash IS REQUESTED.
      et_cmp_hash = et_cmp_std.
    ENDIF.

    IF et_cmp_sort IS REQUESTED.
      et_cmp_sort = et_cmp_std.
    ENDIF.

  ENDMETHOD.


  METHOD get_immediate_subclass_names.

    IF gv_immed_subcls_nam_read EQ abap_false. " Lazy initialization

      SELECT clsname
        INTO CORRESPONDING FIELDS OF TABLE gt_immed_subcls_nam
        FROM seometarel
        WHERE refclsname EQ gs_def-clsname.             "#EC CI_GENBUFF

      gv_immed_subcls_nam_read = abap_true.
    ENDIF.

    rt_clsname = gt_immed_subcls_nam.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS clsname = iv_clsname
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( clsname = iv_clsname ).
      ls_multiton-obj = NEW #( ).

      SELECT SINGLE *
        INTO @ls_multiton-obj->gs_def
        FROM seoclass
        WHERE clsname EQ @ls_multiton-clsname.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-clsname )
            tabname  = c_tabname_def.
      ENDIF.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_instanceable_subclasses.

    IF gv_insta_subcls_nam_read EQ abap_false. " Lazy initialization

      gv_insta_subcls_nam_read = abap_true.

      gt_insta_subcls_nam = get_recursive_subclass_names( ).
      IF gt_insta_subcls_nam IS INITIAL.
        RETURN.
      ENDIF.

      DATA(lt_clsname_rng) = VALUE tt_clsname_rng(
        FOR ls_cn IN gt_insta_subcls_nam (
          option = c_option_eq
          sign = c_sign_i
          low = ls_cn-clsname
        )
      ).

      SELECT clsname
        INTO TABLE @DATA(lt_abstract)
        FROM seoclassdf AS sd1
        WHERE
          clsname IN @lt_clsname_rng AND
          version GT 0 AND
          version EQ (
            SELECT MAX( version )
            FROM seoclassdf AS sd2
            WHERE clsname EQ sd1~clsname
          ) AND
          clsabstrct EQ @abap_true
        ORDER BY clsname.                              "#EC CI_BUFFSUBQ

      LOOP AT gt_insta_subcls_nam ASSIGNING FIELD-SYMBOL(<ls_clsname>).

        READ TABLE lt_abstract
          TRANSPORTING NO FIELDS
          WITH KEY clsname = <ls_clsname>-clsname
          BINARY SEARCH.

        CHECK sy-subrc EQ 0.

        DELETE gt_insta_subcls_nam.
        CONTINUE.

      ENDLOOP.

    ENDIF.

    rt_clsname = gt_insta_subcls_nam.

  ENDMETHOD.


  METHOD get_recursive_subclass_names.

    IF gv_recur_subcls_nam_read EQ abap_false. " Lazy initialization

      get_recursive_subclass_names_p(
          iv_refclsname = gs_def-clsname
          iv_rec        = abap_false ).

      gv_recur_subcls_nam_read = abap_true.
    ENDIF.

    rt_clsname = gt_recur_subcls_nam.

  ENDMETHOD.


  METHOD get_recursive_subclass_names_p.

    TRY.
        DATA(lt_clsname_local) = zcl_bc_abap_class=>get_instance( iv_refclsname )->get_immediate_subclass_names( ).
      CATCH cx_root ##no_handler .
        RETURN.
    ENDTRY.

    LOOP AT lt_clsname_local ASSIGNING FIELD-SYMBOL(<ls_cl>).
      APPEND:
        <ls_cl> TO gt_recur_subcls_nam,
        LINES OF get_recursive_subclass_names_p(
          iv_refclsname = <ls_cl>-clsname
          iv_rec        = abap_true
        ) TO gt_recur_subcls_nam.
    ENDLOOP.

    IF iv_rec EQ abap_true.
      RETURN.
    ENDIF.

    SORT gt_recur_subcls_nam BY clsname.
    DELETE ADJACENT DUPLICATES FROM gt_recur_subcls_nam COMPARING clsname.

    rt_clsname[] = gt_recur_subcls_nam[].

  ENDMETHOD.


  METHOD get_text.

    IF gv_txt_read EQ abap_false. " Lazy initialization

      SELECT SINGLE *
        INTO @gs_txt
        FROM seoclasstx
        WHERE
          clsname EQ @gs_def-clsname AND
          langu EQ @sy-langu.

      IF sy-subrc NE 0.
        SELECT SINGLE *
          INTO @gs_txt
          FROM seoclasstx
          WHERE clsname EQ @gs_def-clsname
          ##WARN_OK .                                   "#EC CI_NOORDER
      ENDIF.

      gv_txt_read = abap_true.
    ENDIF.

    rs_txt = gs_txt.

  ENDMETHOD.

  METHOD is_in_call_stack.

    DATA lt_cs TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = lt_cs.

    ASSERT lt_cs IS NOT INITIAL. " Can't be

    DATA(lv_progname_pattern) = |{ gs_def-clsname }*|.

    LOOP AT lt_cs
      TRANSPORTING NO FIELDS
      WHERE progname CP lv_progname_pattern.

      rv_stack = abap_true.
      RETURN.

    ENDLOOP.

  ENDMETHOD.

  METHOD read_dokil.

    CHECK gv_dokil_read EQ abap_false.
    gv_dokil_read = abap_true.

    SELECT SINGLE * INTO @gs_dokil FROM dokil WHERE
      id     EQ @c_doku_id_class AND
      object EQ @gs_def-clsname AND
      langu  EQ @sy-langu AND
      typ    EQ @c_doku_typ_class.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * INTO @gs_dokil FROM dokil WHERE
      id     EQ @c_doku_id_class AND
      object EQ @gs_def-clsname AND
      typ    EQ @c_doku_typ_class
      ##WARN_OK.
                                                        "#EC CI_GENBUFF
                                                        "#EC CI_NOORDER

  ENDMETHOD.


  METHOD search_class_doc.

    DATA lt_hitlist TYPE STANDARD TABLE OF tline.

    CHECK it_word IS NOT INITIAL.

    read_dokil( ).
    IF gs_dokil IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_word ASSIGNING FIELD-SYMBOL(<ls_word>).

      CALL FUNCTION 'DOCU_SEARCH_TEXT'
        EXPORTING
          id           = gs_dokil-id
          langu        = gs_dokil-langu
          object       = gs_dokil-object
          typ          = gs_dokil-typ
          searchstring = <ls_word>-dok_text
        TABLES
          hitlist      = lt_hitlist.

      CHECK lt_hitlist IS NOT INITIAL.
      rv_hit = abap_true.
      RETURN.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.