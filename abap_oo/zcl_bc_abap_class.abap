CLASS zcl_bc_abap_class DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

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

      tt_component      TYPE HASHED TABLE OF t_component
        WITH UNIQUE KEY primary_key COMPONENTS cmpname,

      tt_component_sort TYPE SORTED TABLE OF t_component
        WITH UNIQUE KEY primary_key COMPONENTS cmpname,

      tt_component_std  TYPE STANDARD TABLE OF t_component WITH DEFAULT KEY,

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

    DATA core   TYPE REF TO ycl_addict_class READ-ONLY.
    DATA gs_def TYPE seoclass                READ-ONLY.

    CLASS-METHODS:

      check_class_existence
        IMPORTING iv_clsname TYPE seoclsname
        RAISING   zcx_bc_table_content,

      check_class_has_interface
        IMPORTING iv_class     TYPE seoclsname
                  iv_interface TYPE seoclsname
        RAISING   zcx_bc_table_content,

      convert_prgname_to_clsname
        IMPORTING iv_prgname        TYPE clike
        RETURNING VALUE(rv_clsname) TYPE seoclsname,

      get_current_method_safe
        RETURNING VALUE(result) TYPE seocpdname,

      get_instance
        IMPORTING iv_clsname    TYPE seoclsname
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_abap_class
        RAISING   zcx_bc_table_content.

    METHODS:

      accept
        IMPORTING io_visitor TYPE REF TO zif_bc_abap_class_visitor
        RAISING   zcx_bc_class_method,

      dequeue_exec,

      enqueue_exec RAISING zcx_bc_lock,

      get_components
        IMPORTING is_param    TYPE t_component_param OPTIONAL
        EXPORTING et_cmp_hash TYPE tt_component
                  et_cmp_sort TYPE tt_component_sort
                  et_cmp_std  TYPE tt_component_std,

      get_immediate_subclass_names RETURNING VALUE(rt_clsname) TYPE tt_clsname,
      get_instanceable_subclasses  RETURNING VALUE(rt_clsname) TYPE tt_clsname,
      get_recursive_subclass_names RETURNING VALUE(rt_clsname) TYPE tt_clsname,
      get_text                     RETURNING VALUE(rs_txt)     TYPE seoclasstx,

      is_in_call_stack             RETURNING VALUE(rv_stack)   TYPE abap_bool,

      search_class_doc
        IMPORTING it_word       TYPE tt_dok_text
        RETURNING VALUE(rv_hit) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_multiton,
        clsname TYPE seoclsname,
        obj     TYPE REF TO zcl_bc_abap_class,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS clsname.

    CLASS-DATA gt_multiton TYPE tt_multiton.
ENDCLASS.


CLASS zcl_bc_abap_class IMPLEMENTATION.
  METHOD check_class_existence.
    TRY.
        ycl_addict_class=>check_class_existence( iv_clsname ).
      CATCH ycx_addict_table_content INTO DATA(table_content_error).
        zcx_bc_table_content=>raise_from_addict( table_content_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD check_class_has_interface.
    TRY.
        ycl_addict_class=>check_class_has_interface( class     = iv_class
                                                     interface = iv_interface ).
      CATCH ycx_addict_table_content INTO DATA(table_content_error).
        zcx_bc_table_content=>raise_from_addict( table_content_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD search_class_doc.
    rv_hit = me->core->search_class_doc( VALUE #( FOR _word IN it_word
                                                  ( _word-dok_text ) ) ).
  ENDMETHOD.

  METHOD convert_prgname_to_clsname.
    rv_clsname = ycl_addict_class=>convert_prgname_to_clsname( iv_prgname ).
  ENDMETHOD.

  METHOD get_current_method_safe.
    DATA cs TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING et_callstack = cs.

    result = VALUE #( cs[ 2 ]-eventname OPTIONAL ).
  ENDMETHOD.

  METHOD is_in_call_stack.
    rv_stack = me->core->is_in_call_stack( ).
  ENDMETHOD.

  METHOD get_components.
    me->core->get_components( EXPORTING param    = CORRESPONDING #( is_param )
                              IMPORTING cmp_hash = DATA(cmp_hash)
                                        cmp_sort = DATA(cmp_sort)
                                        cmp_std  = DATA(cmp_std) ).

    et_cmp_hash = cmp_hash.
    et_cmp_sort = cmp_sort.
    et_cmp_std  = cmp_std.
  ENDMETHOD.

  METHOD get_immediate_subclass_names.
    rt_clsname = me->core->get_immediate_subclass_names( ).
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key
                        COMPONENTS clsname = iv_clsname ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( clsname = iv_clsname ).
      ls_multiton-obj = NEW #( ).

      TRY.
          ls_multiton-obj->core = ycl_addict_class=>get_instance( iv_clsname ).
        CATCH ycx_addict_table_content INTO DATA(table_content_error).
          zcx_bc_table_content=>raise_from_addict( table_content_error ).
      ENDTRY.

      ls_multiton-obj->gs_def = CORRESPONDING #( ls_multiton-obj->core->def ).
      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD accept.
    io_visitor->visit( me ).
  ENDMETHOD.

  METHOD get_recursive_subclass_names.
    rt_clsname = me->core->get_recursive_subclass_names( ).
  ENDMETHOD.

  METHOD get_text.
    rs_txt = me->core->get_text( ).
  ENDMETHOD.

  METHOD dequeue_exec.
    me->core->dequeue_exec( ).
  ENDMETHOD.

  METHOD enqueue_exec.
    TRY.
        me->core->enqueue_exec( ).
      CATCH ycx_addict_lock INTO DATA(error).
        zcx_bc_lock=>raise_from_addict( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_instanceable_subclasses.
    rt_clsname = me->core->get_instanceable_subclasses( ).
  ENDMETHOD.
ENDCLASS.