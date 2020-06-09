CLASS zcl_bc_tadir_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_tadir_key,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF t_tadir_key,

      tt_tadir_key TYPE STANDARD TABLE OF t_tadir_key WITH DEFAULT KEY.

    class-methods:
      does_object_exist_facade
        IMPORTING !is_key         TYPE t_tadir_key
        RETURNING VALUE(rv_exist) TYPE abap_bool
        raising   zcx_bc_function_subrc.

    METHODS:

      does_object_exist
        IMPORTING !is_key         TYPE t_tadir_key
        RETURNING VALUE(rv_exist) TYPE abap_bool,

      read_tadir
        IMPORTING
          !iv_sysnam          TYPE tmscsys-sysnam optional
          !it_tadir_key       TYPE tt_tadir_key
          !iv_exclude_deleted type abap_bool default abap_False
        raising
          zcx_Bc_function_subrc.

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      begin of t_doef_cache,
        tadir_key type t_tadir_key,
        exists    type abap_bool,
      end of t_Doef_cache,

      tt_doef_cache type hashed table of t_doef_cache with unique key primary_key components tadir_key,
      tt_Tadir type standard table of tadir with default key,
      tt_tadir_key_hash type hashed table of t_Tadir_key with unique key primary_key components pgmid object obj_name,

      begin of t_state,
        tadir_key type tt_tadir_key_hash,
      end of t_state.

    class-data gt_doef_cache type tt_doef_cache.

    data:
      gs_state      type t_state.

ENDCLASS.



CLASS ZCL_BC_TADIR_READER IMPLEMENTATION.


  method does_object_exist.

    rv_exist = xsdbool( line_exists( gs_state-tadir_key[ key primary_key components
      pgmid    = is_key-pgmid
      object   = is_key-object
      obj_name = is_key-obj_name
    ] ) ).

  endmethod.


  method does_object_exist_facade.

    assign gt_Doef_cache[ key primary_key components tadir_key = is_key ] to field-symbol(<ls_cache>).
    if sy-subrc ne 0.

      data(ls_cache) = value t_doef_cache( tadir_key = is_key ).

      data(lo_Reader) = new zcl_bc_tadir_Reader( ).
      lo_reader->read_tadir( value #( ( is_key ) ) ).
      ls_cache-exists = lo_reader->does_object_exist( is_key ).

      insert ls_cache into table gt_doef_cache assigning <ls_cache>.

    endif.

    rv_exist = <ls_cache>-exists.

  endmethod.


  method read_tadir.

    DATA:
      lt_dat     TYPE STANDARD TABLE OF tab512,
      lt_Del_rng type range of tadir-delflag,
      lt_fld     TYPE STANDARD TABLE OF rfc_db_fld,
      lt_opt     TYPE STANDARD TABLE OF rfc_db_opt,
      lt_Tadir   type tt_Tadir.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Hazırlık
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    clear gs_state.
    check it_tadir_key is not initial.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Aynı sistem ise, doğrudan tabloya git
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    if iv_sysnam is initial.

      if iv_exclude_deleted eq abap_true.
        lt_del_rng = value #( ( option = zcl_Bc_ddic_toolkit=>c_option_eq
                                sign   = zcl_Bc_ddic_toolkit=>c_sign_e
                                low    = abap_true
                            ) ).
      endif.

      select pgmid, object, obj_name
        into corresponding fields of table @gs_state-tadir_key
        from tadir
        for all entries in @it_tadir_key
        where pgmid    eq @it_Tadir_key-pgmid    and
              object   eq @it_Tadir_key-object   and
              obj_name eq @it_tadir_key-obj_name and
              delflag  in @lt_del_rng.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Farklı bir sistem söz konusuysa, değerleri RFC ile al
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    else.

      loop at it_Tadir_key assigning field-symbol(<ls_Tadir_key>).

        if lt_opt is not initial.
          append value #( text = 'OR' ) to lt_opt.
        endif.

        APPEND VALUE #( text = '(' ) TO lt_opt.

        APPEND VALUE #( text = |PGMID = '{ <ls_tadir_key>-pgmid }' AND| ) TO lt_opt.
        APPEND VALUE #( text = |OBJECT = '{ <ls_tadir_key>-object }' AND| ) TO lt_opt.
        APPEND VALUE #( text = |OBJ_NAME = '{ <ls_tadir_key>-obj_name }'| ) TO lt_opt.

        if iv_exclude_deleted eq abap_True.
          APPEND VALUE #( text = | AND ( DELFLAG = '' OR DELFLAG IS NULL )| ) TO lt_opt.
        endif.

        APPEND VALUE #( text = ')' ) TO lt_opt.

      endloop.

      check lt_opt is not initial.

      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION iv_sysnam
        EXPORTING
          query_table          = 'TADIR'
        TABLES
          options              = lt_opt
          fields               = lt_fld
          data                 = lt_dat
        EXCEPTIONS
          table_not_available  = 1
          table_without_data   = 2
          option_not_valid     = 3
          field_not_valid      = 4
          not_authorized       = 5
          data_buffer_exceeded = 6
          OTHERS               = 7 ##FM_SUBRC_OK.

      zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'RFC_READ_TABLE' ).

      lt_Tadir = lt_Dat.
      gs_state-tadir_key = corresponding #( lt_tadir ).

    endif.

  endmethod.
ENDCLASS.