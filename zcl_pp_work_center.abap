CLASS zcl_pp_work_center DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_header,
        objty TYPE crhd-objty,
        objid TYPE crhd-objid,
        arbpl TYPE crhd-arbpl,
        werks TYPE crhd-werks,
        lvorm TYPE crhd-lvorm,
        begda TYPE crhd-begda,
        endda TYPE crhd-endda,
      END OF t_header.

    DATA gs_header TYPE t_header.

    CLASS-METHODS:
      convert_arbpl
        IMPORTING !iv_objid       TYPE objid
        RETURNING VALUE(rv_arbpl) TYPE arbpl,

      get_instance_by_arbpl
        IMPORTING
          !iv_arbpl     TYPE crhd-arbpl
          !iv_werks     TYPE crhd-werks
        RETURNING
          VALUE(ro_obj) TYPE REF TO zcl_pp_work_center
        RAISING
          zcx_bc_creation,

      get_ktext
        IMPORTING
          !iv_arbpl       TYPE arbpl
          !iv_spras       TYPE sylangu DEFAULT sy-langu
          !iv_werks       TYPE werks_d
        RETURNING
          VALUE(rv_ktext) TYPE ktext .

    METHODS:
      get_activity_prices
        RETURNING VALUE(rt_prices) TYPE zmmtt_maf_cost_item
        RAISING   zcx_bc_int_data_read,

      get_text RETURNING VALUE(rv_ktext) TYPE ktext.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_arbpl,
        objid TYPE crhd-objid,
        arbpl TYPE crhd-arbpl,
      END OF t_arbpl,

      BEGIN OF t_ktext,
        arbpl TYPE crhd_v1-arbpl,
        spras TYPE crhd_v1-spras,
        werks TYPE crhd_v1-werks,
        ktext TYPE crhd_v1-ktext,
      END OF t_ktext ,

      BEGIN OF t_arbpl_multiton,
        arbpl       TYPE crhd-arbpl,
        werks       TYPE crhd-werks,
        work_center TYPE REF TO zcl_pp_work_center,
      END OF t_arbpl_multiton,

      tt_arbpl_multiton TYPE HASHED TABLE OF t_arbpl_multiton
                        WITH UNIQUE KEY primary_key COMPONENTS arbpl werks,

      tt_kostl                TYPE STANDARD TABLE OF kostl WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_tabname,
        header TYPE tabname VALUE 'CRHD',
      END OF c_tabname.

    CLASS-DATA:
      gt_arbpl          TYPE HASHED TABLE OF t_arbpl WITH UNIQUE KEY primary_key COMPONENTS objid,
      gt_arbpl_multiton TYPE tt_arbpl_multiton,
      gt_ktext          TYPE HASHED TABLE OF t_ktext WITH UNIQUE KEY primary_key COMPONENTS arbpl spras werks.

    DATA:
      gt_activity_price_cache TYPE zmmtt_maf_cost_item,
      gt_kostl_cache          TYPE tt_kostl,
      gv_activity_price_read  type abap_bool,
      gv_kostl_read           TYPE abap_bool.

    METHODS:
      get_kostl_list RETURNING VALUE(rt_kostl) TYPE tt_kostl.

ENDCLASS.



CLASS zcl_pp_work_center IMPLEMENTATION.


  METHOD convert_arbpl.

    DATA ls_arbpl TYPE t_arbpl.

    ASSIGN gt_arbpl[ KEY primary_key COMPONENTS objid = CONV #( iv_objid ) ] TO FIELD-SYMBOL(<ls_arbpl>).
    IF sy-subrc NE 0.

      SELECT SINGLE arbpl FROM crhd INTO ls_arbpl-arbpl
        WHERE objty EQ 'A' AND
              objid EQ iv_objid.

      ls_arbpl-objid = iv_objid.
      INSERT ls_arbpl INTO TABLE gt_arbpl ASSIGNING <ls_arbpl>.
    ENDIF.

    rv_arbpl = <ls_arbpl>-arbpl.

  ENDMETHOD.

  METHOD get_activity_prices.

    TRY.

        IF gv_Activity_price_Read eq abap_false.

          LOOP AT get_kostl_list( ) ASSIGNING FIELD-SYMBOL(<lv_kostl>).
            APPEND LINES OF zcl_mm_maf_cost=>get_instance(
                              VALUE #( kostl = <lv_kostl>
                                       werks = gs_Header-werks )
                            )->gt_cost TO gt_activity_price_cache.
          ENDLOOP.

          SORT gt_activity_price_cache BY lstar.
          DELETE ADJACENT DUPLICATES FROM gt_activity_price_cache COMPARING lstar. " Paranoya

          gv_activity_price_read = abap_true.

        ENDIF.

        rt_prices = gt_activity_price_cache.

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_int_data_read
          EXPORTING
            textid    = zcx_bc_int_data_read=>cant_read_data_type
            previous  = lo_diaper
            objectid  = |{ gs_header-arbpl } { gs_header-werks }|
            data_type = CONV #( TEXT-602 ).
    ENDTRY.


  ENDMETHOD.

  METHOD get_instance_by_arbpl.

    TRY.

        ASSIGN gt_arbpl_multiton[ KEY primary_key COMPONENTS
                                  arbpl = iv_arbpl
                                  werks = iv_werks
                                ] TO FIELD-SYMBOL(<ls_multiton>).

        IF sy-subrc NE 0.
          DATA(ls_multiton) = VALUE t_arbpl_multiton( arbpl = iv_arbpl
                                                      werks = iv_werks ).

          ls_multiton-work_center = NEW #( ).

          SELECT SINGLE objty, objid, arbpl, werks, lvorm, begda, endda
                 FROM crhd
                 WHERE arbpl EQ @ls_multiton-arbpl AND
                       werks EQ @ls_multiton-werks
                 INTO CORRESPONDING FIELDS OF @ls_multiton-work_center->gs_header.

          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE cx_no_entry_in_table
              EXPORTING
                table_name = CONV #( c_tabname-header )
                entry_name = |{ ls_multiton-arbpl } { ls_multiton-werks }|.
          ENDIF.

          INSERT ls_multiton INTO TABLE gt_arbpl_multiton ASSIGNING <ls_multiton>.

        ENDIF.

        ro_obj = <ls_multiton>-work_center.

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_creation
          EXPORTING
            textid      = zcx_bc_creation=>object_creation_with_key
            previous    = lo_diaper
            object_name = CONV #( TEXT-046 )
            object_key  = |{ iv_arbpl } { iv_werks }|.
    ENDTRY.

  ENDMETHOD.

  METHOD get_kostl_list.

    IF gv_kostl_read EQ abap_false.
      SELECT DISTINCT kostl FROM crco
             WHERE objty EQ @gs_header-objty AND
                   objid EQ @gs_header-objid AND
                   endda GE @sy-datum AND
                   begda LE @sy-datum
             INTO TABLE @gt_kostl_cache.
    ENDIF.

    rt_kostl = gt_kostl_cache.

  ENDMETHOD.

  METHOD get_ktext.

    DATA ls_ktext TYPE t_ktext.

    ASSIGN gt_ktext[ KEY primary_key COMPONENTS
      arbpl = iv_arbpl
      spras = iv_spras
      werks = iv_werks
    ] TO FIELD-SYMBOL(<ls_ktext>).

    IF sy-subrc NE 0.

      SELECT SINGLE ktext INTO ls_ktext-ktext
        FROM crhd_v1
        WHERE arbpl EQ iv_arbpl
          AND spras EQ iv_spras
          AND werks EQ iv_werks
        ##WARN_OK.

      ls_ktext-arbpl = iv_arbpl.
      ls_ktext-spras = iv_spras.
      ls_ktext-werks = iv_werks.
      INSERT ls_ktext INTO TABLE gt_ktext ASSIGNING <ls_ktext>.
    ENDIF.

    rv_ktext = <ls_ktext>-ktext.

  ENDMETHOD.

  METHOD get_text.
    rv_ktext = get_ktext( iv_arbpl = gs_header-arbpl
                          iv_werks = gs_header-werks ).
  ENDMETHOD.

ENDCLASS.