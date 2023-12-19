CLASS zcl_mm_vendor DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS: c_fnam_company TYPE fieldname VALUE 'BUKRS',
               c_fnam_vendor  TYPE fieldname VALUE 'LIFNR'.

    DATA gs_def TYPE lfa1.

    CLASS-METHODS cache_itab_multiton
      IMPORTING ir_itab        TYPE REF TO data
                iv_fnam_vendor TYPE fieldname DEFAULT c_fnam_vendor
      RAISING   zcx_bc_method_parameter.

    CLASS-METHODS cache_itab_with_adrc_data
      IMPORTING ir_itab         TYPE REF TO data
                iv_fnam_vendor  TYPE fieldname DEFAULT c_fnam_vendor
                iv_fnam_company TYPE fieldname DEFAULT c_fnam_company
      RAISING   zcx_bc_method_parameter.

    CLASS-METHODS cache_itab_with_comp_code_data
      IMPORTING ir_itab         TYPE REF TO data
                iv_fnam_vendor  TYPE fieldname DEFAULT c_fnam_vendor
                iv_fnam_company TYPE fieldname DEFAULT c_fnam_company
      RAISING   zcx_bc_method_parameter.

    CLASS-METHODS get_default_acc_grp_vals
      IMPORTING iv_bukrs      TYPE bukrs
                iv_ktokk      TYPE ktokk
      RETURNING VALUE(rs_def) TYPE zfit_xk_dzahls
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_instance
      IMPORTING iv_lifnr      TYPE lifnr
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_vendor
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_instance_by_tax_code
      IMPORTING stcd2         TYPE stcd2
      RETURNING VALUE(output) TYPE REF TO zcl_mm_vendor
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_lfa1
      IMPORTING iv_lifnr       TYPE lifnr
      RETURNING VALUE(rs_lfa1) TYPE lfa1.

    CLASS-METHODS get_name1_safe
      IMPORTING iv_lifnr        TYPE lifnr
      RETURNING VALUE(rv_name1) TYPE lfa1-name1.

    METHODS get_adrc RETURNING VALUE(rs_adrc) TYPE adrc.

    METHODS get_company_code_data
      IMPORTING iv_bukrs     TYPE bukrs
      RETURNING VALUE(rs_cc) TYPE lfb1
      RAISING   zcx_bc_table_content.

    METHODS get_supplier_emails
      IMPORTING werks         TYPE werks_d OPTIONAL
      RETURNING VALUE(result) TYPE zbctt_rec_list.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_company_code_data,
             bukrs TYPE bukrs,
             lfb1  TYPE lfb1,
             cx    TYPE REF TO zcx_bc_table_content,
           END OF t_company_code_data,

           tt_company_code_data TYPE HASHED TABLE OF t_company_code_data
                                  WITH UNIQUE KEY primary_key COMPONENTS bukrs,

           tt_def_acc_grp_val
                                  TYPE HASHED TABLE OF zfit_xk_dzahls
                                  WITH UNIQUE KEY primary_key COMPONENTS bukrs ktokk,

           BEGIN OF t_lazy_flg,
             adrc TYPE abap_bool,
           END OF t_lazy_flg,

           BEGIN OF t_lazy_val,
             adrc TYPE adrc,
           END OF t_lazy_val,

           BEGIN OF t_lazy,
             flg TYPE t_lazy_flg,
             val TYPE t_lazy_val,
           END OF t_lazy,

           BEGIN OF t_lfa1_key,
             lifnr TYPE lfa1-lifnr,
           END OF t_lfa1_key,

           tt_lfa1_key TYPE STANDARD TABLE OF t_lfa1_key
                       WITH DEFAULT KEY,

           BEGIN OF t_lfb1_key,
             bukrs TYPE lfb1-bukrs,
             lifnr TYPE lfb1-lifnr,
           END OF t_lfb1_key,

           tt_lfb1_key TYPE STANDARD TABLE OF t_lfb1_key
                       WITH DEFAULT KEY,

           BEGIN OF t_mt,
             lifnr TYPE lifnr,
             obj   TYPE REF TO zcl_mm_vendor,
           END OF t_mt,

           tt_mt TYPE HASHED TABLE OF t_mt WITH UNIQUE KEY primary_key COMPONENTS lifnr.

    TYPES: BEGIN OF tax_multiton_dict,
             stcd2 TYPE lfa1-stcd2,
             obj   TYPE REF TO zcl_mm_vendor,
             cx    TYPE REF TO zcx_bc_table_content,
           END OF tax_multiton_dict,

           tax_multiton_set TYPE HASHED TABLE OF tax_multiton_dict
                            WITH UNIQUE KEY primary_key COMPONENTS stcd2.

    CONSTANTS: c_clsname_me            TYPE seoclsname VALUE 'ZCL_MM_VENDOR',
               c_meth_cim              TYPE seocpdname VALUE 'CACHE_ITAB_MULTITON',
               c_meth_ciwccd           TYPE seocpdname VALUE 'CACHE_ITAB_WITH_COMP_CODE_DATA',
               c_tabname_company_data  TYPE tabname    VALUE 'LFB1',
               c_tabname_def           TYPE tabname    VALUE 'LFA1',
               c_tabname_def_pay_block TYPE tabname    VALUE 'ZFIT_XK_DZAHLS'.

    CLASS-DATA: gt_def_acc_grp_val TYPE tt_def_acc_grp_val,
                gt_lfa1            TYPE HASHED TABLE OF lfa1 WITH UNIQUE KEY primary_key COMPONENTS lifnr,
                gt_mt              TYPE tt_mt.

    CLASS-DATA tax_multiton TYPE tax_multiton_set.

    DATA: gs_lazy              TYPE t_lazy,
          gt_company_code_data TYPE tt_company_code_data.

    CLASS-METHODS create_and_cache_multiton IMPORTING it_lfa1_key TYPE tt_lfa1_key.

    METHODS constructor
      IMPORTING iv_lifnr TYPE lifnr OPTIONAL
                is_lfa1  TYPE lfa1  OPTIONAL
      RAISING   zcx_bc_table_content.

ENDCLASS.


CLASS zcl_mm_vendor IMPLEMENTATION.
  METHOD cache_itab_with_adrc_data.
    DATA: lv_append_a TYPE abap_bool,
          lv_append_b TYPE abap_bool,
          lt_lfa1_key TYPE tt_lfa1_key,
          lt_adrc_key TYPE tt_lfa1_key.

    FIELD-SYMBOLS: <lt_itab>  TYPE ANY TABLE,
                   <lv_lifnr> TYPE lfb1-lifnr.

    CHECK ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.

    IF <lt_itab> IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).
      ASSIGN COMPONENT iv_fnam_vendor OF STRUCTURE <ls_itab> TO <lv_lifnr>.

      IF <lv_lifnr> IS NOT ASSIGNED.
        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING textid      = zcx_bc_method_parameter=>param_value_invalid
                    class_name  = c_clsname_me
                    method_name = c_meth_ciwccd
                    param_name  = |{ iv_fnam_vendor }|.
      ENDIF.

      CLEAR: lv_append_a,
             lv_append_b.

      ASSIGN gt_mt[ lifnr = <lv_lifnr> ]
             TO FIELD-SYMBOL(<ls_mt>).

      IF sy-subrc = 0.
        lv_append_a = abap_false.
        lv_append_b = xsdbool( <ls_mt>-obj->gs_lazy-flg-adrc = abap_false ).
      ELSE.
        lv_append_a = abap_true.
        lv_append_b = abap_true.
      ENDIF.

      IF lv_append_a = abap_true.
        APPEND VALUE #( lifnr = <lv_lifnr> ) TO lt_lfa1_key.
      ENDIF.

      IF lv_append_b = abap_true.
        APPEND VALUE #( lifnr = <lv_lifnr> ) TO lt_adrc_key.
      ENDIF.
    ENDLOOP.

    SORT lt_lfa1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1_key.
    SORT lt_adrc_key.
    DELETE ADJACENT DUPLICATES FROM lt_adrc_key.

    create_and_cache_multiton( lt_lfa1_key ).

    IF lt_adrc_key IS NOT INITIAL.

      SELECT lfa1~lifnr, adrc~*
             FROM lfa1
                  INNER JOIN adrc ON adrc~addrnumber = lfa1~adrnr
             FOR ALL ENTRIES IN @lt_adrc_key
             WHERE lfa1~lifnr     = @lt_adrc_key-lifnr AND
                   adrc~date_from <= @sy-datum AND
                   adrc~date_to   >= @sy-datum
             INTO TABLE @DATA(lt_adrc).

      LOOP AT lt_adrc ASSIGNING FIELD-SYMBOL(<ls_adrc>).
        ASSIGN gt_mt[ KEY primary_key COMPONENTS lifnr = <ls_adrc>-lifnr ]
               TO <ls_mt>.

        CHECK sy-subrc = 0. " Paranoya

        <ls_mt>-obj->gs_lazy-flg-adrc = abap_true.
        <ls_mt>-obj->gs_lazy-val-adrc = CORRESPONDING #( <ls_adrc>-adrc ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD cache_itab_with_comp_code_data.
    DATA: lv_append_a TYPE abap_bool,
          lv_append_b TYPE abap_bool,
          lt_lfa1_key TYPE tt_lfa1_key,
          lt_lfb1_key TYPE tt_lfb1_key.

    FIELD-SYMBOLS: <lt_itab>  TYPE ANY TABLE,
                   <lv_bukrs> TYPE lfb1-bukrs,
                   <lv_lifnr> TYPE lfb1-lifnr.

    CHECK ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.

    IF <lt_itab> IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).
      ASSIGN COMPONENT: iv_fnam_company OF STRUCTURE <ls_itab> TO <lv_bukrs>,
                        iv_fnam_vendor  OF STRUCTURE <ls_itab> TO <lv_lifnr>.

      IF    <lv_lifnr> IS NOT ASSIGNED
         OR <lv_bukrs> IS NOT ASSIGNED.

        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING textid      = zcx_bc_method_parameter=>param_value_invalid
                    class_name  = c_clsname_me
                    method_name = c_meth_ciwccd
                    param_name  = |{ iv_fnam_company } { iv_fnam_vendor }|.
      ENDIF.

      CLEAR: lv_append_a,
             lv_append_b.

      ASSIGN gt_mt[ lifnr = <lv_lifnr> ] TO FIELD-SYMBOL(<ls_mt>).

      IF sy-subrc = 0.
        lv_append_a = abap_false.

        lv_append_b =
          xsdbool( NOT line_exists( <ls_mt>-obj->gt_company_code_data[ bukrs = <lv_bukrs> ] ) ).

      ELSE.
        lv_append_a = abap_true.
        lv_append_b = abap_true.
      ENDIF.

      IF lv_append_a = abap_true.
        APPEND VALUE #( lifnr = <lv_lifnr> )
               TO lt_lfa1_key.
      ENDIF.

      IF lv_append_b = abap_true.
        APPEND VALUE t_lfb1_key( lifnr = <lv_lifnr>
                                 bukrs = <lv_bukrs> )
               TO lt_lfb1_key.
      ENDIF.
    ENDLOOP.

    SORT lt_lfa1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1_key.
    SORT lt_lfb1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfb1_key.

    create_and_cache_multiton( lt_lfa1_key ).

    IF lt_lfb1_key IS NOT INITIAL.
      SELECT * FROM lfb1
               FOR ALL ENTRIES IN @lt_lfb1_key
               WHERE lifnr = @lt_lfb1_key-lifnr AND
                     bukrs = @lt_lfb1_key-bukrs
               INTO TABLE @DATA(lt_lfb1).

      LOOP AT lt_lfb1 ASSIGNING FIELD-SYMBOL(<ls_lfb1>).
        ASSIGN gt_mt[ KEY primary_key COMPONENTS lifnr = <ls_lfb1>-lifnr ]
               TO <ls_mt>.

        CHECK sy-subrc = 0.

        INSERT VALUE #( bukrs = <ls_lfb1>-bukrs
                        lfb1  = <ls_lfb1> )
               INTO TABLE <ls_mt>-obj->gt_company_code_data.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_name1_safe.
    TRY.
        rv_name1 = get_instance( iv_lifnr )->gs_def-name1.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    IF is_lfa1 IS SUPPLIED.
      gs_def = is_lfa1.
      RETURN.
    ENDIF.

    SELECT SINGLE * INTO @gs_def
      FROM lfa1
      WHERE lifnr = @iv_lifnr.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING textid   = zcx_bc_table_content=>entry_missing
                  objectid = CONV #( iv_lifnr )
                  tabname  = c_tabname_def.
    ENDIF.
  ENDMETHOD.

  METHOD create_and_cache_multiton.
    CHECK it_lfa1_key IS NOT INITIAL.

    SELECT * FROM lfa1
             FOR ALL ENTRIES IN @it_lfa1_key
             WHERE lifnr = @it_lfa1_key-lifnr
             INTO TABLE @DATA(lt_lfa1).

    LOOP AT lt_lfa1 ASSIGNING FIELD-SYMBOL(<ls_lfa1>).
      TRY.
          INSERT VALUE #( lifnr = <ls_lfa1>-lifnr
                          obj   = NEW #( is_lfa1 = <ls_lfa1> ) )
                 INTO TABLE gt_mt.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_by_tax_code.
    DATA(multiton) = REF #( zcl_mm_vendor=>tax_multiton ).

    ASSIGN multiton->*[ KEY primary_key COMPONENTS stcd2 = stcd2 ]
           TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(new_multiton) = VALUE tax_multiton_dict( stcd2 = stcd2 ).

      SELECT lifnr FROM lfa1
             WHERE stcd2 = @new_multiton-stcd2 ORDER BY PRIMARY KEY
             INTO @DATA(lifnr)
             UP TO 1 ROWS.
      ENDSELECT.

      CASE sy-subrc.
        WHEN 0.
          TRY.
              new_multiton-obj = get_instance( lifnr ).
            CATCH zcx_bc_table_content INTO new_multiton-cx ##NO_HANDLER.
          ENDTRY.

        WHEN OTHERS.
          new_multiton-cx = NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                      objectid = CONV #( new_multiton-stcd2 )
                                                      tabname  = c_tabname_def ).
      ENDCASE.

      INSERT new_multiton INTO TABLE multiton->* ASSIGNING <multiton>.
    ENDIF.

    IF <multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <multiton>-cx.
    ENDIF.

    output = <multiton>-obj.
  ENDMETHOD.

  METHOD get_adrc.
    IF gs_lazy-flg-adrc = abap_false.
      ##WARN_OK
      SELECT SINGLE * FROM adrc
             WHERE addrnumber = @gs_def-adrnr AND
                   date_from  <= @sy-datum AND
                   date_to    >= @sy-datum
             INTO @gs_lazy-val-adrc. "#EC CI_NOORDER

      gs_lazy-flg-adrc = abap_true.
    ENDIF.

    rs_adrc = gs_lazy-val-adrc.
  ENDMETHOD.

  METHOD get_company_code_data.
    ASSIGN gt_company_code_data[ KEY primary_key COMPONENTS bukrs = iv_bukrs ]
           TO FIELD-SYMBOL(<ls_cc>).

    IF sy-subrc <> 0.
      DATA(ls_cc) = VALUE t_company_code_data( bukrs = iv_bukrs ).

      SELECT SINGLE * FROM lfb1
             WHERE lifnr = @gs_def-lifnr AND
                   bukrs = @ls_cc-bukrs
             INTO @ls_cc-lfb1.

      IF sy-subrc <> 0.
        ls_cc-cx = NEW #( textid   = zcx_bc_table_content=>entry_missing
                          objectid = |{ gs_def-lifnr } { ls_cc-bukrs }|
                          tabname  = c_tabname_company_data ).
      ENDIF.

      INSERT ls_cc INTO TABLE gt_company_code_data
             ASSIGNING <ls_cc>.
    ENDIF.

    IF <ls_cc>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cc>-cx.
    ENDIF.

    rs_cc = <ls_cc>-lfb1.
  ENDMETHOD.

  METHOD get_supplier_emails.
    SELECT businesspartner UP TO 1 ROWS
           FROM i_businesspartnersupplier
           INTO @DATA(businesspartner)
           WHERE supplier = @gs_def-lifnr
           ORDER BY PRIMARY KEY.
    ENDSELECT.

    CHECK sy-subrc = 0.

    DATA(abtnr_rng) = COND zbctt_abtnr_range( WHEN werks IS SUPPLIED
                                              THEN VALUE #( ( sign   = ycl_addict_toolkit=>sign-include
                                                              option = ycl_addict_toolkit=>option-eq
                                                              low    = werks ) )
                                              ELSE VALUE #( ) ).

    SELECT DISTINCT smtp_address AS smtpadr
           INTO CORRESPONDING FIELDS OF TABLE @result
           FROM but051
           WHERE partner1       = @businesspartner AND
                 abtnr         IN @abtnr_rng       AND
                 smtp_address  <> @space           AND
                 ( NOT EXISTS ( SELECT mandt FROM zmmt_depar "#EC CI_BUFFSUBQ
                                WHERE  abtnr          = but051~abtnr AND
                                       no_vendor_mail = @abap_true ) ).

    SORT result BY smtpadr.
    DELETE ADJACENT DUPLICATES FROM result COMPARING smtpadr.
  ENDMETHOD.

  METHOD cache_itab_multiton.
    DATA lt_lfa1_key TYPE tt_lfa1_key.

    FIELD-SYMBOLS: <lt_itab>  TYPE ANY TABLE,
                   <lv_lifnr> TYPE lfa1-lifnr.

    CHECK ir_itab IS NOT INITIAL.
    ASSIGN ir_itab->* TO <lt_itab>.

    IF <lt_itab> IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).
      ASSIGN COMPONENT iv_fnam_vendor OF STRUCTURE <ls_itab> TO <lv_lifnr>.

      IF <lv_lifnr> IS NOT ASSIGNED.
        RAISE EXCEPTION TYPE zcx_bc_method_parameter
          EXPORTING textid      = zcx_bc_method_parameter=>param_value_invalid
                    class_name  = c_clsname_me
                    method_name = c_meth_cim
                    param_name  = CONV #( iv_fnam_vendor ).
      ENDIF.

      CHECK NOT line_exists( gt_mt[ KEY primary_key
                                    COMPONENTS lifnr = <lv_lifnr> ] ).

      APPEND VALUE #( lifnr = <lv_lifnr> ) TO lt_lfa1_key.
    ENDLOOP.

    IF lt_lfa1_key IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_lfa1_key.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1_key.
    create_and_cache_multiton( lt_lfa1_key ).
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_mt[ KEY primary_key COMPONENTS lifnr = iv_lifnr ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      INSERT VALUE #( lifnr = iv_lifnr
                      obj   = NEW #( iv_lifnr = iv_lifnr ) )
             INTO TABLE gt_mt ASSIGNING <ls_mt>.
    ENDIF.

    ro_obj = <ls_mt>-obj.
  ENDMETHOD.

  METHOD get_lfa1.
    DATA ls_lfa1 TYPE lfa1.

    ASSIGN gt_lfa1[ KEY primary_key COMPONENTS lifnr = iv_lifnr ] TO FIELD-SYMBOL(<ls_lfa1>).
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = iv_lifnr.
      ls_lfa1-lifnr = iv_lifnr.
      INSERT ls_lfa1 INTO TABLE gt_lfa1 ASSIGNING <ls_lfa1>.
    ENDIF.

    rs_lfa1 = <ls_lfa1>.
  ENDMETHOD.

  METHOD get_default_acc_grp_vals.
    IF gt_def_acc_grp_val IS INITIAL.
      SELECT * FROM zfit_xk_dzahls INTO TABLE @gt_def_acc_grp_val. "#EC CI_NOWHERE
      IF gt_def_acc_grp_val IS INITIAL.
        INSERT INITIAL LINE INTO TABLE gt_def_acc_grp_val.
      ENDIF.
    ENDIF.

    TRY.
        rs_def = gt_def_acc_grp_val[ KEY primary_key
                                     COMPONENTS bukrs = iv_bukrs
                                                ktokk = iv_ktokk ].

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING textid   = zcx_bc_table_content=>entry_missing
                    previous = lo_silnf
                    objectid = |{ iv_bukrs } { iv_ktokk }|
                    tabname  = c_tabname_def_pay_block.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.