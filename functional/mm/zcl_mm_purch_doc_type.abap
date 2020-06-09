CLASS zcl_mm_purch_doc_type DEFINITION
  PUBLIC
  FINAL
  CREATE private .

  PUBLIC SECTION.

    data:
      gs_def type T161 read-only.

    class-methods:
      get_instance
        importing
          !iv_Bstyp type bstyp
          !iv_bsart type esart
        returning
          value(ro_obj) type ref to zcl_mm_purch_doc_type
        raising
          zcx_bc_table_content.

    class-methods:
      get_Text_safe
        importing
          !iv_Bstyp type bstyp
          !iv_bsart type esart
        returning
          value(rv_batxt) type batxt.

    methods:
      get_Text returning value(rv_batxt) type batxt.

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      begin of t_multiton,
        bstyp type bstyp,
        bsart type esart,
        cx    type ref to zcx_Bc_table_Content,
        obj   type ref to zcl_mm_purch_doc_type,
      end of t_multiton,

      tt_multiton
        type hashed table of T_multiton
        with unique key primary_key components bstyp bsart,

      begin of t_lazy_Flg,
        text type abap_bool,
      end of t_lazy_flg,

      begin of t_lazy_val,
        text type batxt,
      end of t_lazy_val,

      begin of t_lazy,
        flg type t_lazy_flg,
        val type t_lazy_val,
      end of t_lazy.

    CONSTANTS:
      c_tabname_def TYPE tabname VALUE 'T161'.

    class-data:
      gt_multiton type tt_multiton.

    data:
      gs_lazy type t_lazy.

    methods:
      constructor
        importing
          !iv_Bstyp type bstyp
          !iv_bsart type esart
        raising
          zcx_bc_table_content.

ENDCLASS.


CLASS zcl_mm_purch_doc_type IMPLEMENTATION.

  method constructor.

    select single *
      from t161
      where
        bstyp eq @iv_Bstyp and
        bsart eq @iv_bsart
      into @gs_def.

    if sy-subrc ne 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = |{ iv_bstyp } { iv_Bsart }|
          tabname  = c_tabname_def.
    endif.

  endmethod.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        bstyp = iv_Bstyp
        bsart = iv_Bsart
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton(
        bstyp = iv_Bstyp
        bsart = iv_Bsart
      ).

      TRY.
          ls_mt-obj = NEW #(
            iv_bstyp = ls_mt-bstyp
            iv_bsart = ls_mt-bsart
          ).
        CATCH zcx_bc_table_content INTO ls_mt-cx ##no_Handler .
      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

  method get_text.

    if gs_lazy-flg-text eq abap_False.

      select single batxt
        from t161t
        where
          spras eq @sy-langu     and
          bsart eq @gs_Def-bsart and
          bstyp eq @gs_def-bstyp
        into @gs_lazy-val-text.

      gs_lazy-flg-text = abap_True.
    endif.

    rv_batxt = gs_lazy-val-text.

  endmethod.

  method get_Text_safe.

    try.
        rv_batxt = get_instance(
            iv_bstyp = iv_bstyp
            iv_bsart = iv_bsart
          )->get_Text( ).
      catch cx_root ##no_Handler .
    endtry.

  endmethod.

ENDCLASS.