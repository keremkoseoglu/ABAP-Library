CLASS zcl_mm_vendor DEFINITION
  PUBLIC
  FINAL
  CREATE private .

  PUBLIC SECTION.

    data gs_def type lfa1.

    class-methods:
      get_instance
        importing !iv_lifnr type lifnr
        returning value(ro_obj) type ref to zcl_mm_vendor
        raising   zcx_Bc_table_Content,

      GET_LFA1
        importing !IV_LIFNR type LIFNR
        returning value(RS_LFA1) type LFA1 .

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      begin of t_mt,
        lifnr type lifnr,
        obj   type ref to zcl_mm_vendor,
      end of t_mt,

      tt_mt type hashed table of t_mt with unique key primary_key components lifnr.

    constants c_tabname_def type tabname value 'LFA1'.

    class-data:
      gt_lfa1 TYPE HASHED TABLE OF lfa1 WITH UNIQUE KEY primary_key COMPONENTS lifnr ,
      gt_mt   type tt_mt.

ENDCLASS.



CLASS zcl_mm_vendor IMPLEMENTATION.

  method get_instance.

    assign gt_mt[ key primary_key components lifnr = iv_lifnr ] to field-symbol(<ls_mt>).

    if sy-subrc ne 0.

      data(ls_mt) = value t_mt( lifnr = iv_lifnr ).

      ls_mt-obj = new #( ).

      select single * into @ls_mt-obj->gs_def
        from lfa1
        where lifnr eq @ls_mt-lifnr.

      if sy-subrc ne 0.

        raise exception type zcx_bc_table_content
          EXPORTING
            textid    = zcx_bc_table_Content=>entry_missing
            objectid  = conv #( ls_mt-lifnr )
            tabname   = c_Tabname_def.

      endif.

      insert ls_mt into table gt_mt assigning <ls_mt>.

    endif.

    ro_obj = <ls_mt>-obj.

  endmethod.

  METHOD GET_LFA1.

    DATA ls_lfa1 TYPE lfa1.

    ASSIGN gt_lfa1[ KEY primary_key COMPONENTS lifnr = iv_lifnr ] TO FIELD-SYMBOL(<ls_lfa1>).
    IF sy-subrc ne 0.
      SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = iv_lifnr.
      ls_lfa1-lifnr = iv_lifnr.
      INSERT ls_lfa1 INTO TABLE gt_lfa1 assigning <ls_lfa1>.
    ENDIF.

    rs_lfa1 = <ls_lfa1>.

  ENDMETHOD.

ENDCLASS.