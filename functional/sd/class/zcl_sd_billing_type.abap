CLASS zcl_sd_billing_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
        gv_fkart TYPE fkart READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING !iv_fkart     TYPE fkart
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_billing_type
        RAISING   zcx_bc_table_content.

    methods:
      get_head
        returning value(rs_head) type tvfk.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        fkart TYPE fkart,
        obj   TYPE REF TO zcl_sd_billing_type,
      END OF t_multiton,

      tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS fkart.

    constants c_tabname_head type tabname value 'TVFK'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    data:
      gs_tvfk_lazy type tvfk.

ENDCLASS.



CLASS ZCL_SD_BILLING_TYPE IMPLEMENTATION.

  method get_head.

    if gs_tvfk_lazy-fkart is initial.
      select single *
        from tvfk
        where fkart eq @gv_fkart
        into @gs_tvfk_lazy.

      assert sy-subrc eq 0.
    endif.

    rs_head = gs_tvfk_lazy.

  endmethod.

  METHOD get_instance.

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS fkart = iv_fkart ] TO FIELD-SYMBOL(<ls_mt>).
    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton( fkart = iv_fkart ).

      ls_mt-obj = NEW #( ).

      SELECT SINGLE fkart
          INTO ls_mt-obj->gv_fkart
          FROM tvfk
          WHERE fkart EQ ls_mt-fkart.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_mt-fkart )
            tabname  = c_tabname_head.
      ENDIF.

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.
ENDCLASS.