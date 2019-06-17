CLASS zcl_sd_sales_org DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_def,
        vkorg TYPE tvko-vkorg,
        bukrs TYPE tvko-bukrs,
        waers TYPE tvko-waers,
      END OF t_def,

      BEGIN OF t_vkorg,
        vkorg TYPE vkorg,
      END OF t_vkorg,

      tt_vkorg     TYPE STANDARD TABLE OF t_vkorg WITH DEFAULT KEY,

      tt_vkorg_rng TYPE RANGE OF vkorg,
      tt_waers_rng TYPE RANGE OF waers.

    DATA:
      go_company TYPE REF TO zcl_fi_company READ-ONLY,
      gs_def     TYPE t_def READ-ONLY.

    CLASS-METHODS:

      get_instance
        IMPORTING
          !iv_vkorg     TYPE vkorg
        RETURNING
          VALUE(ro_obj) TYPE REF TO zcl_sd_sales_org
        RAISING
          zcx_fi_company_code_def
          zcx_sd_sales_org_def,

      get_sales_org_list
        IMPORTING
          !it_vkorg_rng   TYPE tt_vkorg_rng OPTIONAL
          !it_waers_rng   TYPE tt_waers_rng OPTIONAL
        RETURNING
          VALUE(rt_vkorg) TYPE tt_vkorg,

      get_sales_org_range
        IMPORTING
          !it_vkorg_rng       TYPE tt_vkorg_rng OPTIONAL
          !it_waers_rng       TYPE tt_waers_rng OPTIONAL
        RETURNING
          VALUE(rt_vkorg_rng) TYPE tt_vkorg_rng.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        vkorg TYPE vkorg,
        obj   TYPE REF TO zcl_sd_sales_org,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key
        COMPONENTS vkorg.

    CONSTANTS c_tabname_tvko TYPE tabname VALUE 'TVKO'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

ENDCLASS.



CLASS zcl_sd_sales_org IMPLEMENTATION.


  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS vkorg = iv_vkorg
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( vkorg = iv_vkorg ).
      ls_multiton-obj = NEW #( ).

      SELECT SINGLE vkorg, bukrs, waers
        INTO CORRESPONDING FIELDS OF @ls_multiton-obj->gs_def
        FROM tvko
        WHERE vkorg EQ @ls_multiton-vkorg.

      IF sy-subrc NE 0.

        DATA(lo_tc) = NEW zcx_bc_table_content(
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_vkorg )
          tabname  = c_tabname_tvko
        ).

        RAISE EXCEPTION TYPE zcx_sd_sales_org_def
          EXPORTING
            previous = lo_tc
            vkorg    = iv_vkorg.

      ENDIF.

      ls_multiton-obj->go_company = zcl_fi_company=>get_instance( ls_multiton-obj->gs_def-bukrs ).

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.

  method get_sales_org_list.

    SELECT tvko~vkorg
      FROM tvko
      WHERE
        vkorg IN @it_vkorg_rng AND
        waers IN @it_waers_rng
      INTO table @rt_vkorg.

  endmethod.

  METHOD get_sales_org_range.

    rt_vkorg_rng = value #(
      for _vkorg in get_sales_org_list(
        it_vkorg_rng = it_Vkorg_rng
        it_waers_rng = it_waers_rng
      )
      (
        sign   = zcl_bc_ddic_toolkit=>c_sign_i
        option = zcl_Bc_Ddic_Toolkit=>c_option_eq
        low    = _vkorg
      )
    ).

  ENDMETHOD.

ENDCLASS.