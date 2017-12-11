CLASS zcl_sd_sales_org DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_def,
        vkorg TYPE tvko-vkorg,
        bukrs TYPE tvko-bukrs,
      END OF t_def,

      BEGIN OF t_vkorg,
        vkorg TYPE vkorg,
      END OF t_vkorg,

      tt_vkorg     TYPE STANDARD TABLE OF t_vkorg WITH DEFAULT KEY,

      tt_vkorg_rng TYPE RANGE OF vkorg.

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
          ZCX_FI_COMPANY_CODE_DEF
          zcx_sd_sales_org_def.

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

      SELECT SINGLE vkorg, bukrs
        INTO CORRESPONDING FIELDS OF @ls_multiton-obj->gs_def
        FROM tvko
        WHERE vkorg EQ @ls_multiton-vkorg.

      IF sy-subrc NE 0.

        data(lo_tc) = new zcx_bc_table_content(
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( iv_vkorg )
          tabname  = c_tabname_tvko
        ).

        raise exception type zcx_sd_sales_org_def
          EXPORTING
            previous = lo_tc
            vkorg    = iv_vkorg.

      ENDIF.

      ls_multiton-obj->go_company = zcl_fi_company=>get_instance( ls_multiton-obj->gs_def-bukrs ).

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.
ENDCLASS.