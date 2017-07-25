CLASS zcl_bc_mdf_domain_line DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      go_domain type ref to zcl_Bc_Abap_domain,
      gs_line   TYPE zcl_Bc_abap_domain=>t_line  READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_domname   TYPE domname
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_mdf_domain_line
      RAISING
        zcx_bc_table_content.

    METHODS set_value
      IMPORTING
        !iv_value TYPE val_single
      RAISING
        zcx_bc_domain.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_singleton,
        domname TYPE domname,
        obj     TYPE REF TO zcl_bc_mdf_domain_line,
      END OF t_singleton,

      tt_singleton
        TYPE HASHED TABLE OF t_singleton
        WITH UNIQUE KEY primary_key COMPONENTS domname.

    CLASS-DATA gt_singleton TYPE tt_singleton.
ENDCLASS.



CLASS ZCL_BC_MDF_DOMAIN_LINE IMPLEMENTATION.


  METHOD get_instance.

    ASSIGN gt_singleton[ KEY primary_key COMPONENTS domname = iv_domname ] TO FIELD-SYMBOL(<ls_singleton>).

    IF sy-subrc NE 0.

      DATA(ls_sing) = VALUE t_singleton( domname = iv_domname ).

      ls_sing-obj = NEW #( ).

      ls_sing-obj->go_domain = zcl_Bc_abap_Domain=>get_instance( ls_sing-domname ).

      INSERT ls_sing INTO TABLE gt_singleton ASSIGNING <ls_singleton>.

    ENDIF.

    ro_obj = <ls_singleton>-obj.

  ENDMETHOD.


  METHOD set_value.
    gs_line = go_domain->get_value_line( iv_Value ).
  ENDMETHOD.
ENDCLASS.