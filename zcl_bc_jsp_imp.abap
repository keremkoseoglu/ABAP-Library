CLASS zcl_bc_jsp_imp DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA: go_imp type ref to zif_bc_jsp_imp,
          gs_def TYPE zbct_jsp_imp,
          gs_sys type ZBCT_JSP_IMPS.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_impid     TYPE zbcd_jsp_impid
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_jsp_imp
      RAISING
        zcx_bc_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_multiton,
             impid TYPE zbcd_jsp_impid,
             obj   TYPE REF TO zcl_bc_jsp_imp,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS impid.

    CONSTANTS: c_tabname_imp  TYPE tabname VALUE 'ZBCT_JSP_IMP',
               c_tabname_imps TYPE tabname VALUE 'ZBCT_JSP_IMPS'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

ENDCLASS.



CLASS ZCL_BC_JSP_IMP IMPLEMENTATION.


  METHOD get_instance.

    DATA: lo_obj TYPE REF TO object.

    ASSIGN gt_multiton[ KEY primary_key COMPONENTS impid = iv_impid ] TO FIELD-SYMBOL(<ls_multiton>).
    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( impid = iv_impid ).

      ls_multiton-obj = new #( ).

      SELECT SINGLE * INTO ls_multiton-obj->gs_Def FROM zbct_jsp_imp WHERE impid EQ ls_multiton-impid.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-impid )
            tabname  = c_tabname_imp.
      ENDIF.

      SELECT SINGLE * INTO ls_multiton-obj->gs_sys FROM zbct_jsp_imps WHERE impid EQ ls_multiton-impid.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_table_content
          EXPORTING
            textid   = zcx_bc_table_content=>entry_missing
            objectid = CONV #( ls_multiton-impid )
            tabname  = c_tabname_imps.
      ENDIF.

      TRY.
          CREATE OBJECT lo_obj TYPE (ls_multiton-obj->gs_def-clsname).
          ls_multiton-obj->go_imp ?= lo_obj.
        CATCH cx_root INTO DATA(lo_cx_root).
          RAISE EXCEPTION TYPE zcx_bc_table_content
            EXPORTING
              textid    = zcx_bc_table_content=>value_invalid
              previous  = lo_cx_root
              objectid  = CONV #( ls_multiton-impid )
              tabname   = c_tabname_imp
              fieldname = 'CLSNAME'.
      ENDTRY.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.
ENDCLASS.