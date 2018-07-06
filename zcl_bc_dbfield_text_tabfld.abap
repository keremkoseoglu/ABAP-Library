CLASS zcl_bc_dbfield_text_tabfld DEFINITION
  PUBLIC
  INHERITING FROM zcl_bc_dbfield_text_abs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BC_DBFIELD_TEXT_TABFLD IMPLEMENTATION.


  METHOD get_text.

    TRY.
        rv_ddtext = zcl_bc_table_field=>get_instance_by_fullname( iv_dbfield )->get_text( ).
      CATCH cx_root ##no_Handler.
    ENDTRY.

    CHECK
      rv_ddtext IS INITIAL and
      go_next is not initial.

    rv_ddtext = go_next->get_text( iv_dbfield ).

  ENDMETHOD.
ENDCLASS.