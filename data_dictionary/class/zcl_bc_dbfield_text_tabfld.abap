CLASS zcl_bc_dbfield_text_tabfld DEFINITION
  PUBLIC
  INHERITING FROM zcl_bc_dbfield_text_abs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA core TYPE REF TO YCL_ADDICT_DBFIELD_TEXT_TABFLD.
ENDCLASS.



CLASS ZCL_BC_DBFIELD_TEXT_TABFLD IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->core = NEW #( ).
  ENDMETHOD.


  METHOD get_text.
    rv_ddtext = me->core->get_text( iv_dbfield ).
  ENDMETHOD.
ENDCLASS.