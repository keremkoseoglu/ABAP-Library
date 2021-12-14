CLASS zcl_bc_dbfield_text_dtel DEFINITION
  PUBLIC
  INHERITING FROM zcl_bc_dbfield_text_abs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA core TYPE REF TO ycl_addict_dbfield_text_dtel.
ENDCLASS.



CLASS zcl_bc_dbfield_text_dtel IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->core = NEW #( ).
  ENDMETHOD.


  METHOD get_text.
    rv_ddtext = me->core->get_text( iv_dbfield ).
  ENDMETHOD.
ENDCLASS.