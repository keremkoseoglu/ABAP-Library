CLASS zcl_bc_dbfield_text_dtel DEFINITION
  PUBLIC
  INHERITING FROM zcl_bc_dbfield_text_abs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BC_DBFIELD_TEXT_DTEL IMPLEMENTATION.


  METHOD get_text.

    TRY.
        rv_ddText = cast zcl_bc_data_element(
            zcl_bc_multiton=>get_obj(
              iv_clsname  = zcl_bc_data_element=>c_clsname_me
              iv_objectid = conv #( iv_dbfield )
            )
          )->get_Text( ).

      CATCH cx_root ##no_Handler.
    ENDTRY.

    CHECK
      rv_ddtext IS INITIAL and
      go_next is not initial.

    rv_ddtext = go_next->get_text( iv_dbfield ).

  ENDMETHOD.
ENDCLASS.