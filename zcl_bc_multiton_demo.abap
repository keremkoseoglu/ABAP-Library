CLASS zcl_bc_multiton_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    interfaces ZIF_BC_MULTITON.

    data:
      gv_id    type char15,
      gv_erdat type erdat,
      gv_ernam type ernam.

    methods:
      constructor importing iv_id type char15.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_multiton_demo IMPLEMENTATION.

  method constructor.
    gv_id = iv_id.
    gv_erdat = sy-datum.
    gv_Ernam = sy-uname.
  endmethod.

  METHOD zif_bc_multiton~get_instance.
    ro_obj ?= new zcl_Bc_multiton_demo( conv #( iv_objectid ) ).
  ENDMETHOD.

ENDCLASS.