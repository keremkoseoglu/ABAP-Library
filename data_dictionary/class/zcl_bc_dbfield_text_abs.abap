CLASS zcl_bc_dbfield_text_abs DEFINITION
  PUBLIC
  abstract
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:

      build_cor
        RETURNING
          VALUE(ro_first) TYPE REF TO zcl_bc_dbfield_text_abs,

      get_text_via_chain
        IMPORTING
          !iv_dbfield      TYPE clike
        RETURNING
          VALUE(rv_ddtext) TYPE ddtext.

    METHODS:

      get_text abstract
        IMPORTING
          !iv_dbfield      TYPE clike
        RETURNING
          VALUE(rv_ddtext) TYPE ddtext,

      set_next IMPORTING !io_next TYPE REF TO zcl_bc_dbfield_text_abs.

  PROTECTED SECTION.

    DATA:
      go_next TYPE REF TO zcl_bc_dbfield_text_abs.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_cor,
        clsname TYPE seoclsname,
        obj     TYPE REF TO zcl_bc_dbfield_text_abs,
      END OF t_cor,

      tt_cor TYPE STANDARD TABLE OF t_cor WITH DEFAULT KEY.

    CONSTANTS:
      c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DBFIELD_TEXT_ABS'.

    CLASS-DATA:
      gt_cor TYPE tt_cor.

ENDCLASS.



CLASS ZCL_BC_DBFIELD_TEXT_ABS IMPLEMENTATION.


  METHOD build_cor.

    DATA:
      lo_obj TYPE REF TO object.

    try.
        gt_cor = corresponding #( zcl_bc_abap_Class=>get_instance( c_clsname_me )->get_instanceable_subclasses( ) ).
      catch cx_root ##no_Handler.
    endtry.

    LOOP AT gt_cor ASSIGNING FIELD-SYMBOL(<ls_cor>).

      DATA(lv_tabix) = sy-tabix.

      TRY.
          CREATE OBJECT lo_obj TYPE (<ls_cor>-clsname).
          <ls_cor>-obj ?= lo_obj.
        CATCH cx_root.
          DELETE gt_cor.
          CONTINUE.
      ENDTRY.

      CHECK lv_tabix GT 1.

      <ls_cor>-obj->set_next( gt_cor[ lv_tabix - 1 ]-obj ).

    ENDLOOP.

    CHECK gt_cor IS NOT INITIAL.

    ro_first = gt_cor[ lines( gt_cor ) ]-obj.

  ENDMETHOD.


  method get_text_via_chain.
    rv_ddtext = ycl_addict_dbfield_text_abs=>get_text_via_chain( iv_dbfield ).
  endmethod.


  method set_next.
    go_next = io_next.
  endmethod.
ENDCLASS.