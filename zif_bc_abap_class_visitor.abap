INTERFACE zif_bc_abap_class_visitor
  PUBLIC .

  METHODS visit
    IMPORTING
      !io_class TYPE REF TO zcl_bc_abap_class
    RAISING
      zcx_bc_class_method.

ENDINTERFACE.