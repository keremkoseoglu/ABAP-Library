INTERFACE zif_sd_shipment_visitor
  PUBLIC .

  CONSTANTS:
    c_meth_visit TYPE seocpdname VALUE 'VISIT'.

  METHODS:
    visit
      IMPORTING
        !io_shipment TYPE REF TO zcl_sd_shipment
        !io_log      type ref to zcl_bc_applog_facade optional
      RAISING
        zcx_bc_class_method.

ENDINTERFACE.