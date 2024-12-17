INTERFACE zif_bc_locale
  PUBLIC.

  METHODS set_by_language    IMPORTING langu TYPE sylangu.

  METHODS set_by_company     IMPORTING bukrs TYPE bukrs.

  METHODS set_by_sales_order IMPORTING vbeln TYPE vbeln_va.

  METHODS reset.

ENDINTERFACE.