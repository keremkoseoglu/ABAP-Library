INTERFACE zif_bc_year_period_range
  PUBLIC.

  METHODS get_month_ends
    RETURNING VALUE(result) TYPE datum_tab
    RAISING   zcx_bc_period_range.

ENDINTERFACE.