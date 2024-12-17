INTERFACE zif_bc_date_interval
  PUBLIC.

  METHODS get_first_date            RETURNING VALUE(result) TYPE dats.
  METHODS get_last_date             RETURNING VALUE(result) TYPE dats.

  METHODS get_first_month_first_day RETURNING VALUE(result) TYPE dats.

  METHODS get_last_month_last_day RETURNING VALUE(result) TYPE dats
                                  RAISING   zcx_bc_date.

ENDINTERFACE.