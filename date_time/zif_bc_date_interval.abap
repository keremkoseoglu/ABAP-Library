INTERFACE zif_bc_date_interval
  PUBLIC.

  METHODS get_first_date RETURNING VALUE(result) TYPE dats
                         RAISING   zcx_bc_date_interval.

  METHODS get_last_date RETURNING VALUE(result) TYPE dats
                        RAISING   zcx_bc_date_interval.

  METHODS get_first_month_first_day RETURNING VALUE(result) TYPE dats
                                    RAISING   zcx_bc_date_interval.

  METHODS get_last_month_last_day RETURNING VALUE(result) TYPE dats
                                  RAISING   zcx_bc_date_interval.

  METHODS get_date_range
    RETURNING VALUE(result) TYPE date_t_range
    RAISING   zcx_bc_date_interval.

  METHODS get_start_period RETURNING VALUE(result) TYPE jahrper.

  METHODS is_multi_year RETURNING VALUE(result) TYPE abap_bool
                        RAISING   zcx_bc_date_interval.

  METHODS includes_year
    IMPORTING gjahr         TYPE gjahr
    RETURNING VALUE(result) TYPE abap_bool
    RAISING   zcx_bc_date_interval.

ENDINTERFACE.