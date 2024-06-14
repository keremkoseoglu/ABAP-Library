INTERFACE zif_bc_dimension
  PUBLIC.

  METHODS get_uoms  RETURNING VALUE(result) TYPE meins_tty.

  METHODS get_uom_range RETURNING VALUE(result) TYPE mdg_bs_mat_t_range_meinh.

ENDINTERFACE.