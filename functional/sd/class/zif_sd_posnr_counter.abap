INTERFACE zif_sd_posnr_counter
  PUBLIC.

  METHODS reset.

  METHODS get_next RETURNING VALUE(result) TYPE vbap-posnr.

ENDINTERFACE.