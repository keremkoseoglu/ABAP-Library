INTERFACE zif_sd_division_set
  PUBLIC.

  METHODS collect  IMPORTING spart         TYPE spart.

  METHODS to_range RETURNING VALUE(result) TYPE tms_t_spart_range.

ENDINTERFACE.