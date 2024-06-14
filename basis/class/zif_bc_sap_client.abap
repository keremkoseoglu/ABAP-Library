INTERFACE zif_bc_sap_client
  PUBLIC.

  METHODS ensure_customizing_client RAISING   zcx_bc_sap_client.

  METHODS is_customizing_client     RETURNING VALUE(result) TYPE abap_bool.

  METHODS is_customizable           RETURNING VALUE(result) TYPE abap_bool.

  METHODS get_def                   RETURNING VALUE(result) TYPE t000.

ENDINTERFACE.