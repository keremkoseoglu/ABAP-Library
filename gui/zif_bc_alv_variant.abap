INTERFACE zif_bc_alv_variant
  PUBLIC.

  METHODS get_disvariant RETURNING VALUE(result) TYPE disvariant.

  METHODS delete         RAISING   zcx_bc_alv_variant.

  METHODS is_initial     RETURNING VALUE(result) TYPE abap_bool.

ENDINTERFACE.