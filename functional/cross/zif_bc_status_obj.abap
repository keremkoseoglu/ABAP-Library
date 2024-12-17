INTERFACE zif_bc_status_obj
  PUBLIC.

  METHODS get_objnr RETURNING VALUE(result) TYPE jsto-objnr.

  METHODS is_completed RETURNING VALUE(result) TYPE abap_bool
                       RAISING   zcx_bc_obj_status.

ENDINTERFACE.