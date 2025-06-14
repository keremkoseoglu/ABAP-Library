INTERFACE zif_co_control_code
  PUBLIC.

  METHODS get_text  RETURNING VALUE(result) TYPE tka01-bezei.

  METHODS get_kokrs RETURNING VALUE(result) TYPE tka01-kokrs.

  METHODS get_erkrs RETURNING VALUE(result) TYPE tka01-erkrs.

ENDINTERFACE.