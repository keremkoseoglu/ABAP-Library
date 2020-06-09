INTERFACE zif_bc_tcr_reader
  PUBLIC .

  METHODS get_list
    IMPORTING
      !it_trkorr_rng  TYPE cts_organizer_tt_wd_request OPTIONAL
      !it_as4user_rng TYPE ucon_user_range OPTIONAL
      !it_gekod       TYPE zcl_bc_tpalog_reader=>tt_gekod OPTIONAL
      !it_sys_data    TYPE zcl_bc_tpalog_reader=>tt_sys_data OPTIONAL
    EXPORTING
      !et_list        TYPE zcl_bc_tpalog_reader=>tt_list
    CHANGING
      !co_log         TYPE REF TO zcl_bc_applog_facade
    RAISING
      zcx_bc_class_method.

ENDINTERFACE.