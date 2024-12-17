INTERFACE zif_pm_quality_notification
  PUBLIC.

  METHODS get_qmnum RETURNING VALUE(result) TYPE qmnum.

  METHODS close
    IMPORTING with_email TYPE abap_bool DEFAULT abap_true
    EXPORTING !messages  TYPE bapirettab
    RAISING   zcx_pm_qual_notif_close.

  METHODS save
    EXPORTING !messages TYPE bapirettab
    RAISING   zcx_pm_qual_notif_save.

  METHODS close_and_save
    IMPORTING with_email TYPE abap_bool DEFAULT abap_true
    EXPORTING !messages  TYPE bapirettab
    RAISING   zcx_pm_qual_notif_write.

  METHODS update_custom_fields
    IMPORTING zzadd_fiori       TYPE qmel-zzadd_fiori       OPTIONAL
              zzsahakontrol     TYPE qmel-zzsahakontrol     OPTIONAL
              zzcompletion_text TYPE qmel-zzcompletion_text OPTIONAL
    RAISING   zcx_pm_qual_notif_save.

  METHODS set_breakdown_duration
    IMPORTING auszt TYPE eauszt
    RAISING   zcx_pm_qual_notif_save.

  METHODS get_status_object
    RETURNING VALUE(result) TYPE REF TO zif_bc_status_obj
    RAISING   zcx_pm_qual_notif.

  METHODS update_pm_work_center
    IMPORTING pm_wkctr TYPE lgwid
    RAISING   zcx_pm_qual_notif_save.

ENDINTERFACE.