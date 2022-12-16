INTERFACE zif_bc_logoff_user
  PUBLIC .

  CONSTANTS: BEGIN OF method,
               logoff_user TYPE seocpdname VALUE 'LOGOFF_USER',
             END OF method.

  METHODS logoff_user
    IMPORTING !user   TYPE syuname
              !client TYPE symandt DEFAULT sy-mandt
    RAISING   ycx_addict_class_method.

ENDINTERFACE.