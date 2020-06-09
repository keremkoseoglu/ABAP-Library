CLASS zcl_bc_chglog_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF t_changed_obj,
             objectid   TYPE cdhdr-objectid,
             udate      TYPE cdhdr-udate,
             utime      TYPE cdhdr-utime,
             change_ind TYPE cdhdr-change_ind,
           END OF t_changed_obj,

           tt_changed_obj TYPE STANDARD TABLE OF t_changed_obj WITH DEFAULT KEY.

    CLASS-METHODS get_odt_since
      IMPORTING
        !iv_objectclas   TYPE cdhdr-objectclas
        !iv_udate        TYPE cdhdr-udate
        !iv_utime        TYPE cdhdr-utime OPTIONAL
        !iv_distinct_oid TYPE abap_bool DEFAULT abap_false
      CHANGING
        !co_log          TYPE REF TO zcl_bc_applog_facade OPTIONAL
      RETURNING
        VALUE(rt_chg)    TYPE tt_changed_obj.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_msgid_zbc TYPE symsgid  VALUE 'ZBC',
               c_option_ge TYPE ddoption VALUE 'GE',
               c_sign_i    TYPE ddsign   VALUE 'I'.

ENDCLASS.



CLASS ZCL_BC_CHGLOG_FACADE IMPLEMENTATION.


  METHOD get_odt_since.

    DATA lt_utime_rng TYPE RANGE OF cdhdr-utime.

*   Parametre hazırlığı

    IF iv_utime IS SUPPLIED.
      APPEND VALUE #( option = c_option_ge
                      sign   = c_sign_i
                      low    = iv_utime ) TO lt_utime_rng.
    ENDIF.

*   Veri okuma

    SELECT DISTINCT objectid udate utime change_ind
           INTO CORRESPONDING FIELDS OF TABLE rt_chg
           FROM cdhdr
           WHERE objectclas EQ iv_objectclas
             AND ( udate GT iv_udate OR ( udate EQ iv_udate AND utime IN lt_utime_rng  ) ).

*   Çift kayıtları eleme

    IF iv_distinct_oid EQ abap_true.
      SORT rt_chg BY objectid ASCENDING
                     udate    DESCENDING
                     utime    DESCENDING.

      DELETE ADJACENT DUPLICATES FROM rt_chg COMPARING objectid.
    ENDIF.

*   Loglama

    IF co_log IS SUPPLIED AND co_log IS NOT INITIAL.

      LOOP AT rt_chg ASSIGNING FIELD-SYMBOL(<ls_chg>).
        co_log->add_t100_msg( iv_msgid = c_msgid_zbc
                              iv_msgno = '012'
                              iv_msgty = zcl_bc_applog_facade=>c_msgty_s
                              iv_msgv1 = <ls_chg>-objectid
                              iv_msgv2 = <ls_chg>-udate
                              iv_msgv3 = <ls_chg>-utime
                              iv_msgv4 = <ls_chg>-change_ind ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.