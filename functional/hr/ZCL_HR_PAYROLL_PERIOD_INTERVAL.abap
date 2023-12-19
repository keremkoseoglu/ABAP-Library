CLASS zcl_hr_payroll_period_interval DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING begda         TYPE begda
                endda         TYPE endda
      RETURNING VALUE(result) TYPE REF TO zcl_hr_payroll_period_interval.

    METHODS get_periods
      RETURNING VALUE(result) TYPE t549q_tab.

    METHODS get_ace_ds_periods
      IMPORTING !currency     TYPE fins_currw
      RETURNING VALUE(result) TYPE zfitt_ace_ds_period_value.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             begda TYPE begda,
             endda TYPE endda,
             obj   TYPE REF TO zcl_hr_payroll_period_interval,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS begda endda.

    TYPES: BEGIN OF ace_ds_period_cache_dict,
             currency       TYPE fins_currw,
             ace_ds_periods TYPE zfitt_ace_ds_period_value,
           END OF ace_ds_period_cache_dict,

           ace_ds_period_cache_set TYPE HASHED TABLE OF ace_ds_period_cache_dict
                                   WITH UNIQUE KEY primary_key COMPONENTS currency.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: begda          TYPE begda,
          endda          TYPE endda,
          periods        TYPE t549q_tab,
          periods_read   TYPE abap_bool,
          ace_ds_periods TYPE ace_ds_period_cache_set.

    METHODS constructor
      IMPORTING begda TYPE begda
                endda TYPE endda.
ENDCLASS.


CLASS zcl_hr_payroll_period_interval IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN zcl_hr_payroll_period_interval=>multitons[ KEY primary_key
                                                      COMPONENTS begda = begda
                                                                 endda = endda ]
           TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      INSERT VALUE #( begda = begda
                      endda = endda
                      obj   = NEW #( begda = begda
                                     endda = endda ) )
             INTO TABLE zcl_hr_payroll_period_interval=>multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_periods.
    IF me->periods_read = abap_false.
      ##FM_SUBRC_OK
      CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
        EXPORTING  get_begda       = me->begda
                   get_endda       = me->endda
        TABLES     get_periods     = me->periods
        EXCEPTIONS no_period_found = 1
                   no_valid_permo  = 2
                   OTHERS          = 3.

      me->periods_read = abap_true.
    ENDIF.

    result = me->periods.
  ENDMETHOD.

  METHOD get_ace_ds_periods.
    ASSIGN me->ace_ds_periods[ KEY primary_key
                               COMPONENTS currency = currency ]
           TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      INSERT VALUE #( currency       = currency
                      ace_ds_periods = VALUE #( FOR _per IN get_periods( )
                                                ( date_from = _per-begda
                                                  date_to   = _per-endda
                                                  currency  = currency ) ) )
             INTO TABLE me->ace_ds_periods ASSIGNING <cache>.
    ENDIF.

    result = <cache>-ace_ds_periods.
  ENDMETHOD.

  METHOD constructor.
    me->begda = begda.
    me->endda = endda.
  ENDMETHOD.
ENDCLASS.