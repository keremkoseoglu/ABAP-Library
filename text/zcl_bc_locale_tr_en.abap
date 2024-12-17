CLASS zcl_bc_locale_tr_en DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_locale.

    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO zcl_bc_locale_tr_en.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF language,
                 turkish TYPE sylangu VALUE 'T',
                 english TYPE sylangu VALUE 'E',
               END OF language.

    CLASS-DATA singleton TYPE REF TO zcl_bc_locale_tr_en.

    DATA sy_langu TYPE sylangu.

    METHODS constructor.

    METHODS set_langu IMPORTING langu TYPE sylangu.
ENDCLASS.


CLASS zcl_bc_locale_tr_en IMPLEMENTATION.
  METHOD get_instance.
    IF singleton IS INITIAL.
      singleton = NEW #( ).
    ENDIF.

    result = singleton.
  ENDMETHOD.

  METHOD constructor.
    me->sy_langu = sy-langu.
  ENDMETHOD.

  METHOD set_langu.
    SET LANGUAGE langu.
    SET LOCALE LANGUAGE langu.
  ENDMETHOD.

  METHOD zif_bc_locale~reset.
    set_langu( me->sy_langu ).
  ENDMETHOD.

  METHOD zif_bc_locale~set_by_language.
    set_langu( langu ).
  ENDMETHOD.

  METHOD zif_bc_locale~set_by_company.
    TRY.
        DATA(company_language) = zcl_fi_company=>get_instance( bukrs )->gs_def-land1.

      CATCH cx_root. " Paranoya
        CLEAR company_language.
    ENDTRY.

    DATA(new_language) = SWITCH sylangu( company_language
                                         WHEN me->language-turkish
                                         THEN me->language-turkish
                                         ELSE me->language-english ).

    set_langu( new_language ).
  ENDMETHOD.

  METHOD zif_bc_locale~set_by_sales_order.
    TRY.
        DATA(sales_order)   = zcl_sd_sales_order=>get_instance( vbeln ).
        DATA(order_company) = sales_order->get_header( )-bukrs_vf.

      CATCH cx_root.
        CLEAR order_company.
    ENDTRY.

    zif_bc_locale~set_by_company( order_company ).
  ENDMETHOD.
ENDCLASS.