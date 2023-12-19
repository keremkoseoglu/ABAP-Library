CLASS zcl_sd_tax_class DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING taxm1         TYPE a002-taxm1
      RETURNING VALUE(result) TYPE REF TO zcl_sd_tax_class
      RAISING   zcx_sd_tax_class.

    CLASS-METHODS get_instance_by_tax_rate
      IMPORTING tax_rate      TYPE zi_sd_tr_vat_rate-vat_rate
      RETURNING VALUE(result) TYPE REF TO zcl_sd_tax_class
      RAISING   zcx_sd_tax_class.

    METHODS get_tax_class_id           RETURNING VALUE(result) TYPE taxm1.
    METHODS get_tax_rate               RETURNING VALUE(result) TYPE zi_sd_tr_vat_rate-vat_rate.
    METHODS get_tax_rate_text          RETURNING VALUE(result) TYPE char3.
    METHODS get_tax_rate_text_pre_perc RETURNING VALUE(result) TYPE char4.

    METHODS calc_taxed_amount
      IMPORTING tax_free_amount TYPE dmbtr
      RETURNING VALUE(result)   TYPE dmbtr.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             taxm1 TYPE a002-taxm1,
             obj   TYPE REF TO zcl_sd_tax_class,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS taxm1.

    TYPES: BEGIN OF tax_rate_multiton_dict,
             tax_rate TYPE zi_sd_tr_vat_rate-vat_rate,
             obj   TYPE REF TO zcl_sd_tax_class,
           END OF tax_rate_multiton_dict,

           tax_rate_multiton_set TYPE HASHED TABLE OF tax_rate_multiton_dict
                                 WITH UNIQUE KEY primary_key COMPONENTS tax_rate.

    TYPES tax_rate_set TYPE HASHED TABLE OF zi_sd_tr_vat_rate
                       WITH UNIQUE KEY primary_key COMPONENTS taxm1.

    TYPES taxm1_list TYPE STANDARD TABLE OF taxm1 WITH KEY table_line.

    CLASS-DATA: multitons           TYPE multiton_set,
                tax_rate_multitons  TYPE tax_rate_multiton_set,
                lazy_tax_rates      TYPE tax_rate_set,
                lazy_tax_rates_read TYPE abap_bool.

    DATA tax_rate TYPE REF TO zi_sd_tr_vat_rate.

    CLASS-METHODS lazy_get_tax_rates RETURNING VALUE(result) TYPE REF TO tax_rate_set.

    METHODS constructor
      IMPORTING taxm1 TYPE a002-taxm1
      RAISING   zcx_sd_tax_class.
ENDCLASS.


CLASS zcl_sd_tax_class IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_sd_tax_class=>multitons[ KEY primary_key COMPONENTS taxm1 = taxm1 ] ).
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( taxm1 = taxm1
                        obj   = NEW #( taxm1 ) )
               INTO TABLE zcl_sd_tax_class=>multitons REFERENCE INTO mt.
    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_instance_by_tax_rate.
    TRY.
        DATA(mt) = REF #( zcl_sd_tax_class=>tax_rate_multitons[ KEY primary_key COMPONENTS tax_rate = tax_rate ] ).
      CATCH cx_sy_itab_line_not_found.
        DATA(tax_rates) = lazy_get_tax_rates( ).

        DATA(taxm1s) = VALUE taxm1_list( FOR GROUPS _gr OF _tr IN tax_rates->*
                                         WHERE ( vat_rate = tax_rate )
                                         GROUP BY _tr-taxm1
                                         ( _gr ) ).

        CASE lines( taxm1s ).
          WHEN 0.
            RAISE EXCEPTION NEW zcx_sd_tax_class( textid   = zcx_sd_tax_class=>no_tax_class_for_rate
                                                  vat_rate = tax_rate ).

          WHEN 1.
            INSERT VALUE #( tax_rate = tax_rate
                            obj      = NEW #( taxm1s[ 1 ] ) )
                   INTO TABLE zcl_sd_tax_class=>tax_rate_multitons
                   REFERENCE INTO mt.

          WHEN OTHERS.
            RAISE EXCEPTION NEW zcx_sd_tax_class( textid   = zcx_sd_tax_class=>multi_tax_class_for_rate
                                                  vat_rate = tax_rate ).
        ENDCASE.
    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_tax_class_id.
    result = me->tax_rate->taxm1.
  ENDMETHOD.

  METHOD get_tax_rate.
    result = me->tax_rate->vat_rate.
  ENDMETHOD.

  METHOD get_tax_rate_text.
    result = me->tax_rate->vat_rate_txt.
  ENDMETHOD.

  METHOD get_tax_rate_text_pre_perc.
    result = |%{ me->tax_rate->vat_rate_txt }|.
  ENDMETHOD.

  METHOD calc_taxed_amount.
    DATA(taxed_multiplier) = CONV przsa( 1 + ( me->tax_rate->vat_rate / 100 ) ).
    result = tax_free_amount * taxed_multiplier.
  ENDMETHOD.

  METHOD lazy_get_tax_rates.
    IF zcl_sd_tax_class=>lazy_tax_rates_read = abap_false.
      SELECT FROM zi_sd_tr_vat_rate FIELDS *
             INTO CORRESPONDING FIELDS OF TABLE @zcl_sd_tax_class=>lazy_tax_rates.

      zcl_sd_tax_class=>lazy_tax_rates_read = abap_true.
    ENDIF.

    result = REF #( zcl_sd_tax_class=>lazy_tax_rates ).
  ENDMETHOD.

  METHOD constructor.
    DATA(tax_rates) = lazy_get_tax_rates( ).

    TRY.
        me->tax_rate = REF #( tax_rates->*[ KEY primary_key COMPONENTS taxm1 = taxm1 ] ).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION NEW zcx_sd_tax_class( textid = zcx_sd_tax_class=>undefined_tax_class
                                              taxm1  = taxm1 ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.