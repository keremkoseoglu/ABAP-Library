CLASS zcl_sd_sales_office DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING vkbur         TYPE vkbur
      RETURNING VALUE(result) TYPE REF TO zcl_sd_sales_office
      RAISING   zcx_sd_sales_office.

    CLASS-METHODS get_bezei
      IMPORTING iv_spras        TYPE spras DEFAULT sy-langu
                iv_vkbur        TYPE vkbur
      RETURNING VALUE(rv_bezei) TYPE tvkbt-bezei.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             vkbur TYPE vkbur,
             obj   TYPE REF TO zcl_sd_sales_office,
             cx    TYPE REF TO zcx_sd_sales_office,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
           WITH UNIQUE KEY primary_key COMPONENTS vkbur.

    TYPES tvkbt_set TYPE HASHED TABLE OF tvkbt WITH UNIQUE KEY primary_key COMPONENTS spras vkbur.

    CLASS-DATA: multitons TYPE multiton_set,
                tvkbts    TYPE tvkbt_set.

    DATA vkbur TYPE vkbur.

    METHODS constructor
      IMPORTING vkbur TYPE vkbur
      RAISING   zcx_sd_sales_office.

ENDCLASS.


CLASS zcl_sd_sales_office IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_sd_sales_office=>multitons[ KEY primary_key
                                                          vkbur = vkbur ] ).
      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE multiton_dict( vkbur = vkbur ).

        TRY.
            new_mt-obj = NEW #( new_mt-vkbur ).
          CATCH zcx_sd_sales_office INTO new_mt-cx ##NO_HANDLER.
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_sd_sales_office=>multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->cx IS NOT INITIAL.
      RAISE EXCEPTION mt->cx.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_bezei.
    IF zcl_sd_sales_office=>tvkbts IS INITIAL.
      SELECT * INTO TABLE zcl_sd_sales_office=>tvkbts FROM tvkbt.
      IF sy-subrc <> 0.
        INSERT INITIAL LINE INTO TABLE zcl_sd_sales_office=>tvkbts.
      ENDIF.
    ENDIF.

    ASSIGN zcl_sd_sales_office=>tvkbts[ KEY primary_key
                                        spras = iv_spras
                                        vkbur = iv_vkbur ]
           TO FIELD-SYMBOL(<tvkbt>).

    CHECK sy-subrc = 0.

    rv_bezei = <tvkbt>-bezei.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM tvbur
           FIELDS @abap_true
           WHERE vkbur = @vkbur
           INTO @DATA(record_exists).

    IF record_exists = abap_false.
      RAISE EXCEPTION NEW zcx_sd_sales_office( textid = zcx_sd_sales_office=>undefined_sales_office
                                               vkbur  = vkbur ).
    ENDIF.

    me->vkbur = vkbur.
  ENDMETHOD.
ENDCLASS.