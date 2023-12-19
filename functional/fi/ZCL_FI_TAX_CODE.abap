CLASS zcl_fi_tax_code DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING kalsm         TYPE t007a-kalsm
                mwskz         TYPE t007a-mwskz
      RETURNING VALUE(result) TYPE REF TO zcl_fi_tax_code
      RAISING   zcx_fi_tax_code.

    CLASS-METHODS get_instance_via_company
      IMPORTING bukrs         TYPE t001-bukrs
                mwskz         TYPE t007a-mwskz
      RETURNING VALUE(result) TYPE REF TO zcl_fi_tax_code
      RAISING   zcx_fi_tax_code.

    CLASS-METHODS get_instance_via_country
      IMPORTING land1         TYPE t005-land1
                mwskz         TYPE t007a-mwskz
      RETURNING VALUE(result) TYPE REF TO zcl_fi_tax_code
      RAISING   zcx_fi_tax_code.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             kalsm TYPE t007a-kalsm,
             mwskz TYPE t007a-mwskz,
             obj   TYPE REF TO zcl_fi_tax_code,
             cx    TYPE REF TO zcx_fi_tax_code,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS kalsm mwskz.

    TYPES: BEGIN OF company_multiton_dict,
             bukrs TYPE bukrs,
             mwskz TYPE t007a-mwskz,
             obj   TYPE REF TO zcl_fi_tax_code,
             cx    TYPE REF TO zcx_fi_tax_code,
           END OF company_multiton_dict,

           company_multiton_set TYPE HASHED TABLE OF company_multiton_dict
                                WITH UNIQUE KEY primary_key COMPONENTS bukrs mwskz.

    TYPES: BEGIN OF country_multiton_dict,
             land1 TYPE land1,
             mwskz TYPE t007a-mwskz,
             obj   TYPE REF TO zcl_fi_tax_code,
             cx    TYPE REF TO zcx_fi_tax_code,
           END OF country_multiton_dict,

           country_multiton_set TYPE HASHED TABLE OF country_multiton_dict
                                WITH UNIQUE KEY primary_key COMPONENTS land1 mwskz.

    CLASS-DATA: multitons         TYPE multiton_set,
                company_multitons TYPE company_multiton_set,
                country_multitons TYPE country_multiton_set.

    DATA: kalsm TYPE t007a-kalsm,
          mwskz TYPE t007a-mwskz.

    METHODS constructor
      IMPORTING kalsm TYPE t007a-kalsm
                mwskz TYPE t007a-mwskz
      RAISING   zcx_fi_tax_code.
ENDCLASS.


CLASS zcl_fi_tax_code IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_fi_tax_code=>multitons[ KEY primary_key
                                                      COMPONENTS kalsm = kalsm
                                                                 mwskz = mwskz ] ).
      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE multiton_dict( kalsm = kalsm
                                            mwskz = mwskz ).

        TRY.
            new_mt-obj = NEW #( kalsm = kalsm
                                mwskz = mwskz ).
          CATCH zcx_fi_tax_code INTO new_mt-cx ##NO_HANDLER.
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_fi_tax_code=>multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->cx IS NOT INITIAL.
      RAISE EXCEPTION mt->cx.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_instance_via_company.
    TRY.
        DATA(mt) = REF #( zcl_fi_tax_code=>company_multitons[ KEY primary_key
                                                              COMPONENTS bukrs = bukrs
                                                                         mwskz = mwskz ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE company_multiton_dict( bukrs = bukrs
                                                    mwskz = mwskz ).

        TRY.
            DATA(company) = zcl_fi_company=>get_instance( new_mt-bukrs ).

            new_mt-obj    = get_instance_via_country( land1 = company->gs_def-land1
                                                      mwskz = new_mt-mwskz ).

          CATCH zcx_fi_tax_code INTO new_mt-cx ##NO_HANDLER.

          CATCH cx_root INTO DATA(company_error).
            new_mt-cx = NEW #( textid   = zcx_fi_tax_code=>company_error
                               previous = company_error
                               bukrs    = new_mt-bukrs
                               mwskz    = new_mt-mwskz ).
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_fi_tax_code=>company_multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->cx IS NOT INITIAL.
      RAISE EXCEPTION mt->cx.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_instance_via_country.
    TRY.
        DATA(mt) = REF #( zcl_fi_tax_code=>country_multitons[ KEY primary_key
                                                              COMPONENTS land1 = land1
                                                                         mwskz = mwskz ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE country_multiton_dict( land1 = land1
                                                    mwskz = mwskz ).

        TRY.
            DATA(country) = zcl_bc_land=>get_instance( VALUE #( land1 = new_mt-land1 ) ).

            new_mt-obj    = NEW #( kalsm = country->def-kalsm
                                   mwskz = new_mt-mwskz ).

          CATCH zcx_fi_tax_code INTO new_mt-cx ##NO_HANDLER.

          CATCH cx_root INTO DATA(country_error).
            new_mt-cx = NEW #( textid   = zcx_fi_tax_code=>country_error
                               previous = country_error
                               land1    = new_mt-land1
                               mwskz    = new_mt-mwskz ).
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_fi_tax_code=>country_multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->cx IS NOT INITIAL.
      RAISE EXCEPTION mt->cx.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM t007a
           FIELDS @abap_true
           WHERE  kalsm = @kalsm AND
                  mwskz = @mwskz
           INTO   @DATA(keys_valid).

    IF keys_valid = abap_false.
      RAISE EXCEPTION NEW zcx_fi_tax_code( textid = zcx_fi_tax_code=>undefined
                                           kalsm  = kalsm
                                           mwskz  = mwskz ).
    ENDIF.

    me->kalsm = kalsm.
    me->mwskz = mwskz.
  ENDMETHOD.
ENDCLASS.