CLASS zcl_fi_account DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS conv_exit_saknr_input
      IMPORTING !saknr        TYPE saknr
      RETURNING VALUE(result) TYPE saknr.

    CLASS-METHODS does_company_account_exist
      IMPORTING !bukrs        TYPE skb1-bukrs
                !saknr        TYPE skb1-saknr
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS ensure_company_account_exists
      IMPORTING !bukrs TYPE skb1-bukrs
                !saknr TYPE skb1-saknr
      RAISING   cx_no_entry_in_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF comp_acc_existence_dict,
             bukrs  TYPE skb1-bukrs,
             saknr  TYPE skb1-saknr,
             exists TYPE abap_bool,
           END OF comp_acc_existence_dict,

           comp_acc_existence_set TYPE HASHED TABLE OF comp_acc_existence_dict
                                  WITH UNIQUE KEY primary_key COMPONENTS bukrs saknr.

    CONSTANTS: BEGIN OF table,
                 comp_acc TYPE tabname VALUE 'SKB1',
               END OF table.

    CLASS-DATA comp_acc_exist_cache TYPE comp_acc_existence_set.
ENDCLASS.



CLASS zcl_fi_account IMPLEMENTATION.


  METHOD conv_exit_saknr_input.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " SAKNR dönüşümü
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = saknr
      IMPORTING
        output = result.
  ENDMETHOD.


  METHOD does_company_account_exist.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Şirket kodunda hesap tanımlı mı değil mi?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cache) = REF #( zcl_fi_account=>comp_acc_exist_cache ).
    DATA(converted_saknr) = conv_exit_saknr_input( saknr ).

    ASSIGN cache->*[ KEY primary_key COMPONENTS
                     bukrs = bukrs
                     saknr = converted_saknr ]
           TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE comp_acc_existence_dict(
                                bukrs = bukrs
                                saknr = converted_saknr ).

      SELECT @abap_true
      FROM i_glaccountincompanycode
      INTO @DATA(lv_check)
      UP TO 1 ROWS
      WHERE companycode = @new_cache-bukrs
        AND glaccount = @new_cache-saknr
        ORDER BY PRIMARY KEY .
      ENDSELECT.
      
      new_cache-exists = xsdbool( sy-subrc = 0 ).

      INSERT new_cache INTO TABLE cache->* ASSIGNING <cache>.
    ENDIF.

    result = <cache>-exists.
  ENDMETHOD.


  METHOD ensure_company_account_exists.
    CHECK NOT does_company_account_exist(
                bukrs = bukrs
                saknr = saknr ).

    RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( table-comp_acc )
                                              entry_name = |{ bukrs } { saknr }| ).
  ENDMETHOD.
ENDCLASS.