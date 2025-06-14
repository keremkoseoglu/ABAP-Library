CLASS zcl_co_control_code DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_co_control_code.

    CLASS-METHODS get_instance
      IMPORTING kokrs         TYPE kokrs
      RETURNING VALUE(result) TYPE REF TO zcl_co_control_code
      RAISING   zcx_co_control_code.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             kokrs        TYPE kokrs,
             control_code TYPE REF TO zcl_co_control_code,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict WITH UNIQUE KEY primary_key COMPONENTS kokrs.

    TYPES: BEGIN OF header_dict,
             erkrs TYPE tka01-erkrs,
           END OF header_dict.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: kokrs        TYPE kokrs,
          bezei        TYPE tka01-bezei,
          header       TYPE header_dict,
          header_built TYPE abap_bool.

    METHODS constructor
      IMPORTING kokrs TYPE kokrs
      RAISING   zcx_co_control_code.

    METHODS get_header_lazy RETURNING VALUE(result) TYPE REF TO header_dict.
ENDCLASS.


CLASS zcl_co_control_code IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( multitons[ KEY primary_key
                                     kokrs = kokrs ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( kokrs        = kokrs
                        control_code = NEW #( kokrs ) )
               INTO TABLE multitons REFERENCE INTO mt.
    ENDTRY.

    result = mt->control_code.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM tka01
           FIELDS @abap_true
           WHERE kokrs = @kokrs
           INTO @DATA(exists_in_db).

    IF exists_in_db IS INITIAL.
      RAISE EXCEPTION NEW zcx_co_control_code( textid = zcx_co_control_code=>not_found
                                               kokrs  = kokrs ).
    ENDIF.

    me->kokrs = kokrs.
  ENDMETHOD.

  METHOD get_header_lazy.
    result = REF #( me->header ).
    CHECK me->header_built = abap_false.

    SELECT SINGLE FROM tka01
           FIELDS erkrs
           WHERE kokrs = @me->kokrs
           INTO CORRESPONDING FIELDS OF @result->*.

    me->header_built = abap_true.
  ENDMETHOD.

  METHOD zif_co_control_code~get_text.
    IF me->bezei IS INITIAL.
      SELECT SINGLE FROM tka01
             FIELDS bezei
             WHERE kokrs = @me->kokrs
             INTO @me->bezei.
    ENDIF.

    result = me->bezei.
  ENDMETHOD.

  METHOD zif_co_control_code~get_kokrs.
    result = me->kokrs.
  ENDMETHOD.

  METHOD zif_co_control_code~get_erkrs.
    result = get_header_lazy( )->erkrs.
  ENDMETHOD.
ENDCLASS.