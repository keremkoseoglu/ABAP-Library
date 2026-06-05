CLASS zcl_co_valuation_area DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF val_map,
             bukrs TYPE bukrs,
           END OF val_map.

    DATA: bwkey TYPE bwkey   READ-ONLY,
          val   TYPE val_map READ-ONLY.

    CLASS-METHODS get_instance IMPORTING bwkey         TYPE bwkey
                               RETURNING VALUE(result) TYPE REF TO zcl_co_valuation_area
                               RAISING   zcx_co_bwkey.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             bwkey TYPE bwkey,
             obj   TYPE REF TO zcl_co_valuation_area,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map WITH UNIQUE KEY bwkey.

    CLASS-DATA multitons TYPE multiton_set.

    METHODS constructor
      IMPORTING bwkey TYPE bwkey
      RAISING   zcx_co_bwkey.

ENDCLASS.


CLASS zcl_co_valuation_area IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key
                      bwkey = bwkey ] TO FIELD-SYMBOL(<mt>).
    IF sy-subrc <> 0.
      INSERT VALUE #( bwkey = bwkey
                      obj   = NEW #( bwkey ) ) INTO TABLE multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD constructor.
    IF bwkey IS INITIAL.
      RAISE EXCEPTION NEW zcx_co_bwkey( textid = zcx_co_bwkey=>empty_bwkey ).
    ENDIF.

    SELECT SINGLE FROM t001k
           FIELDS bukrs
           WHERE bwkey = @bwkey
           INTO CORRESPONDING FIELDS OF @me->val.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_co_bwkey( textid = zcx_co_bwkey=>undefined
                                        bwkey  = bwkey ).
    ENDIF.

    me->bwkey = bwkey.
  ENDMETHOD.
ENDCLASS.