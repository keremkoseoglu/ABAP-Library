CLASS zcl_bc_dimension DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_dimension.

    CLASS-METHODS get_instance
      IMPORTING dimid         TYPE dimid
      RETURNING VALUE(result) TYPE REF TO zcl_bc_dimension
      RAISING   zcx_bc_dimension.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             dimid     TYPE dimid,
             dimension TYPE REF TO zcl_bc_dimension,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict WITH UNIQUE KEY primary_key COMPONENTS dimid.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'T006D',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: dimid            TYPE dimid,
          _uoms            TYPE meins_tty,
          _uoms_read       TYPE abap_bool,
          _uom_range       TYPE mdg_bs_mat_t_range_meinh,
          _uom_range_built TYPE abap_bool.

    METHODS constructor
      IMPORTING dimid TYPE dimid
      RAISING   zcx_bc_dimension.

    METHODS get_uoms      RETURNING VALUE(result) TYPE REF TO meins_tty.

    METHODS get_uom_range RETURNING VALUE(result) TYPE REF TO mdg_bs_mat_t_range_meinh.
ENDCLASS.


CLASS zcl_bc_dimension IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_bc_dimension=>multitons[ KEY primary_key dimid = dimid ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( dimid     = dimid
                        dimension = NEW #( dimid ) )
               INTO TABLE zcl_bc_dimension=>multitons REFERENCE INTO mt.
    ENDTRY.

    result = mt->dimension.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM t006d
           FIELDS dimid
           WHERE dimid = @dimid
             AND (    leng  <> 0
                   OR mass  <> 0
                   OR timex <> 0
                   OR ecurr <> 0
                   OR temp  <> 0
                   OR molqu <> 0
                   OR light <> 0 )
           INTO @me->dimid.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_bc_dimension( textid   = zcx_bc_dimension=>unknown_dimension
                                            dimid    = dimid
                                            previous = NEW ycx_addict_table_content(
                                                textid   = ycx_addict_table_content=>no_entry_for_objectid
                                                tabname  = zcl_bc_dimension=>table-def
                                                objectid = CONV #( dimid ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_uoms.
    IF me->_uoms_read = abap_false.
      SELECT FROM t006
             FIELDS msehi
             WHERE dimid = @me->dimid
             INTO TABLE @me->_uoms.

      me->_uoms_read = abap_true.
    ENDIF.

    result = REF #( me->_uoms ).
  ENDMETHOD.

  METHOD get_uom_range.
    IF me->_uom_range_built = abap_false.
      DATA(uoms) = get_uoms( ).

      me->_uom_range       = COND #(
                              WHEN uoms->* IS NOT INITIAL
                              THEN VALUE #( FOR _uom IN uoms->*
                                            ( sign   = ycl_addict_toolkit=>sign-include
                                              option = ycl_addict_toolkit=>option-eq
                                              low    = _uom ) )

                              ELSE VALUE #( ( sign   = ycl_addict_toolkit=>sign-exclude
                                              option = ycl_addict_toolkit=>option-cp
                                              low    = '*' ) ) ).

      me->_uom_range_built = abap_true.
    ENDIF.

    result = REF #( me->_uom_range ).
  ENDMETHOD.

  METHOD zif_bc_dimension~get_uoms.
    result = get_uoms( )->*.
  ENDMETHOD.

  METHOD zif_bc_dimension~get_uom_range.
    result = get_uom_range( )->*.
  ENDMETHOD.
ENDCLASS.