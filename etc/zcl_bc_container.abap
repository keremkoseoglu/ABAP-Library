CLASS zcl_bc_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF t_var,
             context TYPE string,
             name    TYPE string,
             value   TYPE REF TO data,
           END OF t_var,

           tt_var TYPE HASHED TABLE OF t_var WITH UNIQUE KEY primary_key COMPONENTS context name.

    METHODS get_var
      IMPORTING
        !iv_context   TYPE string DEFAULT space
        !iv_name      TYPE string
      RETURNING
        VALUE(rr_val) TYPE REF TO data
      RAISING
        zcx_bc_container_var.

    METHODS set_var
      IMPORTING
        !is_var TYPE t_var.

    METHODS set_vars
      IMPORTING
        !it_var TYPE tt_var.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_var TYPE tt_var.

ENDCLASS.



CLASS ZCL_BC_CONTAINER IMPLEMENTATION.


  METHOD get_var.

    ASSIGN gt_var[ KEY primary_key
                   COMPONENTS context = iv_context
                              name    = iv_name ] TO FIELD-SYMBOL(<ls_var>).

    IF <ls_var> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_bc_container_var
        EXPORTING
          context = iv_context
          name    = iv_name
          textid  = zcx_bc_container_var=>var_missing.
    ENDIF.

    rr_val = <ls_var>-value.

  ENDMETHOD.


  METHOD set_var.

    ASSIGN gt_var[ KEY primary_key
                   COMPONENTS context = is_var-context
                              name    = is_var-name ] TO FIELD-SYMBOL(<ls_var>).

    IF <ls_var> IS NOT ASSIGNED.
      INSERT VALUE #( context = is_var-context
                      name    = is_var-name ) INTO TABLE gt_var ASSIGNING <ls_var>.
    ENDIF.

    <ls_var>-value = is_var-value.

  ENDMETHOD.


  METHOD set_vars.

    LOOP AT it_var ASSIGNING FIELD-SYMBOL(<ls_var>).
      set_var( <ls_var> ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.