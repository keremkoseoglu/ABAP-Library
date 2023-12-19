CLASS zcl_bc_date DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF date_enum,
                 evermore TYPE sydatum VALUE '00010101',
                 forever  TYPE sydatum VALUE '99991231',
               END OF date_enum.

    DATA date TYPE dats READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !date         TYPE dats
      RETURNING VALUE(result) TYPE REF TO zcl_bc_date.

    METHODS get_month_end RETURNING VALUE(result) TYPE sydatum.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             date TYPE dats,
             obj  TYPE REF TO zcl_bc_date,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS date.

    CLASS-DATA multitons TYPE multiton_set.

    DATA month_end TYPE sydatum.

    METHODS constructor IMPORTING !date TYPE dats.
ENDCLASS.


CLASS zcl_bc_date IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN zcl_bc_date=>multitons[ KEY primary_key COMPONENTS date = date ] TO FIELD-SYMBOL(<mt>).
    IF sy-subrc <> 0.
      INSERT VALUE #( date = date
                      obj  = NEW #( date ) )
             INTO TABLE zcl_bc_date=>multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_month_end.
    IF me->month_end IS INITIAL.
      cl_reca_date=>get_date_info( EXPORTING id_date           = me->date
                                   IMPORTING ed_date_month_end = me->month_end ).
    ENDIF.

    result = me->month_end.
  ENDMETHOD.

  METHOD constructor.
    me->date = date.
  ENDMETHOD.
ENDCLASS.