CLASS zcl_bc_sap_user_language DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF spras,
                 turkish TYPE sylangu VALUE 'T',
                 english TYPE sylangu VALUE 'E',
               END OF spras.

    DATA val TYPE sylangu READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING langu         TYPE sylangu
      RETURNING VALUE(result) TYPE REF TO zcl_bc_sap_user_language.

    CLASS-METHODS get_normalized_instance
      IMPORTING langu         TYPE sylangu
      RETURNING VALUE(result) TYPE REF TO zcl_bc_sap_user_language.

    CLASS-METHODS get_default RETURNING VALUE(result) TYPE REF TO zcl_bc_sap_user_language.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             langu TYPE sylangu,
             obj   TYPE REF TO zcl_bc_sap_user_language,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map WITH UNIQUE KEY langu.

    CLASS-DATA: multitons      TYPE multiton_set,
                norm_multitons TYPE multiton_set.

    METHODS constructor IMPORTING langu TYPE sylangu.
ENDCLASS.


CLASS zcl_bc_sap_user_language IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key
                      langu = langu ] TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      INSERT VALUE #( langu = langu
                      obj   = NEW #( langu ) )
             INTO TABLE multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_normalized_instance.
    ASSIGN norm_multitons[ KEY primary_key
                           langu = langu ] TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      DATA(normalized_langu) = SWITCH sylangu( langu
                                               WHEN spras-turkish OR spras-english THEN langu
                                               WHEN space                          THEN spras-turkish
                                               ELSE                                     spras-english ).

      INSERT VALUE #( langu = langu
                      obj   = get_instance( normalized_langu ) )
             INTO TABLE norm_multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD get_default.
    result = get_instance( spras-turkish ).
  ENDMETHOD.

  METHOD constructor.
    me->val = langu.
  ENDMETHOD.
ENDCLASS.