CLASS zcl_bc_sap_client DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_bc_sap_client.

    CLASS-METHODS get_instance
      IMPORTING mandt         TYPE symandt DEFAULT sy-mandt
      RETURNING VALUE(result) TYPE REF TO zcl_bc_sap_client
      RAISING   zcx_bc_table_content.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             mandt TYPE sy-mandt,
             obj   TYPE REF TO zcl_bc_sap_client,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS mandt.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'T000',
               END OF table.

    CONSTANTS: BEGIN OF cccategory,
                 customizing TYPE t000-cccategory VALUE 'C',
               END OF cccategory.

    CONSTANTS: BEGIN OF cccoractiv,
                 changes_wo_auto_rec         TYPE cccoractiv VALUE space,
                 auto_rec_of_changes         TYPE cccoractiv VALUE '1',
                 no_changes                  TYPE cccoractiv VALUE '2',
                 change_wo_auto_rec_no_trans TYPE cccoractiv VALUE '3',
               END OF cccoractiv.

    CLASS-DATA multitons TYPE multiton_set.

    DATA def TYPE t000.

ENDCLASS.


CLASS zcl_bc_sap_client IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN zcl_bc_sap_client=>multitons[ KEY primary_key
                                         mandt = mandt ]
           TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_dict( mandt = mandt ).

      multiton-obj = NEW #( ).

      SELECT SINGLE * INTO @multiton-obj->def
             FROM t000
             WHERE mandt = @multiton-mandt.

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                  objectid = CONV #( multiton-mandt )
                                                  tabname  = zcl_bc_sap_client=>table-def ).
      ENDIF.

      INSERT multiton INTO TABLE zcl_bc_sap_client=>multitons ASSIGNING <multiton>.
    ENDIF.

    result = <multiton>-obj.
  ENDMETHOD.

  METHOD zif_bc_sap_client~ensure_customizing_client.
    CHECK NOT zif_bc_sap_client~is_customizing_client( ).

    RAISE EXCEPTION NEW zcx_bc_sap_client( textid = zcx_bc_sap_client=>not_customizing_client
                                           mandt  = me->def-mandt ).
  ENDMETHOD.

  METHOD zif_bc_sap_client~is_customizing_client.
    result = xsdbool( me->def-cccategory = zcl_bc_sap_client=>cccategory-customizing ).
  ENDMETHOD.

  METHOD zif_bc_sap_client~is_customizable.
    result = xsdbool( me->def-cccoractiv <> zcl_bc_sap_client=>cccoractiv-no_changes ).
  ENDMETHOD.

  METHOD zif_bc_sap_client~get_def.
    result = me->def.
  ENDMETHOD.
ENDCLASS.