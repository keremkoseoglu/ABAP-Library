CLASS zcl_bc_restful_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA def TYPE zbct_restful_001          READ-ONLY.
    DATA imp TYPE REF TO zif_bc_restful_api READ-ONLY.

    METHODS constructor
      IMPORTING apiid TYPE zbct_restful_001-apiid
      RAISING   zcx_bc_table_content
                zcx_bc_authorization.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'ZBCT_RESTFUL_001',
               END OF table,

               BEGIN OF field,
                 clsname TYPE fieldname VALUE 'CLSNAME',
               END OF field.

    METHODS create_imp_object RAISING zcx_bc_table_content.

    METHODS read_definition
      IMPORTING apiid TYPE zbct_restful_001-apiid
      RAISING   zcx_bc_table_content.

    METHODS check_authorization RAISING zcx_bc_authorization.

ENDCLASS.


CLASS zcl_bc_restful_api IMPLEMENTATION.
  METHOD constructor.
    read_definition( apiid ).
    check_authorization( ).
    create_imp_object( ).
  ENDMETHOD.

  METHOD create_imp_object.
    DATA obj TYPE REF TO object.

    IF me->def-clsname IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid    = zcx_bc_table_content=>entry_field_initial
                                                objectid  = CONV #( me->def-apiid )
                                                tabname   = zcl_bc_restful_api=>table-def
                                                fieldname = zcl_bc_restful_api=>field-clsname ).
    ENDIF.

    TRY.
        CREATE OBJECT obj TYPE (me->def-clsname).
        me->imp = CAST #( obj ).
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_bc_table_content( textid    = zcx_bc_table_content=>value_invalid
                                                  objectid  = CONV #( me->def-apiid )
                                                  tabname   = zcl_bc_restful_api=>table-def
                                                  fieldname = zcl_bc_restful_api=>field-clsname ).
    ENDTRY.
  ENDMETHOD.

  METHOD read_definition.
    SELECT SINGLE * FROM zbct_restful_001
           WHERE apiid = @apiid
           INTO CORRESPONDING FIELDS OF @me->def.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                objectid = CONV #( apiid )
                                                tabname  = zcl_bc_restful_api=>table-def ).
    ENDIF.

    IF me->def-lvorm = abap_true.
      RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_has_deletion_mark
                                                objectid = CONV #( apiid )
                                                tabname  = zcl_bc_restful_api=>table-def ).
    ENDIF.
  ENDMETHOD.

  METHOD check_authorization.
    AUTHORITY-CHECK OBJECT 'ZBCAOREST'
                    ID 'ZBC_APIID' FIELD me->def-apiid.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_bc_authorization( textid   = zcx_bc_authorization=>no_auth_for_objectid
                                                objectid = CONV #( me->def-apiid ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.