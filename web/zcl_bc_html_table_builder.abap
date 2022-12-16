CLASS zcl_bc_html_table_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO zcl_bc_html_table_builder.

    METHODS execute_with_tabname
      IMPORTING !itab            TYPE ANY TABLE
                !tabname         TYPE tabname
                !excluded_fields TYPE fieldname_tab OPTIONAL
                !max_decimals    TYPE i OPTIONAL
      RETURNING VALUE(result)    TYPE w3html_tab
      RAISING   zcx_bc_html.

    METHODS execute_with_alv_fcat
      IMPORTING !itab         TYPE ANY TABLE
                !alv_fcat     TYPE slis_t_fieldcat_alv
                !tabname      TYPE tabname OPTIONAL
                !max_decimals TYPE i OPTIONAL
      RETURNING VALUE(result) TYPE w3html_tab
      RAISING   zcx_bc_html.

    METHODS execute_with_dyn_itab
      IMPORTING !dyn_itab     TYPE REF TO ycl_addict_dynamic_itab
                !tabname      TYPE tabname OPTIONAL
                !max_decimals TYPE i OPTIONAL
      RETURNING VALUE(result) TYPE w3html_tab
      RAISING   zcx_bc_html.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ewaf_dict,
             itab     TYPE REF TO data,
             alv_fcat TYPE slis_t_fieldcat_alv,
             html     TYPE REF TO zcl_bc_html_builder,
           END OF ewaf_dict.

    CONSTANTS: BEGIN OF data_type,
                 curr TYPE dd03l-datatype VALUE 'CURR',
                 quan TYPE dd03l-datatype VALUE 'QUAN',
               END OF data_type.

    CLASS-DATA singleton TYPE REF TO zcl_bc_html_table_builder.
    DATA ewaf TYPE ewaf_dict.

    METHODS open_table RAISING zcx_bc_html.
    METHODS build_header  RAISING zcx_bc_html.
    METHODS build_body  RAISING zcx_bc_html.
    METHODS close_table  RAISING zcx_bc_html.

    METHODS is_field_numeric
      IMPORTING !fcat_entry   TYPE REF TO slis_fieldcat_alv
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS build_td
      IMPORTING !fcat_entry   TYPE REF TO slis_fieldcat_alv
      RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_bc_html_table_builder IMPLEMENTATION.
  METHOD get_instance.
    IF zcl_bc_html_table_builder=>singleton IS INITIAL.
      zcl_bc_html_table_builder=>singleton = NEW #( ).
    ENDIF.
    result = zcl_bc_html_table_builder=>singleton.
  ENDMETHOD.


  METHOD execute_with_tabname.
    TRY.
        DATA(fcat) = VALUE slis_t_fieldcat_alv( ).

        ##FM_SUBRC_OK
        CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name       = tabname
          CHANGING
            ct_fieldcat            = fcat
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'REUSE_ALV_FIELDCATALOG_MERGE' ).

        LOOP AT excluded_fields REFERENCE INTO DATA(excluded_field).
          DELETE fcat WHERE fieldname = excluded_field->*.
        ENDLOOP.

        result = execute_with_alv_fcat( itab         = itab
                                        alv_fcat     = fcat
                                        tabname      = tabname
                                        max_decimals = max_decimals ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_bc_html( textid    = zcx_bc_html=>html_table_build_error
                                         previous  = diaper
                                         structure = tabname ).
    ENDTRY.
  ENDMETHOD.


  METHOD execute_with_dyn_itab.
    FIELD-SYMBOLS <dyn_itab> TYPE ANY TABLE.

    TRY.
        DATA(fcat)          = dyn_itab->get_alv_fcat( ).
        DATA(dyn_itab_ref)  = dyn_itab->get_itab_ref( ).
        ASSIGN dyn_itab_ref->* TO <dyn_itab>.

        result = execute_with_alv_fcat( itab         = <dyn_itab>
                                        alv_fcat     = fcat
                                        tabname      = tabname
                                        max_decimals = max_decimals ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_bc_html( textid    = zcx_bc_html=>html_table_build_error
                                         previous  = diaper
                                         structure = tabname ).
    ENDTRY.
  ENDMETHOD.


  METHOD execute_with_alv_fcat.
    TRY.
        me->ewaf = VALUE #( itab      = REF #( itab )
                            alv_fcat  = alv_fcat
                            html      = NEW #( ) ).

        IF max_decimals IS NOT INITIAL.
          MODIFY me->ewaf-alv_fcat FROM         VALUE #( decimals_out = max_decimals )
                                   TRANSPORTING decimals_out
                                   WHERE        decimals_out IS INITIAL OR
                                                decimals_out > max_decimals.
        ENDIF.

        open_table( ).
        build_header( ).
        build_body( ).
        close_table( ).

        result = me->ewaf-html->get_html_itab( ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW zcx_bc_html( textid    = zcx_bc_html=>html_table_build_error
                                         previous  = diaper
                                         structure = tabname ).
    ENDTRY.
  ENDMETHOD.


  METHOD open_table.
    me->ewaf-html->append_html( |<table border='1' cellspacing='1' cellpadding='1'>| ).
  ENDMETHOD.


  METHOD build_header.
    me->ewaf-html->append_html( |<tr>| ).

    LOOP AT me->ewaf-alv_fcat REFERENCE INTO DATA(fcat_entry).
      me->ewaf-html->append_html( build_td( fcat_entry ) ).
      me->ewaf-html->append_html( |<b>{ fcat_entry->seltext_l }</b>| ).
      me->ewaf-html->append_html( |</td>| ).
    ENDLOOP.

    me->ewaf-html->append_html( |</tr>| ).
  ENDMETHOD.


  METHOD build_body.
    FIELD-SYMBOLS: <itab>  TYPE ANY TABLE,
                   <cell>  TYPE any,
                   <waers> TYPE waers,
                   <meins> TYPE meins.

    ASSIGN me->ewaf-itab->* TO <itab>.

    LOOP AT <itab> ASSIGNING FIELD-SYMBOL(<entry>).
      me->ewaf-html->append_html( |<tr>| ).

      LOOP AT me->ewaf-alv_fcat REFERENCE INTO DATA(fcat_entry).
        me->ewaf-html->append_html( build_td( fcat_entry ) ).

        ASSIGN COMPONENT fcat_entry->fieldname OF STRUCTURE <entry> TO <cell>.
        ASSERT sy-subrc = 0.

        CASE fcat_entry->datatype.
          WHEN me->data_type-curr.
            ASSIGN COMPONENT fcat_entry->cfieldname OF STRUCTURE <entry> TO <waers>.
            ASSERT sy-subrc = 0.

            me->ewaf-html->append_value( value     = <cell>
                                         waers     = <waers>
                                         decimals  = CONV #( fcat_entry->decimals_out ) ).

          WHEN me->data_type-quan.
            ASSIGN COMPONENT fcat_entry->qfieldname OF STRUCTURE <entry> TO <meins>.
            ASSERT sy-subrc = 0.

            me->ewaf-html->append_value( value     = <cell>
                                         meins     = <meins>
                                         decimals  = CONV #( fcat_entry->decimals_out ) ).

          WHEN OTHERS.
            me->ewaf-html->append_value( value     = <cell>
                                         decimals  = CONV #( fcat_entry->decimals_out ) ).
        ENDCASE.


        me->ewaf-html->append_html( |</td>| ).
      ENDLOOP.

      me->ewaf-html->append_html( |</tr>| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD close_table.
    me->ewaf-html->append_html( |</table>| ).
  ENDMETHOD.


  METHOD is_field_numeric.
    result = xsdbool( fcat_entry->inttype = 'P' OR
                      fcat_entry->inttype = 'N' OR
                      fcat_entry->inttype = 'I' ).
  ENDMETHOD.


  METHOD build_td.
    DATA(alignment) = SWITCH char5( is_field_numeric( fcat_entry )
                                    WHEN abap_true
                                    THEN 'right'
                                    ELSE 'left' ).

    result = |<td align='{ alignment }'>|.
  ENDMETHOD.
ENDCLASS.