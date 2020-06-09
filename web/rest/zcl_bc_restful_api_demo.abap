CLASS zcl_bc_restful_api_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_bc_restful_api.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF state_type,
             json_input  TYPE zbcs_restful_api_demo_input,
             json_output TYPE string,
             data_added  TYPE abap_bool,
           END OF state_type.

    DATA state TYPE state_type.

    METHODS raise_error RAISING zcx_bc_class_method.
    METHODS add_variable_to_output.
    METHODS add_structure_to_output.
    METHODS add_table_to_output.
    METHODS add_comma.

ENDCLASS.



CLASS zcl_bc_restful_api_demo IMPLEMENTATION.

  METHOD zif_bc_restful_api~accept_json_input.
    TRY.
        CLEAR me->state.

        /ui2/cl_json=>deserialize(
          EXPORTING json = json_input
          CHANGING  data = me->state-json_input ).

        IF me->state-json_input-i_want_error EQ abap_true.
          raise_error( ).
        ENDIF.

        me->state-json_output = |\{ "output": \{ |.

        IF me->state-json_input-i_want_variable EQ abap_true.
          add_comma( ).
          add_variable_to_output( ).
          me->state-data_added = abap_true.
        ENDIF.

        IF me->state-json_input-i_want_structure EQ abap_true.
          add_comma( ).
          add_structure_to_output( ).
          me->state-data_added = abap_true.
        ENDIF.

        IF me->state-json_input-i_want_table EQ abap_true.
          add_comma( ).
          add_table_to_output( ).
          me->state-data_added = abap_true.
        ENDIF.

        me->state-json_output = |{ me->state-json_output } \} \}|.

        json_output = me->state-json_output.

      CATCH zcx_bc_class_method INTO DATA(method_error).
        RAISE EXCEPTION method_error.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = diaper
            class    = CONV #( cl_abap_classdescr=>get_class_name( me ) )
            method   = zif_bc_restful_api=>method-accept_json_input.

    ENDTRY.
  ENDMETHOD.


  METHOD raise_error.
    RAISE EXCEPTION TYPE zcx_bc_class_method
      EXPORTING
        textid = zcx_bc_class_method=>unexpected_error
        class  = CONV #( cl_abap_classdescr=>get_class_name( me ) )
        method = zif_bc_restful_api=>method-accept_json_input.
  ENDMETHOD.


  METHOD add_variable_to_output.
    me->state-json_output =
        |{ me->state-json_output } | &&
        |"MANDT": "{ zcl_bc_json_toolkit=>get_json_text( sy-mandt ) }" |.
  ENDMETHOD.


  METHOD add_structure_to_output.

    SELECT SINGLE bukrs, butxt
           FROM t001
           INTO @DATA(company). "#EC CI_NOORDER

    me->state-json_output =
        |{ me->state-json_output } | &&
        |"structure": \{ | &&
        |"BUKRS": "{ zcl_bc_json_toolkit=>get_json_text( company-bukrs ) }", | &&
        |"BUTXT": "{ zcl_bc_json_toolkit=>get_json_text( company-butxt ) }" | &&
        |\}|.
  ENDMETHOD.


  METHOD add_table_to_output.

    SELECT bukrs, butxt
           FROM t001
           INTO TABLE @DATA(companies).

    me->state-json_output = |{ me->state-json_output } "table": [ |.

    LOOP AT companies ASSIGNING FIELD-SYMBOL(<company>).
      IF sy-tabix > 1.
        me->state-json_output = |{ me->state-json_output }, |.
      ENDIF.

      me->state-json_output =
        |{ me->state-json_output } \{ | &&
        |"BUKRS": "{ zcl_bc_json_toolkit=>get_json_text( <company>-bukrs ) }", | &&
        |"BUTXT": "{ zcl_bc_json_toolkit=>get_json_text( <company>-butxt ) }" | &&
        |\}|.
    ENDLOOP.

    me->state-json_output = |{ me->state-json_output } ] |.
  ENDMETHOD.


  METHOD add_comma.
    CHECK me->state-data_added EQ abap_true.
    me->state-json_output = |{ me->state-json_output }, |.
  ENDMETHOD.

ENDCLASS.