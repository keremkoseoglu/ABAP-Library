CLASS zcl_bc_dynamic_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_src TYPE TABLE OF string.

    DATA gs_dynprog TYPE zbct_dynprog READ-ONLY.

    METHODS constructor.

    METHODS delete.

    METHODS execute
      IMPORTING
        !it_src TYPE tt_src
        !it_txt TYPE textpool_table OPTIONAL.

    METHODS generate_name
      RETURNING
        VALUE(rv_name) TYPE programm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS delete_program
      IMPORTING
        !iv_programm TYPE programm
      RAISING
        zcx_bc_table_content.

    METHODS delete_program_forced IMPORTING !iv_programm TYPE programm.

    METHODS maintain_tmp_table.

ENDCLASS.



CLASS ZCL_BC_DYNAMIC_PROG IMPLEMENTATION.


  METHOD constructor.
    maintain_tmp_table( ).
  ENDMETHOD.


  METHOD delete.
    delete_program_forced( gs_dynprog-programm ).
  ENDMETHOD.


  METHOD delete_program.

    SELECT SINGLE mandt INTO sy-mandt
           FROM zbct_dynprog
           WHERE programm EQ iv_programm.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          objectid = CONV #( iv_programm )
          tabname  = 'ZBCT_DYNPROG'
          textid   = zcx_bc_table_content=>entry_missing.
    ENDIF.

    delete_program_forced( iv_programm ).

  ENDMETHOD.


  METHOD delete_program_forced.
    DELETE REPORT iv_programm.
    DELETE TEXTPOOL iv_programm.
    DELETE FROM zbct_dynprog WHERE programm EQ iv_programm.
  ENDMETHOD.


  METHOD execute.

    generate_name( ).

    gs_dynprog-ernam    = sy-uname.
    gs_dynprog-erdat    = sy-datum.
    gs_dynprog-erzet    = sy-uzeit.

    INSERT REPORT gs_dynprog-programm FROM it_src.

    IF it_txt IS SUPPLIED.
      INSERT TEXTPOOL gs_dynprog-programm FROM it_txt LANGUAGE sy-langu.
    ENDIF.

    INSERT zbct_dynprog FROM gs_dynprog.
  ENDMETHOD.


  METHOD generate_name.
    CHECK gs_dynprog-programm IS INITIAL.
    gs_dynprog-programm = |ZTMP_{ cl_reca_guid=>get_new_guid( ) }|.
    gs_dynprog-programm = gs_dynprog-programm+0(30).
    rv_name = gs_dynprog-programm.
  ENDMETHOD.


  METHOD maintain_tmp_table.

    DATA(lv_erdat) = sy-datum - 2.
    SELECT * INTO TABLE @DATA(lt_db) FROM zbct_dynprog WHERE erdat LE @lv_erdat.

    LOOP AT lt_db ASSIGNING FIELD-SYMBOL(<ls_db>).
      delete_program_forced( <ls_db>-programm ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.