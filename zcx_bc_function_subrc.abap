CLASS zcx_bc_function_subrc DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF subrc_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE 'PARAM',
        attr4 TYPE scx_attrname VALUE 'STEXT',
      END OF subrc_error .
    CONSTANTS:
      BEGIN OF remote_func_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '315',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'RFCDEST',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF remote_func_error .
    DATA funcname TYPE funct-funcname .
    DATA subrc TYPE sysubrc .
    DATA param TYPE funct-parameter .
    DATA stext TYPE funct-stext .
    DATA rfcdest TYPE rfcdest .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !funcname TYPE funct-funcname OPTIONAL
        !subrc    TYPE sysubrc DEFAULT sy-subrc
        !param    TYPE funct-parameter OPTIONAL
        !stext    TYPE funct-stext OPTIONAL
        !rfcdest  TYPE rfcdest OPTIONAL.

    CLASS-METHODS raise_if_sysubrc_not_initial
      IMPORTING
        !iv_funcname TYPE funct-funcname
      RAISING
        zcx_bc_function_subrc .

    CLASS-METHODS raise_rfc_if_sysubrc_not_init
      IMPORTING
        !iv_funcname TYPE funct-funcname
        !iv_rfcdest  TYPE rfcdest
      RAISING
        zcx_bc_function_subrc.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_function_subrc IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->funcname = funcname.
    me->subrc = subrc.
    me->param = param.
    me->stext = stext.
    me->rfcdest = rfcdest.

  ENDMETHOD.


  METHOD raise_if_sysubrc_not_initial.

    CHECK sy-subrc IS NOT INITIAL.

    DATA(lv_subrc_bak) = sy-subrc.

    SELECT SINGLE parameter INTO @DATA(lv_parameter)
           FROM fupararef
           WHERE funcname  EQ @iv_funcname
             AND paramtype EQ @abap_true
             AND pposition EQ @lv_subrc_bak.

    SELECT SINGLE stext INTO @DATA(lv_stext)
           FROM funct
           WHERE spras     EQ @sy-langu
             AND funcname  EQ @iv_funcname
             AND parameter EQ @lv_parameter
             AND kind      EQ @abap_true.

    IF sy-subrc NE 0.
      SELECT SINGLE stext INTO @lv_stext
             FROM funct
             WHERE funcname  EQ @iv_funcname
               AND parameter EQ @lv_parameter
               AND kind      EQ @abap_true.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_bc_function_subrc
      EXPORTING
        funcname = iv_funcname
        param    = lv_parameter
        stext    = lv_stext
        subrc    = lv_subrc_bak
        textid   = zcx_bc_function_subrc=>subrc_error.

  ENDMETHOD.


  METHOD raise_rfc_if_sysubrc_not_init.

    CHECK sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE zcx_bc_function_subrc
      EXPORTING
        textid   = zcx_bc_function_subrc=>remote_func_error
        funcname = iv_funcname
        rfcdest  = iv_rfcdest.

  ENDMETHOD.

ENDCLASS.