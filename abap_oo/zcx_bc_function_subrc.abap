CLASS zcx_bc_function_subrc DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_bc_function_subrc,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '324',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bc_function_subrc .
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
      BEGIN OF function_returned_error,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '382',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error .
    CONSTANTS:
      BEGIN OF function_returned_error_txt,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '456',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'ERROR_TEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error_txt .
    CONSTANTS:
      BEGIN OF no_result_returned,
        msgid TYPE symsgid VALUE 'ZBC',
        msgno TYPE symsgno VALUE '584',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'STEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_result_returned .
    DATA funcname TYPE funct-funcname .
    DATA subrc TYPE sysubrc .
    DATA param TYPE funct-parameter .
    DATA stext TYPE funct-stext .
    DATA error_text TYPE string .

    CLASS-METHODS raise_if_sysubrc_not_initial
      IMPORTING
        !iv_funcname TYPE funct-funcname
      RAISING
        zcx_bc_function_subrc .

    CLASS-METHODS raise_from_addict
      IMPORTING !error TYPE REF TO ycx_addict_function_subrc
      RAISING   zcx_bc_function_subrc.

    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !funcname   TYPE funct-funcname OPTIONAL
        !subrc      TYPE sysubrc OPTIONAL
        !param      TYPE funct-parameter OPTIONAL
        !stext      TYPE funct-stext OPTIONAL
        !error_text TYPE string OPTIONAL .


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_BC_FUNCTION_SUBRC IMPLEMENTATION.


  METHOD raise_if_sysubrc_not_initial.

    CHECK sy-subrc IS NOT INITIAL.

    DATA(lv_subrc_bak) = sy-subrc.

    SELECT SINGLE parameter INTO @DATA(lv_parameter)
           FROM fupararef
           WHERE funcname  = @iv_funcname
             AND paramtype = @abap_true
             AND pposition = @lv_subrc_bak.

    SELECT SINGLE stext INTO @DATA(lv_stext)
           FROM funct
           WHERE spras     = @sy-langu
             AND funcname  = @iv_funcname
             AND parameter = @lv_parameter
             AND kind      = @abap_true.

    IF sy-subrc <> 0.
      SELECT SINGLE stext INTO @lv_stext
             FROM funct
             WHERE funcname  = @iv_funcname
               AND parameter = @lv_parameter
               AND kind      = @abap_true.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_bc_function_subrc
      EXPORTING
        funcname = iv_funcname
        param    = lv_parameter
        stext    = lv_stext
        subrc    = lv_subrc_bak
        textid   = zcx_bc_function_subrc=>subrc_error.

  ENDMETHOD.


  METHOD raise_from_addict.
    RAISE EXCEPTION TYPE zcx_bc_function_subrc
      EXPORTING
        textid     = error->if_t100_message~t100key
        previous   = error
        funcname   = error->funcname
        subrc      = error->subrc
        param      = error->param
        stext      = error->stext
        error_text = error->error_text.
  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->funcname = funcname .
    me->subrc = subrc .
    me->param = param .
    me->stext = stext .
    me->error_text = error_text .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_bc_function_subrc .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.