CLASS zcl_co_cost_component_text DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF input_dict,
             bdatj TYPE bdatj,
             poper TYPE poper,
             bukrs TYPE bukrs,
           END OF input_dict.

    TYPES: BEGIN OF tckh1_dict,
             elemt TYPE ck_element,
             txele TYPE ck_txele,
           END OF tckh1_dict,

           tckh1_set TYPE HASHED TABLE OF tckh1_dict WITH UNIQUE KEY primary_key COMPONENTS elemt.

    TYPES: BEGIN OF tckh3_dict,
             el_hv TYPE ck_el_hv,
             elemt TYPE ck_element,
           END OF tckh3_dict,

           tckh3_list TYPE STANDARD TABLE OF tckh3_dict WITH EMPTY KEY.

    DATA elehk  TYPE ck_elesmhk READ-ONLY.
    DATA tckh1s TYPE tckh1_set  READ-ONLY.
    DATA tckh3s TYPE tckh3_list READ-ONLY.

    CLASS-METHODS set_fcat_text_from_itabs
      IMPORTING
        !kst_no      TYPE numc3
        !tckh1s      TYPE tckh1_set
        !tckh3s      TYPE tckh3_list
        !prefix      TYPE clike OPTIONAL
      EXPORTING
        !tckh3_found TYPE abap_bool
        !tckh1_found TYPE abap_bool
        !text_set    TYPE abap_bool
      CHANGING
        !fcat        TYPE slis_fieldcat_alv.

    CLASS-METHODS get_txele_from_itabs
      IMPORTING
        !kst_no      TYPE numc3
        !tckh1s      TYPE tckh1_set
        !tckh3s      TYPE tckh3_list
      EXPORTING
        !tckh3_found TYPE abap_bool
        !tckh1_found TYPE abap_bool
        !txele       TYPE ck_txele.

    METHODS constructor
      IMPORTING !input TYPE input_dict
      RAISING   zcx_bc_class_method.

    METHODS set_fcat_text
      IMPORTING !kst_no TYPE numc3
      CHANGING  !fcat   TYPE slis_fieldcat_alv.

    METHODS get_txele
      IMPORTING !kst_no      TYPE numc3
      EXPORTING !tckh3_found TYPE abap_bool
                !tckh1_found TYPE abap_bool
                !txele       TYPE ck_txele.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF table,
                 cost_variant TYPE tabname VALUE 'TCK07',
               END OF table.

    CONSTANTS: BEGIN OF method,
                 constructor TYPE seocpdname VALUE 'CONSTRUCTOR',
               END OF method.

    DATA input TYPE input_dict.

    METHODS read_component_texts
      RAISING
        zcx_bc_method_parameter
        zcx_bc_function_subrc
        zcx_bc_table_content.
ENDCLASS.



CLASS zcl_co_cost_component_text IMPLEMENTATION.
  METHOD set_fcat_text_from_itabs.
    CLEAR: tckh1_found,
           tckh3_found,
           text_set.

    get_txele_from_itabs(
      EXPORTING kst_no      = kst_no
                tckh1s      = tckh1s
                tckh3s      = tckh3s
      IMPORTING tckh3_found = tckh3_found
                tckh1_found = tckh1_found
                txele       = DATA(txele) ).

    IF tckh1_found = abap_false OR
       tckh3_found = abap_false.
      RETURN.
    ENDIF.

    fcat-seltext_l    =
    fcat-seltext_m    =
    fcat-seltext_s    =
    fcat-reptext_ddic = |{ prefix }{ txele }|.

    text_set = abap_true.
  ENDMETHOD.


  METHOD constructor.
    TRY.
        me->input = input.
        read_component_texts( ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = diaper
            class    = CONV #( cl_abap_classdescr=>get_class_name( me ) )
            method   = method-constructor.
    ENDTRY.
  ENDMETHOD.


  METHOD read_component_texts.
    " Hazırlık """"""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA last_day TYPE dats.
    zcl_co_ckm_periv=>set_first_period( CORRESPONDING #( me->input ) ).
    DATA(company) = zcl_co_ckm_bukrs=>get_instance( me->input-bukrs ).

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr        = me->input-bdatj
        i_periv        = company->go_periv->gv_periv
        i_poper        = me->input-poper
      IMPORTING
        e_date         = last_day
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        OTHERS         = 4 ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'LAST_DAY_IN_PERIOD_GET' ).

    " Varyant """""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE elehk FROM tck07 AS t1
           WHERE bukrs      = @me->input-bukrs AND
                 valid_from = ( SELECT MAX( valid_from ) FROM tck07 AS t2
                                WHERE bukrs      = t1~bukrs AND
                                      valid_from < @last_day )
           INTO @me->elehk ##WARN_OK.                  "#EC CI_BUFFSUBQ
                                                        "#EC CI_NOORDER

    IF me->elehk IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_table_content
        EXPORTING
          textid   = zcx_bc_table_content=>entry_missing
          objectid = CONV #( |{ company->gv_bukrs } { last_day }| )
          tabname  = me->table-cost_variant.
    ENDIF.

    " Metinler """"""""""""""""""""""""""""""""""""""""""""""""""""""
    " Metinler için ELEHK ile TCKH1’e git. Her KST’nin kendi metni var,
    " spras = sy-langu
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT elemt, txele FROM tckh1
           WHERE spras = @sy-langu AND
                 elehk = @me->elehk
           INTO CORRESPONDING FIELDS OF TABLE @me->tckh1s.

    " ELEMT listesi """""""""""""""""""""""""""""""""""""""""""""""""
    " ELEMT listesi için TCKH3'e gidiyoruz
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT el_hv, elemt FROM tckh3
           WHERE elehk = @me->elehk
           ORDER BY el_hv
           INTO CORRESPONDING FIELDS OF TABLE @me->tckh3s. "#EC CI_BYPASS
  ENDMETHOD.


  METHOD set_fcat_text.
    set_fcat_text_from_itabs(
      EXPORTING kst_no = kst_no
                tckh1s = me->tckh1s
                tckh3s = me->tckh3s
      CHANGING  fcat   = fcat ).
  ENDMETHOD.


  METHOD get_txele_from_itabs.
    CLEAR: tckh1_found,
           tckh3_found,
           txele.

    ASSIGN tckh3s[ el_hv = kst_no ] TO FIELD-SYMBOL(<tckh3>).
    CASE sy-subrc.
      WHEN 0     . tckh3_found = abap_true.
      WHEN OTHERS. RETURN.
    ENDCASE.

    ASSIGN tckh1s[ KEY primary_key COMPONENTS
                   elemt = <tckh3>-elemt
                 ] TO FIELD-SYMBOL(<tckh1>).
    CASE sy-subrc.
      WHEN 0     . tckh1_found = abap_true.
      WHEN OTHERS. RETURN.
    ENDCASE.

    txele = <tckh1>-txele.
  ENDMETHOD.


  METHOD get_txele.
    CLEAR: tckh1_found,
           tckh3_found,
           txele.

    get_txele_from_itabs(
      EXPORTING kst_no      = kst_no
                tckh1s      = me->tckh1s
                tckh3s      = me->tckh3s
      IMPORTING tckh3_found = tckh3_found
                tckh1_found = tckh1_found
                txele       = txele ).
  ENDMETHOD.
ENDCLASS.