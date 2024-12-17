CLASS zcl_bc_abap_domain DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_line,
        value TYPE val_single,
        text  TYPE val_text,
      END OF t_line,

      tt_line TYPE HASHED TABLE OF t_line
        WITH UNIQUE KEY primary_key COMPONENTS value.

    DATA gs_def TYPE dd01l                    READ-ONLY.
    DATA core   TYPE REF TO ycl_addict_domain READ-ONLY.

    CLASS-METHODS:
      get_instance
        IMPORTING iv_domname       TYPE domname
        RETURNING VALUE(ro_domain) TYPE REF TO zcl_bc_abap_domain
        RAISING   zcx_bc_table_content,

      get_value_text_safe
        IMPORTING iv_domname     TYPE domname
                  iv_value       TYPE val_single
                  iv_langu       TYPE sylangu DEFAULT sy-langu
        RETURNING VALUE(rv_text) TYPE val_text.

    METHODS:
      get_text RETURNING VALUE(rv_text) TYPE ddtext,

      get_value_line
        IMPORTING iv_value       TYPE val_single
        RETURNING VALUE(rs_line) TYPE t_line
        RAISING   zcx_bc_domain,

      get_value_tab RETURNING VALUE(rt_tab) TYPE tt_line,

      get_value_text
        IMPORTING iv_value       TYPE val_single
        RETURNING VALUE(rv_text) TYPE val_text
        RAISING   zcx_bc_domain,

      validate_value
        IMPORTING iv_value TYPE val_single
        RAISING   zcx_bc_domain
                  zcx_bc_table_content.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_multiton,
             domname TYPE domname,
             obj     TYPE REF TO zcl_bc_abap_domain,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton
                      WITH UNIQUE KEY primary_key COMPONENTS domname.

    CLASS-DATA gt_multiton TYPE tt_multiton.
ENDCLASS.


CLASS zcl_bc_abap_domain IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key
                        COMPONENTS domname = iv_domname ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( domname = iv_domname ).

      ls_multiton-obj = NEW #( ).

      TRY.
          ls_multiton-obj->core = ycl_addict_domain=>get_instance( iv_domname ).
        CATCH ycx_addict_table_content INTO DATA(tc).
          zcx_bc_table_content=>raise_from_addict( tc ).
      ENDTRY.

      ls_multiton-obj->gs_def = CORRESPONDING #( ls_multiton-obj->core->def ).

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    ro_domain = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD get_text.
    rv_text = me->core->get_text( ).
  ENDMETHOD.

  METHOD validate_value.
    TRY.
        me->core->validate_value( iv_value ).
      CATCH ycx_addict_domain INTO DATA(domain_error).
        zcx_bc_domain=>raise_from_addict( domain_error ).
      CATCH ycx_addict_table_content INTO DATA(tc).
        zcx_bc_table_content=>raise_from_addict( tc ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_value_text_safe.
    CASE iv_langu.
      WHEN sy-langu.
        rv_text = ycl_addict_domain=>get_value_text_safe( domname = iv_domname
                                                          value   = iv_value ).

      WHEN OTHERS.
        SELECT SINGLE
               FROM dd07t
                    INNER JOIN dd07l
                      ON  dd07l~domname  = dd07t~domname
                      AND dd07l~as4local = dd07t~as4local
                      AND dd07l~valpos   = dd07t~valpos
                      AND dd07l~as4vers  = dd07t~as4vers
               FIELDS dd07t~ddtext
               WHERE dd07t~domname    = @iv_domname
                 AND dd07t~ddlanguage = @iv_langu
                 AND dd07l~domvalue_l = @iv_value
               INTO @rv_text.
    ENDCASE.
  ENDMETHOD.

  METHOD get_value_line.
    TRY.
        rs_line = me->core->get_value_line( iv_value ).
      CATCH ycx_addict_domain INTO DATA(domain_error).
        zcx_bc_domain=>raise_from_addict( domain_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_value_tab.
    rt_tab = me->core->get_value_tab( ).
  ENDMETHOD.

  METHOD get_value_text.
    TRY.
        rv_text = me->core->get_value_text( iv_value ).
      CATCH ycx_addict_domain INTO DATA(domain_error).
        zcx_bc_domain=>raise_from_addict( domain_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.