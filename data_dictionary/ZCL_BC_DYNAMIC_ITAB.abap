CLASS zcl_bc_dynamic_itab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPE-POOLS slis.

    TYPES: BEGIN OF t_fld,
             fnam TYPE fieldname,
             dtel TYPE rollname,
           END OF t_fld,

           tt_fld      TYPE STANDARD TABLE OF t_fld WITH DEFAULT KEY,
           tt_fnam_rng TYPE RANGE OF fieldname.

    DATA gt_fld TYPE tt_fld READ-ONLY.

    CLASS-METHODS get_instance_from_addict
      IMPORTING !core         TYPE REF TO ycl_addict_dynamic_itab
      RETURNING VALUE(output) TYPE REF TO zcl_bc_dynamic_itab
      RAISING   zcx_bc_method_parameter.

    CLASS-METHODS create_range
      IMPORTING
        !im_v_field TYPE fieldname
      EXPORTING
        !ex_r_wa    TYPE REF TO data
        !ex_r_tab   TYPE REF TO data .

    CLASS-METHODS get_instance_as_range
      IMPORTING
        !iv_rollname  TYPE rollname
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_dynamic_itab
      RAISING
        zcx_bc_method_parameter
        zcx_bc_table_content.

    CLASS-METHODS get_instance_with_tabname
      IMPORTING
        !iv_tabname   TYPE tabname
        !it_fnam_rng  TYPE tt_fnam_rng OPTIONAL
        !it_extra_fld TYPE tt_fld OPTIONAL
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_dynamic_itab
      RAISING
        zcx_bc_method_parameter.

    METHODS constructor
      IMPORTING
        !it_fld TYPE tt_fld
      RAISING
        zcx_bc_method_parameter.

    METHODS get_alv_fcat
      RETURNING
        VALUE(rt_fcat) TYPE slis_t_fieldcat_alv.

    METHODS get_lvc_fcat
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat.

    METHODS get_itab_ref
      RETURNING
        VALUE(rr_ref) TYPE REF TO data.

    METHODS get_wa_ref
      RETURNING
        VALUE(rr_ref) TYPE REF TO data.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA dont_run_constructor TYPE abap_bool.
    DATA core TYPE REF TO ycl_addict_dynamic_itab.
ENDCLASS.



CLASS zcl_bc_dynamic_itab IMPLEMENTATION.
  METHOD get_instance_from_addict.
    dont_run_constructor = abap_true.
    TRY.
        output = NEW #( CORRESPONDING #( core->fields ) ).
      CLEANUP.
        dont_run_constructor = abap_false.
    ENDTRY.
    dont_run_constructor = abap_false.

    output->core = core.
    output->gt_fld = CORRESPONDING #( core->fields ).
  ENDMETHOD.


  METHOD constructor.
    CHECK dont_run_constructor = abap_false.

    TRY.
        me->core = NEW #( CORRESPONDING #( it_fld ) ).
        me->gt_fld = CORRESPONDING #( me->core->fields ).
      CATCH ycx_addict_method_parameter INTO DATA(mp_error).
        zcx_bc_method_parameter=>raise_from_addict( mp_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_range.
    ycl_addict_dynamic_itab=>create_range(
      EXPORTING field = im_v_field
      IMPORTING wa    = DATA(wa)
                tab   = DATA(tab) ).

    ex_r_wa  = wa.
    ex_r_tab = tab.
  ENDMETHOD.


  METHOD get_alv_fcat.
    rt_fcat = me->core->get_alv_fcat( ).
  ENDMETHOD.


  METHOD get_lvc_fcat.
    rt_fcat = me->core->get_lvc_fcat( ).
  ENDMETHOD.


  METHOD get_instance_as_range.
    TRY.
        DATA(addict) = ycl_addict_dynamic_itab=>get_instance_as_range( iv_rollname ).
        ro_obj = get_instance_from_addict( addict ).

      CATCH ycx_addict_method_parameter INTO DATA(mp_error).
        zcx_bc_method_parameter=>raise_from_addict( mp_error ).
      CATCH ycx_addict_table_content INTO DATA(tc_error).
        zcx_bc_table_content=>raise_from_addict( tc_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_instance_with_tabname.
    TRY.
        DATA(addict) = ycl_addict_dynamic_itab=>get_instance_with_tabname(
            tabname   = iv_tabname
            fnam_rng  = CORRESPONDING #( it_fnam_rng )
            extra_fld = CORRESPONDING #( it_extra_fld ) ).

        ro_obj = get_instance_from_addict( addict ).

      CATCH ycx_addict_method_parameter INTO DATA(mp_error).
        zcx_bc_method_parameter=>raise_from_addict( mp_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_itab_ref.
    rr_ref = me->core->get_itab_ref( ).
  ENDMETHOD.


  METHOD get_wa_ref.
    rr_ref = me->core->get_wa_ref( ).
  ENDMETHOD.
ENDCLASS.