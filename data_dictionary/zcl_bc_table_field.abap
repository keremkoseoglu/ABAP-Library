CLASS zcl_bc_table_field DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    DATA go_data_element TYPE REF TO zcl_bc_data_element READ-ONLY.
    DATA gs_def          TYPE dd03l READ-ONLY.
    DATA core            TYPE REF TO ycl_addict_table_field READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !iv_tab       TYPE tabname
                !iv_fld       TYPE fieldname
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_table_field
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_instance_by_fullname
      IMPORTING !iv_fullname  TYPE clike
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_table_field
      RAISING   zcx_bc_method_parameter
                zcx_bc_table_content.

    CLASS-METHODS get_instance_from_addict
      IMPORTING !core         TYPE REF TO ycl_addict_table_field
      RETURNING VALUE(output) TYPE REF TO zcl_bc_table_field
      RAISING   zcx_bc_table_content.

    METHODS get_text RETURNING VALUE(rv_text) TYPE ddtext.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_multiton,
        tab TYPE tabname,
        fld TYPE fieldname,
        obj TYPE REF TO zcl_bc_table_field,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS tab fld.

    CLASS-DATA gt_multiton TYPE tt_multiton.
ENDCLASS.



CLASS zcl_bc_table_field IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS
                        tab = iv_tab
                        fld = iv_fld
                      ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton(
          tab = iv_tab
          fld = iv_fld ).

      CREATE OBJECT ls_multiton-obj.

      TRY.
          ls_multiton-obj->core = ycl_addict_table_field=>get_instance(
              tab = iv_tab
              fld = iv_fld ).
        CATCH ycx_addict_table_content INTO DATA(tco).
          zcx_bc_table_content=>raise_from_addict( tco ).
      ENDTRY.

      ls_multiton-obj->gs_def = CORRESPONDING #( ls_multiton-obj->core->def ).
      ls_multiton-obj->go_data_element = zcl_bc_data_element=>get_instance_from_addict( ls_multiton-obj->core->data_element ).

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.


  METHOD get_instance_by_fullname.
    TRY.
        DATA(core_instance) = ycl_addict_table_field=>get_instance_by_fullname( iv_fullname ).
      CATCH ycx_addict_method_parameter INTO DATA(mp).
        zcx_bc_method_parameter=>raise_from_addict( mp ).
      CATCH ycx_addict_table_content INTO DATA(tc).
        zcx_bc_table_content=>raise_from_addict( tc ).
    ENDTRY.

    ro_obj = get_instance_from_addict( core_instance ).
  ENDMETHOD.


  METHOD get_instance_from_addict.
    output = get_instance(
        iv_tab = core->def-tabname
        iv_fld = core->def-fieldname ).

    output->core = core.
  ENDMETHOD.


  METHOD get_text.
    rv_text = me->core->get_text( ).
  ENDMETHOD.
ENDCLASS.