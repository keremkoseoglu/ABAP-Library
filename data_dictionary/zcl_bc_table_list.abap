CLASS zcl_bc_table_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_contflag_rng TYPE RANGE OF zbcs_table_list-contflag,
      tt_devclass_rng TYPE RANGE OF zbcs_table_list-devclass,
      tt_tabclass_rng TYPE RANGE OF zbcs_table_list-tabclass,
      tt_tabname_rng  TYPE RANGE OF zbcs_table_list-tabname,
      tt_table_list   TYPE STANDARD TABLE OF zbcs_table_list WITH DEFAULT KEY,

      BEGIN OF t_param,
        contflag_rng TYPE tt_contflag_rng,
        devclass_rng TYPE tt_devclass_rng,
        tabclass_rng TYPE tt_tabclass_rng,
        tabname_rng  TYPE tt_tabname_rng,
      END OF t_param.

    constants:
      c_tabname_list type tabname value 'ZBCS_TABLE_LIST'.

    METHODS:
      constructor IMPORTING !is_param TYPE t_param,
      get_result RETURNING VALUE(rt_list) TYPE tt_table_list.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      tt_obj_name
        TYPE HASHED TABLE OF tadir-obj_name
        WITH UNIQUE KEY primary_key COMPONENTS table_line.

    CONSTANTS:
      c_object_fugr  TYPE tadir-object VALUE 'FUGR',
      c_object_table TYPE tadir-object VALUE 'TABL',
      c_object_tcode TYPE tadir-object VALUE 'TRAN',
      c_pgmid_r3tr   TYPE tadir-pgmid  VALUE 'R3TR'.

    DATA:
      gs_param TYPE t_param,
      gt_list  TYPE tt_table_list.

    METHODS:
      build_table_list.

ENDCLASS.


CLASS zcl_bc_table_list IMPLEMENTATION.

  METHOD build_table_list.

    DATA:
      lt_fugrp TYPE tt_obj_name,
      lt_tcode TYPE tt_obj_name.

    SELECT DISTINCT obj_name
      FROM tadir
      WHERE
        pgmid    EQ @c_pgmid_r3tr AND
        object   EQ @c_object_fugr AND
        obj_name IN @gs_param-tabname_rng
      INTO TABLE @lt_fugrp.

    SELECT DISTINCT obj_name
      FROM tadir
      WHERE
        pgmid    EQ @c_pgmid_r3tr AND
        object   EQ @c_object_tcode AND
        obj_name IN @gs_param-tabname_rng
      INTO TABLE @lt_tcode.

    SELECT
        dd02l~contflag, tadir~devclass, dd02l~tabclass, dd02l~tabname,
        dd02t~ddtext
      FROM
        dd02l
        INNER JOIN tadir ON
          tadir~pgmid EQ @c_pgmid_r3tr AND
          tadir~object EQ @c_object_table AND
          tadir~obj_name EQ dd02l~tabname
        LEFT JOIN dd02t ON
          dd02t~tabname EQ dd02l~tabname AND
          dd02t~ddlanguage EQ @sy-langu
      WHERE
        dd02l~tabname  IN @gs_param-tabname_rng AND
        dd02l~contflag IN @gs_param-contflag_rng AND
        dd02l~tabclass IN @gs_param-tabclass_rng AND
        tadir~devclass IN @gs_param-devclass_rng
      INTO CORRESPONDING FIELDS OF TABLE @gt_list
      ##too_many_itab_fields.

    LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<ls_list>).

      <ls_list>-has_fugrp = xsdbool(
        line_exists(
          lt_fugrp[
            KEY primary_key COMPONENTS
            table_line = <ls_list>-tabname
          ]
        )
      ).

      <ls_list>-has_tcode = xsdbool(
        line_exists(
          lt_tcode[
            KEY primary_key COMPONENTS
            table_line = <ls_list>-tabname
          ]
        )
      ).

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    gs_param = is_param.
    build_table_list( ).
  ENDMETHOD.

  METHOD get_result.
    rt_list = gt_list.
  ENDMETHOD.

ENDCLASS.