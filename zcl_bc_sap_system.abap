CLASS zcl_bc_sap_system DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      c_dest_dev   TYPE rfcdest VALUE 'TDECLNT100',
      c_dest_live  TYPE rfcdest VALUE 'TPECLNT100',
      c_dest_qa    TYPE rfcdest VALUE 'TQECLNT100',

      c_sysid_dev  TYPE sysysid VALUE 'TDE',
      c_sysid_live TYPE sysysid VALUE 'TPE',
      c_sysid_qa   TYPE sysysid VALUE 'TQE'.

    CLASS-METHODS:

      convert_dest_to_sysid
        IMPORTING !iv_dest        TYPE tmscsys-sysnam
        RETURNING VALUE(rv_sysid) TYPE sysysid,

      convert_sysid_to_dest
        IMPORTING !iv_sysid      TYPE sysysid
        RETURNING VALUE(rv_dest) TYPE tmscsys-sysnam,

      is_current_system_live RETURNING VALUE(rv_live) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_sap_system IMPLEMENTATION.

  METHOD convert_dest_to_sysid.

    rv_sysid = SWITCH #( iv_dest
        WHEN c_dest_dev  THEN c_sysid_dev
        WHEN c_dest_qa   THEN c_sysid_qa
        WHEN c_dest_live THEN c_sysid_live
    ).

    ASSERT rv_sysid IS NOT INITIAL.

  ENDMETHOD.

  METHOD convert_sysid_to_dest.

    rv_dest = SWITCH #( iv_sysid
        WHEN c_sysid_dev  THEN c_dest_dev
        WHEN c_sysid_qa   THEN c_dest_qa
        WHEN c_sysid_live THEN c_dest_live
    ).

    ASSERT rv_dest IS NOT INITIAL.

  ENDMETHOD.

  METHOD is_current_system_live.
    rv_live = xsdbool( sy-sysid EQ c_sysid_live ).
  ENDMETHOD.

ENDCLASS.