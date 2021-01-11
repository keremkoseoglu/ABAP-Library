CLASS zcl_bc_abap_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_package_rng TYPE RANGE OF tdevc-devclass,

      BEGIN OF t_tadir_key,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF t_tadir_key,

      tt_tadir_key TYPE STANDARD TABLE OF t_tadir_key WITH DEFAULT KEY,

      BEGIN OF t_pack_cache,
        key      TYPE t_tadir_key,
        devclass TYPE tadir-devclass,
      END OF t_pack_cache,

      tt_pack_cache TYPE HASHED TABLE OF t_pack_cache
        WITH UNIQUE KEY primary_key
        COMPONENTS key.

    CLASS-METHODS:
      get_package_of_obj
        IMPORTING
          !is_key            TYPE t_tadir_key
        RETURNING
          VALUE(rv_devclass) TYPE tadir-devclass,

      get_package_of_objects
        IMPORTING
          !it_key            TYPE tt_tadir_key
        RETURNING
          VALUE(rt_devclass) TYPE tt_pack_cache,

      get_nonsap_package_rng
        RETURNING
          VALUE(rt_rng) TYPE tt_package_rng,

      is_obj_custom_development
        IMPORTING
          !is_key          TYPE t_tadir_key
        RETURNING
          VALUE(rv_nonsap) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_abap_package IMPLEMENTATION.
  METHOD get_nonsap_package_rng.
    rt_rng = ycl_addict_package=>get_nonsap_package_rng( ).
  ENDMETHOD.


  METHOD get_package_of_obj.
    rv_devclass = ycl_addict_package=>get_package_of_obj( CORRESPONDING #( is_key ) ).
  ENDMETHOD.


  METHOD get_package_of_objects.
    rt_devclass = ycl_addict_package=>get_package_of_objects( CORRESPONDING #( it_key ) ).
  ENDMETHOD.


  METHOD is_obj_custom_development.
    rv_nonsap = ycl_addict_package=>is_obj_custom_development( CORRESPONDING #( is_key ) ).
  ENDMETHOD.
ENDCLASS.