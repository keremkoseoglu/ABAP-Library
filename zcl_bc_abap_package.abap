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

    TYPES:
      BEGIN OF t_custom_dev_cache,
        key    TYPE t_tadir_key,
        custom TYPE abap_bool,
      END OF t_custom_dev_cache,

      tt_custom_dev_cache TYPE HASHED TABLE OF t_custom_dev_cache
        WITH UNIQUE KEY primary_key
        COMPONENTS key.

    CLASS-DATA:
      gt_custom_dev_cache TYPE tt_custom_dev_cache,
      gt_nonsap_pack_rng  TYPE tt_package_rng,
      gt_pack_cache       TYPE tt_pack_cache.

ENDCLASS.



CLASS ZCL_BC_ABAP_PACKAGE IMPLEMENTATION.


  METHOD get_nonsap_package_rng.

    IF gt_nonsap_pack_rng IS INITIAL.

      SELECT
          @zcl_bc_ddic_toolkit=>c_option_eq AS option,
          @zcl_bc_ddic_toolkit=>c_sign_i AS sign,
          devclass AS low
        INTO CORRESPONDING FIELDS OF TABLE @gt_nonsap_pack_rng
        FROM tdevc
        WHERE pdevclass LIKE 'Z%'
        ##too_many_itab_fields.

    ENDIF.


    rt_rng = gt_nonsap_pack_rng.

  ENDMETHOD.


  METHOD get_package_of_obj.

    DATA(lt_ret) = get_package_of_objects( VALUE #( ( is_key ) ) ).

    TRY.
        rv_devclass = lt_ret[
            KEY primary_key COMPONENTS
            key = is_key
        ]-devclass.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_package_of_objects.

    DATA:
      lt_new_key TYPE tt_tadir_key.

    " ______________________________
    " Hazırlık

    CHECK it_key IS NOT INITIAL.

    DATA(lt_key) = it_key.
    SORT lt_key BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_key COMPARING pgmid object obj_name.

    " ______________________________
    " Daha önce zaten okumuş olduklarımızı ekle

    LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<ls_key>).

      TRY.
          INSERT gt_pack_cache[
              KEY primary_key COMPONENTS
              key = <ls_key>
          ] INTO TABLE rt_devclass.
        CATCH cx_sy_itab_line_not_found.
          APPEND <ls_key> TO lt_new_key.
      ENDTRY.

    ENDLOOP.

    " ______________________________
    " Yenileri oku

    CHECK lt_new_key IS NOT INITIAL.

    SELECT pgmid, object, obj_name, devclass
      INTO TABLE @DATA(lt_new)
      FROM tadir
      FOR ALL ENTRIES IN @lt_new_key
      WHERE (
        pgmid EQ @lt_new_key-pgmid AND
        object EQ @lt_new_key-object AND
        obj_name EQ @lt_new_key-obj_name
      ).

    LOOP AT lt_new ASSIGNING FIELD-SYMBOL(<ls_new>).

      INSERT VALUE #(
        key = VALUE #(
            pgmid = <ls_new>-pgmid
            object = <ls_new>-object
            obj_name = <ls_new>-obj_name
        )
        devclass = <ls_new>-devclass
      ) INTO TABLE:
        gt_pack_cache,
        rt_devclass.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_obj_custom_development.

    ASSIGN gt_custom_dev_cache[
        KEY primary_key
        COMPONENTS key = is_key
    ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      INSERT VALUE #(
          key = is_key
          custom = xsdbool( get_package_of_obj( is_key ) IN get_nonsap_package_rng( ) )
      ) INTO TABLE gt_custom_dev_cache ASSIGNING <ls_cache>.

    ENDIF.

    rv_nonsap = <ls_cache>-custom.

  ENDMETHOD.
ENDCLASS.