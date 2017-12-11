CLASS zcl_sd_transport_zone DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_head TYPE tzone.

    CLASS-METHODS:
      get_instance
        IMPORTING
          !iv_land1     TYPE land1
          !iv_zone1     TYPE lzone
        RETURNING
          VALUE(ro_obj) TYPE REF TO zcl_sd_transport_zone
        RAISING
          zcx_sd_tzone.

    METHODS:
      get_text RETURNING VALUE(rv_vtext) TYPE tzont-vtext.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flg,
        vtext TYPE abap_bool,
      END OF t_lazy_flg,

      BEGIN OF t_lazy_val,
        vtext TYPE tzont-vtext,
      END OF t_lazy_val,

      BEGIN OF t_lazy,
        flg TYPE t_lazy_flg,
        val TYPE t_lazy_val,
      END OF t_lazy,

      BEGIN OF t_multiton,
        land1 TYPE land1,
        zone1 TYPE lzone,
        cx    TYPE REF TO zcx_sd_tzone,
        obj   TYPE REF TO zcl_sd_transport_zone,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS land1 zone1.

    DATA:
      gs_lazy TYPE t_lazy.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    METHODS:
      constructor
        IMPORTING
          !iv_land1 TYPE land1
          !iv_zone1 TYPE lzone
        RAISING
          zcx_sd_tzone.

ENDCLASS.



CLASS zcl_sd_transport_zone IMPLEMENTATION.

  METHOD constructor.

    SELECT SINGLE *
      FROM tzone
      WHERE
        land1 EQ @iv_land1 AND
        zone1 EQ @iv_zone1
      INTO CORRESPONDING FIELDS OF @gs_head.

    CHECK sy-subrc NE 0.

    RAISE EXCEPTION TYPE zcx_sd_tzone
      EXPORTING
        land1 = iv_land1
        zone1 = iv_zone1.

  ENDMETHOD.

  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        land1 = iv_land1
        zone1 = iv_zone1
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      DATA(ls_mt) = VALUE t_multiton(
        land1 = iv_land1
        zone1 = iv_zone1
      ).

      TRY.
          ls_mt-obj = NEW #(
            iv_land1 = ls_mt-land1
            iv_zone1 = ls_mt-zone1
          ).

        CATCH zcx_sd_tzone INTO ls_mt-cx ##no_handler.
        CATCH cx_root INTO DATA(lo_diaper).

          RAISE EXCEPTION TYPE zcx_sd_tzone
            EXPORTING
              textid   = zcx_sd_tzone=>def_error
              previous = lo_diaper
              land1    = ls_mt-land1
              zone1    = ls_mt-zone1.

      ENDTRY.

      INSERT ls_mt
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.

  METHOD get_text.

    IF gs_lazy-flg-vtext IS INITIAL.

      SELECT SINGLE vtext
        FROM tzont
        WHERE
          spras EQ @sy-langu AND
          land1 EQ @gs_head-land1 AND
          zone1 EQ @gs_head-zone1
        INTO @gs_lazy-val-vtext.

      gs_lazy-flg-vtext = abap_true.

    ENDIF.

    rv_vtext = gs_lazy-val-vtext.

  ENDMETHOD.

ENDCLASS.