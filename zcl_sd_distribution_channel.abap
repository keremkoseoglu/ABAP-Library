CLASS zcl_sd_distribution_channel DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA:
      gs_def TYPE tvtw READ-ONLY.

    CLASS-METHODS:

      get_instance
        IMPORTING !iv_vtweg     TYPE vtweg
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_distribution_channel
        RAISING   zcx_sd_dist_channel_def,

      get_vtext
        IMPORTING
          !iv_spras       TYPE spras DEFAULT sy-langu
          !iv_vtweg       TYPE vtweg
        RETURNING
          VALUE(rv_vtext) TYPE tvtwt-vtext .

    methods:
      is_internet_Dist returning value(rv_int) type abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_multiton,
        vtweg TYPE vtweg,
        obj   TYPE REF TO zcl_sd_distribution_channel,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS vtweg,

      tt_tvtw
        TYPE HASHED TABLE OF tvtw
        WITH UNIQUE KEY primary_key COMPONENTS vtweg,

      tt_tvtwt
        TYPE HASHED TABLE OF tvtwt
        WITH UNIQUE KEY primary_key COMPONENTS spras vtweg .

    CONSTANTS:
      c_vtweg_int   type vtweg   value '13'.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton,
      gt_tvtw     TYPE tt_tvtw,
      gt_tvtwt    TYPE tt_tvtwt.

    METHODS:
      constructor
        IMPORTING !iv_vtweg TYPE vtweg
        RAISING   zcx_sd_dist_channel_def.
ENDCLASS.



CLASS ZCL_SD_DISTRIBUTION_CHANNEL IMPLEMENTATION.


  METHOD constructor.

    IF gt_tvtw IS INITIAL.
      SELECT * INTO TABLE gt_tvtw FROM tvtw.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE gt_tvtw.
      ENDIF.
    ENDIF.

    TRY.

        gs_def = gt_tvtw[
          KEY primary_key COMPONENTS
          vtweg = iv_vtweg
        ].

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_silnf).

        RAISE EXCEPTION TYPE zcx_sd_dist_channel_def
          EXPORTING
            vtweg    = iv_vtweg
            previous = lo_silnf.

    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_multiton[
        KEY primary_key COMPONENTS
        vtweg = iv_vtweg
      ] TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc NE 0.

      INSERT
        VALUE #(
          vtweg = iv_vtweg
          obj   = NEW #( iv_vtweg )
        )
        INTO TABLE gt_multiton
        ASSIGNING <ls_mt>.

    ENDIF.

    ro_obj = <ls_mt>-obj.

  ENDMETHOD.


  METHOD get_vtext.

    IF gt_tvtwt[] IS INITIAL.
      SELECT * INTO TABLE gt_tvtwt FROM tvtwt.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE gt_tvtwt.
      ENDIF.
    ENDIF.

    ASSIGN gt_tvtwt[ KEY primary_key COMPONENTS
      spras = iv_spras
      vtweg = iv_vtweg
    ] TO FIELD-SYMBOL(<ls_tvtwt>).

    CHECK sy-subrc EQ 0.

    rv_vtext = <ls_tvtwt>-vtext.

  ENDMETHOD.


  method is_internet_dist.
    rv_int = xsdbool( gs_def-vtweg eq c_vtweg_int ).
  endmethod.
ENDCLASS.