CLASS zcl_sd_storage_location DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    DATA gs_head TYPE t001l.

    CLASS-METHODS get_instance
      IMPORTING iv_werks      TYPE werks_d
                iv_lgort      TYPE lgort_d
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_sd_storage_location
      RAISING   zcx_sd_stloc_def.

    CLASS-METHODS get_lgobe_safe
      IMPORTING lgort         TYPE lgort_d
                werks         TYPE werks_d OPTIONAL
      RETURNING VALUE(result) TYPE t001l-lgobe.

    CLASS-METHODS ensure_storage_location_exists
      IMPORTING iv_lgort TYPE lgort_d
      RAISING   zcx_sd_stloc_def.

    METHODS get_address_no RETURNING VALUE(result) TYPE twlad-adrnr.

  PRIVATE SECTION.
    TYPES: BEGIN OF lgobe_cache_dict,
             werks TYPE t001l-werks,
             lgort TYPE t001l-lgort,
             lgobe TYPE t001l-lgobe,
           END OF lgobe_cache_dict,

           lgobe_cache_set TYPE HASHED TABLE OF lgobe_cache_dict
                           WITH UNIQUE KEY primary_key COMPONENTS werks lgort.

    TYPES:
      BEGIN OF t_multiton,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        cx    TYPE REF TO zcx_sd_stloc_def,
        obj   TYPE REF TO zcl_sd_storage_location,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS werks lgort.

    CLASS-DATA: gt_multiton TYPE tt_multiton,
                lgobe_cache TYPE lgobe_cache_set.

    DATA: adrnr      TYPE twlad-adrnr,
          adrnr_read TYPE abap_bool.

    METHODS constructor
      IMPORTING iv_werks TYPE werks_d
                iv_lgort TYPE lgort_d
      RAISING   zcx_sd_stloc_def.

ENDCLASS.


CLASS zcl_sd_storage_location IMPLEMENTATION.
  METHOD constructor.
    SELECT SINGLE * FROM t001l
           WHERE werks = @iv_werks AND
                 lgort = @iv_lgort
           INTO @gs_head.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_sd_stloc_def( werks = iv_werks
                                            lgort = iv_lgort ).
    ENDIF.
  ENDMETHOD.

  METHOD get_lgobe_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Hata üretmeden, depo yeri metnini döndürmeye çalışır
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cache) = REF #( zcl_sd_storage_location=>lgobe_cache ).

    ASSIGN cache->*[ KEY primary_key COMPONENTS werks = werks
                                                lgort = lgort ]
           TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE lgobe_cache_dict( werks = werks
                                                lgort = lgort ).

      IF new_cache-werks IS NOT INITIAL.
        TRY.
            DATA(stge_loc) = get_instance( iv_werks = new_cache-werks
                                           iv_lgort = new_cache-lgort ).

            new_cache-lgobe = stge_loc->gs_head-lgobe.
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      IF new_cache-lgobe IS INITIAL.
        SELECT SINGLE lgobe FROM t001l                  "#EC CI_GENBUFF
               WHERE lgort =  @new_cache-lgort AND      "#EC CI_NOORDER
                     lgobe <> @space
               INTO @new_cache-lgobe.
      ENDIF.

      INSERT new_cache INTO TABLE cache->* ASSIGNING <cache>.
    ENDIF.

    result = <cache>-lgobe.
  ENDMETHOD.

  METHOD ensure_storage_location_exists.
    SELECT SINGLE FROM t001l
           FIELDS @abap_true
           WHERE  lgort = @iv_lgort
           INTO   @DATA(lv_exists).

    CHECK lv_exists = abap_false.

    RAISE EXCEPTION NEW zcx_sd_stloc_def( textid = zcx_sd_stloc_def=>unknown_storage_location
                                          lgort  = iv_lgort ).
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS werks = iv_werks
                                                   lgort = iv_lgort ]
           TO FIELD-SYMBOL(<ls_mt>).

    IF sy-subrc <> 0.
      DATA(ls_mt) = VALUE t_multiton( werks = iv_werks
                                      lgort = iv_lgort ).

      TRY.
          ls_mt-obj = NEW #( iv_werks = ls_mt-werks
                             iv_lgort = ls_mt-lgort ).

        CATCH zcx_sd_stloc_def INTO ls_mt-cx ##NO_HANDLER.
        CATCH cx_root INTO DATA(lo_diaper).
          RAISE EXCEPTION NEW zcx_sd_stloc_def( textid   = zcx_sd_stloc_def=>def_error
                                                previous = lo_diaper
                                                werks    = ls_mt-werks
                                                lgort    = ls_mt-lgort ).
      ENDTRY.

      INSERT ls_mt INTO TABLE gt_multiton ASSIGNING <ls_mt>.
    ENDIF.

    IF <ls_mt>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_mt>-cx.
    ENDIF.

    ro_obj = <ls_mt>-obj.
  ENDMETHOD.

  METHOD get_address_no.
    IF me->adrnr_read = abap_false.
      SELECT adrnr UP TO 1 ROWS INTO @DATA(adrnr)
             FROM twlad
             WHERE werks = @gs_head-werks AND
                   lgort = @gs_head-lgort
             ORDER BY PRIMARY KEY.
      ENDSELECT.

      me->adrnr_read = abap_true.
    ENDIF.

    result = me->adrnr.
  ENDMETHOD.
ENDCLASS.