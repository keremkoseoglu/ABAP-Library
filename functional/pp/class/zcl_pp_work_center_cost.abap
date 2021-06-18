CLASS zcl_pp_work_center_cost DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_pp_work_center.

  PUBLIC SECTION.

    METHODS:
      get_activity_prices
        IMPORTING
          !iv_gjahr        TYPE gjahr DEFAULT sy-datum+0(4)
          !iv_monat        TYPE monat DEFAULT sy-datum+4(2)
        RETURNING
          VALUE(rt_prices) TYPE zmmtt_maf_cost_item
        RAISING
          zcx_bc_int_data_read.

  PROTECTED SECTION.

    METHODS:
      constructor IMPORTING !io_work_center TYPE REF TO zcl_pp_work_center.

  PRIVATE SECTION.

    TYPES: BEGIN OF t_activity_price,
             gjahr  TYPE gjahr,
             monat  TYPE monat,
             prices TYPE zmmtt_maf_cost_item,
           END OF t_activity_price,

           tt_activity_prices TYPE HASHED TABLE OF t_activity_price
                              WITH UNIQUE KEY primary_key COMPONENTS gjahr monat,

           tt_kostl           TYPE STANDARD TABLE OF kostl WITH DEFAULT KEY,

           BEGIN OF t_kostl_of_period,
             gjahr TYPE gjahr,
             monat TYPE monat,
             kostl TYPE tt_kostl,
           END OF t_kostl_of_period,

           tt_kostl_of_period TYPE HASHED TABLE OF t_kostl_of_period
                              WITH UNIQUE KEY primary_key COMPONENTS gjahr monat.

    DATA: go_work_center     TYPE REF TO zcl_pp_work_center,
          gt_activity_prices TYPE tt_activity_prices,
          gt_kostl_of_period TYPE tt_kostl_of_period.

    METHODS:
      get_kostl_list
        IMPORTING
          !iv_gjahr       TYPE gjahr
          !iv_monat       TYPE monat
        RETURNING
          VALUE(rt_kostl) TYPE tt_kostl.

ENDCLASS.



CLASS zcl_pp_work_center_cost IMPLEMENTATION.

  METHOD get_activity_prices.
    TRY.

        ASSIGN gt_activity_prices[ KEY primary_key COMPONENTS
                                   gjahr = iv_gjahr
                                   monat = iv_monat
                                 ] TO FIELD-SYMBOL(<ls_activity_price>).

        IF sy-subrc NE 0.
          DATA(ls_activity_price) = VALUE t_activity_price( gjahr = iv_gjahr
                                                            monat = iv_monat ).

          DATA(lt_kostl_of_period) = get_kostl_list( iv_gjahr = ls_activity_price-gjahr
                                                     iv_monat = ls_activity_price-monat ).

          LOOP AT lt_kostl_of_period ASSIGNING FIELD-SYMBOL(<lv_kostl>).
            APPEND LINES OF zcl_mm_maf_cost=>get_instance(
                              VALUE #( gjahr = iv_gjahr
                                       monat = iv_monat
                                       kostl = <lv_kostl>
                                       werks = go_work_center->gs_header-werks )
                            )->gt_cost TO ls_activity_price-prices.
          ENDLOOP.

          SORT ls_activity_price-prices BY lstar.
          DELETE ADJACENT DUPLICATES FROM ls_activity_price-prices COMPARING lstar. " Paranoya
          INSERT ls_activity_price INTO TABLE gt_activity_prices ASSIGNING <ls_activity_price>.
        ENDIF.

        rt_prices = <ls_activity_price>-prices.

      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_int_data_read
          EXPORTING
            textid    = zcx_bc_int_data_read=>cant_read_data_type
            previous  = lo_diaper
            objectid  = |{ go_work_center->gs_header-arbpl } { go_work_center->gs_header-werks } { iv_gjahr } { iv_monat }|
            data_type = CONV #( TEXT-602 ).
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    go_work_center = io_work_center.
  ENDMETHOD.


  METHOD get_kostl_list.

    ASSIGN gt_kostl_of_period[ KEY primary_key COMPONENTS
                               gjahr = iv_gjahr
                               monat = iv_monat
                             ] TO FIELD-SYMBOL(<ls_kostl_of_period>).

    IF sy-subrc NE 0.
      DATA(ls_kostl_of_period) = VALUE t_kostl_of_period( gjahr = iv_gjahr
                                                          monat = iv_monat ).

      DATA(lv_key_date) = COND datum( WHEN iv_gjahr EQ sy-datum+0(4) AND
                                           iv_monat EQ sy-datum+4(2)
                                      THEN sy-datum
                                      ELSE |{ iv_gjahr }{ iv_monat }01| ).

      SELECT DISTINCT kostl FROM crco
             WHERE objty EQ @go_work_center->gs_header-objty AND
                   objid EQ @go_work_center->gs_header-objid AND
                   endda GE @lv_key_date AND
                   begda LE @lv_key_date
             INTO TABLE @ls_kostl_of_period-kostl.

      INSERT ls_kostl_of_period INTO TABLE gt_kostl_of_period ASSIGNING <ls_kostl_of_period>.
    ENDIF.

    rt_kostl = ls_kostl_of_period-kostl.
  ENDMETHOD.

ENDCLASS.