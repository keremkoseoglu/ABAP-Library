CLASS zcl_bc_user_tcode_access DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_period,
        month TYPE monat,
        year  TYPE gjahr,
      END OF t_period,

      BEGIN OF t_param,
        p_start TYPE t_period,
        s_tcode TYPE RANGE OF tcode,
      END OF t_param,

      tt_return TYPE STANDARD TABLE OF zbcs_user_tcode_access_list WITH DEFAULT KEY.

    constants:
      c_tabname_list type tabname value 'ZBCS_USER_TCODE_ACCESS_LIST'.

    METHODS:
      constructor IMPORTING !is_param TYPE t_param,
      execute RETURNING VALUE(rt_list) TYPE tt_return.

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      begin of t_Tcode,
        tcode type tcode,
        ttext type tstct-ttext,
      end of t_tcode,

      tt_tcode type hashed table of t_Tcode with unique key primary_key components tcode.

    CLASS-DATA:
      gs_period_today TYPE t_period.

    CONSTANTS:
      c_instance    TYPE swnchostname VALUE 'TOTAL',
      c_period_type TYPE swncperitype VALUE 'M'.

    DATA:
      gs_param TYPE t_param,
      gt_return TYPE tt_return,
      gt_tcode type tt_tcode,
      gv_sysid type SWNCSYSID.

    METHODS:
      period_exceeded_today
        IMPORTING
          !is_period       TYPE t_period
        RETURNING
          VALUE(rv_exceed) TYPE abap_bool,

      period_plus_plus CHANGING !cs_period TYPE t_period,

      read_master_data,
      read_user_tcode_access,
      read_user_tcode_access_per IMPORTING !is_period TYPE t_period.

ENDCLASS.



CLASS ZCL_BC_USER_TCODE_ACCESS IMPLEMENTATION.


  METHOD constructor.
    gs_param = is_param.

    gs_period_today = VALUE #(
        month = sy-datum+4(2)
        year  = sy-datum+0(4)
    ).

    gv_sysid = sy-sysid.

    select
        tstc~tcode tstct~ttext
      into corresponding fields of table gt_Tcode
      from
        tstc
        left join tstct on
          tstct~sprsl eq sy-langu and
          tstct~tcode eq tstc~tcode
      where tstc~tcode in gs_param-s_tcode.

  ENDMETHOD.


  METHOD execute.
    CLEAR gt_return.
    read_user_tcode_access( ).
    read_master_data( ).
    rt_list = gt_return.
  ENDMETHOD.


  METHOD period_exceeded_today.

    rv_exceed = xsdbool(
        is_period-year GT gs_period_today-year OR
        (
            is_period-year EQ gs_period_today-year AND
            is_period-month GT gs_period_today-month
        )
    ).

  ENDMETHOD.


  METHOD period_plus_plus.
    ADD 1 TO cs_period-month.
    CHECK cs_period-month GT 12.
    cs_period-month = 1.
    ADD 1 TO cs_period-year.
  ENDMETHOD.


  method read_master_data.

    check gt_return is not initial.

    loop at gt_return assigning field-symbol(<ls_return>).

      <ls_Return>-ttext = gt_tcode[
        key primary_key components
        tcode = <ls_Return>-tcode
      ]-ttext.

      <ls_return>-name_text = zcl_Bc_sap_user=>get_full_name_wo_error( <ls_return>-bname ).

    endloop.

  endmethod.


  METHOD read_user_tcode_access.

    DATA(ls_do_period) = gs_param-p_start.

    DO.

      IF period_exceeded_today( ls_do_period ) EQ abap_true.
        EXIT.
      ENDIF.

      read_user_tcode_access_per( ls_do_period ).

      period_plus_plus( CHANGING cs_period = ls_do_period ).

    ENDDO.

  ENDMETHOD.


  METHOD read_user_tcode_access_per.

    DATA:
      lt_user_tcode   TYPE swncgl_t_aggusertcode,
      lv_period_start TYPE swncdatum.

    lv_period_start =
        |{ is_period-year }| &&
        |{ is_period-month }| &&
        |01|.

    CALL FUNCTION 'SWNC_GET_WORKLOAD_STATISTIC'
      EXPORTING
        systemid           = gv_sysid
        instance           = c_instance
        periodtype         = c_period_type
        periodstrt         = lv_period_start
      IMPORTING
        usertcode          = lt_user_tcode
      EXCEPTIONS
        unknown_periodtype = 1
        no_data_found      = 2
        unknown_error      = 3
        OTHERS             = 4
        ##FM_SUBRC_OK.

    ASSERT sy-subrc NE 1.
    CHECK sy-subrc EQ 0.

    LOOP AT lt_user_tcode ASSIGNING FIELD-SYMBOL(<ls_ut>).

      check line_exists( gt_Tcode[ key primary_key components tcode = conv #( <ls_ut>-entry_id ) ] ).

      COLLECT VALUE zbcs_user_tcode_access_list(
          tcode  = <ls_ut>-entry_id
          bname  = <ls_ut>-account
          gjahr  = is_period-year
          dcount = <ls_ut>-dcount
      ) INTO gt_return.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.