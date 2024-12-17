CLASS zcl_bc_tcode_list DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: tt_devclass_rng TYPE RANGE OF zbcs_tcode_list-devclass,
           tt_gekod_rng    TYPE RANGE OF zbcs_tcode_list-gekod,
           tt_tpclass_rng  TYPE RANGE OF zbcs_tcode_list-tpclass,
           tt_tcode_rng    TYPE RANGE OF zbcs_tcode_list-tcode,

           BEGIN OF t_param,
             devclass_rng TYPE tt_devclass_rng,
             gekod_rng    TYPE tt_gekod_rng,
             tcode_rng    TYPE tt_tcode_rng,
             tpclass_rng  TYPE tt_tpclass_rng,
           END OF t_param,

           tt_tcode_list TYPE STANDARD TABLE OF zbcs_tcode_list WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING is_param TYPE t_param.

    METHODS get_data
      RETURNING VALUE(rt_tcode_list) TYPE tt_tcode_list.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_trans,
             trkorr   TYPE e070-trkorr,
             strkorr  TYPE e070-strkorr,
             as4text  TYPE e07t-as4text,
             obj_name TYPE e071-obj_name,
           END OF t_trans,

           tt_trans TYPE STANDARD TABLE OF t_trans WITH DEFAULT KEY.

    CONSTANTS: c_object_tran TYPE trobjtype VALUE 'TRAN',
               c_pgmid_r3tr  TYPE pgmid     VALUE 'R3TR'.

    DATA: gt_ret   TYPE tt_tcode_list,
          gt_trans TYPE tt_trans,
          gs_param TYPE t_param.

    METHODS read_basic.
    METHODS read_requests.

    METHODS set_req_data
      IMPORTING is_trans TYPE t_trans
      CHANGING  cs_ret   TYPE zbcs_tcode_list.
ENDCLASS.


CLASS zcl_bc_tcode_list IMPLEMENTATION.
  METHOD constructor.
    gs_param = is_param.
  ENDMETHOD.

  METHOD get_data.
    read_basic( ).
    read_requests( ).
    rt_tcode_list[] = gt_ret[].
  ENDMETHOD.

  METHOD read_basic.
    CLEAR gt_ret[].

    SELECT tstc~tcode tadir~author tadir~created_on tadir~check_date tadir~srcsystem tadir~devclass tdevc~tpclass
           tstct~ttext
           INTO CORRESPONDING FIELDS OF TABLE gt_ret
           FROM tstc
                INNER JOIN tadir
                  ON  tadir~pgmid    = c_pgmid_r3tr
                  AND tadir~object   = c_object_tran
                  AND tadir~obj_name = tstc~tcode
                INNER JOIN tdevc
                  ON tdevc~devclass = tadir~devclass
                LEFT JOIN tstct
                  ON  tstct~sprsl = sy-langu
                  AND tstct~tcode = tstc~tcode
           WHERE tstc~tcode     IN gs_param-tcode_rng
             AND tadir~devclass IN gs_param-devclass_rng
             AND tdevc~tpclass  IN gs_param-tpclass_rng
           ORDER BY tstc~tcode
       ##TOO_MANY_ITAB_FIELDS. "#EC CI_BUFFJOIN
  ENDMETHOD.

  METHOD read_requests.
    CHECK gt_ret[] IS NOT INITIAL.

    gt_trans = CORRESPONDING #(
      zcl_bc_transport_request=>get_request_and_objects( it_tag = VALUE #( FOR ls_ret IN gt_ret
                                                                           ( pgmid    = c_pgmid_r3tr
                                                                             object   = c_object_tran
                                                                             obj_name = ls_ret-tcode ) ) ) ).

    LOOP AT gt_ret ASSIGNING FIELD-SYMBOL(<ls_ret>).

      LOOP AT gt_trans ASSIGNING FIELD-SYMBOL(<ls_trans1>) WHERE obj_name = <ls_ret>-tcode.

        set_req_data( EXPORTING is_trans = <ls_trans1>
                      CHANGING  cs_ret   = <ls_ret> ).

        IF <ls_ret>-gekod IS NOT INITIAL.
          EXIT.
        ENDIF.

        ASSIGN gt_trans[ trkorr = <ls_trans1>-strkorr ] TO FIELD-SYMBOL(<ls_trans2>).
        IF sy-subrc = 0.
          set_req_data( EXPORTING is_trans = <ls_trans1>
                        CHANGING  cs_ret   = <ls_ret> ).

          IF <ls_ret>-gekod IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF sy-subrc = 0 AND <ls_ret>-trkorr IS INITIAL.
        IF <ls_trans2> IS ASSIGNED.
          <ls_ret>-trkorr  = <ls_trans2>-trkorr.
          <ls_ret>-as4text = <ls_trans2>-as4text.
        ELSE.
          <ls_ret>-trkorr  = <ls_trans1>-trkorr.
          <ls_ret>-as4text = <ls_trans1>-as4text.
        ENDIF.

      ENDIF.

      UNASSIGN: <ls_trans1>, <ls_trans2>.

    ENDLOOP.

    DELETE gt_ret WHERE NOT ( gekod IN gs_param-gekod_rng ).
  ENDMETHOD.

  METHOD set_req_data.
    DATA lt_split TYPE STANDARD TABLE OF string.

    TRY.
        CHECK     is_trans-as4text IS NOT INITIAL
              AND zcl_bc_jira_project=>is_proj_integrable_curr_jira( is_trans-as4text+0(3) )  = abap_true.

      CATCH cx_root.
        RETURN.
    ENDTRY.

    cs_ret-trkorr  = is_trans-trkorr.
    cs_ret-as4text = is_trans-as4text.

    SPLIT cs_ret-as4text AT space INTO TABLE lt_split.

    TRY.
        cs_ret-gekod = lt_split[ 1 ].

        LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<lv_str>).
          CHECK sy-tabix > 2.
          cs_ret-gesum = |{ cs_ret-gesum } { <lv_str> }|.
        ENDLOOP.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.