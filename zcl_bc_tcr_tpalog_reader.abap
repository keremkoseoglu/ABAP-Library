CLASS zcl_bc_tcr_tpalog_reader DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bc_tcr_reader.

  PROTECTED SECTION.

    METHODS get_sysnam ABSTRACT RETURNING VALUE(rv_sysnam) TYPE tmscsys-sysnam.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_bc_tcr_tpalog_reader IMPLEMENTATION.


  METHOD zif_bc_tcr_reader~get_list.

    TRY.

        CLEAR et_list.

        IF it_trkorr_rng IS INITIAL AND
           it_gekod IS INITIAL.

          et_list = NEW zcl_bc_tpalog_reader( )->get_list(
            iv_sysnam   = get_sysnam( )
            it_sys_data = it_sys_data
          ).

        ELSE.

          IF it_trkorr_rng IS NOT INITIAL.
            DATA(lt_list_by_trkorr) = NEW zcl_bc_tpalog_reader( )->get_list(
              iv_sysnam     = get_sysnam( )
              it_trkorr_rng = it_trkorr_rng
              it_sys_data   = it_sys_data
            ).
          ENDIF.

          IF it_gekod IS NOT INITIAL.
            DATA(lt_list_by_gekod) = NEW zcl_bc_tpalog_reader( )->get_list(
              iv_sysnam     = get_sysnam( )
              it_gekod      = it_gekod
              it_sys_data   = it_sys_data
            ).
          ENDIF.

          et_list = lt_list_by_trkorr.
          APPEND LINES OF lt_list_by_gekod TO et_list.

        ENDIF.

      CATCH cx_root INTO DATA(lo_cx_root).

        co_log->add_exception( lo_cx_root ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.