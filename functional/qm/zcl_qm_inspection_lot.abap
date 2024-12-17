CLASS zcl_qm_inspection_lot DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb,
                zif_qm_inspection_lot.

    CLASS-METHODS get_instance
      IMPORTING prueflos      TYPE  qals-prueflos
      RETURNING VALUE(result) TYPE REF TO zcl_qm_inspection_lot
      RAISING   zcx_qm_inspection_lot.

    CLASS-METHODS get_latest_mat_doc_date
      IMPORTING VALUE(mandt)    TYPE mandt
                VALUE(prueflos) TYPE qals-prueflos
      EXPORTING VALUE(cpudt)    TYPE qamb-cpudt.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             prueflos TYPE qals-prueflos,
             obj      TYPE REF TO zcl_qm_inspection_lot,
             error    TYPE REF TO zcx_qm_inspection_lot,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
           WITH UNIQUE KEY primary_key COMPONENTS prueflos.

    CLASS-DATA multitons TYPE multiton_set.

    DATA prueflos TYPE qals-prueflos.

    METHODS constructor
      IMPORTING prueflos TYPE qals-prueflos
      RAISING   zcx_qm_inspection_lot.
ENDCLASS.


CLASS zcl_qm_inspection_lot IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_qm_inspection_lot=>multitons[ KEY primary_key
                                                            prueflos = prueflos ] ).

      CATCH cx_sy_itab_line_not_found.
        DATA(new_mt) = VALUE multiton_dict( prueflos = prueflos ).

        TRY.
            new_mt-obj = NEW #( new_mt-prueflos ).
          CATCH zcx_qm_inspection_lot INTO new_mt-error ##NO_HANDLER.
        ENDTRY.

        INSERT new_mt INTO TABLE zcl_qm_inspection_lot=>multitons REFERENCE INTO mt.
    ENDTRY.

    IF mt->error IS NOT INITIAL.
      RAISE EXCEPTION mt->error.
    ENDIF.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_latest_mat_doc_date
    BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING qamb.

    cpudt = '00000000';

    mat_docs =
      select    zaehler, cpudt
      from      QAMB
      where     mandt    =    :mandt    and
                prueflos =    :prueflos  and
                mblnr    like '4%'
      order by  zaehler desc;

    if is_empty( :mat_docs ) then
      return;
    end if;

    select top 1 cpudt into cpudt from :mat_docs;
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE FROM qals
           FIELDS @abap_true
           WHERE prueflos = @prueflos
           INTO @DATA(qals_found).

    IF qals_found = abap_false.
      RAISE EXCEPTION NEW zcx_qm_inspection_lot( textid   = zcx_qm_inspection_lot=>insp_lot_not_found
                                                 prueflos = prueflos ).
    ENDIF.

    me->prueflos = prueflos.
  ENDMETHOD.
ENDCLASS.