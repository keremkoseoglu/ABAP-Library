CLASS zcl_mm_purchase_order DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_header,
             ernam TYPE ekko-ernam,
             bstyp TYPE ekko-bstyp,
             bsart TYPE ekko-bsart,
             ekgrp TYPE ekko-ekgrp,
           END OF t_header,

           BEGIN OF t_key,
             ebeln TYPE ebeln,
           END OF t_key.

    CONSTANTS: BEGIN OF c_procstat,
                 approved         TYPE ekko-procstat VALUE '05',
                 waiting_approval TYPE ekko-procstat VALUE '03',
               END OF c_procstat.

    DATA: gs_header TYPE t_header,
          gv_ebeln  TYPE ebeln READ-ONLY.

    CLASS-METHODS get_creator
      IMPORTING iv_ebeln        TYPE ebeln
      RETURNING VALUE(rv_ernam) TYPE ekko-ernam
      RAISING   zcx_bc_table_content.

    CLASS-METHODS enqueue_po
      IMPORTING iv_ebeln            TYPE ebeln
                iv_max_wait_seconds TYPE i OPTIONAL
      RAISING   zcx_bc_lock.

    CLASS-METHODS dequeue_po
      IMPORTING iv_ebeln TYPE ebeln.

    CLASS-METHODS wait_until_po_unlocked
      IMPORTING iv_ebeln            TYPE ebeln
                iv_max_wait_seconds TYPE i DEFAULT 60
      RAISING   zcx_bc_lock.

    CLASS-METHODS wait_until_po_exists
      IMPORTING ebeln            TYPE ebeln
                max_wait_seconds TYPE i DEFAULT 60
      RAISING   zcx_bc_commit_wait_expire.

    CLASS-METHODS get_instance
      IMPORTING is_key        TYPE t_key
      RETURNING VALUE(ro_obj) TYPE REF TO zcl_mm_purchase_order
      RAISING   cx_no_entry_in_table.

    METHODS cancel_old_active_workflows.

    METHODS ensure_item_exists
      IMPORTING ebelp TYPE ebelp
      RAISING   zcx_mm_purchase_order.

    METHODS get_items_pr_requester_name
      IMPORTING ebelp         TYPE ebelp
      RETURNING VALUE(result) TYPE ad_namtext.

    METHODS get_first_plant RETURNING VALUE(result) TYPE werks_d.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_multiton,
             key TYPE t_key,
             obj TYPE REF TO zcl_mm_purchase_order,
             cx  TYPE REF TO cx_no_entry_in_table,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS key.

    TYPES: BEGIN OF t_po_item,
             ebelp TYPE ekpo-ebelp,
             banfn TYPE ekpo-banfn,
             werks TYPE ekpo-werks,
           END OF t_po_item,

           tt_po_item TYPE HASHED TABLE OF t_po_item
                 WITH UNIQUE KEY primary_key COMPONENTS ebelp.

    TYPES: BEGIN OF t_item_pr_requester_name,
             ebelp             TYPE ebelp,
             pr_requester_name TYPE ad_namtext,
           END OF t_item_pr_requester_name,

           tt_item_pr_requester TYPE HASHED TABLE OF t_item_pr_requester_name
           WITH UNIQUE KEY primary_key COMPONENTS ebelp.

    CONSTANTS: BEGIN OF c_catid,
                 business_object TYPE sww_wi2obj-catid VALUE 'BO',
               END OF c_catid,

               BEGIN OF c_field,
                 creator TYPE fieldname VALUE 'ERNAM',
               END OF c_field,

               BEGIN OF c_tabname,
                 def TYPE tabname VALUE 'EKKO',
               END OF c_tabname,

               BEGIN OF c_typeid,
                 purchase_order TYPE sww_wi2obj-typeid VALUE 'BUS2012',
               END OF c_typeid.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA: gt_items                   TYPE tt_po_item,
          gv_items_read              TYPE abap_bool,
          gt_item_pr_requester_cache TYPE tt_item_pr_requester.

    METHODS constructor
      IMPORTING is_key TYPE t_key
      RAISING   cx_no_entry_in_table.

    METHODS get_po_items RETURNING VALUE(result) TYPE REF TO tt_po_item.

ENDCLASS.


CLASS zcl_mm_purchase_order IMPLEMENTATION.
  METHOD enqueue_po.
    DATA(lv_total_wait_time) = 0.

    DO.
      CALL FUNCTION 'ENQUEUE_EMEKKOE'
        EXPORTING  ebeln          = iv_ebeln
        EXCEPTIONS foreign_lock   = 1
                   system_failure = 2
                   OTHERS         = 3.

      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      lv_total_wait_time += 1.

      IF lv_total_wait_time > iv_max_wait_seconds.
        RAISE EXCEPTION NEW zcx_bc_lock( textid   = zcx_bc_lock=>locked_for_too_long
                                         bname    = CONV #( sy-msgv1 )
                                         objectid = CONV #( iv_ebeln ) ).
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.
  ENDMETHOD.

  METHOD dequeue_po.
    CALL FUNCTION 'DEQUEUE_EMEKKOE'
      EXPORTING ebeln = iv_ebeln.
  ENDMETHOD.

  METHOD wait_until_po_unlocked.
    enqueue_po( iv_ebeln            = iv_ebeln
                iv_max_wait_seconds = iv_max_wait_seconds ).

    dequeue_po( iv_ebeln ).
  ENDMETHOD.

  METHOD wait_until_po_exists.
    DATA(total_wait_time) = 0.

    DO.
      SELECT SINGLE FROM ekko
             FIELDS @abap_true
             WHERE ebeln = @ebeln
             INTO @DATA(po_found).

      IF po_found = abap_true.
        RETURN.
      ENDIF.

      total_wait_time += 1.

      IF total_wait_time > max_wait_seconds.
        RAISE EXCEPTION NEW zcx_bc_commit_wait_expire( textid  = zcx_bc_commit_wait_expire=>entry_db_wait_expired
                                                       doc_key = CONV #( ebeln ) ).
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.
  ENDMETHOD.

  METHOD get_creator.
    TRY.
        rv_ernam = zcl_mm_purchase_order=>get_instance( VALUE #( ebeln = iv_ebeln ) )->gs_header-ernam.
      CATCH cx_no_entry_in_table INTO DATA(lo_neit).
        RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>entry_missing
                                                  previous = lo_neit
                                                  objectid = CONV #( iv_ebeln )
                                                  tabname  = c_tabname-def ).
    ENDTRY.

    IF rv_ernam IS INITIAL. " Paranoya
      RAISE EXCEPTION NEW zcx_bc_table_content( textid    = zcx_bc_table_content=>entry_field_initial
                                                objectid  = CONV #( iv_ebeln )
                                                tabname   = c_tabname-def
                                                fieldname = c_field-creator ).
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key COMPONENTS key = is_key ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( key = is_key ).

      TRY.
          ls_multiton-obj = NEW #( ls_multiton-key ).
        CATCH cx_no_entry_in_table INTO ls_multiton-cx ##NO_HANDLER.
      ENDTRY.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    IF <ls_multiton>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_multiton>-cx.
    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD cancel_old_active_workflows.
    zcl_bc_wf_toolkit=>cancel_old_active_workflows( iv_catid  = c_catid-business_object
                                                    iv_instid = CONV #( gv_ebeln )
                                                    iv_typeid = c_typeid-purchase_order ).
  ENDMETHOD.

  METHOD ensure_item_exists.
    DATA(items) = get_po_items( ).

    IF NOT line_exists( items->*[ KEY primary_key COMPONENTS ebelp = ebelp ] ).
      RAISE EXCEPTION NEW zcx_mm_purchase_order( textid = zcx_mm_purchase_order=>item_not_found
                                                 ebeln  = gv_ebeln
                                                 ebelp  = ebelp ).
    ENDIF.
  ENDMETHOD.

  METHOD get_items_pr_requester_name.
    ASSIGN gt_item_pr_requester_cache[ KEY primary_key
                                       ebelp = ebelp ] TO FIELD-SYMBOL(<cache>).
    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE t_item_pr_requester_name( ebelp = ebelp ).

      DO 1 TIMES.
        DATA(po_items) = get_po_items( ).

        ASSIGN po_items->*[ KEY primary_key COMPONENTS ebelp = new_cache-ebelp ] TO FIELD-SYMBOL(<po_item>).
        CHECK sy-subrc = 0 AND <po_item>-banfn IS NOT INITIAL.

        SELECT SINGLE FROM eban
               FIELDS ernam
               WHERE banfn = @<po_item>-banfn
               INTO @DATA(pr_creator).

        CHECK sy-subrc = 0 AND pr_creator IS NOT INITIAL.

        TRY.
            new_cache-pr_requester_name = zcl_bc_sap_user=>get_instance( iv_bname             = pr_creator
                                                                         iv_tolerate_inactive = abap_true
                                          )->get_full_name( ).

          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

        CHECK new_cache-pr_requester_name IS INITIAL.
        new_cache-pr_requester_name = pr_creator.
      ENDDO.

      INSERT new_cache INTO TABLE gt_item_pr_requester_cache ASSIGNING <cache>.
    ENDIF.

    result = <cache>-pr_requester_name.
  ENDMETHOD.

  METHOD get_first_plant.
    DATA(items) = get_po_items( ).

    LOOP AT items->* REFERENCE INTO DATA(i) WHERE werks IS NOT INITIAL.
      result = i->werks.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    gv_ebeln = is_key-ebeln.

    SELECT SINGLE ernam, bstyp, bsart, ekgrp
           FROM ekko
           WHERE ebeln = @gv_ebeln
           INTO CORRESPONDING FIELDS OF @gs_header.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW cx_no_entry_in_table( table_name = CONV #( c_tabname-def )
                                                entry_name = |{ gv_ebeln }| ).
    ENDIF.
  ENDMETHOD.

  METHOD get_po_items.
    IF gv_items_read = abap_false.
      SELECT FROM ekpo
             FIELDS ebelp, banfn, werks
             WHERE ebeln = @gv_ebeln
             INTO CORRESPONDING FIELDS OF TABLE @gt_items.

      gv_items_read = abap_true.
    ENDIF.

    result = REF #( gt_items ).
  ENDMETHOD.
ENDCLASS.