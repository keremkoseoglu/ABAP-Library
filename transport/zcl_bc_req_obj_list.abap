CLASS zcl_bc_req_obj_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    types:
      tt_db type standard table of ZBCT_REQOBJ_LIST with default key,

      tt_reqobj type standard table of ZBCS_REQOBJ_LIST with default key.

    constants c_tabname_list type tabname value 'ZBCS_REQOBJ_LIST'.

    methods:
      get_list
        importing !iv_guid type recaguid
        returning value(rt_list) type tt_reqobj,

      set_list
        importing
          !iv_guid type recaguid
          !it_list type tt_reqobj.

    class-methods:

      class_constructor,
      list_objects changing !ct_reqobj type tt_reqobj.

  PROTECTED SECTION.
  PRIVATE SECTION.

    class-methods:

      insert_object_list
        importing
          !iv_guid   type recaguid
          !it_reqobj type tt_reqobj,

      self_maintenance.

ENDCLASS.



CLASS ZCL_BC_REQ_OBJ_LIST IMPLEMENTATION.


  method class_constructor.
    self_maintenance( ).
  endmethod.


  method get_list.

    select pgmid, object, obj_name
      into corresponding fields of table @rt_list
      from ZBCT_REQOBJ_LIST
      where guid eq @iv_guid
      ##TOO_MANY_ITAB_FIELDS.

    loop at rt_list assigning field-symbol(<ls_list>).

      try.
          zcl_bc_dol_model=>get_dol_obj(
            EXPORTING iv_object = <ls_list>-object
            IMPORTING eo_dol    = data(lo_dol)
          ).

          <ls_list>-object_txt = lo_dol->get_object_txt(
            iv_pgmid  = <ls_list>-pgmid
            iv_object = <ls_list>-object
          ).

          <ls_list>-ddtext = lo_dol->get_ddtext(
            iv_pgmid    = <ls_list>-pgmid
            iv_object   = <ls_list>-object
            iv_obj_name = conv #( <ls_list>-obj_name )
          ).

        catch cx_root ##no_handler . "Diaper
      endtry.

    endloop.

  endmethod.


  method insert_object_list.

    data:
      lt_Db    type tt_Db,
      lv_posnr type ZBCT_REQOBJ_LIST-posnr.

    delete from ZBCT_REQOBJ_LIST where guid eq @iv_guid.

    loop at it_reqobj assigning field-symbol(<ls_reqobj>).
      add 1 to lv_posnr.

      append value #(
        guid       = iv_guid
        posnr      = lv_posnr
        pgmid      = <ls_reqobj>-pgmid
        object     = <ls_reqobj>-object
        obj_name   = <ls_reqobj>-obj_name
        object_txt = <ls_reqobj>-object_txt
        ddtext     = <ls_reqobj>-ddtext
        ernam      = sy-uname
        erdat      = sy-datum
        erzet      = sy-uzeit
      ) to lt_Db.

    endloop.

    insert ZBCT_REQOBJ_LIST from table lt_Db.
    commit work and wait.

  endmethod.


  method list_objects.

    data lv_guid  type ZBCT_REQOBJ_LIST-guid.

    lv_guid = cl_reca_guid=>get_new_guid( ).

    insert_object_list(
      iv_guid   = lv_guid
      it_reqobj = ct_reqobj
    ).

    submit zbcp_req_obj_list
      with p_guid eq lv_Guid
      and return.

    clear ct_reqobj.

    select pgmid, object, obj_name, object_txt, ddtext
      into corresponding fields of table @ct_reqobj
      from ZBCT_REQOBJ_LIST
      where (
        guid eq @lv_guid and
        (
          pgmid ne @space or
          object ne @space or
          obj_name ne @space
        )
      ).

    delete from ZBCT_REQOBJ_LIST where guid eq @lv_guid.

  endmethod.


  method self_maintenance.
    data(lv_erdat) = Sy-datum - 2.
    delete from ZBCT_REQOBJ_LIST where erdat le lv_erdat.
  endmethod.


  method set_list.

    insert_object_list(
      iv_guid   = iv_guid
      it_reqobj = it_list
    ).

  endmethod.
ENDCLASS.