*----------------------------------------------------------------------*
*       CLASS ZBCCL_TAB_VAL_SCAN DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class zbccl_tab_val_scan definition
  public
  final
  create public .

  public section.
*"* public components of class ZBCCL_TAB_VAL_SCAN
*"* do not include other source files here!!!

    types:
      tt_result type standard table of zbcs_tvs_result with default key ,
      tt_val    type standard table of zbcd_cval with default key.
    types:
      begin of t_param ,
        s_dom type range of domname,
        s_tab type range of tabname,
        t_val type tt_val,
      end of t_param .

    methods constructor
      importing
        !pwa_i_param type t_param .
    methods scan .
    methods get_result
      returning
        value(pit_r_result) type tt_result .
protected section.
*"* protected components of class ZBCCL_TAB_VAL_SCAN
*"* do not include other source files here!!!
private section.
*"* private components of class ZBCCL_TAB_VAL_SCAN
*"* do not include other source files here!!!

  types:
    begin of t_table ,
    tabname type dd02l-tabname,
    ddtext  type dd02t-ddtext,
  end of t_table .
  types:
    begin of t_field,
    tabname   type dd03l-tabname,
    fieldname type dd03l-fieldname,
    rollname  type dd03l-rollname,
    domname   type dd03l-domname,
    reptext   type dd04t-reptext,
  end of t_field .
  types:
    tt_table type standard table of t_table with default key .
  types:
    tt_field type standard table of t_field with default key
             with non-unique sorted key k1 components tabname .

  data gwa_param type t_param .
  data git_table type tt_table .
  data git_field type tt_field .
  data git_result type tt_result .
  constants c_tabclass_transp type tabclass value 'TRANSP'. "#EC NOTEXT
  constants c_tabclass_cluster type tabclass value 'CLUSTER'. "#EC NOTEXT
  constants c_tabclass_pool type tabclass value 'POOL'. "#EC NOTEXT

  methods build_table_list .
  methods build_field_list .
  methods hunt_values .
endclass.



class zbccl_tab_val_scan implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_TAB_VAL_SCAN->BUILD_FIELD_LIST
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method build_field_list.

  data: lrd_table type ref to t_table.

* Paranoya
  check git_table[] is not initial.

* Alan listesi oluşturalım
  select dd03l~tabname dd03l~fieldname dd03l~rollname dd03l~domname dd04t~reptext
         into corresponding fields of table git_field
         from dd03l
              left join dd04t on dd04t~rollname   eq dd03l~rollname
                             and dd04t~ddlanguage eq sy-langu
                             and dd04t~as4local   eq dd03l~as4local
                             and dd04t~as4vers    eq dd03l~as4vers
         for all entries in git_table
         where dd03l~tabname   eq git_table-tabname
           and dd03l~domname   in gwa_param-s_dom.

  sort git_field by tabname fieldname.
  delete adjacent duplicates from git_field comparing tabname fieldname.

* Alanı olmayan tabloları ortadan kaldıralım
  loop at git_table reference into lrd_table.

    loop at git_field transporting no fields using key k1 where tabname eq lrd_table->tabname.
      exit.
    endloop.

    check sy-subrc ne 0.
    delete git_table.
    continue.

  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_TAB_VAL_SCAN->BUILD_TABLE_LIST
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method build_table_list.

  select distinct dd02l~tabname dd02t~ddtext
         into corresponding fields of table git_table
         from dd02l
              left join dd02t on dd02t~tabname    eq dd02l~tabname
                             and dd02t~ddlanguage eq sy-langu
                             and dd02t~as4local   eq dd02l~as4local
                             and dd02t~as4vers    eq dd02l~as4vers
         where dd02l~tabname in gwa_param-s_tab
           and ( dd02l~tabclass eq c_tabclass_transp  or
                 dd02l~tabclass eq c_tabclass_cluster or
                 dd02l~tabclass eq c_tabclass_pool )
         order by dd02l~tabname.

  delete adjacent duplicates from git_table comparing tabname.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_TAB_VAL_SCAN->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] PWA_I_PARAM                    TYPE        T_PARAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
method constructor.
  gwa_param = pwa_i_param.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_TAB_VAL_SCAN->GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] PIT_R_RESULT                   TYPE        TT_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_result.
  pit_r_result[] = git_result[].
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_TAB_VAL_SCAN->HUNT_VALUES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method hunt_values.

  data: lfd_dummy  type string,
        lfd_where  type string,

        lrd_field  type ref to t_field,
        lrd_result type ref to zbcs_tvs_result,

        lrd_table  type ref to t_table,
        lrd_val    type ref to zbcd_cval.

* __________
* Paranoya
* ----------

  check git_table[] is not initial and
        git_field[] is not initial.

* __________
* Alan taraması
* ----------

  loop at git_table reference into lrd_table.
    loop at git_field reference into lrd_field using key k1 where tabname eq lrd_table->tabname.
      loop at gwa_param-t_val reference into lrd_val.

        concatenate lrd_field->fieldname ` EQ '` lrd_val->* `'` into lfd_where.

        select single (lrd_field->fieldname) into lfd_dummy
               from (lrd_table->tabname)
               where (lfd_where).

        check sy-subrc eq 0.

        append initial line to git_result reference into lrd_result.
        lrd_result->tabname   = lrd_table->tabname.
        lrd_result->ddtext    = lrd_table->ddtext.
        lrd_result->fieldname = lrd_field->fieldname.
        lrd_result->reptext   = lrd_field->reptext.
        lrd_result->rollname  = lrd_field->rollname.
        lrd_result->domname   = lrd_field->domname.
        lrd_result->cval      = lrd_val->*.

      endloop.
    endloop.
  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_TAB_VAL_SCAN->SCAN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method scan.

  check gwa_param-t_val[] is not initial.

  build_table_list( ).
  check git_table[] is not initial.

  build_field_list( ).
  check git_field[] is not initial.

  hunt_values( ).

endmethod.
endclass.
