class ZCL_BC_HTML_TOOLKIT definition
  public
  final
  create public .

public section.

  class-methods CREATE_HTML
    importing
      !IV_STRUCTURE_NAME type DD02L-TABNAME
      !IT_TABLE type ANY TABLE
      !IV_TITLE type CHAR80 optional
      value(IT_BODY_TEXT) type BCSY_TEXT optional
      !IT_COLUMN type ZSDTT_COLUMN optional
    exporting
      !ET_HTML type BCSY_TEXT
    raising
      ZCX_BC_HTML .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_HTML_TOOLKIT IMPLEMENTATION.


  method create_html.

   data :
          lt_def    type standard table of dd03p,
          lt_header type standard table of w3head,
          ls_title  type w3head,
          lt_fields type standard table of w3fields,
          lt_html   type standard table of w3html,
          ls_head   type w3head,
          lv_index type i value 1,
          ls_body_text type line of bcsy_text,
          lv_just type w3fields-just.
   try.
*--------------------------------------------------------------------*
* Tablonun detayını al
*--------------------------------------------------------------------*
    lt_def = zcl_bc_gui_toolkit=>get_definition_ddic( iv_structure_name ).

    if lines( lt_def ) eq 0.
      raise exception type zcx_bc_html
         exporting
            textid    = zcx_bc_html=>cant_read_structure
            structure = iv_structure_name.
    endif.

    loop at lt_def assigning field-symbol(<ls_def>).
      read table it_column assigning field-symbol(<ls_column>) index sy-tabix.
      if sy-subrc eq 0.
         ls_head-text = <ls_column>-zcolumn_txt.
      else.
         ls_head-text = <ls_def>-scrtext_l.
      endif.

      if <ls_def>-datatype eq 'CURR' or
         <ls_def>-datatype eq 'QUAN'.
         lv_just = 'right'.
      else.
         lv_just = 'center'.
      endif.

*  -Populate the Column Headings
      call function 'WWW_ITAB_TO_HTML_HEADERS' ##FM_OLDED
        exporting
          field_nr = sy-tabix
          text     = ls_head-text
          fgcolor  = 'white'
          bgcolor  = 'green'
          justified = lv_just
        tables
          header   = lt_header.
*  -Populate Column Properties
      call function 'WWW_ITAB_TO_HTML_LAYOUT' ##FM_OLDED
        exporting
          field_nr = sy-tabix
          fgcolor  = 'black'
          size     = '3'
          justified = lv_just
        tables
          fields   = lt_fields.
    endloop.


  refresh lt_html.

*-Title of the Display
 if iv_title is not initial.
  ls_title-text = iv_title .
  ls_title-font = 'Arial' ##NO_TEXT.
  ls_title-size = '2'.
 endif.


*--------------------------------------------------------------------*
* CONVERT OUTPUT FORMAT
*--------------------------------------------------------------------*
* create text table
    field-symbols : <lt_table> type standard table.

    data : lo_element    type ref to cl_abap_elemdescr,
           lt_components type cl_abap_structdescr=>component_table,
           lo_ref type ref to data.
    loop at lt_def assigning <ls_def>.
     lo_element ?=  cl_abap_elemdescr=>describe_by_name( 'CHAR255' ).
     append value #( name = <ls_def>-fieldname type = lo_element ) to lt_components.
    endloop.
    data(lo_tab) = cl_abap_tabledescr=>create( p_line_type  = cl_abap_structdescr=>create( lt_components )
                                               p_table_kind = cl_abap_tabledescr=>tablekind_std
                                               p_unique     = abap_false ).
    create data lo_ref type handle lo_tab.
    assign lo_ref->* to <lt_table>.
* conversion
    loop at it_table assigning field-symbol(<ls_table>).
      append initial line to <lt_table> assigning field-symbol(<ls_text_table>).
      loop at lt_def assigning <ls_def>.
        assign component <ls_def>-fieldname of structure <ls_table>      to field-symbol(<lv_source>).
        assign component <ls_def>-fieldname of structure <ls_text_table> to field-symbol(<lv_target>).
        if sy-subrc eq 0.
          try.
            if <ls_def>-reffield is not initial.
               assign component <ls_def>-reffield of structure <ls_table> to field-symbol(<lv_refvalue>).
            endif.
            if sy-subrc eq 0 and <ls_def>-reffield is not initial.
            zcl_bc_gui_toolkit=>convert_any_input_to_output(
                     exporting iv_source   = <lv_source>
                               iv_refvalue = <lv_refvalue>
                               iv_datatype = <ls_def>-datatype
                     changing cv_target = <lv_target>
               ).
            else.

              zcl_bc_gui_toolkit=>convert_any_input_to_output(
                       exporting iv_source   = <lv_source>
                                 iv_convexit = <ls_def>-convexit
                       changing cv_target = <lv_target>
                 ).
            endif.

          catch cx_root into data(lo_cx_conversion).
            raise exception type zcx_bc_html
               exporting
                  previous  = lo_cx_conversion
                  textid    = zcx_bc_html=>conversion_error
                  structure = iv_structure_name.
          endtry.

        endif.
      endloop.
    endloop.
*--------------------------------------------------------------------*
* MAPPING
*--------------------------------------------------------------------*
    call function 'WWW_ITAB_TO_HTML' ##FM_OLDED
      exporting
        table_header = ls_title
      tables
        html         = lt_html
        fields       = lt_fields
        row_header   = lt_header
        itable       = <lt_table>.

  et_html = lt_html.

  if it_body_text is not initial.

    loop at it_body_text into ls_body_text.
      ls_body_text-line = |<br>{ ls_body_text-line }</br>|.
      insert ls_body_text into et_html index lv_index.
      add 1 to lv_index.
    endloop.

  endif.

  catch cx_root into data(lx_root).
   message lx_root type 'S' display like 'E'.
  endtry.

  endmethod.
ENDCLASS.