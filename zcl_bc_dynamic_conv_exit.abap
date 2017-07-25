CLASS zcl_bc_dynamic_conv_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    constants: c_dir_input  type zbcd_conv_exit_dir value 'INPUT',
               c_dir_output type zbcd_conv_exit_dir value 'OUTPUT'.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_singleton) TYPE REF TO zcl_bc_dynamic_conv_exit.

    METHODS convert
      IMPORTING
        !iv_dir TYPE zbcd_conv_exit_dir
        !iv_tab TYPE tabname
        !iv_fld TYPE fieldname
        !iv_val TYPE any
      EXPORTING
        !ev_val TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_tab_fld,
             tabname   TYPE tabname,
             fieldname TYPE fieldname,
             rollname  TYPE rollname,
             domname   TYPE dd03l-domname,
             convexit  TYPE dd01l-convexit,
           END OF t_tab_fld,

           tt_tab_fld TYPE HASHED TABLE OF t_tab_fld WITH UNIQUE KEY primary_key COMPONENTS tabname fieldname.

    CLASS-DATA go_singleton TYPE REF TO zcl_bc_dynamic_conv_exit.

    DATA gt_tab_fld TYPE tt_tab_fld.

ENDCLASS.



CLASS ZCL_BC_DYNAMIC_CONV_EXIT IMPLEMENTATION.


  METHOD convert.

    DATA: lv_func(50),
          lr_new  TYPE REF TO data.

    FIELD-SYMBOLS <lv_new> TYPE any.

*   En kötü ihtimal: Aynı değeri birebir döndür
    ev_val = iv_val.

*   Tablo alanının detaylarını oku ve Conversion Exit olduğundan emin ol
    ASSIGN gt_tab_fld[ KEY primary_key
                       COMPONENTS tabname   = iv_tab
                                  fieldname = iv_fld
                     ] TO FIELD-SYMBOL(<ls_tab_fld>).

    IF sy-subrc NE 0.

      DATA(ls_tab_fld) = VALUE t_tab_fld( tabname   = iv_tab
                                          fieldname = iv_fld ).

      SELECT SINGLE dd03l~rollname, dd03l~domname, dd01l~convexit
             INTO CORRESPONDING FIELDS OF @ls_tab_fld
             FROM dd03l
                  INNER JOIN dd01l ON dd01l~domname EQ dd03l~domname
             WHERE tabname    EQ @ls_tab_fld-tabname
               AND fieldname  EQ @ls_tab_fld-fieldname ##WARN_OK.

      INSERT ls_tab_fld INTO TABLE gt_tab_fld ASSIGNING <ls_tab_fld>.

    ENDIF.

    CHECK <ls_tab_fld>-rollname IS NOT INITIAL AND
          <ls_tab_fld>-convexit IS NOT INITIAL.

    CREATE DATA lr_new TYPE (<ls_tab_fld>-rollname).
    ASSIGN lr_new->* TO <lv_new>.
    CHECK <lv_new> IS ASSIGNED.

*   Dönüşümü yap ve başarılıysa döndür

    lv_func = |CONVERSION_EXIT_{ <ls_tab_fld>-convexit }_{ iv_dir }|.

    CALL FUNCTION lv_func
      EXPORTING
        input        = iv_val
      IMPORTING
        output       = <lv_new>
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2 ##fm_subrc_ok.

    CHECK sy-subrc EQ 0.

    ev_val = <lv_new>.

  ENDMETHOD.


  METHOD get_instance.

    IF go_singleton IS INITIAL.
      go_singleton = NEW #( ).
    ENDIF.

    ro_singleton = go_singleton.

  ENDMETHOD.
ENDCLASS.