CLASS zcl_mm_material_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      class_constructor,

      get_text_safe
        IMPORTING !iv_statm       TYPE t132t-statm
        RETURNING VALUE(rv_sttxt) TYPE t132t-sttxt.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: tt_t132  TYPE HASHED TABLE OF t132 WITH UNIQUE KEY primary_key COMPONENTS statm,
           tt_t132t TYPE HASHED TABLE OF t132t WITH UNIQUE KEY primary_key COMPONENTS statm,

           BEGIN OF MESH t_view,
             views TYPE tt_t132 ASSOCIATION text TO texts ON statm = statm USING KEY primary_key,
             texts TYPE tt_t132t,
           END OF MESH t_view.

    CLASS-DATA gs_view TYPE t_view.

ENDCLASS.



CLASS zcl_mm_material_view IMPLEMENTATION.

  METHOD class_constructor.
    SELECT * FROM t132 INTO CORRESPONDING FIELDS OF TABLE gs_view-views.
    SELECT * FROM t132t WHERE spras EQ @sy-langu INTO CORRESPONDING FIELDS OF TABLE @gs_view-texts.
  ENDMETHOD.


  METHOD get_text_safe.
    ASSIGN gs_view-views[ KEY primary_key COMPONENTS statm = iv_statm
                        ] TO FIELD-SYMBOL(<ls_view>).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    rv_sttxt = VALUE #( gs_view-views\text[ <ls_view> ]-sttxt DEFAULT space ).
  ENDMETHOD.

ENDCLASS.