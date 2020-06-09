CLASS zcl_mm_purchasing_group DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    class-methods GET_T024
      importing !IV_EKGRP type EKGRP
      returning value(RS_T024) type T024 .

  PROTECTED SECTION.
  PRIVATE SECTION.

    class-data:
      gt_t024 TYPE HASHED TABLE OF t024 WITH UNIQUE KEY primary_key COMPONENTS ekgrp .

ENDCLASS.



CLASS ZCL_MM_PURCHASING_GROUP IMPLEMENTATION.


  METHOD get_t024.

    DATA ls_t024 TYPE t024.

    ASSIGN gt_t024[ KEY primary_key COMPONENTS ekgrp = iv_ekgrp ] TO FIELD-SYMBOL(<ls_t024>).

    IF sy-subrc ne 0.
      SELECT SINGLE * FROM t024 INTO ls_t024 WHERE ekgrp = iv_ekgrp.
      ls_t024-ekgrp = iv_ekgrp.
      INSERT ls_t024 INTO TABLE gt_t024 assigning <ls_t024>.
    ENDIF.

    rs_t024 = <ls_t024>.

  ENDMETHOD.
ENDCLASS.