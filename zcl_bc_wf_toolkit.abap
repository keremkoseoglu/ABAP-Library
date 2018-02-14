CLASS zcl_bc_wf_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    constants:
      c_wi_Type_w type SWW_WITYPE value 'W'.

    CLASS-METHODS:
      refresh_buffer EXPORTING et_msg TYPE tab_bdcmsgcoll.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_tcode_buff_refr TYPE sytcode VALUE 'SWU_OBUF'.

ENDCLASS.



CLASS ZCL_BC_WF_TOOLKIT IMPLEMENTATION.


  METHOD refresh_buffer.

    CLEAR et_msg.

    DATA(lo_bdc) = NEW zcl_bc_bdc( ).
    lo_bdc->add_scr( iv_prg = 'SAPLSWUO'   iv_dyn = '0100' ).
    lo_bdc->add_fld( iv_nam = 'BDC_OKCODE' iv_val = '=REFR' ).

    lo_bdc->submit(
      EXPORTING
        iv_tcode  = c_tcode_buff_refr
        is_option = VALUE #( dismode = 'N'
                             updmode = 'S' )
      IMPORTING
        et_msg    = et_msg
    ).

  ENDMETHOD.
ENDCLASS.