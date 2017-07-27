class zimcl_util definition
  public
  final
  create public .

public section.
*"* public components of class ZIMCL_UTIL
*"* do not include other source files here!!!

  class-methods convert_posnr_to_posid
    importing
      !pfd_i_posnr type ima_posnr
      !pfd_i_gjahr type gjahr default sy-datum+0(4)
    returning
      value(pfd_r_posid) type im_posid
    raising
      zcx_im_yatirim_talebi .
protected section.
*"* protected components of class ZIMCL_UTIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZIMCL_UTIL
*"* do not include other source files here!!!
endclass.



class zimcl_util implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZIMCL_UTIL=>CONVERT_POSNR_TO_POSID
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_POSNR                    TYPE        IMA_POSNR
* | [--->] PFD_I_GJAHR                    TYPE        GJAHR (default =SY-DATUM+0(4))
* | [<-()] PFD_R_POSID                    TYPE        IM_POSID
* | [!CX!] ZCX_IM_YATIRIM_TALEBI
* +--------------------------------------------------------------------------------------</SIGNATURE>
method convert_posnr_to_posid.

  data: lfd_objnr type imzo-objnr,
        lfd_posnr type imzo-posnr.

  concatenate 'IQ' pfd_i_posnr into lfd_objnr.

  select single posnr into lfd_posnr from imzo where objnr eq lfd_objnr
                                                 and gjahr eq pfd_i_gjahr.
  if sy-subrc ne 0.
    raise exception type zcx_im_yatirim_talebi
      exporting
        textid = zcx_im_yatirim_talebi=>imzo_kaydi_yok
        posnr  = pfd_i_posnr.
  endif.

  select single posid into pfd_r_posid from impr where posnr eq lfd_posnr.
  if sy-subrc ne 0.
    raise exception type zcx_im_yatirim_talebi
      exporting
        textid = zcx_im_yatirim_talebi=>impr_kaydi_yok
        posnr  = pfd_i_posnr.
  endif.

endmethod.
endclass.