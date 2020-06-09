class zbccl_ent_julian definition
  public
  final
  create public .

public section.
*"* public components of class ZBCCL_ENT_JULIAN
*"* do not include other source files here!!!

  class-methods convert_date_to_julian
    importing
      !pfd_i_datum type dats
      !prc_i_sistem type ref to zbccl_ent_sistem
    returning
      value(pfd_r_julda) type zbcd_julda
    raising
      zcx_bc_ent_julian
      zcx_bc_ent_sistem .
protected section.
*"* protected components of class ZBCCL_ENT_JULIAN
*"* do not include other source files here!!!
private section.
*"* private components of class ZBCCL_ENT_JULIAN
*"* do not include other source files here!!!

  class-methods get_year_floor_ceil
    importing
      !pfd_i_gjahr type gjahr
    exporting
      !pfd_e_floor type gjahr
      !pfd_e_ceil type gjahr
      !pfd_e_first_3 type numc3
    raising
      zcx_bc_ent_julian .
endclass.



class zbccl_ent_julian implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBCCL_ENT_JULIAN=>CONVERT_DATE_TO_JULIAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_DATUM                    TYPE        DATS
* | [--->] PRC_I_SISTEM                   TYPE REF TO ZBCCL_ENT_SISTEM
* | [<-()] PFD_R_JULDA                    TYPE        ZBCD_JULDA
* | [!CX!] ZCX_BC_ENT_JULIAN
* | [!CX!] ZCX_BC_ENT_SISTEM
* +--------------------------------------------------------------------------------------</SIGNATURE>
method convert_date_to_julian.

  statics:
    lit_cache type tt_julian.

  data:
    lwa_cache type t_julian,
    lrd_cache type ref to t_julian,

    lfd_imported_date type dats,

    lfd_datum_floor type gjahr,
    lfd_datum_ceil  type gjahr,
    lfd_today_floor type gjahr,
    lfd_today_ceil  type gjahr,

    lfd_begda type d,
    lfd_days  type i,

    lfd_p1(3) type n,
    lfd_p2(3) type n.

* Temizlik
  clear pfd_r_julda.

* Tarih yoksa döndürme
  check pfd_i_datum is not initial.

* Daha önce Buffer'lanmış bir tarihse doğrudan döndürelim
  read table lit_cache reference into lrd_cache
    with key primary_key components datum = pfd_i_datum
                                    siste = prc_i_sistem->gfd_siste.

  if sy-subrc eq 0.
    pfd_r_julda = lrd_cache->julda.
    exit.
  endif.

* Bize gönderilen tarih sisteme ait üst sınır tarihten daha ileride ise, bu tarihi
* üst sınır ile kısıtlayacağız
  lfd_imported_date = prc_i_sistem->get_trimmed_date( pfd_i_datum ).

* Julian tarih örneği:
* 2012-02-01 = 112032

* İlk 3 hane, 1900'den / 2900'den / 3900'den / ... kaç yıl uzakta olduğumuzu
* göstermektedir. İkinci 3 hane, o yılın kaçıncı gününde olduğumuzu göstermektedir.
*
* Öncelikle gönderilen tarihin hangi iki yıl arasında olduğumuzu tespit edelim.
  get_year_floor_ceil( exporting pfd_i_gjahr   = lfd_imported_date+0(4)
                       importing pfd_e_floor   = lfd_datum_floor
                                 pfd_e_ceil    = lfd_datum_ceil
                                 pfd_e_first_3 = lfd_p1 ).

* Mesela 1900 - 2899 dilimindeysek...
  get_year_floor_ceil( exporting pfd_i_gjahr   = sy-datum+0(4)
                       importing pfd_e_floor   = lfd_today_floor
                                 pfd_e_ceil    = lfd_today_ceil ).

* ... 1800'lü bir yıl için hata üretmeliyiz
  if lfd_datum_floor lt lfd_today_floor.
    raise exception type zcx_bc_ent_julian
      exporting
        textid  = zcx_bc_ent_julian=>tarih_dilimi_gecersiz
        datum   = lfd_imported_date
        floor   = lfd_today_floor
        ceiling = lfd_today_ceil.
  endif.

* ... 2899 yılının son gününden daha ilerideki her gün bizim için 999365 olacaktır.
  if lfd_datum_ceil gt lfd_today_ceil.
    pfd_r_julda = '999365'.

* ... normal durumlarda ...
  else.

*   İkinci 3 hane (032) : Yılın 32. günü
    concatenate lfd_imported_date+0(4) '0101' into lfd_begda.

    call function 'HR_99S_INTERVAL_BETWEEN_DATES'
      exporting
        begda = lfd_begda
        endda = lfd_imported_date
      importing
        days  = lfd_days.

    lfd_p2 = lfd_days.

*   Birleştir
    concatenate lfd_p1 lfd_p2 into pfd_r_julda.

  endif.

* Cache'e ekle
  lwa_cache-datum = pfd_i_datum.
  lwa_cache-siste = prc_i_sistem->gfd_siste.
  lwa_cache-julda = pfd_r_julda.
  insert lwa_cache into table lit_cache.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZBCCL_ENT_JULIAN=>GET_YEAR_FLOOR_CEIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_GJAHR                    TYPE        GJAHR
* | [<---] PFD_E_FLOOR                    TYPE        GJAHR
* | [<---] PFD_E_CEIL                     TYPE        GJAHR
* | [<---] PFD_E_FIRST_3                  TYPE        NUMC3
* | [!CX!] ZCX_BC_ENT_JULIAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_year_floor_ceil.

* Temizlik
  clear: pfd_e_floor, pfd_e_ceil, pfd_e_first_3.

* Hangi iki yıl arasında olduğumuzu tespit edelim.
  pfd_e_floor = 1900.

  do.

    if pfd_e_floor eq 9900.
      pfd_e_ceil = 9999.
    else.
      pfd_e_ceil = pfd_e_floor + 999.
    endif.

    if pfd_i_gjahr ge pfd_e_floor and
       pfd_i_gjahr le pfd_e_ceil.

      pfd_e_first_3 = pfd_i_gjahr - pfd_e_floor.
      exit.

    endif.

    if pfd_e_floor ge 9900.
      raise exception type zcx_bc_ent_julian
        exporting
          textid = zcx_bc_ent_julian=>yilin_ait_oldugu_dilim
          gjahr  = pfd_i_gjahr.
    endif.

    add 1000 to pfd_e_floor.

  enddo.

endmethod.
endclass.
