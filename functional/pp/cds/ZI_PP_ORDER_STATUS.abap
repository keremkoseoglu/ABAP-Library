@AbapCatalog.sqlViewName: 'ZPPV_023'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Üretim siparişi durumu'
define view ZI_PP_ORDER_STATUS
  as select from aufk
    inner join   jest on
    jest.objnr = aufk.objnr
  association [0..1] to tj02t as _status_text      on
    _status_text.istat = $projection.stat and
    _status_text.spras = $session.system_language
  association [0..1] to tj30t as _user_status_text on
    _user_status_text.stsma = 'ZPP_SIP'        and
    _user_status_text.estat = $projection.stat and
    _user_status_text.spras = $session.system_language {
  /* Alanlar */
  aufk.aufnr,
  jest.objnr,
  jest.stat,
  jest.inact,
  /* Association */
  _status_text,
  _user_status_text
}