@AbapCatalog.sqlViewName: 'ZPPV_010'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PLMK alanları'
define view ZI_PP_PLMK
  as select from plmk
  association [0..*] to ZI_PP_PLPO as _plpo on
    _plpo.plnty = $projection.plnty and
    _plpo.plnnr = $projection.plnnr and
    _plpo.plnkn = $projection.plnkn {
  key plnty,
  key plnnr,
  key plnkn,
  key kzeinstell,
  key merknr,
  key zaehl,
  toleranzob,
  toleranzun,
  plausioben,
  plausiunte,
  sollwert,
  ltextkz,
  qpmk_zaehl,
  verwmerkm,
  mkversion,
  ltextspr,
  kurztext,
  dummy10,
  dummy20,
  dummy40,
  katab1,
  katalgart1,
  auswmenge1,
  stellen,
  masseinhsw,
  _plpo
}
where
  loekz = ''