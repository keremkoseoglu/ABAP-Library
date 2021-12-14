@AbapCatalog.sqlViewName: 'ZPPV_008'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PLKO alanları'
define view ZI_PP_PLKO
  as select from plko
    inner join   zbct_user_sy as _sy on
    _sy.bname = $session.user
  association [0..*] to ZI_PP_PLMK as _plmk on
    _plmk.plnty = $projection.plnty and
    _plmk.plnnr = $projection.plnnr {
  key plko.plnty,
  key plko.plnnr,
  key plko.plnal,
  key plko.zaehl,
  verwe,
  _plmk
}
where
  plko.loekz =  '' and
  plko.datuv <= _sy.sydatum