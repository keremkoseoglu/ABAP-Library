@AbapCatalog.sqlViewName: 'ZPPV_009'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PLPO alanları'
define view ZI_PP_PLPO
  as select from plpo
    inner join   ZI_BC_SY as _sy on
    _sy.mandt = plpo.mandt
  association [0..1] to crhd on
    crhd.objty = 'A'               and
    crhd.werks = $projection.werks and
    crhd.objid = $projection.arbid {
  key plpo.plnty,
  key plpo.plnnr,
  key plpo.plnkn,
  key plpo.zaehl,
  plpo.vornr,
  plpo.ltxa1,
  plpo.werks,
  plpo.arbid,
  crhd
}
where
  plpo.loekz =  '' and
  plpo.datuv <= _sy.sydatum