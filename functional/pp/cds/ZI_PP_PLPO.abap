@AbapCatalog.sqlViewName: 'ZPPV_009'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PLPO alanları'
define view ZI_PP_PLPO
  as select distinct from plpo
    inner join            plko on
    plko.plnty = plpo.plnty    and
    plko.plnnr = plpo.plnnr
    inner join            plas on
    plas.plnty = plpo.plnty    and
    plas.plnnr = plpo.plnnr    and
    plas.plnkn = plpo.plnkn
    inner join            zbct_user_sy as _sy on
    _sy.bname = $session.user
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
  plpo.steus,
  crhd
}
where
  plpo.loekz =  ''          and
  plko.loekz =  ''          and
  plas.loekz =  ''          and
  plpo.datuv <= _sy.sydatum and
  plko.datuv <= _sy.sydatum and
  plas.datuv <= _sy.sydatum