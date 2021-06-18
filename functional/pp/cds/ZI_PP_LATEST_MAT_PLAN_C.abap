@AbapCatalog.sqlViewName: 'ZQMV_006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Yardımcı View'
define view ZI_PP_LATEST_MAT_PLAN_C
  as select from zi_pp_latest_mat_plan_b as _b
    inner join   ZI_PP_LATEST_MAT_PLAN_A as _a on
    _a.matnr = _b.matnr                        and
    _a.werks = _b.werks                        and
    _a.plnnr = _b.plnnr {
  key _b.matnr,
  key _b.werks,
  key _b.plnnr,
  max( _a.plnal ) as plnal
}
group by
  _b.matnr,
  _b.werks,
  _b.plnnr