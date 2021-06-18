@AbapCatalog.sqlViewName: 'ZQMV_003'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'En güncel malzeme'
define view ZI_PP_LATEST_MAT_PLAN
  as select from ZI_PP_LATEST_MAT_PLAN_C as _c
    inner join   ZI_PP_LATEST_MAT_PLAN_A as _a on
    _a.matnr = _c.matnr                        and
    _a.werks = _c.werks                        and
    _a.plnal = _c.plnal                        and
    _a.plnnr = _c.plnnr
  association [1..*] to ZI_PP_PLKO as _plko on
    _plko.plnty = $projection.plnty and
    _plko.plnnr = $projection.plnnr and
    _plko.plnal = $projection.plnal {
  key _a.matnr,
  key _a.werks,
  _a.plnty,
  _a.plnnr,
  _a.plnal,
  _plko
}