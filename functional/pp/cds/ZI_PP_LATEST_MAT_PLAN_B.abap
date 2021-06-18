@AbapCatalog.sqlViewName: 'ZQMV_005'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Yardımcı View'
define view zi_pp_latest_mat_plan_b
  as select from ZI_PP_LATEST_MAT_PLAN_A {
  key matnr,
  key werks,
  max( plnnr ) as plnnr
}
group by
  matnr,
  werks