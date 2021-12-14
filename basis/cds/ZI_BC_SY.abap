@AbapCatalog.sqlViewName: 'ZBCV_100'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sistem değerleri'
define view ZI_BC_SY
  as select from t000 {
  key mandt,
  $session.system_language as sylangu
}
where
  t000.mandt = $session.client