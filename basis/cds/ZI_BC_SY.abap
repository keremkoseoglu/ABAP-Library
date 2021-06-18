@AbapCatalog.sqlViewName: 'ZBCV_100'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sistem değerleri'
define view ZI_BC_SY
  as select from t000 {
  key mandt,
  cast (substring( cast( tstmp_current_utctimestamp()
      as abap.char(17) ), 1, 8 ) as abap.dats) as sydatum,
  cast(substring( cast( tstmp_current_utctimestamp()
      as abap.char(17) ), 9, 6 ) as abap.tims) as syuzeit
}
where
  t000.mandt = $session.client