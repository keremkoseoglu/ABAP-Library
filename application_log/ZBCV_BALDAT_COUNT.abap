@AbapCatalog.sqlViewName: 'ZBCV_006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BALDAT kayıtlarının boyutunu hesaplar'
@ClientDependent: false
define view ZBCV_BALDAT_COUNT
  as select from ZBCV_BALDAT_COUNT_CLUSTR_CAST as _bal {
  _bal.mandant,
  _bal.object,
  _bal.subobject,
  sum( _bal.TOTAL_SIZE ) as TOTAL_SIZE
}
group by
  _bal.mandant,
  _bal.object,
  _bal.subobject