@AbapCatalog.sqlViewName: 'ZBCV_004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BALHDR kayıtlarının sayısı'
define view ZBCV_BALHDR_COUNT
  as select from balhdr {
  key object,
  key subobject,
  count(*) as ENTRY_COUNT
}
group by
  object,
  subobject