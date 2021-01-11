@AbapCatalog.sqlViewName: 'ZBCV_007'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CAST edilmiş CLUSTR değerleri'
@ClientDependent: false
define view ZBCV_BALDAT_COUNT_CLUSTR_CAST
  as select from balhdr
    inner join   baldat on
    baldat.log_handle = balhdr.log_handle {
  balhdr.mandant,
  balhdr.object,
  balhdr.subobject,
  cast( baldat.clustr as abap.int8 ) as TOTAL_SIZE
}