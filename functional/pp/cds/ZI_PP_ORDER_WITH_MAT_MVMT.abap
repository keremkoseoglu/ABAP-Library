@AbapCatalog.sqlViewName: 'ZPPV_006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Malzeme hareketi olan siparişler'
define view ZI_PP_ORDER_WITH_MAT_MVMT
  as select distinct from aufk
    inner join            aufm on
    aufm.aufnr = aufk.aufnr {
  key aufm.aufnr
}
where
  aufk.autyp = '10'