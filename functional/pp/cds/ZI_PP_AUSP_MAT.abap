@AbapCatalog.sqlViewName: 'ZPPV_022'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Malzeme karakteristikleri'
define view ZI_PP_AUSP_MAT
  as select distinct from ZI_PP_AUSP {
  cast( objek as matnr ) as matnr,
  atnam,
  atflv
}
where
  ZI_PP_AUSP.mafid = 'O' and
  ZI_PP_AUSP.klart = '001'