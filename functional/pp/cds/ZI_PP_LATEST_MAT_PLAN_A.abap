@AbapCatalog.sqlViewName: 'ZQMV_004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Yardımcı View'
define view ZI_PP_LATEST_MAT_PLAN_A
  as select from mapl
    inner join   ZI_PP_PLKO as _plko on
    _plko.plnty = mapl.plnty         and
    _plko.plnnr = mapl.plnnr         and
    _plko.plnal = mapl.plnal         and
    _plko.verwe = '5' {
  key mapl.matnr,
  key mapl.werks,
  mapl.plnty,
  mapl.plnnr,
  mapl.plnal
}
where
  mapl.plnty = 'Q' and
  mapl.loekz = ''