@AbapCatalog.sqlViewName: 'ZPPV_021'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AUSP erişimi için CDS View'
define view ZI_PP_AUSP
  as select distinct from ausp
    inner join            cabn on
    cabn.atinn = ausp.atinn {
  ausp.objek,
  cabn.atnam,
  ausp.mafid,
  ausp.klart,
  ausp.atflv
}