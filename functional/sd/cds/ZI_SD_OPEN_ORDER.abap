@AbapCatalog.sqlViewName: 'ZSDV_029'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Açık siparişler'
define view ZI_SD_OPEN_ORDER
  as select from vbak
    inner join   vbap on
    vbap.vbeln = vbak.vbeln
    inner join V_Vbup_S4 as vbup       on
    vbup.vbeln = vbap.vbeln and
    vbup.posnr = vbap.posnr {
  key vbap.vbeln,
  key vbap.posnr,
  vbap.matnr,
  vbap.kwmeng,
  vbap.vrkme,
  vbap.werks,
  vbap.lgort,
  vbak.erdat
}
where
  vbak.vbtyp   = 'C' and
  vbap.abgru   = ''  and
  (
    vbup.lfsta = 'A' or
    vbup.lfsta = 'B'
  )
