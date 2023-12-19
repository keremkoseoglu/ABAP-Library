@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Türkiye ÖTV oranları'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_SD_TR_SCT_RATE
  as select from a996
    inner join   konp on  konp.knumh = a996.knumh
                      and konp.kopos = '01'
    inner join   tvko on tvko.vkorg = a996.vkorg
    inner join   t001 on t001.bukrs = tvko.bukrs
{
  key a996.vkorg,
  key a996.taxm2,
      fltp_to_dec(cast(konp.kbetr as float) / cast(10 as float) as abap.dec( 2, 0 ))                         as vat_rate,
      cast(fltp_to_dec(cast(konp.kbetr as float) / cast(10 as float) as abap.dec( 2, 0 )) as abap.char( 4 )) as vat_rate_txt,
      t001.bukrs,
      t001.land1
}
where
      a996.kappl = 'V'
  and a996.kschl = 'ZOTV'
  and a996.aland = 'TR'
  and a996.taxk1 = '1'
  and a996.datab <= $session.system_date
  and a996.datbi >= $session.system_date
