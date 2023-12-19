@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Türkiye KDV oranları'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_SD_TR_VAT_RATE
  as select from a002
    inner join   konp on  konp.knumh = a002.knumh
                      and konp.kopos = '01'
{
  key a002.taxm1,
      fltp_to_dec(cast(konp.kbetr as float) / cast(10 as float) as abap.dec( 2, 0 ))                         as vat_rate,
      cast(fltp_to_dec(cast(konp.kbetr as float) / cast(10 as float) as abap.dec( 2, 0 )) as abap.char( 4 )) as vat_rate_txt
}
where
      a002.kappl = 'V'
  and a002.kschl = 'MWST'
  and a002.aland = 'TR'
  and a002.taxk1 = '1'
  and a002.datab <= $session.system_date
  and a002.datbi >= $session.system_date
