@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'İndirim koşulları'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_SD_DISCOUNT_CONDITION
  as select from t685a
{
  key kschl
}
where
      kappl = 'V'
  and koaid = 'A'
