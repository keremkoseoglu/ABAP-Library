@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Etkin ve hatasız iş akışları'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_BC_ACTIVE_FAULTLESS_WFS
  as select from swwwihead
{
  key wi_id,
      wi_rh_task // WS90000008
}
where
      wi_type =  'F'
  and wi_stat <> 'COMPLETED'
  and wi_stat <> 'CANCELLED'
  and wi_stat <> 'ERROR'
  and wi_stat <> 'EXCPCAUGHT'
  and wi_stat <> 'EXCPHANDLR'
