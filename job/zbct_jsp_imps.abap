@EndUserText.label : 'Sistem bazlı Job Split uygulama tanımları'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zbct_jsp_imps {
  key mandt : mandt not null;
  @AbapCatalog.foreignKey.screenCheck : true
  key impid : zbcd_jsp_impid not null
    with foreign key zbct_jsp_imp
      where impid = zbct_jsp_imps.impid;
  pjmax     : zbcd_jsp_pjmax;
  jimax     : zbcd_jsp_jimax;

}