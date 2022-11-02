@EndUserText.label : 'Job paketine ait kilometre taşları'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zbct_jsp_jpmil {
  key mandt : mandt not null;
  @AbapCatalog.foreignKey.screenCheck : true
  key jphid : zbcd_jsp_jphid not null
    with foreign key zbct_jsp_jphdr
      where mandt = zbct_jsp_jpmil.mandt
        and jphid = zbct_jsp_jpmil.jphid;
  key milst : zbcd_jsp_milst not null;
  include zbcs_jsp_flag_step;
  jobcount  : btcjobcnt;

}