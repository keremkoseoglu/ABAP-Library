@EndUserText.label : 'Job paket kalemi'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zbct_jsp_jpitm {
  key mandt : mandt not null;
  @AbapCatalog.foreignKey.screenCheck : true
  key jphid : zbcd_jsp_jphid not null
    with foreign key zbct_jsp_jphdr
      where mandt = zbct_jsp_jpitm.mandt
        and jphid = zbct_jsp_jpitm.jphid;
  key jppos : zbcd_jsp_jppos not null;
  include zbcs_jsp_flag_step;
  jobcount  : btcjobcnt;

}