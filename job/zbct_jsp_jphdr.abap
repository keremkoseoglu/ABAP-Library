@EndUserText.label : 'Job paket başlığı'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zbct_jsp_jphdr {
  key mandt : mandt not null;
  key jphid : zbcd_jsp_jphid not null;
  include zbcs_jsp_jphdr_fld
    extend impid :
      remove foreign key;
  include zbcs_jsp_flag_gen;

}