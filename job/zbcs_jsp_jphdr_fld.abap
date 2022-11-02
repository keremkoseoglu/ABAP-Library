@EndUserText.label : 'Job paket başlığı düz alanlar'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
define structure zbcs_jsp_jphdr_fld {
  stext   : stext;
  @AbapCatalog.foreignKey.screenCheck : true
  impid   : zbcd_jsp_impid
    with foreign key zbct_jsp_imp
      where impid = zbcs_jsp_jphdr_fld.impid;
  pjmax   : zbcd_jsp_pjmax;
  jimax   : zbcd_jsp_jimax;
  jobname : btcjob;
  prot    : zbcd_jsp_prot;

}