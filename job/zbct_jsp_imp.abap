@EndUserText.label : 'Job Split uygulamaları'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #C
@AbapCatalog.dataMaintenance : #ALLOWED
define table zbct_jsp_imp {
  key impid : zbcd_jsp_impid not null;
  imptx     : zbcd_jsp_imptx;
  @AbapCatalog.foreignKey.screenCheck : true
  clsname   : seoclsname
    with foreign key seoclass
      where clsname = zbct_jsp_imp.clsname;

}