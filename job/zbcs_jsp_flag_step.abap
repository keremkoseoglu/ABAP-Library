@EndUserText.label : 'Adımlara ait göstergeler'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
define structure zbcs_jsp_flag_step {
  include zbcs_jsp_flag_gen;
  mstar : zbcd_jsp_mstar;
  msnam : zbcd_jsp_msnam;
  msdat : zbcd_jsp_msdat;
  mszet : zbcd_jsp_mszet;
  mfini : zbcd_jsp_mfini;
  mfnam : zbcd_jsp_mfnam;
  mfdat : zbcd_jsp_mfdat;
  mfzet : zbcd_jsp_mfzet;

}