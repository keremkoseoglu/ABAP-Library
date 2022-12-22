@AbapCatalog.sqlViewName: 'ZBCV_WF_002'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Etkin iş akışları'
define view ZI_BC_ACTIVE_WFS
  as select from swwwihead {
  key wi_id,
  wi_rh_task // WS90000008
}
where
  wi_type =  'F'         and
  wi_stat <> 'COMPLETED' and
  wi_stat <> 'CANCELLED'
