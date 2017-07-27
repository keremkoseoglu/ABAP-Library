class zbccl_util definition
  public
  final
  create public .

public section.
*"* public components of class ZBCCL_UTIL
*"* do not include other source files here!!!

  class-methods convert_date_to_internal
    importing
      !pfd_i_date type clike
    returning
      value(pfd_r_date) type datum
    raising
      cx_reca_symsg .
  class-methods convert_unit_input
    importing
      !pfd_i_meins type clike
    returning
      value(pfd_r_meins) type meins
    raising
      cx_reca_symsg .
  class-methods get_wpinfo_with_detail
    importing
      !pit_i_mainprog_rng type gle_runadm_tab_range_cprog optional
    returning
      value(pit_r_wp) type zbctt_server_wp
    raising
      zcx_bc_server
      zcx_bc_wpinfo .
  class-methods dequeue_if_cprog_inactive
    importing
      !pfd_i_gname type seqg3-gname
      !pfd_i_garg type seqg3-garg
    raising
      zcx_bc_enque_read
      zcx_bc_wpinfo
      zcx_bc_program_active
      zcx_bc_enque_delete .
  class-methods reset_pwd
    importing
      !pfd_i_email type adr6-smtp_addr
      !pfd_i_pwtxt type zbcd_pwtxt optional
    raising
      zcx_bc_method_param
      zcx_bc_email_addr
      cx_reca_symsg .