interface ZIF_BC_JAS_CARRIER
  public .

    constants:
      c_clsname_me type seoclsname value 'ZIF_BC_JAS_CARRIER',
      c_meth_sm    type seocpdname value 'SEND_MESSAGE'.

    methods:
      send_message
        importing
          !iv_subject type clike optional
          !iv_body    type clike optional
          !it_to      type rke_userid
        raising
          zcx_bc_class_method.

endinterface.