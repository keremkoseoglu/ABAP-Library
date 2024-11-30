INTERFACE zif_bc_print_binary
  PUBLIC .

  TYPES: BEGIN OF t_print_param,
           auto_Delete     type TSP01-RQ1DISPO,
           dest            TYPE tsp01-rqdest,
           immediate_print TYPE tsp01-rq1dispo,
         END OF t_print_param.

  CONSTANTS c_meth_print_bin TYPE seocpdname VALUE 'PRINT_BIN'.

  METHODS print_bin
    IMPORTING
      !it_solix    TYPE solix_tab
      !iv_filesize type i
      !is_param    TYPE t_print_param
    RAISING
      zcx_bc_class_method.

ENDINTERFACE.