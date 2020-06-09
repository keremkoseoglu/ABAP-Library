INTERFACE zif_bc_restful_api
  PUBLIC .

  CONSTANTS: BEGIN OF class,
               me TYPE seoclsname VALUE 'ZIF_BC_RESTFUL_API',
             END OF class,

             BEGIN OF method,
               accept_json_input TYPE seocpdname VALUE 'ACCEPT_JSON_INPUT',
             END OF method.

  METHODS accept_json_input
    IMPORTING json_input         TYPE string
    RETURNING VALUE(json_output) TYPE string
    RAISING   zcx_bc_class_method.

ENDINTERFACE.