CLASS zcl_bc_rest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

* rest methods
  CONSTANTS c_method_post    TYPE string VALUE 'POST'.
  CONSTANTS c_method_get     TYPE string VALUE 'GET'.
  CONSTANTS c_method_put     TYPE string VALUE 'PUT'.
  CONSTANTS c_method_delete  TYPE string VALUE 'DELETE'.

* error codes
  CONSTANTS c_405_method       TYPE i VALUE '405'.
  CONSTANTS c_400_bad_request  TYPE i VALUE '400'.
  CONSTANTS c_401_unauthorized TYPE i VALUE '401'.
  CONSTANTS c_404_not_found    TYPE i VALUE '404'.
  CONSTANTS c_417_unexpected   TYPE i VALUE '417'.

* success codes
  CONSTANTS c_200_ok           TYPE i VALUE '200'.
  CONSTANTS c_204_no_content   TYPE i VALUE '204'.

  CLASS-METHODS set_content_json
    IMPORTING
      !io_server TYPE REF TO if_http_server .

  CLASS-METHODS get_req_method
    IMPORTING
      !io_server TYPE REF TO if_http_server
    RETURNING
      VALUE(rv_method) TYPE string .

  CLASS-METHODS error
     IMPORTING
       !io_server TYPE REF TO if_http_server
       !iv_code   TYPE i
       !iv_reason TYPE string.

  CLASS-METHODS get_json_data
      IMPORTING
        !io_server TYPE REF TO if_http_server
       RETURNING VALUE(rv_json) TYPE string.

  CLASS-METHODS cx_error
       IMPORTING
        !io_cx     TYPE REF TO cx_root
        !io_server TYPE REF TO if_http_server.

PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BC_REST IMPLEMENTATION.


 METHOD cx_error.

      DATA lv_message type string.

      MESSAGE io_cx TYPE 'S'.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                    INTO lv_message.
      error( io_server  = io_server
                          iv_code    = c_417_unexpected
                          iv_reason  = lv_message  ).

 ENDMETHOD.


  METHOD error.
    io_server->response->set_status( code = iv_code reason = CONV #( iv_reason ) ).
  ENDMETHOD.


  METHOD get_json_data.
    rv_json = io_server->request->get_cdata( ).
  ENDMETHOD.


  METHOD get_req_method.
    rv_method = io_server->request->get_header_field( name = '~request_method' ).
  ENDMETHOD.


  METHOD set_content_json.
    io_server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
  ENDMETHOD.
ENDCLASS.