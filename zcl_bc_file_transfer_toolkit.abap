class ZCL_BC_FILE_TRANSFER_TOOLKIT definition
  public
  final
  create public .

public section.

  constants C_FTP_DEST type RFCDEST value 'SAPFTPA' ##NO_TEXT.
  constants C_FTP_KEY type INT4 value 26101957 ##NO_TEXT.

  class-methods GET_FTP_FILE
    importing
      !IV_SERVER type CHAR200L
      !IV_USERNAME type ZFID_FTP_USER
      !IV_PASSWORD type ZFID_FTP_PASS
      !IV_FILE type CHAR200
      !IV_BINARY_FILE type XFELD optional
    exporting
      !EV_BLOB_LENGTH type INT4
      !ET_TEXT_DATA type ZBCTT_TEXT2000
      !ET_BLOB_DATA type ETXML_XLINE_TABTYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_FILE_TRANSFER_TOOLKIT IMPLEMENTATION.


  method get_ftp_file.

    data: lv_len        type int4,
          lv_ftp_handle type int4,
          lv_msg        type bapi_msg,
          lv_password   type char50, "
          lv_username   type char50, "
          lv_command    type char50 value 'dir',
          lv_debug      type os_boolean value '',
          lv_char_mode  type os_boolean,
          lv_ok         type os_boolean.

    clear: et_text_data[], et_blob_data[], ev_blob_length.

    lv_password = iv_password.
    lv_len = strlen( lv_password ).
    call function 'HTTP_SCRAMBLE'
      exporting
        source      = lv_password
        sourcelen   = lv_len
        key         = c_ftp_key
      importing
        destination = lv_password.

    lv_username = iv_username.
    call function 'FTP_CONNECT'
      exporting
        user            = lv_username
        password        = lv_password
        host            = iv_server
        rfc_destination = c_ftp_dest
      importing
        handle          = lv_ftp_handle
      exceptions
        not_connected   = 1
        others          = 2.
    if sy-subrc <> 0.
      return.
    endif.


    if iv_binary_file = abap_false.
      lv_char_mode = abap_true.
    endif.

    call function 'FTP_SERVER_TO_R3'
      exporting
        handle         = lv_ftp_handle
        fname          = iv_file
        character_mode = lv_char_mode
      importing
        blob_length    = ev_blob_length
      tables
        blob           = et_blob_data[]
        text           = et_text_data[]
      exceptions
        tcpip_error    = 1
        command_error  = 2
        data_error     = 3
        others         = 4.

    if sy-subrc <> 0.
      data(lv_file) = iv_file.
      concatenate '/' lv_file into lv_file.

      call function 'FTP_SERVER_TO_R3'
        exporting
          handle         = lv_ftp_handle
          fname          = lv_file
          character_mode = lv_char_mode
        importing
          blob_length    = ev_blob_length
        tables
          blob           = et_blob_data[]
          text           = et_text_data[]
        exceptions
          tcpip_error    = 1
          command_error  = 2
          data_error     = 3
          others         = 4.
      if sy-subrc <> 0.
      endif.
    endif.

    call function 'FTP_DISCONNECT'
      exporting
        handle = lv_ftp_handle.

    if iv_binary_file = abap_true.
      clear et_text_data[].
    else.
      clear et_blob_data[].
    endif.


  endmethod.
ENDCLASS.