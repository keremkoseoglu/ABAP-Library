class zbccl_log_search definition
  public
  final
  create public .

public section.
*"* public components of class ZBCCL_LOG_SEARCH
*"* do not include other source files here!!!

  methods constructor
    importing
      !pit_i_object type bal_r_obj
      !pit_i_subobject type bal_r_sub
      !pit_i_aldate type bal_r_date
      !pit_i_aluser type bal_r_user
      !pfd_i_stext type zbcd_stext
      !pfd_i_sinte type zbcd_sinte .
  methods search
    raising
      cx_reca_symsg .
  class-methods get_instance
    importing
      !pit_i_object type bal_r_obj
      !pit_i_subobject type bal_r_sub
      !pit_i_aldate type bal_r_date
      !pit_i_aluser type bal_r_user
      !pfd_i_stext type zbcd_stext
      !pfd_i_sinte type zbcd_sinte
    returning
      value(prc_r_search) type ref to zbccl_log_search .
  methods get_html
    returning
      value(pit_r_html) type tttext255 .
  methods write_results .
protected section.
*"* protected components of class ZBCCL_LOG_SEARCH
*"* do not include other source files here!!!
private section.
*"* private components of class ZBCCL_LOG_SEARCH
*"* do not include other source files here!!!

  types:
    begin of t_balm,
    balm type balm,
    text type string,
    text_uc type string,
    result type checkbox,
    index type i,
    end of t_balm .
  types:
    tt_balm type sorted table of t_balm
    with unique key primary_key components balm-lognumber balm-msgnumber .

  data gwa_lfil type bal_s_lfil .
  data gfd_stext type zbcd_stext .
  data gfd_sinte type zbcd_sinte .
  data git_balhdr type balhdr_t .
  data git_balm type tt_balm .
  data git_balm_res type tt_balm .
  data git_html type tttext255 .

  methods search__head
    raising
      cx_reca_symsg .
  methods search__msg .
  methods search__msg__add_result
    importing
      !prd_i_balm type ref to t_balm
      !pfd_i_index type sytabix
      !pfd_i_result type checkbox .
  methods append_html
    importing
      !pfd_i_code type data .
  methods get_p_tag
    importing
      !pfd_i_result type checkbox
      !pfd_i_msgty type symsgty
    returning
      value(pfd_r_tag) type string .
endclass.



class zbccl_log_search implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_LOG_SEARCH->APPEND_HTML
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_CODE                     TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method append_html.

  data:
    lrd_text type ref to text255.

  append initial line to git_html reference into lrd_text.
  lrd_text->* = pfd_i_code.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_LOG_SEARCH->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] PIT_I_OBJECT                   TYPE        BAL_R_OBJ
* | [--->] PIT_I_SUBOBJECT                TYPE        BAL_R_SUB
* | [--->] PIT_I_ALDATE                   TYPE        BAL_R_DATE
* | [--->] PIT_I_ALUSER                   TYPE        BAL_R_USER
* | [--->] PFD_I_STEXT                    TYPE        ZBCD_STEXT
* | [--->] PFD_I_SINTE                    TYPE        ZBCD_SINTE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method constructor.

* Alan aktarımları
  gfd_stext = pfd_i_stext.
  gfd_sinte = pfd_i_sinte.

  gwa_lfil-object[]    = pit_i_object[].
  gwa_lfil-subobject[] = pit_i_subobject[].
  gwa_lfil-aldate[]    = pit_i_aldate[].
  gwa_lfil-aluser[]    = pit_i_aluser[].

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_LOG_SEARCH->GET_HTML
* +-------------------------------------------------------------------------------------------------+
* | [<-()] PIT_R_HTML                     TYPE        TTTEXT255
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_html.

  data:
    lrd_balhdr  type ref to balhdr,
    lrd_balm    type ref to t_balm.

* Temizlik
  refresh git_html.

* HTML :: Başlık
  append_html( '<!-- Dr. Kerem Koseoglu, Tesuji Consulting -->' ).
  append_html( '<html>' ).
  append_html( '<head>' ).
  append_html( '<title>Arama Sonuçları</title>' ).
  append_html( '<meta http -equiv="Content-Type" content="text/html; charset=UTF-8">' ).
  append_html( '<style type="text/css">' ).
  append_html( 'body { font-family: "Arial"; }' ) ##no_text.
  append_html( 'h1 { font-size: 14px; font-weight: bold; }' ).
  append_html( 'p.cerror { color: #ff0000; }' ).
  append_html( 'p.cwarning { color: orange; }' ).
  append_html( 'p.cinfo { color: #00aa00; }' ).
  append_html( 'p.error { color: #ff0000; font-weight: bold; font-size: 14px; }' ).
  append_html( 'p.warning { color: orange; font-weight: bold; font-size: 14px; }' ).
  append_html( 'p.info { color: #00aa00; font-weight:bold; font-size: 14px; }' ).
  append_html( 'td { padding: 4; font-size: 12px; }' ).
  append_html( '</style>' ).
  append_html( '</head>' ).
  append_html( '<body>' ).

* Her bir başlık için...
  append_html( '<table> ').

  loop at git_balhdr reference into lrd_balhdr.

*   HTML :: Arama başlığı
    append_html( '<tr>' ).
    append_html( '<td bgcolor=#cccccc valign=top>' ).
    append_html( '<h1>' ).
    append_html( lrd_balhdr->object ).
    append_html( '-' ).
    append_html( lrd_balhdr->subobject ).
    append_html( '</h1>' ).
    append_html( '<small>' ).
    append_html( lrd_balhdr->aldate ).
    append_html( lrd_balhdr->altime ).
    append_html( '<br>' ).
    append_html( lrd_balhdr->aluser ).
    append_html( '</small>' ).
    append_html( '</td>' ).
    append_html( '<td>' ).


*   HTML :: İletiler
    loop at git_balm_res reference into lrd_balm where balm-lognumber = lrd_balhdr->lognumber.

      append_html( get_p_tag( pfd_i_result = lrd_balm->result
                              pfd_i_msgty  = lrd_balm->balm-msgty ) ).
      append_html( lrd_balm->index ).
      append_html( `. ` ).
      append_html( lrd_balm->text ).
      append_html( '</p>' ).

    endloop.

*   HTML :: Arama başlığı kapanışı
    append_html( '</td></tr>   <tr><td></td><td>&nbsp;</td></tr> ').

  endloop. " BALHDR

* HTML :: Footer
  append_html( '</table>' ).

  if git_balhdr[] is initial.
    append_html( text-001 ).
  endif.

* İstatistik bölümü
  append_html( '<center><table>' ).
  append_html( '<tr><td align=left valign=middle bgcolor=#cccccc><b>').
  append_html( text-003 ).
  append_html( '</b></td><td align=left valign=middle bgcolor=#eeeeee>' ).
  append_html( sy-uname ).
  append_html( sy-datum ).
  append_html( sy-uzeit ).
  append_html( '</td></tr>' ).

  append_html( '<tr><td align=left valign=middle bgcolor=#cccccc><b>').
  append_html( text-005 ).
  append_html( '</b></td><td align=left valign=middle bgcolor=#eeeeee>' ).
  append_html( gfd_stext ).
  append_html( '</td></tr>' ).

  append_html( '</table></center><br>' ).

* Final
  append_html( '</body>' ).
  append_html( '</html>' ).

* Dönüş
  pit_r_html[] = git_html[].

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBCCL_LOG_SEARCH=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PIT_I_OBJECT                   TYPE        BAL_R_OBJ
* | [--->] PIT_I_SUBOBJECT                TYPE        BAL_R_SUB
* | [--->] PIT_I_ALDATE                   TYPE        BAL_R_DATE
* | [--->] PIT_I_ALUSER                   TYPE        BAL_R_USER
* | [--->] PFD_I_STEXT                    TYPE        ZBCD_STEXT
* | [--->] PFD_I_SINTE                    TYPE        ZBCD_SINTE
* | [<-()] PRC_R_SEARCH                   TYPE REF TO ZBCCL_LOG_SEARCH
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_instance.

  create object prc_r_search
    exporting
      pit_i_object    = pit_i_object
      pit_i_subobject = pit_i_subobject
      pit_i_aldate    = pit_i_aldate
      pit_i_aluser    = pit_i_aluser
      pfd_i_stext     = pfd_i_stext
      pfd_i_sinte     = pfd_i_sinte.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_LOG_SEARCH->GET_P_TAG
* +-------------------------------------------------------------------------------------------------+
* | [--->] PFD_I_RESULT                   TYPE        CHECKBOX
* | [--->] PFD_I_MSGTY                    TYPE        SYMSGTY
* | [<-()] PFD_R_TAG                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_p_tag.

  clear pfd_r_tag.

  case pfd_i_msgty.

    when zbccl_log=>c_msgty_a or zbccl_log=>c_msgty_e or zbccl_log=>c_msgty_x.
      if pfd_i_result eq abap_true.
        pfd_r_tag = '<p class=error>'.
      else.
        pfd_r_tag = '<p class=cerror>'.
      endif.

    when zbccl_log=>c_msgty_i or zbccl_log=>c_msgty_s.
      if pfd_i_result eq abap_true.
        pfd_r_tag = '<p class=info>'.
      else.
        pfd_r_tag = '<p class=cinfo>'.
      endif.

    when zbccl_log=>c_msgty_w.
      if pfd_i_result eq abap_true.
        pfd_r_tag = '<p class=warning>'.
      else.
        pfd_r_tag = '<p class=cwarning>'.
      endif.

    when others.
      assert id zbccg_ent condition 1 eq 0.
  endcase.

  assert id zbccg_ent condition pfd_r_tag is not initial.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_LOG_SEARCH->SEARCH
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_RECA_SYMSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method search.

* Başlık araması
  search__head( ).

* Bulduğumuz başlık sonuçları arasında ileti araması yapalım
  search__msg( ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_LOG_SEARCH->SEARCH__HEAD
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_RECA_SYMSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method search__head.

  data:
    lit_lognumber type standard table of szal_lognumber,
    lrd_lognumber type ref to szal_lognumber,

    lit_balm type tab_balm_nokey_sc,
    lrd_balm type ref to balm,

    lrc_cx_symsg type ref to cx_reca_symsg,

    lrd_balhdr type ref to balhdr,

    lwa_balmg type t_balm.

* Gönderilen parametrelere uygun Application Log kayıtlarını tespit edelim
  call function 'BAL_DB_SEARCH'
    exporting
      i_s_log_filter     = gwa_lfil
    importing
      e_t_log_header     = git_balhdr
    exceptions
      log_not_found      = 1
      no_filter_criteria = 2
      others             = 3.

  if sy-subrc <> 0.
    create object lrc_cx_symsg.
    lrc_cx_symsg->init_by_symsg( ).
    raise exception lrc_cx_symsg.
  endif.

* İletileri çekelim
  loop at git_balhdr reference into lrd_balhdr.
    append initial line to lit_lognumber reference into lrd_lognumber.
    lrd_lognumber->item = lrd_balhdr->lognumber.
  endloop.

  call function 'APPL_LOG_READ_DB_WITH_LOGNO'
    tables
      lognumbers = lit_lognumber
      messages   = lit_balm.

* BALM oluşumu
  loop at lit_balm reference into lrd_balm.
    clear lwa_balmg.
    move-corresponding lrd_balm->* to lwa_balmg-balm.

    message
        id     lwa_balmg-balm-msgid
        type   lwa_balmg-balm-msgty
        number lwa_balmg-balm-msgno
        with   lwa_balmg-balm-msgv1
               lwa_balmg-balm-msgv2
               lwa_balmg-balm-msgv3
               lwa_balmg-balm-msgv4
      into lwa_balmg-text.

    lwa_balmg-text_uc = lwa_balmg-text.
    translate lwa_balmg-text_uc to upper case.

    insert lwa_balmg into table git_balm.

  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_LOG_SEARCH->SEARCH__MSG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method search__msg.

  data:
    lfd_found   type checkbox,
    lfd_tabix   type sy-tabix,
    lfd_tabix_c type sy-tabix,

    lrd_balhdr  type ref to balhdr,

    lrd_balm    type ref to t_balm,
    lrd_balm_c  type ref to t_balm.

* Her bir başlık için...
  loop at git_balhdr reference into lrd_balhdr.

*   Temizlik
    clear lfd_found.

*   Her bir ileti için...
    loop at git_balm reference into lrd_balm where balm-lognumber = lrd_balhdr->lognumber.

*     Flag
      lfd_tabix = sy-tabix.

*     Bu ileti gönderilen Text'e uyuyor mu? Uymuyorsa, yapacak bir şey yok
      find first occurrence of gfd_stext in lrd_balm->text_uc.
      check sy-subrc eq 0.

*     Flag
      lfd_found = abap_true.

*     Bu iletinin x ileti öncesini ekleyelim
      lfd_tabix_c = lfd_tabix - gfd_sinte - 1.

      do gfd_sinte times.
        add 1 to lfd_tabix_c.
        check lfd_tabix_c gt 0.

        read table git_balm reference into lrd_balm_c index lfd_tabix_c.
        check lrd_balm_c->balm-lognumber eq lrd_balhdr->lognumber.

        search__msg__add_result( prd_i_balm   = lrd_balm_c
                                 pfd_i_index  = lfd_tabix_c
                                 pfd_i_result = abap_false ).

      enddo.

*     Bu iletinin kendisini ekleyelim
      search__msg__add_result( prd_i_balm   = lrd_balm
                               pfd_i_index  = lfd_tabix
                               pfd_i_result = abap_true ).

*     Bu iletinin x ileti sonrasını ekleyelim
      lfd_tabix_c = lfd_tabix.

      do gfd_sinte times.
        add 1 to lfd_tabix_c.

        read table git_balm reference into lrd_balm_c index lfd_tabix_c.
        check lrd_balm_c->balm-lognumber eq lrd_balhdr->lognumber.

        search__msg__add_result( prd_i_balm   = lrd_balm_c
                                 pfd_i_index  = lfd_tabix_c
                                 pfd_i_result = abap_false ).

      enddo.

    endloop. " BALM

*   Arama kriterlerine uyan hiçbir mesaj yoksa, bu başlık bizi ilgilendirmiyor
    if lfd_found is initial.
      delete git_balhdr.
      continue.
    endif.

  endloop. " BALHDR

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBCCL_LOG_SEARCH->SEARCH__MSG__ADD_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PRD_I_BALM                     TYPE REF TO T_BALM
* | [--->] PFD_I_INDEX                    TYPE        SYTABIX
* | [--->] PFD_I_RESULT                   TYPE        CHECKBOX
* +--------------------------------------------------------------------------------------</SIGNATURE>
method search__msg__add_result.

  data:
    lrd_balm type ref to t_balm.

* Daha önce eklediysek yapacak bir şey yok
  read table git_balm_res reference into lrd_balm
    with key balm-lognumber = prd_i_balm->balm-lognumber
             balm-msgnumber = prd_i_balm->balm-msgnumber.

  if sy-subrc eq 0.
    if lrd_balm->result is initial and pfd_i_result is not initial.
      lrd_balm->result = pfd_i_result.
    endif.

    return.
  endif.

* Ek yapalım
  insert prd_i_balm->* into table git_balm_res reference into lrd_balm.
  lrd_balm->index  = pfd_i_index.
  lrd_balm->result = pfd_i_result.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBCCL_LOG_SEARCH->WRITE_RESULTS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method write_results.

  data:
    lfd_icon type icon_d,

    lrd_balhdr  type ref to balhdr,
    lrd_balm    type ref to t_balm.

* Her bir başlık için...
  loop at git_balhdr reference into lrd_balhdr.

*   Başlık bilgilerinin yazdırılması
    format color col_heading on.

    new-line.
    write:
      lrd_balhdr->object,
      ` - `,
      lrd_balhdr->subobject,
      ` `,
      lrd_balhdr->aldate,
      ` `,
      lrd_balhdr->altime,
      ` `,
      lrd_balhdr->aluser.

    format color col_heading off.

*   Bu başlık altındaki her bir kalem için...
    loop at git_balm_res reference into lrd_balm where balm-lognumber = lrd_balhdr->lognumber.

*     Kalem bilgilerinin yazdırılması
      lfd_icon = zbccl_log=>get_msgty_icon( lrd_balm->balm-msgty ).

      new-line.

      write:
        lfd_icon,
        lrd_balm->index,
        lrd_balm->text.

    endloop.

*   Çizgi
    new-line.
    write sy-uline.

  endloop.

* Arama sonucu yoksa belirtelim
  if sy-subrc ne 0.
    new-line.
    write text-001.
  endif.

* İstatistik bölümü
  format color col_total on.

  new-line.
  write:
    text-003,
    sy-uname,
    sy-datum,
    sy-uzeit.

  new-line.
  write:
    text-005,
    gfd_stext.

  format color col_total off.

endmethod.
endclass.
