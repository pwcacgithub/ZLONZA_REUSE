
##needed
TABLES nast.
TYPES: BEGIN OF ty_smtp_addr,
         email TYPE ad_smtpadr,
       END OF ty_smtp_addr.
TYPES: BEGIN OF ty_sd_docs,
         vgbel TYPE vgbel,
       END OF ty_sd_docs.
TYPES: BEGIN OF ty_po_docs,
         bstkd TYPE bstkd,
         bstdk TYPE bstdk,
       END OF ty_po_docs.
##needed
DATA: gs_sd_docs TYPE ty_sd_docs,
      gt_sd_docs TYPE TABLE OF ty_sd_docs,
      gs_po_docs TYPE ty_po_docs,
      gt_po_docs TYPE TABLE OF ty_po_docs,
      gt_nast    TYPE STANDARD TABLE OF nast,
      gs_nast    TYPE nast.

DATA: go_mr_api TYPE REF TO if_mr_api.
##needed
DATA gv_folder TYPE boole_d.
##needed
DATA: gx_current  TYPE xstring,
      gx_current1 TYPE xstring,
      gx_current2 TYPE xstring.
##needed
DATA: gv_length   TYPE i,
      gv_len2     TYPE i,
      gv_po_lines TYPE i.
##needed
DATA: gs_loio      TYPE skwf_io,
      gv_b64data   TYPE  string,
      gv_path      TYPE rlgrap-filename,
      gv_img_path  TYPE string,
      gv_img       TYPE string,
      gv_img_name  TYPE string,
      gv_img_type  TYPE string,
      gv_img_size  TYPE sood-objlen,
      gv_tdformat  TYPE tdformat,
      gv_std_txt   TYPE thead-tdname,
      gv_fedex_no  TYPE string,
      gv_fedex_url TYPE string,
      gv_ship_date TYPE string.
##needed
DATA: gt_mail_body TYPE soli_tab,
      gs_mail_body TYPE soli,
      go_document  TYPE REF TO cl_document_bcs VALUE IS INITIAL, "document object
      go_bcs       TYPE REF TO cl_bcs VALUE IS INITIAL,
      go_sender    TYPE REF TO if_sender_bcs VALUE IS INITIAL, "sender
      go_receiver  TYPE REF TO if_recipient_bcs VALUE IS INITIAL. "recipient
##needed
DATA :gt_hex TYPE solix_tab,
      gs_hex LIKE LINE OF gt_hex.
##needed
DATA: gv_result      TYPE boolean,
      gt_recipients  TYPE TABLE OF uiys_iusr,
      gs_recipients  TYPE uiys_iusr,
      gt_attachments TYPE rmps_t_post_content,
      gs_attachments LIKE LINE OF gt_attachments.
##needed
DATA: gt_lines  TYPE TABLE OF tline,
      gs_lines  TYPE tline,
      gt_lines2 TYPE TABLE OF tline,
      gs_lines2 TYPE tline.
##needed
DATA: retcode     LIKE sy-subrc,         "Returncode
      return_code LIKE sy-subrc.
##needed
DATA: repeat(1) TYPE c.
##needed
DATA: xscreen(1)   TYPE c,               "Output on printer or screen
      us_screen(1) TYPE c.
##needed
DATA: gv_kvgr4       TYPE knvv-kvgr4,
      gv_addr_no(10) TYPE c,
      gv_smtp_addr1  TYPE ad_smtpadr,
      gv_smtp_addr2  TYPE ad_smtpadr,
      gt_smtp_addr   TYPE TABLE OF ty_smtp_addr,
      gs_smtp_addr   TYPE ty_smtp_addr,
      gv_csr_email   TYPE ad_smtpadr,
      gv_po_no       TYPE string,
      gv_po_date     TYPE bstdk,
*      gs_vbak TYPE vbak,
      gs_likp        TYPE likp,
      gv_csr_name    TYPE ad_name1,
      gv_csr_tel     TYPE ad_tlnmbr1,
      gv_send_flag   TYPE boolean,
      gv_kunnr       TYPE kunnr,
      gv_std_text    TYPE thead-tdname,
      gv_kschl       TYPE nast-kschl,
      gv_vstat       TYPE nast-vstat,
      gv_sform       TYPE na_fname.
##needed
DATA: gv_sender_mail      TYPE adr6-smtp_addr,
      gv_recipient_mail   TYPE adr6-smtp_addr,
      gv_subject          TYPE string,
      gv_subject1         TYPE so_obj_des,
      gv_language         TYPE thead-tdspras,
      gv_receipient_lines TYPE i,
      gv_error_msg        TYPE string.

*Class for cobining HMTL & Image
##needed
DATA : go_mime_helper     TYPE REF TO cl_gbt_multirelated_service.

##needed
FIELD-SYMBOLS: <fs_mail_body> TYPE soli.


CONSTANTS: c_st       TYPE thead-tdid VALUE 'ST',
           c_language TYPE thead-tdspras VALUE 'E',
           c_std_txt1 TYPE thead-tdname VALUE 'Z_SHIPPIN_NOTIF_TXT',
           c_object   TYPE thead-tdobject VALUE 'TEXT',
           c_p1       TYPE tdformat VALUE 'P1',
           c_p2       TYPE tdformat VALUE 'P2',
           c_p3       TYPE tdformat VALUE 'P3',
           c_i1       TYPE tdformat VALUE 'I1',
           c_i2       TYPE tdformat VALUE 'I2',
           c_p5       TYPE tdformat VALUE 'P5',
           c_cp       TYPE parvw VALUE 'AP',
           c_cs       TYPE parvw VALUE 'CS',
           c_sn       TYPE parvw VALUE 'SN',
           c_z3       TYPE parvw VALUE 'Z3',
           c_fslash   TYPE c VALUE '/',
           c_001      TYPE kvgr4 VALUE '001',
           c_002      TYPE kvgr4 VALUE '002',
           c_003      TYPE kvgr4 VALUE '003',
           c_sne      TYPE sna_kschl VALUE 'ZDSE',
           c_sns      TYPE sna_kschl VALUE 'ZDSS',
           c_e        TYPE c VALUE 'E',
           c_s        TYPE c VALUE 'S',
           c_x        TYPE c VALUE 'X',
           c_kschl_pl TYPE sna_kschl VALUE 'ZXPD',
           c_nacha_pl TYPE na_nacha VALUE '1',
           c_kappl_pl TYPE kappl VALUE 'V2'.


DATA: gs_nast_pl  TYPE nast,
      gs_tnapr_pl TYPE tnapr.
