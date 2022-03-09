*&---------------------------------------------------------------------*
*& Report Z_SHIPPING_NOTIFIC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SHIPPING_NOTIFIC.

INCLUDE zotce0065_shipping_top.       "Shipping Email/Image Declarations

FORM entry USING return_code TYPE i ##CALLED
                 us_screen TYPE c.

  CLEAR retcode.
  xscreen = us_screen.
  PERFORM processing.
  IF retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.

ENDFORM.                    "ENTRY

*&---------------------------------------------------------------------*
*&      Form  processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM processing.

  DATA: lv_po_no     TYPE string,
        lv_csr_name  TYPE string,
        lv_csr_tel   TYPE string,
        lv_csr_email TYPE string,
        lv_po_date   TYPE string,
        lv_erdat     TYPE string.

  DATA : lv_wbstk TYPE wbstk.

  CONSTANTS : lc_c  TYPE wbstk VALUE 'C',
              lc_bh TYPE vsbed VALUE 'BH'.

  DATA: lv_so_adrnr   TYPE tvko-adrnr,
        lv_ad_uri     TYPE string, "adr12-uri_addr,
        lv_url_string TYPE string VALUE 'www.capsugel.com/sales/terms-and-cond-us'.


  DATA: lv_vbeln    TYPE likp-vbeln,
        lt_otf      TYPE tsfotf,
        lv_msg      TYPE bapi_msg,
        lv_len      TYPE sood-objlen,
        gt_goscofa  TYPE rmps_t_post_content,
        gs_goscofa  TYPE rmps_post_content,
        lt_solix    TYPE solix_tab,
        lv_xtring   TYPE xstring,
        ls_doc_data TYPE sodocchgi1,
        lv_key      TYPE swo_typeid,                   " Key
        lv_desc     TYPE so_obj_des.                   " Test pdf documents
  CONSTANTS: lc_typeid    TYPE swo_objtyp VALUE 'LIKP'.


  CLEAR: gv_kvgr4, gv_addr_no, gv_smtp_addr1, gv_smtp_addr2, gv_csr_email,
         gv_po_no, gv_po_date, gs_likp, gs_mail_body, gv_csr_name, gv_csr_tel.
  REFRESH gt_mail_body.

  CREATE OBJECT go_mime_helper.

  CLEAR: gs_likp, gv_vstat, gs_nast.
  CLEAR: gs_nast_pl, gs_tnapr_pl.
  REFRESH gt_nast.
  CLEAR: gv_smtp_addr1,
         gv_smtp_addr2.
  REFRESH: gt_smtp_addr.

  SELECT *
    FROM nast
    INTO TABLE gt_nast
    WHERE kappl = nast-kappl AND
          objky = nast-objky AND
          kschl = nast-kschl AND
          spras = nast-spras AND
          parnr = nast-parnr AND
          parvw = nast-parvw.
  IF sy-subrc EQ 0.
    SORT gt_nast BY erdat DESCENDING eruhr DESCENDING.
    READ TABLE gt_nast INTO gs_nast INDEX 1.
    gv_vstat = gs_nast-vstat.
  ENDIF.

  IF nast-vsztp NE '4'.
    IF nast-manue EQ abap_true AND gv_vstat EQ '1'.
      MESSAGE text-026 TYPE 'E'.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM likp INTO gs_likp WHERE vbeln = nast-objky.

  SELECT vgbel FROM lips INTO TABLE gt_sd_docs WHERE vbeln = nast-objky.
  IF sy-subrc EQ 0.
    SORT gt_sd_docs.
    DELETE ADJACENT DUPLICATES FROM gt_sd_docs.

    SELECT bstkd bstdk
      FROM vbkd
      INTO TABLE gt_po_docs
      FOR ALL ENTRIES IN gt_sd_docs
      WHERE vbeln = gt_sd_docs-vgbel.

    READ TABLE gt_sd_docs INTO gs_sd_docs INDEX 1.
  ENDIF.

  IF sy-subrc EQ 0.

    PERFORM get_cp_email CHANGING gt_smtp_addr.

*   Get CSR Details (Name,email,Tel No)
    CLEAR: gv_addr_no, gv_csr_name, gv_csr_tel, gv_csr_email.
    SELECT SINGLE adrnr FROM vbpa INTO gv_addr_no WHERE vbeln = gs_likp-vbeln AND parvw = c_z3. "#EC WARNOK
    IF sy-subrc <> 0.
      SELECT SINGLE addrnumber FROM v_soucadcp INTO gv_addr_no WHERE sapnam = gs_likp-ernam.
      IF sy-subrc <> 0.
        CLEAR gv_addr_no.
      ENDIF.
    ENDIF.

    IF gv_addr_no IS NOT INITIAL.
      SELECT SINGLE smtp_addr FROM adr6 INTO gv_csr_email WHERE addrnumber = gv_addr_no. "#EC WARNOK
      IF sy-subrc <> 0.
        CLEAR gv_csr_email.
      ENDIF.
      SELECT SINGLE name1 tel_number FROM adrc INTO (gv_csr_name, gv_csr_tel)
        WHERE addrnumber = gv_addr_no.
      IF sy-subrc <> 0.
        CLEAR : gv_csr_name, gv_csr_tel.
      ENDIF.
    ENDIF.

  ENDIF.

  gv_language = nast-spras.
  IF gv_language IS INITIAL.
    gv_language = c_language.
  ENDIF.

  CLEAR gv_kschl.
  gv_kschl = nast-kschl.

  gv_std_txt = c_std_txt1.

* Data Fetch logic for the details inside the table of email body.
  IF gv_kschl IS NOT INITIAL.

    REFRESH: gt_lines.

*--- Read the body of the email from standard text so10.------
    ##fm_subrc_ok
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = c_st
        language                = gv_language
        name                    = gv_std_txt
        object                  = c_object
      TABLES
        lines                   = gt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

  ENDIF.

  IF gt_lines IS NOT INITIAL.

    CLEAR gs_lines.
    LOOP AT gt_lines INTO gs_lines WHERE tdformat EQ c_p1. "'P1'.
      gs_mail_body = gs_lines-tdline.
      APPEND gs_mail_body TO gt_mail_body.
    ENDLOOP.

    IF go_mr_api IS INITIAL.

      go_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    ENDIF.

    CLEAR: gs_mail_body, gs_lines, gv_path, gv_img_name, gv_img_type,
           gv_b64data, gv_folder, gx_current, gs_loio, gs_hex,
           gv_length, gv_len2, gv_tdformat.
    REFRESH: gt_hex.

*------------Image1 - start-------------------------------
* gv_path1 = '/SAP/PUBLIC/ABAP/order_shipment.JPG'.
    gv_tdformat = c_i1. "'I1'.
    PERFORM image_embed_logic USING gv_tdformat.
*------------Image1 - End------------------------------

    CLEAR gs_mail_body.
* Take FedEx Number.
    gv_fedex_no = gs_likp-bolnr.
    CONDENSE gv_fedex_no.

* Delete the Tracking Number row from table
    IF gv_fedex_no IS INITIAL.
      DELETE gt_lines WHERE tdformat EQ c_p5.
    ENDIF.

*Build the email body table first part
    LOOP AT gt_lines INTO gs_lines WHERE tdformat EQ c_p2 OR tdformat EQ c_p5.
      gs_mail_body = gs_lines-tdline.
      APPEND gs_mail_body TO gt_mail_body.
    ENDLOOP.

    CLEAR: gs_mail_body, gs_lines, gv_path, gv_img_name, gv_img_type,
           gv_b64data, gv_folder, gx_current, gs_loio, gs_hex,
           gv_length, gv_len2, gv_tdformat.
    REFRESH: gt_hex.

*------------Image2 - start----------------------------
*gv_path2 = '/SAP/PUBLIC/ABAP/banner3.JPG'.
    gv_tdformat = c_i2.
    PERFORM image_embed_logic USING gv_tdformat.
*------------Image2 - End------------------------------

    CLEAR gs_mail_body.
*Build the email body table second part
    LOOP AT gt_lines INTO gs_lines WHERE tdformat EQ c_p3. "'P3'.
      gs_mail_body = gs_lines-tdline.
      APPEND gs_mail_body TO gt_mail_body.
    ENDLOOP.

    CONDENSE: gv_po_no, gv_csr_name, gv_csr_tel, gv_csr_email,
              gv_smtp_addr1, gv_smtp_addr2.

    lv_po_no      = gv_po_no.
    lv_csr_name   = gv_csr_name.
    lv_csr_tel    = gv_csr_tel.
    lv_csr_email  = gv_csr_email.

    CONDENSE: lv_po_no, lv_csr_name, lv_csr_tel, lv_csr_email.
*Format the items in the table display.
    CONCATENATE gv_po_date+4(2) gv_po_date+6(2) gv_po_date+0(4) INTO lv_po_date SEPARATED BY '/'.

**TABLE contents replacements
    CLEAR gv_po_lines.
    DESCRIBE TABLE gt_po_docs LINES gv_po_lines.

    LOOP AT gt_mail_body ASSIGNING <fs_mail_body>.
      IF gv_po_lines GT 1.
        CLEAR gs_po_docs.
        SEARCH <fs_mail_body> FOR text-001.
        IF sy-subrc EQ 0.
          LOOP AT gt_po_docs INTO gs_po_docs.
            CLEAR gv_po_no.
            gv_po_no = gs_po_docs-bstkd.
            CONDENSE gv_po_no.
            IF sy-tabix EQ 1.
              REPLACE ALL OCCURRENCES OF text-001 IN <fs_mail_body> WITH gv_po_no .
              CONCATENATE <fs_mail_body> '<br>' INTO <fs_mail_body>.
            ELSE.
              CONCATENATE <fs_mail_body> '<B>' gv_po_no '</B>' '<br>' INTO <fs_mail_body>.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        CLEAR: gs_po_docs, gv_po_no.
        READ TABLE gt_po_docs INTO gs_po_docs INDEX 1.
        gv_po_no = gs_po_docs-bstkd.
        REPLACE ALL OCCURRENCES OF text-001 IN <fs_mail_body> WITH gv_po_no .
      ENDIF.

      CONDENSE gs_likp-vbeln.
      REPLACE ALL OCCURRENCES OF text-002 IN <fs_mail_body> WITH gs_likp-vbeln .

      CLEAR gv_ship_date.
      IF gs_likp-wadat_ist CN '00000000'.
        CONCATENATE gs_likp-wadat_ist+4(2) gs_likp-wadat_ist+6(2) gs_likp-wadat_ist+0(4) INTO gv_ship_date SEPARATED BY '/'.
        REPLACE ALL OCCURRENCES OF text-003 IN <fs_mail_body> WITH gv_ship_date.
      ELSE.
        REPLACE ALL OCCURRENCES OF text-003 IN <fs_mail_body> WITH ' ' .
      ENDIF.

      CONDENSE gv_csr_name.

      IF gv_fedex_no IS NOT INITIAL.
        CONCATENATE'https://www.fedex.com/apps/fedextrack/?tracknumbers=' gv_fedex_no '&cntry_code=us' INTO gv_fedex_url.
        CONDENSE gv_fedex_url.
        REPLACE ALL OCCURRENCES OF text-023 IN <fs_mail_body> WITH gv_fedex_url .
        REPLACE ALL OCCURRENCES OF text-024 IN <fs_mail_body> WITH gv_fedex_no .
      ELSE.
        REPLACE ALL OCCURRENCES OF text-025 IN <fs_mail_body> WITH '' .

      ENDIF.

      CLEAR gv_fedex_url.

      IF gv_fedex_no IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF text-013 IN <fs_mail_body> WITH gv_fedex_no .
      ELSE.
        REPLACE ALL OCCURRENCES OF text-013 IN <fs_mail_body> WITH 'N/A'.
      ENDIF.

      REPLACE ALL OCCURRENCES OF text-014 IN <fs_mail_body> WITH gv_fedex_url .

      IF gv_csr_name IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF text-005 IN <fs_mail_body> WITH lv_csr_name .
      ELSE.
        REPLACE ALL OCCURRENCES OF text-005 IN <fs_mail_body> WITH ' ' .
      ENDIF.

      IF gv_csr_tel IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF text-006 IN <fs_mail_body> WITH lv_csr_tel .
      ELSE.
        REPLACE ALL OCCURRENCES OF text-006 IN <fs_mail_body>  WITH ' ' .
      ENDIF.

      IF gv_csr_email IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF text-007 IN <fs_mail_body> WITH lv_csr_email .
      ELSE.
        REPLACE ALL OCCURRENCES OF text-007 IN <fs_mail_body> WITH ' ' .
      ENDIF.

    ENDLOOP.

    CLEAR: gv_subject, gv_sender_mail, gs_recipients, gv_recipient_mail,
           gv_receipient_lines, gv_send_flag, gv_po_no, gs_po_docs.
    REFRESH: gt_recipients, gt_attachments.

    READ TABLE gt_po_docs INTO gs_po_docs INDEX 1.
    gv_po_no = gs_po_docs-bstkd.
    CONDENSE gv_po_no.
    IF gv_po_lines EQ 1.
      IF gv_kschl EQ c_sne.
        IF gv_language = c_e.
          gv_subject = text-017.
        ELSEIF gv_language = c_s.
          gv_subject = text-027.
        ENDIF.
      ENDIF.
      CONCATENATE gv_subject ' ' text-015 ' ' gv_po_no INTO gv_subject RESPECTING BLANKS.
    ELSEIF gv_po_lines GT 1.
      IF gv_kschl EQ c_sne.
        IF gv_language = c_e.
          gv_subject = text-017.
        ELSEIF gv_language = c_s.
          gv_subject = text-027.
        ENDIF.
      ENDIF.
      CONCATENATE gv_subject ' ' text-015 ' ' gv_po_no ' + '  INTO gv_subject RESPECTING BLANKS.
    ENDIF.

* Sender Email Address
    gv_sender_mail = gv_csr_email.

* Recipient Email Address
    LOOP AT gt_smtp_addr INTO gs_smtp_addr.
      gs_recipients-email = gs_smtp_addr-email.
      APPEND gs_recipients TO gt_recipients.
    ENDLOOP.

    DELETE gt_recipients WHERE email IS INITIAL.
    DESCRIBE TABLE gt_recipients LINES gv_receipient_lines.

    CLEAR gv_error_msg.
    gv_send_flag = abap_true.

    SELECT SINGLE kunnr INTO gv_kunnr
       FROM vbpa WHERE vbeln = nast-objky
                   AND parvw = c_cs .
    IF sy-subrc NE 0.
      SELECT SINGLE kunnr INTO gv_kunnr
         FROM vbpa WHERE vbeln = nast-objky
                     AND parvw = c_sn .
      IF sy-subrc NE 0.
        gv_send_flag = abap_false.
        gv_error_msg = text-029.
      ENDIF.
    ENDIF.

    IF gv_send_flag = abap_true.

* Check if goods movement is completely processed or not.
      SELECT SINGLE wbstk
        FROM vbuk
        INTO lv_wbstk
        WHERE vbeln = nast-objky.
      IF sy-subrc = 0.
        IF lv_wbstk = lc_c.
* Check the delivery has the PGI reversed or not
          IF gs_nast-anzal = space .
* Check the shipping condition
            IF gs_likp-vsbed = lc_bh.
              gv_send_flag = abap_false.
            ENDIF.
          ELSE.
            gv_send_flag = abap_false.
          ENDIF.
        ELSE.
          gv_send_flag = abap_false.
        ENDIF.
      ENDIF.

    ENDIF.

* Create main HTML body
    CALL METHOD go_mime_helper->set_main_html
      EXPORTING
        content = gt_mail_body.

    IF gv_send_flag = abap_true.

* Build the logic for pack list and attach it to the email.
      PERFORM attach_pack_list.

      gv_subject1 = gv_subject.
*--Call the Function Module to send the email

      CALL FUNCTION 'Z_ORDER_EMAIL_FUNCTION'
        EXPORTING
          doc_no         = nast-objky
          identifier     = '3'           " Delivery: Shipping Notification
          subject        = gv_subject1
          message_body   = gt_mail_body
          attachments    = gt_attachments
          sender_mail    = gv_sender_mail
          recipient_mail = gv_recipient_mail
          o_mime_helper  = go_mime_helper
          recipients     = gt_recipients
        IMPORTING
          result         = gv_result
        EXCEPTIONS
          error_message  = 99.

      IF sy-subrc EQ 0 AND gv_result = abap_true.

*-- No COMMIT WORK should put in an update function module
*-- if dispatch time is Type 4.
        IF nast-vsztp <> '4'.  " Send immediately (when saving the application)
          COMMIT WORK AND WAIT.
        ENDIF.

        IF nast-cmfpnr IS NOT INITIAL..
          CALL FUNCTION 'NAST_PROTOCOL_DELETE'
            EXPORTING
              nr                    = nast-cmfpnr
            EXCEPTIONS
              called_without_values = 1
              OTHERS                = 2.
        ENDIF.


        CLEAR: lv_vbeln, lv_msg, lv_key, lv_desc, lt_otf[], lv_len.
        IMPORT lv_key TO lv_key FROM MEMORY ID 'SWOTYPEID'.
        IMPORT lv_desc TO lv_desc FROM MEMORY ID 'DESCRIPTION'.
        IMPORT gv_len_in TO lv_len FROM MEMORY ID 'LENGTH'.
        IMPORT lt_otf TO lt_otf[] FROM MEMORY ID 'OTFDATA'.
        IF sy-subrc EQ 0 AND nast-kschl EQ c_sne.
          CALL FUNCTION 'Z_SD_GOS_ATTACHMENT_CREATE'" STARTING NEW TASK lv_task DESTINATION 'NONE'
            EXPORTING
              iv_key    = lv_key
              iv_len_in = lv_len
              iv_type   = lc_typeid
              it_otf    = lt_otf[]
              iv_desc   = lv_desc
              iv_vsztp  = nast-vsztp
            IMPORTING
              es_return = lv_msg.
        ENDIF.
        CLEAR: gt_goscofa[], lt_solix[], lv_xtring, ls_doc_data, lv_vbeln.

        IMPORT gt_goscofa TO gt_goscofa[] FROM MEMORY ID 'COFAGOS'.
        IMPORT gv_vbeln TO lv_vbeln FROM MEMORY ID 'VBELN'.
        IF sy-subrc EQ 0 AND nast-kschl EQ c_sne.
          LOOP AT gt_goscofa INTO gs_goscofa.
            CLEAR: lt_solix[], lv_xtring, ls_doc_data, lv_msg.
            lt_solix[] = gs_goscofa-cont_hex[].
* Convert binary solix to xstring
            lv_xtring = cl_bcs_convert=>solix_to_xstring( it_solix = lt_solix[] ).
* Fill Doc Data
            ls_doc_data-doc_size = xstrlen( lv_xtring ).
            ls_doc_data-obj_descr = gs_goscofa-subject.
            ls_doc_data-obj_name =  'COFA'.

            CALL FUNCTION 'Z_SD_GOS_SOLIX'
              EXPORTING
                iv_doc_data = ls_doc_data
                iv_doc_type = gs_goscofa-objtp
                it_solix    = lt_solix[]
                iv_vbeln    = lv_vbeln
                iv_vsztp    = nast-vsztp
              IMPORTING
                es_return   = lv_msg.

          ENDLOOP.
        ENDIF.

      ENDIF.

      IF sy-subrc = 0.
        CLEAR nast-cmfpnr.
        CALL FUNCTION 'RV_MESSAGE_UPDATE_SINGLE'
          EXPORTING
            msg_nast = nast.
      ENDIF.

    ELSE.

      retcode = 1. "Used to set the error flag.

      IF gv_error_msg IS NOT INITIAL.

        IF xscreen = space.
          syst-msgid = 'SD'.
          syst-msgno = 024.
          syst-msgty = 'E'.
          syst-msgv1 = gv_error_msg.

          CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
            EXPORTING
              msg_arbgb = syst-msgid
              msg_nr    = syst-msgno
              msg_ty    = syst-msgty
              msg_v1    = syst-msgv1
            EXCEPTIONS
              OTHERS    = 0.
        ENDIF.

        MESSAGE gv_error_msg TYPE c_e.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    "PROCESSING

*&---------------------------------------------------------------------*
*&      Form  IMAGE_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gv_path  text
*----------------------------------------------------------------------*
FORM image_processing  USING  pv_path TYPE csequence.

  CLEAR: gv_folder, gx_current, gs_loio, gv_b64data.

  CALL METHOD go_mr_api->get
    EXPORTING
      i_url              = pv_path
      i_check_authority  = space
    IMPORTING
      e_is_folder        = gv_folder
      e_content          = gx_current
      e_loio             = gs_loio
    EXCEPTIONS
      parameter_missing  = 1
      error_occured      = 2
      not_found          = 3
      permission_failure = 4
      OTHERS             = 5.
  IF sy-subrc = 0.
*gx_current will hold the image in a XSTRING
**Convert the binary image data into Base64
    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata       = gx_current
      IMPORTING
        b64data       = gv_b64data
      EXCEPTIONS
        error_message = 99.
    IF sy-subrc <> 0.
      CLEAR gv_b64data.
    ENDIF.
  ENDIF.

ENDFORM.                    " IMAGE_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  ADD_IMAGE_B64
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_B64DATA_temp  text
*----------------------------------------------------------------------*
##called
*&---------------------------------------------------------------------*
*&      Form  add_image_b64
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_B64DATA   text
*      -->PV_IMG_NAME  text
*      -->PV_IMG_TYPE  text
*----------------------------------------------------------------------*
FORM add_image_b64  USING pv_b64data TYPE string
                      pv_img_name TYPE string
                      pv_img_type TYPE string.

  CLEAR gs_mail_body.
*Add image Base64 content

  CONCATENATE text-009
              pv_img_name
              c_fslash
              pv_img_type
              text-010
              INTO gs_mail_body.
  CONDENSE gs_mail_body.

  APPEND gs_mail_body TO gt_mail_body.
  CLEAR gs_mail_body.

  gv_length = strlen( pv_b64data ).
  gv_len2 = gv_length / 255.

  gs_mail_body = pv_b64data.

  APPEND gs_mail_body TO gt_mail_body.
  CLEAR gs_mail_body.

  DATA lv_len3 TYPE i.

  DO gv_len2 TIMES.
    lv_len3 = 255 * sy-index.

    IF lv_len3 <= gv_length.
      gs_mail_body = gv_b64data+lv_len3.
      IF gs_mail_body IS NOT INITIAL.
        APPEND gs_mail_body TO gt_mail_body.
        CLEAR gs_mail_body.
      ELSE.
        EXIT.
      ENDIF.
    ELSEIF lv_len3 > gv_length.

      EXIT.

    ENDIF.
  ENDDO.

  gs_mail_body = '"alt="Order Status" align="middle" /><br><br>'.
  APPEND gs_mail_body TO gt_mail_body.

ENDFORM.                    " ADD_IMAGE_B64

*&---------------------------------------------------------------------*
*&      Form  ADD_IMAGE_SOLIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_IMG  text
*      -->PV_IMG_NAME  text
*      -->PV_IMG_TYPE  text
*----------------------------------------------------------------------*
FORM add_image_solix  USING pv_img TYPE string
                            pv_img_name TYPE string
                            pv_img_type TYPE string.


  "Convert Image to Xstring table form
  "Image to Xstring Table form
  DATA : lv_obj_len        TYPE so_obj_len,
         lv_graphic_length TYPE tdlength,
         gr_xstr           TYPE xstring,
         lv_offset         TYPE i,
         lv_length         TYPE i,
         lv_diff           TYPE i,
         ls_solix          TYPE solix,
         lt_solix          TYPE solix_tab.
  DATA: lv_extension TYPE so_fileext.

*    Attach image to HTML body
  DATA: lv_filename     TYPE string,
        lv_content_id   TYPE string,
        lv_content_type TYPE w3conttype.

  lv_obj_len = xstrlen( gx_current ).
  lv_graphic_length = xstrlen( gx_current ).

  "get whole image
  CLEAR gr_xstr.
  gr_xstr = gx_current(lv_obj_len).

  lv_offset = 0.
  lv_length = 255.

  CLEAR lt_solix[].
  WHILE lv_offset < lv_graphic_length.
    lv_diff = lv_graphic_length - lv_offset.
    IF lv_diff > lv_length.
      ls_solix-line = gr_xstr+lv_offset(lv_length).
    ELSE.
      ls_solix-line = gr_xstr+lv_offset(lv_diff).
    ENDIF.
    APPEND ls_solix TO lt_solix.
    ADD lv_length TO lv_offset.
  ENDWHILE.

  lv_extension = pv_img_type.

*  Attach image to HTML body
  lv_filename = pv_img.
  lv_content_id = pv_img.

  lv_content_type = 'image/jpg*'. " Maintained in SWM0

  CALL METHOD go_mime_helper->add_binary_part
    EXPORTING
      content      = lt_solix                    "Xstring in table form
      filename     = lv_filename                 "file name to be given to image
      extension    = lv_extension                "type of file
      description  = text-016                    "description
      content_type = lv_content_type             "content type / Mime type. If mime type not present in system then need to add through tcode : SMW0
      length       = lv_obj_len                  "length of image
      content_id   = lv_content_id.              "content id would be used in html part

  CONCATENATE '<img alt="[image]" src="cid:' lv_content_id '" /><br>' INTO gs_mail_body.
  APPEND gs_mail_body TO gt_mail_body.

ENDFORM.                    " ADD_IMAGE_SOLIX

*&---------------------------------------------------------------------*
*&      Form  IMAGE_EMBED_LOGIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_TDFORMAT  text
*----------------------------------------------------------------------*
FORM image_embed_logic  USING pv_tdformat TYPE tdformat.

  CLEAR: gv_img_path, gv_img, gv_img_name, gv_img_type, gs_lines.

  READ TABLE gt_lines INTO gs_lines WITH KEY tdformat = pv_tdformat.

  IF sy-subrc EQ 0.
    gv_path = gs_lines-tdline.

    CALL FUNCTION 'CACS_SPLIT_PATH'
      EXPORTING
        i_path     = gv_path
      IMPORTING
        e_path     = gv_img_path
        e_filename = gv_img.
    ##fm_subrc_ok
    IF sy-subrc EQ 0.
      SPLIT gv_img AT '.' INTO gv_img_name gv_img_type.
    ENDIF.
  ENDIF.

*******Image Process logic.*****
  PERFORM image_processing USING gv_path.

  PERFORM add_image_solix USING gv_img
                                gv_img_name
                                gv_img_type.


  WHILE gx_current IS NOT INITIAL.
    gs_hex-line = gx_current.
    APPEND gs_hex TO gt_hex.
    SHIFT gx_current LEFT BY 255 PLACES IN BYTE MODE.
  ENDWHILE.

  DESCRIBE TABLE gt_hex LINES gv_img_size.

  gv_img_size = gv_img_size * 255.

  CLEAR gs_attachments.

  gs_attachments-objtp = gv_img_type.
  gs_attachments-subject = gv_img_name.
  gs_attachments-docsize = gv_img_size.
  gs_attachments-cont_hex = gt_hex.

  APPEND gs_attachments TO gt_attachments.

ENDFORM.                    " IMAGE_EMBED_LOGIC

FORM get_pack_list_config .

* Get Do number and its corresponding flags from NAST
  SELECT SINGLE * FROM nast INTO gs_nast_pl
                 WHERE  kappl = c_kappl_pl AND
                        objky = nast-objky AND
                        kschl = c_kschl_pl.

* Check for ZK00 Output type if the ZPL0 is not maintained
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM nast INTO gs_nast_pl
                   WHERE  kappl = c_kappl_pl AND
                          objky = nast-objky AND
                          kschl = 'ZK00'.
  ENDIF.

* Get Smartform & program details from TNAPR.
  SELECT SINGLE * FROM tnapr INTO gs_tnapr_pl
         WHERE kschl = c_kschl_pl AND
               nacha = c_nacha_pl AND
               kappl = c_kappl_pl.

ENDFORM.                    " GET_PACK_LIST_CONFIG

*&---------------------------------------------------------------------*
*&      Form  ATTACH_PACK_LIST
*&---------------------------------------------------------------------*
FORM attach_pack_list .

  IF gv_kschl EQ c_sne "ZDSE - Shipping Notif: English
        AND gs_likp-expkz NE c_x. "Domestic Customers only.

    PERFORM get_pack_list_config.
* Packing List processing required only if a packing list
* is maintained in the output type
    IF gs_nast_pl IS NOT INITIAL.
* Call the FM for Pack List processing
      CALL FUNCTION 'ZOTC_PACK_LIST_PROCESS'
        EXPORTING
          is_nast        = gs_nast_pl
          is_tnapr       = gs_tnapr_pl
          iv_attach      = 'X'
        IMPORTING
          et_attachments = gt_attachments.
    ENDIF.

  ENDIF.

ENDFORM.                    " ATTACH_PACK_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_SMTP_ADDR
*&---------------------------------------------------------------------*
*This routine is used to fetch the email address of partners.
*----------------------------------------------------------------------*
FORM get_cp_email  CHANGING pt_smtp_addr LIKE gt_smtp_addr.
  TYPES: BEGIN OF ts_vbpa ,
           adrnr TYPE adrnr,
           adrnp TYPE ad_persnum,
         END OF ts_vbpa .

  DATA: lt_vbpa       TYPE TABLE OF ts_vbpa,
        lt_smtp_addr1 LIKE gt_smtp_addr.

*Both address number and person number is required for contact person's email
  SELECT adrnr adrnp FROM vbpa
     INTO TABLE lt_vbpa
     WHERE vbeln = gs_likp-vbeln
*       AND parvw = c_cp.                                    "#EC WARNOK
       AND parvw = c_sn.                                    "#EC WARNOK
  IF sy-subrc EQ 0 AND lt_vbpa IS NOT INITIAL.
    SELECT smtp_addr FROM adr6
      INTO TABLE lt_smtp_addr1
      FOR ALL ENTRIES IN lt_vbpa
      WHERE addrnumber = lt_vbpa-adrnr
      AND   persnumber = lt_vbpa-adrnp.
    IF sy-subrc EQ 0.
      APPEND LINES OF: lt_smtp_addr1 TO pt_smtp_addr.
      SORT pt_smtp_addr.
      DELETE ADJACENT DUPLICATES FROM pt_smtp_addr.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_SMTP_ADDR
