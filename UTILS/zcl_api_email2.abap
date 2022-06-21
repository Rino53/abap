 IS_FORMOUT	TYPE FPFORMOUTPUT
 CALL FUNCTION lv_function_name
      EXPORTING
        /1bcdwb/docparams  = ls_document_params
      IMPORTING
        /1bcdwb/formoutput = ls_formout
 
 
 METHOD send_mail_fax.
    DATA: ls_outputparams_fax TYPE sfpoutpar.
    DATA: lv_emailaddr TYPE adr6-smtp_addr.
    DATA: lt_body_text TYPE soli_tab.
    DATA: lt_text132 TYPE lop_tdline_tab.

    READ TABLE gs_data_ers-t_rseg_x ASSIGNING FIELD-SYMBOL(<ls_rseg>) INDEX 1.
    " get default Email id from address no
    " When there is only one mail id then that will have default flag set
    IF gs_nast-kappl EQ c_v_kappl_mr AND <ls_rseg> IS ASSIGNED.
      get_mr_addr( EXPORTING iv_ebeln = <ls_rseg>-ebeln
                   IMPORTING ev_smtp  = lv_emailaddr
                   EXCEPTIONS not_found = 1 ).
    ENDIF.
    IF lv_emailaddr IS INITIAL.
      SELECT SINGLE smtp_addr FROM adr6 INTO @lv_emailaddr
         WHERE addrnumber =  @gs_data_ers-s_lfa1-adrnr
         AND flgdefault = @abap_true.
    ENDIF.
* Set FAX specific setting
    ls_outputparams_fax-telenum = COND #( WHEN ( gs_nast-nacha = 5  ) THEN gs_itcpo-tdtelenum
                                          WHEN ( gs_nast-telfx <> space ) THEN gs_nast-telfx  ).


    ls_outputparams_fax-teleland = COND #( WHEN ( gs_nast-nacha = 5  ) THEN gs_itcpo-tdteleland
                                           WHEN (  gs_nast-telfx <> space
                                                  AND gs_nast-tland = space ) THEN gs_data_ers-s_lfa1-land1
                                           WHEN ( gs_nast-telfx <> space AND gs_nast-tland <> space ) THEN  gs_nast-tland  ).

    TRY.
        " create persistent send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).


*   get PDF xstring and convert it to BCS format
        DATA(lv_pdf_size) = xstrlen( is_formout-pdf ).
        DATA(lt_pdf_content) = transform_to_solix( iv_pdf_size = CONV #( lv_pdf_size )
                                                   iv_pdf = is_formout-pdf  ).
*Cover title
        IF gs_nast-tdcovtitle IS NOT INITIAL.
          DATA(lv_subject) = CONV so_obj_des( gs_nast-tdcovtitle ).
        ELSE.
          SELECT SINGLE objdes FROM tnati INTO @lv_subject
                   WHERE spras EQ @gs_nast-spras
                     AND kappl EQ @gs_nast-kappl
                     AND kschl EQ @gs_nast-kschl.
          IF sy-subrc <> 0.
            lv_subject = gs_nast-objky+4(14).
          ENDIF.
        ENDIF.

        
        IF gs_nast-spras IS NOT INITIAL. " is_formout-langu ?
          DATA(lv_bukrs) = VALUE bukrs( gs_data_ers-t_rseg_x[ 1 ]-bukrs OPTIONAL ).
          IF lv_bukrs IS INITIAL.
            lv_bukrs = '1200'.
          ENDIF.
          DATA(lv_longtext_name) = conv thead-tdname( |ZFI_CREDIT_MEMO_EMAIL_HEAD_{ lv_bukrs }| ).

          zcl_md_longtext=>read_single( EXPORTING iv_object = 'TEXT'
                                                  iv_id     = 'ST'
                                                  iv_name   = lv_longtext_name
                                                  iv_spras  = gs_nast-spras
                                        CHANGING ct_texts = lt_text132 ).
          LOOP AT lt_text132 INTO DATA(lv_text132).
            IF sy-tabix = 1.
              DATA(lv_subject_mail) = CONV so_obj_des( lv_text132 ). " first line of mail head goes to mail title
            ELSE.
              APPEND INITIAL LINE TO lt_body_text ASSIGNING FIELD-SYMBOL(<ls_btxt>).
              <ls_btxt>-line = lv_text132 && '<BR>'.
            ENDIF.
          ENDLOOP.
        ENDIF.

        DATA(lo_document) = cl_document_bcs=>create_document(
                        i_type    = 'HTM'
                        i_text    = lt_body_text
                        i_subject = lv_subject_mail ).

        " Add attachment
        CALL METHOD lo_document->add_attachment
          EXPORTING
            i_attachment_type    = 'PDF'
            i_attachment_subject = lv_subject
            i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( iv_xstring = is_formout-pdf ).


***        DATA(lo_document) = cl_document_bcs=>create_document(
***                     i_type    = 'PDF'
***                     i_hex     = lt_pdf_content
***                     i_length  = CONV #( lv_pdf_size )
***                     i_subject = CONV #( lv_subject ) ).    "#EC NOTEXT
***


*    add document to send request
        lo_send_request->set_document( lo_document ).

        CASE gs_nast-nacha.
          WHEN 5.
            IF gv_comm_type EQ 'INT' OR
               ( ls_outputparams_fax-telenum IS INITIAL AND lv_emailaddr IS NOT INITIAL ). 
*     add recipient (e-mail address)
              DATA(lo_recipient)  = cl_cam_address_bcs=>create_internet_address(
              i_address_string = lv_emailaddr ).
            ELSE.
*    add recipient (fax address)
              lo_recipient = cl_cam_address_bcs=>create_fax_address(
                                 i_country = ls_outputparams_fax-teleland
                                 i_number  = ls_outputparams_fax-telenum ).
            ENDIF.
          WHEN 2.
*    add recipient (fax address)
            lo_recipient = cl_cam_address_bcs=>create_fax_address(
                             i_country = ls_outputparams_fax-teleland
                             i_number  = ls_outputparams_fax-telenum ).
        ENDCASE.

*   add recipient to send request
        lo_send_request->add_recipient( i_recipient = lo_recipient ).

*   send document ---------------------------------------
        DATA(lv_sent_to_all) = lo_send_request->send(
                i_with_error_screen = 'X' ).

        IF lv_sent_to_all = 'X'.
          MESSAGE i022(so).
        ENDIF.

        COMMIT WORK.

      CATCH cx_bcs INTO DATA(lx_bcs_exception).
        "  Sending fax/mail failed
        MESSAGE e650(ws) INTO gv_message.
        fill_protocol( ).
        RAISE EXCEPTION TYPE zcx_fi_print_exception.
    ENDTRY.

  ENDMETHOD.
