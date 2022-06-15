METHOD send_email.

    DATA lv_subject       TYPE so_obj_des.
    DATA lv_mlrec         TYPE so_obj_nam.
    DATA lv_sent_to_all   TYPE os_boolean.
    DATA lr_bcs_exception TYPE REF TO cx_bcs.
    DATA lt_body_hex TYPE solix_tab.
    DATA lt_body_text TYPE soli_tab.
    DATA lt_text132 TYPE lop_tdline_tab.
    DATA lv_copy_addr TYPE ad_smtpadr.

*    lv_subject = TEXT-002. " lang should be from vendor

    TRY.
        "Create send request
        DATA(lr_send_request) = cl_bcs=>create_persistent( ).

        "Email FROM...
        DATA(lr_sender) = cl_sapuser_bcs=>create( sy-uname ).

        "Add sender to send request
        CALL METHOD lr_send_request->set_sender
          EXPORTING
            i_sender = lr_sender.

        "Email TO...
        IF i_email IS NOT INITIAL.
          DATA(lr_recipient) = cl_cam_address_bcs=>create_internet_address( i_email ).

          "Add recipient to send request
          CALL METHOD lr_send_request->add_recipient
            EXPORTING
              i_recipient = lr_recipient
              i_express   = 'X'.

        ENDIF.

        lv_copy_addr = p_email.
        DATA(lr_recipient_p) = cl_cam_address_bcs=>create_internet_address( lv_copy_addr ).

        "Add recipient to send request
        CALL METHOD lr_send_request->add_recipient
          EXPORTING
            i_recipient = lr_recipient_p
            i_express   = 'X'.

        "Email BODY
        IF i_head-text_lang IS NOT INITIAL.
          zcl_md_longtext=>read_single( EXPORTING iv_object = 'TEXT'
                                                  iv_id     = 'ST'
                                                  iv_name   = 'ZMM_ARR_CONF_EMAIL_HEAD'
                                                  iv_spras  = i_head-text_lang
                                        CHANGING ct_texts = lt_text132 ).
          LOOP AT lt_text132 INTO DATA(lv_text132).
            IF sy-tabix = 1.
              lv_subject = lv_text132. " first line of mail head goes to mail title
            ELSE.
              APPEND INITIAL LINE TO lt_body_text ASSIGNING FIELD-SYMBOL(<ls_btxt>).
              <ls_btxt>-line = lv_text132 && '<BR>'.
            ENDIF.
          ENDLOOP.
          CLEAR lt_text132[].
          zcl_md_longtext=>read_single( EXPORTING iv_object = 'TEXT'
                                                  iv_id     = 'ST'
                                                  iv_name   = i_head-greeting
                                                  iv_spras  = i_head-text_lang
                                        CHANGING ct_texts = lt_text132 ).
          LOOP AT lt_text132 INTO lv_text132.
            APPEND INITIAL LINE TO lt_body_text ASSIGNING <ls_btxt>.
            <ls_btxt>-line = lv_text132 && '<BR>'.
          ENDLOOP.
        ENDIF.
        DATA(lr_document) = cl_document_bcs=>create_document(
                        i_type    = 'HTM'
*                        i_hex = lt_body_hex
                        i_text = lt_body_text
                        i_subject = lv_subject ).

        " Add attachment
        CALL METHOD lr_document->add_attachment
          EXPORTING
            i_attachment_type    = 'PDF'
            "i_attachment_size =
            i_attachment_subject = lv_subject
            i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( iv_xstring = i_pdf ).

        "Add document to send request
        CALL METHOD lr_send_request->set_document( lr_document ).

        "Send email
        CALL METHOD lr_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = lv_sent_to_all ).
        IF lv_sent_to_all = abap_true.

        ENDIF.
        "Exception handling
      CATCH cx_bcs INTO lr_bcs_exception.

    ENDTRY.
    IF sy-batch IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

METHOD make_pdf.
    "DATA r_pdf TYPE xstring.
    DATA lc_form_funcname TYPE fpname VALUE 'ZMM_ARR_CONF'.
    DATA lv_function_name TYPE rs38l_fnam.
    DATA ls_outputparams TYPE sfpoutputparams.
    DATA ls_document_params TYPE sfpdocparams.
    DATA ls_formoutput TYPE fpformoutput.
    DATA lv_logo TYPE xstring.

    lv_logo = get_logo( ).

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = lc_form_funcname
          IMPORTING
            e_funcname = lv_function_name.
      CATCH cx_fp_api INTO DATA(lx_exc_api).
        MESSAGE lx_exc_api->get_text( ) TYPE 'E'.
        mo_log->add_message( is_sy = sy ).
    ENDTRY.

    ls_outputparams-dest = 'LOCL'.

    ls_outputparams-nodialog = abap_true.
    ls_outputparams-reqnew = abap_true.
    ls_outputparams-reqimm = abap_true.
    ls_outputparams-getpdf = abap_true.


    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      mo_log->add_message( is_sy = sy ).
    ENDIF.


    ls_document_params-langu = i_header-text_lang. " For debug - EN


    CALL FUNCTION lv_function_name
      EXPORTING
        /1bcdwb/docparams  = ls_document_params
        i_positions        = i_positions
        i_header           = i_header
        i_logo             = lv_logo
      IMPORTING
        /1bcdwb/formoutput = ls_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.

    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      mo_log->add_message( is_sy = sy ).
    ENDIF.

    r_pdf = ls_formoutput-pdf.

  ENDMETHOD.
