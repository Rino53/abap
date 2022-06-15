METHOD send_email.
* Needed to send emails
    DATA: bcs_exception        TYPE REF TO cx_bcs,
          errortext            TYPE string,
          cl_send_request      TYPE REF TO cl_bcs,
          cl_document          TYPE REF TO cl_document_bcs,
          cl_recipient         TYPE REF TO if_recipient_bcs,
          cl_sender            TYPE REF TO cl_cam_address_bcs,
          t_attachment_header  TYPE soli_tab,
          wa_attachment_header LIKE LINE OF t_attachment_header,
          attachment_subject   TYPE sood-objdes,

          sood_bytecount       TYPE sood-objlen,
          mail_title           TYPE so_obj_des,
          t_mailtext           TYPE soli_tab,
          wa_mailtext          LIKE LINE OF t_mailtext,
          send_to              TYPE adr6-smtp_addr,
          sent                 TYPE os_boolean.


    mail_title     = 'Mail title'.
    wa_mailtext    = 'Mailtext'.
    APPEND wa_mailtext TO t_mailtext.

    TRY.
* Create send request
        cl_send_request = cl_bcs=>create_persistent( ).
* Create new document with mailtitle and mailtextg
        cl_document = cl_document_bcs=>create_document( i_type    = 'RAW' "#EC NOTEXT
                                                        i_text    = t_mailtext
                                                        i_subject = mail_title ).
* Add attachment to document
* since the new excelfiles have an 4-character extension .xlsx but the attachment-type only holds 3 charactes .xls,
* we have to specify the real filename via attachment header
* Use attachment_type xls to have SAP display attachment with the excel-icon
        attachment_subject  = gc_save_file_name.
        CONCATENATE '&SO_FILENAME=' attachment_subject INTO wa_attachment_header.
        APPEND wa_attachment_header TO t_attachment_header.
* Attachment
        sood_bytecount = bytecount.  " next method expects sood_bytecount instead of any positive integer *sigh*
        cl_document->add_attachment(  i_attachment_type    = 'XLS' "#EC NOTEXT
                                      i_attachment_subject = attachment_subject
                                      i_attachment_size    = sood_bytecount
                                      i_att_content_hex    = t_rawdata
                                      i_attachment_header  = t_attachment_header ).

* add document to send request
        cl_send_request->set_document( cl_document ).

* set sender in case if no own email is availabe
*        cl_sender  = cl_cam_address_bcs=>create_internet_address( 'sender@sender.sender' ).
*        cl_send_request->set_sender( cl_sender ).

* add recipient(s) - here only 1 will be needed
        send_to = p_email.
        IF send_to IS INITIAL.
          send_to = 'no_email@no_email.no_email'.  " Place into SOST in any case for demonstration purposes
        ENDIF.
        cl_recipient = cl_cam_address_bcs=>create_internet_address( send_to ).
        cl_send_request->add_recipient( cl_recipient ).

* Und abschicken
        sent = cl_send_request->send( i_with_error_screen = 'X' ).

        COMMIT WORK.

        IF sent IS INITIAL.
          MESSAGE i500(sbcoms) WITH p_email.
        ELSE.
          MESSAGE s022(so).
          MESSAGE 'Document ready to be sent - Check SOST or SCOT' TYPE 'I'.
        ENDIF.

      CATCH cx_bcs INTO bcs_exception.
        errortext = bcs_exception->if_message~get_text( ).
        MESSAGE errortext TYPE 'I'.

    ENDTRY.
  ENDMETHOD.                    "send_email
