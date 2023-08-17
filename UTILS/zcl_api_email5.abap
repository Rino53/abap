""" This is draft """

 TYPES: BEGIN OF t_workload,
             bukrs   TYPE bukrs,

             datatype  TYPE string,
             solix_len TYPE i,
             solix_tab TYPE solix_tab,
             filename  TYPE string,

             alv_tab_ref TYPE REF TO DATA,
           END OF t_workload,
           tt_workload TYPE STANDARD TABLE OF t_workload.

DATA: lv_text1  TYPE rp50m-text1 VALUE 'Greetings,',
          lv_text2  TYPE rp50m-text1,
          lv_text3  TYPE rp50m-text1,
          lv_textok TYPE flag.
    IF p_bodpop = abap_true AND
       p_send3 = abap_true.

      CALL FUNCTION 'HR_PBS00MD_POPUP_TEXT'
        IMPORTING
          e_input_done = lv_textok
        CHANGING
          c_text1      = lv_text1
          c_text2      = lv_text2
          c_text3      = lv_text3.

      IF lv_textok <> abap_true.
        CLEAR p_send3.
        MESSAGE e398(00) WITH 'Mail send cancelled' INTO mo_log->mv_message. mo_log->add_message( ).

      ELSE.
        APPEND lv_text1 && '<BR>' TO mt_mailbody.
        APPEND lv_text2 && '<BR>' TO mt_mailbody.
        APPEND lv_text3 TO mt_mailbody.
      ENDIF.

    ENDIF.


  ENDMETHOD.

  METHOD send.

    TRY.
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        DATA(lv_subject) = CONV so_obj_des( p_mailtl ).

*        DATA: lt_body_text TYPE soli_tab.

        IF p_titper = abap_true.
          lv_subject = |{ lv_subject } { mv_periodma }|.
        ENDIF.

        DATA(lo_document) = cl_document_bcs=>create_document(
                        i_type    = 'HTM'
                        i_text    = mt_mailbody
                        i_subject = lv_subject ).

        LOOP AT it_workload REFERENCE INTO DATA(lr_work) WHERE solix_len > 0.
          TRY.
              CALL METHOD lo_document->add_attachment
                EXPORTING
                  i_attachment_type    = 'BIN' " 'XLS' does not open as 'XLSX', use 'BIN'
                  i_attachment_size    = CONV sood-objlen( lr_work->solix_len )
                  i_attachment_subject = CONV sood-objdes( lr_work->filename )
                  i_att_content_hex    = lr_work->solix_tab.

              IF sy-subrc = 0.
                MESSAGE s398(00) WITH 'Attachment added:' lr_work->solix_len lr_work->filename INTO mo_log->mv_message. mo_log->add_message( ).
              ENDIF.

            CATCH cx_document_bcs INTO DATA(lx_doc_bcs).
              mo_log->add_message( iv_textmsg1 = 'Error while adding attachment'
                                   iv_textmsg2 = lr_work->filename
                                   iv_textmsg3 = lx_doc_bcs->get_text( ) iv_textmsgty = 'E' ).
          ENDTRY.
        ENDLOOP.

        lo_send_request->set_document( lo_document ).

        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mailto ).

        lo_send_request->add_recipient( i_recipient = lo_recipient ).
        DATA(lr_sender) = cl_cam_address_bcs=>create_internet_address( p_mailfr ).
        lo_send_request->set_sender( i_sender = lr_sender ).

        IF p_mailt2 IS NOT INITIAL.
          DATA(lo_recipient2) = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mailt2 ).
          lo_send_request->add_recipient( i_recipient = lo_recipient2 ).
        ENDIF.
        IF p_mailcc IS NOT INITIAL.
          DATA(lo_recipient3) = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mailcc ).
          lo_send_request->add_recipient( i_recipient = lo_recipient3
                                          i_copy      = abap_true ).
        ENDIF.
        IF p_mailc2 IS NOT INITIAL.
          DATA(lo_recipient4) = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mailc2 ).
          lo_send_request->add_recipient( i_recipient = lo_recipient4
                                          i_copy      = abap_true ).
        ENDIF.
        IF p_mailb1 IS NOT INITIAL.
          DATA(lo_recipient5) = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mailb1 ).
          lo_send_request->add_recipient( i_recipient = lo_recipient5
                                          i_blind_copy = abap_true ).
        ENDIF.
        IF p_mailb2 IS NOT INITIAL.
          DATA(lo_recipient6) = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mailb2 ).
          lo_send_request->add_recipient( i_recipient = lo_recipient6
                                          i_blind_copy = abap_true ).
        ENDIF.

        DATA(lv_sent_to_all) = lo_send_request->send( i_with_error_screen = 'X' ).

        IF lv_sent_to_all = abap_true.
          COMMIT WORK AND WAIT.

          IF p_sosend = abap_true.

            " Init SOST send procedure, varinat SAP&CONNECTINT
            SUBMIT rsconn01
              WITH mode     = 'INT'
              WITH output   = space
              WITH maxjobs  = '1'
              WITH rfcgroup = space
              WITH maxpsize = '1000'
              WITH minpsize = '20'
              WITH maxsel   = '20000'
              WITH timepo   = '2'
              WITH timeout  = '100'
              WITH commit   = '1'
              AND RETURN.

          ENDIF.

        ENDIF.

      CATCH cx_send_req_bcs INTO DATA(lx_send_err).
        MESSAGE e398(00) WITH 'Error with sending email' INTO mo_log->mv_message. mo_log->add_message( ).
        RETURN.

      CATCH cx_bcs INTO DATA(lx_bcs).
        MESSAGE e398(00) WITH 'Error with creating email' INTO mo_log->mv_message. mo_log->add_message( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
