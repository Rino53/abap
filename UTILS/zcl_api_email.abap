  METHOD send_email_notif.

    TRY.

        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        DATA(lv_subject) = CONV so_obj_des( TEXT-sub ). " Will be rewritten

        DATA(lt_body_text) = VALUE soli_tab( ( |{ TEXT-ma1 }| )
                                             ( |<br>{ TEXT-ma2 } { ms_params-stat-cnt_upd }| )
                                             ( |<br>{ TEXT-ma3 } { ms_params-sscr-p_maxup }| ) ).

        DATA(lo_document) = cl_document_bcs=>create_document(
                           i_type    = 'HTM'
                           i_text    = lt_body_text
                           i_subject = lv_subject ).


        lo_send_request->set_document( lo_document ).

        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address(
                                          i_address_string = ms_params-sscr-p_eradr ).

        lo_send_request->add_recipient( i_recipient = lo_recipient ).

        " Optional!
*    lo_send_request->set_sender( cl_sapuser_bcs=>create( sy-uname ) ).
        lo_send_request->set_message_subject( CONV #( TEXT-sub ) ).

        DATA(lv_send_ok) = lo_send_request->send( ).

        IF lv_send_ok = abap_true.
          COMMIT WORK AND WAIT.
          MESSAGE s022(so).
        ENDIF.


      CATCH cx_bcs INTO DATA(lx_bcs_exception).
        "  Sending fax/mail failed
        MESSAGE s650(ws) DISPLAY LIKE 'E'.
*        fill_protocol( ).
*        RAISE EXCEPTION TYPE zcx_fi_print_exception.
    ENDTRY.

  ENDMETHOD.
