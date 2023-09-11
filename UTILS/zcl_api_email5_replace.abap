FUNCTION zmws_gi_notification.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IS_REFDOC_HEADER) TYPE  ZWMS_REFDOC_HEADER
*"  CHANGING
*"     REFERENCE(CT_RETURN) TYPE  WRF_BAPIRETURN_TTY
*"----------------------------------------------------------------------
* Check errors exist
  LOOP AT ct_return TRANSPORTING NO FIELDS
    WHERE type CA 'EW'.
    EXIT.
  ENDLOOP.
  CHECK sy-subrc = 0.

* Check RFC organizational assignment
  CALL FUNCTION 'ZWMS_CHECK_ORG_ASSIGNMENT'
    EXPORTING
      ip_funcname                 = 'ZMWS_GI_NOTIFICATION'
      ip_bukrs                    = is_refdoc_header-bukrs
    EXCEPTIONS
      org_assignment_not_relevant = 1
      OTHERS                      = 2.
  CHECK sy-subrc = 0.

* Get recipients
  DATA lt_recipients TYPE safm_apt_pp_email.
  CALL FUNCTION 'ZWMS_GET_GI_RECIPIENTS'
    EXPORTING
      is_refdoc_header = is_refdoc_header
    IMPORTING
      et_recipients    = lt_recipients.
  IF lt_recipients IS INITIAL.
    DATA(ls_message) = VALUE bapireturn(
                       type       = if_msg_output=>msgtype_error
                       code       = '018'
                       message_v1 = is_refdoc_header-vbeln
                     ).
    MESSAGE e022(zwms) WITH is_refdoc_header-vbeln INTO ls_message-message.
    INSERT ls_message INTO TABLE ct_return.
    RETURN.
  ENDIF.

* Mail Subject
  DATA(lv_reference) = |{ is_refdoc_header-vbeln ALPHA = OUT }|.
  CONDENSE lv_reference.
  DATA(lv_subject) = zcl_wms_assist=>read_text( 'ZWMS_GI_NOTIFICATION_SUBJECT' ).
  REPLACE '{REFERENCE}' IN lv_subject WITH lv_reference.

* Mail body
  DATA(lv_body) = zcl_wms_assist=>read_text( 'ZWMS_GI_NOTIFICATION_BODY' ).
  REPLACE '{REFERENCE}' IN lv_body WITH lv_reference.
  DATA(lt_body_text) = cl_document_bcs=>string_to_soli( lv_body ).
  LOOP AT ct_return ASSIGNING FIELD-SYMBOL(<ls_return>).
    AT FIRST.
      APPEND LINES OF VALUE soli_tab(
        ( line = '<table border="1">' )
        ( line = '<tr>' )
        ( line = |<td style="font-weight:bold">{ TEXT-t01 }</td>| )
        ( line = |<td style="font-weight:bold">{ TEXT-t02 }</td>| )
        ( line = |<td style="font-weight:bold">{ TEXT-t03 }</td>| )
        ( line = '</tr>' )
      ) TO lt_body_text.
    ENDAT.

    APPEND LINES OF VALUE soli_tab(
      ( line = '<tr>' )
      ( line = |<td>{ <ls_return>-type }</td>| )
      ( line = |<td>{ <ls_return>-code }</td>| )
      ( line = |<td>{ <ls_return>-message }</td>| )
      ( line = '</tr>' )
    ) TO lt_body_text.

    AT LAST.
      APPEND '</table>' TO lt_body_text.
    ENDAT.
  ENDLOOP.

  TRY.
*     Mail utils
      DATA(lo_send_request) = cl_bcs=>create_persistent( ).
      DATA(lo_document) = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_subject = CONV #( lv_subject )
        i_text    = lt_body_text
      ).

      lo_send_request->set_document( lo_document ).

*     Sender
      lo_send_request->set_sender( i_sender = cl_sapuser_bcs=>create( sy-uname ) ).

*     Recipients
      LOOP AT lt_recipients ASSIGNING FIELD-SYMBOL(<lv_recipient>).
        lo_send_request->add_recipient( cl_cam_address_bcs=>create_internet_address( <lv_recipient> ) ).
      ENDLOOP.

*     Send email
      lo_send_request->set_send_immediately( i_send_immediately = abap_true ).
      lo_send_request->send( i_with_error_screen = abap_true ).

      COMMIT WORK.

    CATCH cx_bcs INTO DATA(lo_bcs_exception).
      ls_message = VALUE bapireturn(
                     type       = if_msg_output=>msgtype_error
                     code       = '865'
                     message_v1 = lo_bcs_exception->error_type
                   ).
      MESSAGE i865(so) WITH lo_bcs_exception->error_type INTO ls_message-message.
      INSERT ls_message INTO TABLE ct_return.
  ENDTRY.
ENDFUNCTION.
