* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CLASS=>KILL_SESSION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UNAME                       TYPE        SY-UNAME (default =SY-UNAME)
* | [--->] IV_TCODE                       TYPE        SY-TCODE (default ='SM30')
* | [<-()] RS_BDCMSG                      TYPE        BDCMSGCOLL
* +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD kill_session.

    " Inspired by https://sapyard.com/abap-power-to-kill-2/
    " It will NOT work while debugging!!! (because of new session)

    DATA:
      lv_session_idx  TYPE char02.


    DATA(lo_server_info) = NEW cl_server_info( ).
    DATA(gt_session_list) = lo_server_info->get_session_list( with_application_info = 1 tenant = sy-mandt ).

    DELETE gt_session_list WHERE user_name   <> iv_uname.
    SORT gt_session_list BY session_hdl ASCENDING.

    DATA: lv_app TYPE sy-tcode.
    LOOP AT gt_session_list INTO DATA(ls_sess).
      lv_app = to_upper( ls_sess-application ).
      IF lv_app = to_upper( iv_tcode ).
        lv_session_idx = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_session_idx <= 0.
      rs_bdcmsg-msgtyp = 'E'.
      rs_bdcmsg-msgid = '00'.
      rs_bdcmsg-msgnr = '398'.
      rs_bdcmsg-msgv1 = iv_tcode.
      rs_bdcmsg-msgv2 = 'Session not found for user'.
      rs_bdcmsg-msgv3 = iv_uname.
      RETURN.
    ENDIF.

    DATA(lo_bdc) = NEW zcl_api_bdc( 'SM04' ).

    lo_bdc->options-updmode = 'S'.
    lo_bdc->options-nobinpt = ''.
    lo_bdc->options-dismode = 'N'.
    lo_bdc->options-defsize = ''.

    lo_bdc->screen( 'SAPMSSY0|0120|=&IC1' ).
    lo_bdc->cursor( '02/16' ).

    lo_bdc->screen( 'SAPMSSY0|0120|=&ILT' ).

    lo_bdc->screen( 'SAPLSSEL|1104|=CRET' ).
    lo_bdc->cursor( '%%DYN001-LOW' ).
    lo_bdc->field( fnam = '%%DYN001-LOW' fval = iv_uname ).

    lo_bdc->screen( 'SAPMSSY0|0120|=PMOD' ).
    lo_bdc->cursor( '04/03' ).

    lo_bdc->screen( 'RSM04000_ALV_NEW|2000|=DEL' ).
    lo_bdc->cursor( |MODUS-MTCODE({ lv_session_idx WIDTH = 02 ALPHA = IN })| ).

    lo_bdc->screen( 'SAPMSSY0|0120|=&F03' ).

    lo_bdc->call( check_auth = abap_true ).

    READ TABLE lo_bdc->msgcoll[] INTO rs_bdcmsg WITH KEY msgtyp = 'E'.
    IF sy-subrc <> 0.
      rs_bdcmsg-msgtyp = 'S'.
      rs_bdcmsg-msgid = '00'.
      rs_bdcmsg-msgnr = '398'.
      rs_bdcmsg-msgv1 = iv_tcode.
      rs_bdcmsg-msgv2 = 'Session killed for user'.
      rs_bdcmsg-msgv3 = iv_uname.
      RETURN.
    ENDIF.

  ENDMETHOD.
