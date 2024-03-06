* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /AMAG/BC_STMS=>TEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UNAME                       TYPE        SY-UNAME (default =SY-UNAME)
* | [--->] IV_TCODE                       TYPE        SY-TCODE (default ='SM30')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD test.

    DATA:
      lv_session_idx  TYPE char02.


    DATA(lo_server_info) = new cl_server_info( ).
    DATA(gt_session_list) = lo_server_info->get_session_list( with_application_info = 1 tenant = sy-mandt ).

    DELETE gt_session_list WHERE user_name   <> iv_uname.
    DELETE gt_session_list WHERE tenant      <> sy-mandt.
    SORT gt_session_list BY session_hdl ASCENDING.

    LOOP AT gt_session_list INTO DATA(ls_sess).
      IF ls_sess-application = iv_tcode.
        lv_session_idx = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    DATA(lo_bdc) = NEW /amag/pp_api_bdc( 'SM04' ).

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

*    mo_log->add_message( it_batch = lo_bdc->msgcoll ).




  ENDMETHOD.
