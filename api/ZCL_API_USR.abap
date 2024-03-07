class ZCL_API_USR definition
  public
  final
  create public .

public section.

  class-methods GET_SESSION_IDX
    importing
      !IV_UNAME type CLIKE default SY-UNAME
      !IV_TCODE type CLIKE default 'SM30'
    returning
      value(RV_SESSION_IDX) type CHAR02 .
  class-methods KILL_SESSION
    importing
      !IV_UNAME type CLIKE default SY-UNAME
      !IV_TCODE type CLIKE default 'SM30'
    returning
      value(RS_BDCMSG) type BDCMSGCOLL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_USR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_USR=>GET_SESSION_IDX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UNAME                       TYPE        CLIKE (default =SY-UNAME)
* | [--->] IV_TCODE                       TYPE        CLIKE (default ='SM30')
* | [<-()] RV_SESSION_IDX                 TYPE        CHAR02
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_SESSION_IDX.

    CLEAR: rv_session_idx.

    DATA(lo_server_info) = NEW cl_server_info( ).
    DATA(gt_session_list) = lo_server_info->get_session_list( with_application_info = 1 tenant = sy-mandt ).

    DELETE gt_session_list WHERE user_name   <> iv_uname.
    SORT gt_session_list BY session_hdl ASCENDING.

    DATA: lv_app TYPE sy-tcode.
    LOOP AT gt_session_list INTO DATA(ls_sess).
      lv_app = to_upper( ls_sess-application ).
      IF lv_app = to_upper( iv_tcode ).
        rv_session_idx = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_USR=>KILL_SESSION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UNAME                       TYPE        CLIKE (default =SY-UNAME)
* | [--->] IV_TCODE                       TYPE        CLIKE (default ='SM30')
* | [<-()] RS_BDCMSG                      TYPE        BDCMSGCOLL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD kill_session.

    " Inspired by https://sapyard.com/abap-power-to-kill-2/
    " It will NOT work while debugging!!! (because of new session)

    rs_bdcmsg-msgid = '00'.
    rs_bdcmsg-msgnr = '398'.
    rs_bdcmsg-msgv1 = iv_tcode.
    rs_bdcmsg-msgv3 = iv_uname.

    DATA(lv_session_idx) = get_session_idx( iv_uname = iv_uname
                                            iv_tcode = iv_tcode ).

    IF lv_session_idx IS INITIAL.
      rs_bdcmsg-msgtyp = 'E'.
      rs_bdcmsg-msgv2 = 'Session not found for user'.
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
      lv_session_idx = get_session_idx( iv_uname = iv_uname
                                        iv_tcode = iv_tcode ).

      IF lv_session_idx IS NOT INITIAL.
        rs_bdcmsg-msgtyp = 'E'.
        rs_bdcmsg-msgv2 = 'Session still locked by user'.

      ELSE.
        rs_bdcmsg-msgtyp = 'S'.
        rs_bdcmsg-msgv2 = 'Session killed for user'.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
