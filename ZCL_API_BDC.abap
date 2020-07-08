class ZCL_API_BDC definition
  public
  final
  create public .

*"* public components of class ZCL_API_BDC
*"* do not include other source files here!!!
public section.

  data DELIMITER type CHAR1 value '_' ##NO_TEXT.
  data TCODE type SYTCODE .
  data DATA type TAB_BDCDATA .
  data MSGCOLL type TAB_BDCMSGCOLL .
  data OPTIONS type CTU_PARAMS .
  constants MODE type BDCMODE value 'E' ##NO_TEXT.
  constants UPDATE type BDCUPMODE value 'S' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      value(TCODE) type ANY
      !FVAL type ANY optional
      !IV_DELIMITER type CHAR1 default '|' .
  methods SCREEN
    importing
      !PAR type ANY .
  methods CURSOR
    importing
      !PAR type ANY .
  methods SUBSCR
    importing
      !PAR type ANY .
  methods OKCODE
    importing
      !PAR type ANY .
  methods FIELD
    importing
      !PAR type ANY optional
      !FNAM type ANY optional
      !FVAL type ANY optional
    preferred parameter PAR .
  methods CALL
    importing
      !CHECK_AUTH type BOOLE_D default ABAP_FALSE .
  class-methods BAPI_MSGCOLL
    changing
      !BAPIRET_T type BAPIRET2_T optional
      !BAPIRET type BAPIRET2 optional
      !MSGCOLL_T type TAB_BDCMSGCOLL optional
      !MSGCOLL type BDCMSGCOLL optional .
  class-methods GET_TEXT
    importing
      !IV_OBJNR type JEST-OBJNR optional
      !IV_SPRAS type SY-LANGU default SY-LANGU
      !IV_BYPASS_BUFFER type FLAG default ABAP_FALSE
      !IV_AUFNR type AUFK-AUFNR optional
    exporting
      !EV_STAT_EXIST type FLAG
      !EV_STSMA type JSTO-STSMA
      !EV_SYSTEM_TEXT type BSVX-STTXT
      !EV_USER_TEXT type BSVX-STTXT
      !EV_STONR type TJ30-STONR .
protected section.
*"* protected components of class ZCL_API_BDC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_API_BDC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_API_BDC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_BDC=>BAPI_MSGCOLL
* +-------------------------------------------------------------------------------------------------+
* | [<-->] BAPIRET_T                      TYPE        BAPIRET2_T(optional)
* | [<-->] BAPIRET                        TYPE        BAPIRET2(optional)
* | [<-->] MSGCOLL_T                      TYPE        TAB_BDCMSGCOLL(optional)
* | [<-->] MSGCOLL                        TYPE        BDCMSGCOLL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method BAPI_MSGCOLL.

  data: msg_t      type tab_bdcmsgcoll
      , msg_l      type bdcmsgcoll
      , bapi_t     type bapiret2_t
      , bapi_l     type bapiret2
      .

  append lines of msgcoll_t to msg_t.
  if msgcoll-msgid ne space.
    append msgcoll to msg_t.
  endif.

  append lines of bapiret_t to bapi_t.
  if bapiret-id ne space.
    append bapiret to bapi_t.
  endif.

  if lines( msg_t ) > 0.    " BAPI priority

    clear: bapi_l, bapiret.

    loop at msg_t  into msg_l.

      . bapi_l-type        = msg_l-msgtyp  " Type
      . bapi_l-id          = msg_l-msgid   " Id
      . bapi_l-number      = msg_l-msgnr   " Number
*      . bapi_l-message     =   " text
*      . bapi_l-log_no      =   " journal num
*      . bapi_l-log_msg_no  =   " current num of message
      . bapi_l-message_v1  = msg_l-msgv1
      . bapi_l-message_v2  = msg_l-msgv2
      . bapi_l-message_v3  = msg_l-msgv3
      . bapi_l-message_v4  = msg_l-msgv4
*      . bapi_l-parameter   =   " param name
*      . bapi_l-row         =   "
*      . bapi_l-field       =   "
*      . bapi_l-system      =   "
      .

*      msg_l-tcode    " tcode
*      msg_l-dyname   " bdc module
*      msg_l-dynumb   " bdc dynpro
*      msg_l-msgspra  " lang idx
*      msg_l-env      " operation
*      msg_l-fldname  " fieldname

      append bapi_l  to bapiret_t.

      if bapiret-id eq space.  " first message
        bapiret = bapi_l.
      endif.

    endloop.


  elseif lines( bapi_t ) > 0.

    clear: msg_l, msgcoll.

    loop at bapi_t  into bapi_l.


      . msg_l-msgtyp      = bapi_l-type
      . msg_l-msgid       = bapi_l-id
      . msg_l-msgnr       = bapi_l-number
*      .                   = bapi_l-message
*      .                   = bapi_l-log_no
*      .                   = bapi_l-log_msg_no
      . msg_l-msgv1       = bapi_l-message_v1
      . msg_l-msgv2       = bapi_l-message_v2
      . msg_l-msgv3       = bapi_l-message_v3
      . msg_l-msgv4       = bapi_l-message_v4
*      .                   = bapi_l-parameter
*      .                   = bapi_l-row
*      .                   = bapi_l-field
*      .                   = bapi_l-system
      .

*      msg_l-tcode
*      msg_l-dyname
*      msg_l-dynumb
*      msg_l-msgspra
*      msg_l-env
*      msg_l-fldname

      if msgcoll-msgid eq space.
        msgcoll = msg_l.
      endif.

    endloop.

  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->CALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] CHECK_AUTH                     TYPE        BOOLE_D (default =ABAP_FALSE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD call.

  IF check_auth = abap_true.

    TRY .
        CALL TRANSACTION tcode WITH AUTHORITY-CHECK
        USING   data
        OPTIONS FROM options
        MESSAGES INTO msgcoll[].
      CATCH cx_sy_authorization_error.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.


  ELSE.

    CALL TRANSACTION tcode
      USING   data
      OPTIONS FROM options
      MESSAGES INTO msgcoll[].

  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] TCODE                          TYPE        ANY
* | [--->] FVAL                           TYPE        ANY(optional)
* | [--->] IV_DELIMITER                   TYPE        CHAR1 (default ='|')
* +--------------------------------------------------------------------------------------</SIGNATURE>
method constructor.

  me->tcode = tcode.

  field-symbols: <>    type bdcdata.
  insert initial line  into data  assigning <>  index 1.

  <>-dynbegin = 'T'.
  <>-fnam     = me->tcode.
  <>-fval     = fval.

  options-dismode   = 'E'.  " stop on erros
  options-updmode   = 'L'.  " local
  options-cattmode  = ''.
  options-defsize   = 'X'.
  options-racommit  = ''.
  options-nobinpt   = 'X'.  " no sy-batch
  options-nobiend   = ''.

  delimiter = iv_delimiter.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->CURSOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] PAR                            TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CURSOR.

  field( fnam = 'BDC_CURSOR' fval = par ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] PAR                            TYPE        ANY(optional)
* | [--->] FNAM                           TYPE        ANY(optional)
* | [--->] FVAL                           TYPE        ANY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method FIELD.

  field-symbols: <>    type bdcdata.

  if fnam ne ''.
    append initial line  to data  assigning <>.
    <>-fnam = fnam.
    write fval to <>-fval left-justified.
  else.
    append initial line  to data  assigning <>.
    split par at delimiter into <>-fnam <>-fval.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->OKCODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PAR                            TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method OKCODE.

  field( fnam = 'BDC_OKCODE' fval = par ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->SCREEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] PAR                            TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SCREEN.

  data: okcode_l  type bdcdata-fval.

  field-symbols: <>    type bdcdata.
  append initial line  to data  assigning <>.

  split par  at delimiter into <>-program <>-dynpro okcode_l.
  <>-dynbegin    = 'X'.

  if okcode_l ne space.
    okcode( okcode_l ).
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_BDC->SUBSCR
* +-------------------------------------------------------------------------------------------------+
* | [--->] PAR                            TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SUBSCR.

  field( fnam = 'BDC_SUBSCR' fval = par ).

endmethod.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /SIE/AD_ZMS0_STATUS=>GET_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNR                       TYPE        JEST-OBJNR(optional)
* | [--->] IV_SPRAS                       TYPE        SY-LANGU (default =SY-LANGU)
* | [--->] IV_BYPASS_BUFFER               TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_AUFNR                       TYPE        AUFK-AUFNR(optional)
* | [<---] EV_STAT_EXIST                  TYPE        FLAG
* | [<---] EV_STSMA                       TYPE        JSTO-STSMA
* | [<---] EV_SYSTEM_TEXT                 TYPE        BSVX-STTXT
* | [<---] EV_USER_TEXT                   TYPE        BSVX-STTXT
* | [<---] EV_STONR                       TYPE        TJ30-STONR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_TEXT.
    DATA(lv_objnr) = iv_objnr.

    IF lv_objnr IS INITIAL.

      IF iv_aufnr IS NOT INITIAL.
        SELECT SINGLE objnr
          FROM aufk
          INTO lv_objnr
          WHERE aufnr = iv_aufnr.
      ENDIF.

    ENDIF.

    CHECK lv_objnr IS NOT INITIAL.

    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
*       FLG_USER_STAT     = ' '
        objnr             = lv_objnr
*       ONLY_ACTIVE       = 'X'
        spras             = iv_spras
        bypass_buffer     = iv_bypass_buffer
      IMPORTING
        anw_stat_existing = ev_stat_exist
        e_stsma           = ev_stsma
        line              = ev_system_text
        user_line         = ev_user_text
        stonr             = ev_stonr
      EXCEPTIONS
        OTHERS            = 2.
    IF sy-subrc <> 0.
      CLEAR: ev_system_text, ev_user_text.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
