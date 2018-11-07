class ZCL_API_BDC definition
  public
  final
  create public .

*"* public components of class zcl_api_bdc
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
protected section.
*"* protected components of class zcl_api_bdc
*"* do not include other source files here!!!
private section.
*"* private components of class zcl_api_bdc
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

  if lines( msg_t ) > 0.    " Приоритет BAPI, он заполняется чаще

    clear: bapi_l, bapiret.

    loop at msg_t  into msg_l.

      . bapi_l-type        = msg_l-msgtyp  " Тип сообщения
      . bapi_l-id          = msg_l-msgid   " ИдСообщения
      . bapi_l-number      = msg_l-msgnr   " Номер сообщения
*      . bapi_l-message     =   " Текст сообщения
*      . bapi_l-log_no      =   " Номер журнала
*      . bapi_l-log_msg_no  =   " Текущий № сообщения
      . bapi_l-message_v1  = msg_l-msgv1   " Переменная сообщения
      . bapi_l-message_v2  = msg_l-msgv2   " Переменная сообщения
      . bapi_l-message_v3  = msg_l-msgv3   " Переменная сообщения
      . bapi_l-message_v4  = msg_l-msgv4   " Переменная сообщения
*      . bapi_l-parameter   =   " Имя параметра
*      . bapi_l-row         =   " Строка в параметре
*      . bapi_l-field       =   " Имя поля
*      . bapi_l-system      =   " Логич. сист. (из которой происх. док.)
      .

*      msg_l-tcode    " КодТранз
*      msg_l-dyname   " BDC-модули
*      msg_l-dynumb   " № экрана BDC
*      msg_l-msgspra  " ИндикЯзыка
*      msg_l-env      " Операция
*      msg_l-fldname  " Имя поля

      append bapi_l  to bapiret_t.

      if bapiret-id eq space.  " Первое сообщение
        bapiret = bapi_l.
      endif.

    endloop.


  elseif lines( bapi_t ) > 0.

    clear: msg_l, msgcoll.

    loop at bapi_t  into bapi_l.


      . msg_l-msgtyp      = bapi_l-type         " Тип сообщения
      . msg_l-msgid       = bapi_l-id           " ИдСообщения
      . msg_l-msgnr       = bapi_l-number       " Номер сообщения
*      .                   = bapi_l-message    " Текст сообщения
*      .                   = bapi_l-log_no     " Номер журнала
*      .                   = bapi_l-log_msg_no " Текущий № сообщения
      . msg_l-msgv1       = bapi_l-message_v1   " Переменная сообщения
      . msg_l-msgv2       = bapi_l-message_v2   " Переменная сообщения
      . msg_l-msgv3       = bapi_l-message_v3   " Переменная сообщения
      . msg_l-msgv4       = bapi_l-message_v4   " Переменная сообщения
*      .                   = bapi_l-parameter   " Имя параметра
*      .                   = bapi_l-row         " Строка в параметре
*      .                   = bapi_l-field       " Имя поля
*      .                   = bapi_l-system      " Логич. сист. (из которой происх. док.)
      .

*      msg_l-tcode    " КодТранз
*      msg_l-dyname   " BDC-модули
*      msg_l-dynumb   " № экрана BDC
*      msg_l-msgspra  " ИндикЯзыка
*      msg_l-env      " Операция
*      msg_l-fldname  " Имя поля

      if msgcoll-msgid eq space.  " Первое сообщение
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

  me->tcode = TO_UPPER( tcode ).

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
ENDCLASS.
