class ZCL_API_BDC definition
* SUPPORTS < 7.4 *
  public
  final
  create public .

public section.
*"* public components of class ZCL_API_BDC
*"* do not include other source files here!!!

  data DELIMITER type CHAR1 value '_'. "#EC NOTEXT .
  data TCODE type SYTCODE .
  data DATA type TAB_BDCDATA .
  data MSGCOLL type TAB_BDCMSGCOLL .
  data OPTIONS type CTU_PARAMS .
  constants MODE type BDCMODE value 'E'. "#EC NOTEXT
  constants UPDATE type BDCUPMODE value 'S'. "#EC NOTEXT

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
      !CHECK_AUTH type BOOLE_D default ABAP_FALSE
    returning
      value(BDCMSGCOLL) type TAB_BDCMSGCOLL .
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
  class-methods INIT_TCODE
    importing
      !IV_TCODE type ANY optional
    returning
      value(RO_INST) type ref to ZCL_API_BDC .
protected section.
*"* protected components of class ZCL_API_BDC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_API_BDC
*"* do not include other source files here!!!

  class-methods _EXAMPLES .
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
* | [<-()] BDCMSGCOLL                     TYPE        TAB_BDCMSGCOLL
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD call.

***  IF check_auth = abap_true.
***
***    TRY .
***        CALL TRANSACTION tcode WITH AUTHORITY-CHECK
***        USING   data
***        OPTIONS FROM options
***        MESSAGES INTO msgcoll[].
***      CATCH cx_sy_authorization_error.
***        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
***        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
***    ENDTRY.
***
***
***  ELSE.

    CALL TRANSACTION tcode
      USING   data
      OPTIONS FROM options
      MESSAGES INTO msgcoll[].

    bdcmsgcoll[] = msgcoll[].

***  ENDIF.

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

*  Component Meaning
*    DISMODE Processing mode. Values as for the MODE addition.
*    UPMODE Update mode for processing. Values as for the UPDATE addition.
*    CATTMODE CATT mode for processing. While batch input is used mostly for data transfer,
*       CATT processes are more complex transactions, since they are reusable tests.
*       Values: " " (no CATT mode), "N" (CATT without single screen control), "A" (CATT with single screen control).
*    DEFSIZE Selects whether the screens of the called transaction are displayed in the standard screen size.
*       Values: "X" (standard size), " " (current size).
*    RACOMMIT Selects whether the COMMIT WORK statement terminates processing or not.
*       Values: " " (COMMIT WORK terminates processing), "X" (COMMIT WORK does not terminate processing).
*    NOBINPT Selection for the system field sy-binpt.
*       Values: " " (sy-binpt contains "X" in the called transaction), "X" (sy-binpt contains " " in the called transaction).
*    NOBIEND Selection for the system field sy-binpt.
*       Values: " " (sy-binpt contains "X" after the end of the batch input table data in the called transaction ) "X" (sy-binpt contains " " after the end of the data in the called transaction).


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
* | Static Public Method ZCL_API_BDC=>GET_TEXT
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
    DATA: lv_objnr TYPE JEST-OBJNR.
    lv_objnr = iv_objnr.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_BDC=>INIT_TCODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TCODE                       TYPE        ANY(optional)
* | [<-()] RO_INST                        TYPE REF TO ZCL_API_BDC
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD init_tcode.
  " #Rinat Salakhov @11.2023

  CREATE OBJECT ro_inst
    EXPORTING
      tcode = iv_tcode.

  CHECK ro_inst IS BOUND.
  ro_inst->options-defsize = abap_false.
  ro_inst->options-nobiend = abap_true.
*  ro_inst->options-dismode = 'N'.

ENDMETHOD.


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
* | Static Private Method ZCL_API_BDC=>_EXAMPLES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD _examples.

  DATA: lo_bdc TYPE REF TO zcl_api_bdc,
        caufvd TYPE caufvd.
  CREATE OBJECT lo_bdc EXPORTING tcode = 'COR2'.
  lo_bdc->options-updmode = 'S'.
  lo_bdc->screen( 'SAPLCOKO|5110|=OMLA' ).
  lo_bdc->field( EXPORTING fnam = 'CAUFVD-AUFNR' fval = caufvd-aufnr ).
  lo_bdc->call( check_auth = abap_true ).


  DATA: iv_aufnr TYPE aufnr,
        iv_charg TYPE charg_d.
  " change orders batch (charg) with batch input

  IF iv_aufnr IS NOT INITIAL AND iv_charg IS NOT INITIAL.
    CREATE OBJECT lo_bdc EXPORTING tcode = 'COR2'.
    lo_bdc->options-updmode = 'S'.
    lo_bdc->options-nobinpt = abap_false.
    lo_bdc->options-dismode = 'N'.
*    lo_bdc->options-updmode = 'A'.
    lo_bdc->screen( 'SAPLCOKO|5110|/00' ).
    lo_bdc->field( EXPORTING fnam = 'CAUFVD-AUFNR' fval = iv_aufnr ).
    lo_bdc->screen( 'SAPLCOKO|5115|=KOWE' ).
    lo_bdc->screen( 'SAPLCOKO|5115|/00' ).
    lo_bdc->field( EXPORTING fnam = 'AFPOD-CHARG' fval = iv_charg ).

    lo_bdc->screen( 'SAPLCOKO|5115|=OMLA' ).
    lo_bdc->screen( 'SAPLCOMK|5120|=BACK' ).
    lo_bdc->field( EXPORTING fnam = 'RESBD-CHARG(01)' fval = iv_charg ).
    lo_bdc->screen( 'SAPLCOMD|5100|=BU' ).

    lo_bdc->screen( 'SAPLCOKO|5115|=BU' ).

*    go_sequence->dequeue_sequence( ). " before cor2
    lo_bdc->call( check_auth = abap_true ).
  ENDIF.


  CHECK caufvd-plnbez IS NOT INITIAL.

  CREATE OBJECT lo_bdc EXPORTING tcode = 'QE51N'.
  lo_bdc->options-updmode = 'S'.
  lo_bdc->options-nobinpt = abap_true.
  lo_bdc->options-defsize = abap_false.

  " Exclude work centers
  lo_bdc->screen( 'SAPLQEES|0500|=%022' ).
  lo_bdc->screen( 'SAPLALDB|3000|=NOSV' ).
  lo_bdc->screen( 'SAPLALDB|3000|=ACPT' ).
  lo_bdc->field( EXPORTING fnam = 'RSCSEL_255-SLOW_E(01)' fval = 'EXTERN' ).
  lo_bdc->field( EXPORTING fnam = 'RSCSEL_255-SLOW_E(01)' fval = 'F&E' ).
  lo_bdc->field( EXPORTING fnam = 'RSCSEL_255-SLOW_E(01)' fval = 'LABOR' ).

  " Pass other params
  lo_bdc->screen( 'SAPLQEES|0500|=CRET' ).
  lo_bdc->field( EXPORTING fnam = 'QL_WERKS-LOW' fval = caufvd-werks ).
  lo_bdc->field( EXPORTING fnam = 'QL_MATNR-LOW' fval = caufvd-plnbez ).
  lo_bdc->field( EXPORTING fnam = 'QL_CHARG-LOW' fval = 'charg' ).
  lo_bdc->field( EXPORTING fnam = 'QL_HERKT-LOW' fval = '03' ). " insp lot origin
  lo_bdc->field( EXPORTING fnam = 'QL_HERKT-HIGH' fval = '04' ).
  lo_bdc->field( EXPORTING fnam = 'QL_MAX_R' fval = '99999' ).

  lo_bdc->call( check_auth = abap_true ).

ENDMETHOD.
ENDCLASS.
