class ZCL_API_STMS definition
  public
  final
  create public .

public section.

  data MO_LOG type ref to ZCL_API_LOG .

  class-methods CHECK_SYSTEM_TYPE
    returning
      value(RV_SYST_TYPE) type T000-CCCATEGORY .
  methods CC_IMPORT
    importing
      !IV_TRKORR type E070-TRKORR
    returning
      value(RS_RESULT) type TRWBO_REQUEST .
  methods CC_INSERT_OBJS
    importing
      !IV_TRFROM type E070-TRKORR
      !IV_TRTO type E070-TRKORR .
  methods CC_RELEASE
    importing
      !IV_TRKORR type E070-TRKORR
    returning
      value(RS_RESULT) type TRWBO_REQUEST .
  class-methods TRANSPORT_WITH_COPY
    importing
      !IV_COPY type ABAP_BOOL default ABAP_TRUE
      !IV_RELEASE type ABAP_BOOL default ABAP_TRUE
      !IV_IMPORT type ABAP_BOOL default ABAP_TRUE
      !IV_WORKBENCH type E070-TRKORR default 'DEV123'
      !IV_DESTINATION type SYST-SYSID default 'QAS'
      !IV_SHOW_LOG type ABAP_BOOL default ABAP_TRUE .
  methods CC_CREATE
    importing
      !IV_TRKORR type E070-TRKORR optional
      !IV_TRTYPE type E070-TRFUNCTION default 'T'
      !IV_POSTFIX type TEXT6 default '[copy]'
    preferred parameter IV_TRKORR
    returning
      value(RV_TRNEW) type E070-TRKORR .
  methods CONSTRUCTOR
    importing
      !IV_TARGET_SYS type SYST-SYSID .
  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.

  class-data MS_TMS_SOURCE type TMSCSYS .
  class-data:
    MT_TMS_targets type STANDARD TABLE OF TMSCSYS .
  data MV_TARGET type TMSCSYS-SYSNAM .

  class-methods TEST
    importing
      !IV_UNAME type SY-UNAME default SY-UNAME
      !IV_TCODE type SY-TCODE default 'SM30' .
ENDCLASS.



CLASS ZCL_API_STMS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRKORR                      TYPE        E070-TRKORR(optional)
* | [--->] IV_TRTYPE                      TYPE        E070-TRFUNCTION (default ='T')
* | [--->] IV_POSTFIX                     TYPE        TEXT6 (default ='[copy]')
* | [<-()] RV_TRNEW                       TYPE        E070-TRKORR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cc_create.

    DATA: ls_request TYPE strhi_request_wd.
    ls_request-h-trkorr = iv_trkorr.

    CHECK: ls_request-h-trkorr IS NOT INITIAL.
    CHECK: mv_target IS NOT INITIAL.

    CALL FUNCTION 'TRINT_READ_REQUEST_WD'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_attributes = abap_true
      CHANGING
        cs_request_wd      = ls_request
      EXCEPTIONS
*       ERROR_OCCURED      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      mo_log->e( |Request { iv_trkorr } read error| ).
      RETURN.

    ENDIF.

    " trfunction:
*K  Workbench Request
*W  Customizing Request
*T  Transport of Copies

    DATA(lv_postfix) = |[{ sy-datum+4(4) }.{ sy-timlo(4) }-{ COND char02( WHEN ls_request-h-trfunction = 'K' THEN 'WB'
                                                                          WHEN ls_request-h-trfunction = 'W' THEN 'CU' ELSE 'XX' ) }]|. " [0903.1513-WB]

    DATA(ls_trnew) = ls_request-h.
    ls_trnew-trfunction = iv_trtype.

    DATA(lv_postfix_len) = conv byte( strlen( lv_postfix ) ).
    DATA(lv_desc_max) = conv byte( 60 - strlen( lv_postfix ) ).
    IF strlen( ls_trnew-as4text ) < lv_desc_max.
      DATA(lv_desc_len) = conv byte( strlen( ls_trnew-as4text ) ) - lv_postfix_len.
      IF ls_trnew-as4text+lv_desc_len(lv_postfix_len) <> lv_postfix.
        ls_trnew-as4text = |{ ls_trnew-as4text } { lv_postfix }|. " Max 60 chars
      ENDIF.
    ELSE.
      ls_trnew-as4text+lv_desc_max = lv_postfix.
    ENDIF.


    DATA:
      ls_request_header TYPE  trwbo_request_header,
      lt_task_headers   TYPE  trwbo_request_headers.


    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = ls_trnew-trfunction
        iv_text           = ls_trnew-as4text
        iv_target         = mv_target
*       IT_ATTRIBUTES     =
*       IT_USERS          =
*       IV_TARDEVCL       =
*       IV_DEVCLASS       =
*       IV_TARLAYER       =
*       IV_REPOID         =
*       IV_WITH_BADI_CHECK       =
*       IV_SIMULATION     =
      IMPORTING
        es_request_header = ls_request_header
        et_task_headers   = lt_task_headers
      EXCEPTIONS
*       INSERT_FAILED     = 1
*       ENQUEUE_FAILED    = 2
        OTHERS            = 3.
    IF sy-subrc = 0.

      rv_trnew = ls_request_header-trkorr.
      mo_log->s( |Request { rv_trnew } type { ls_trnew-trfunction } created| ).

    ENDIF.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_IMPORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRKORR                      TYPE        E070-TRKORR
* | [<-()] RS_RESULT                      TYPE        TRWBO_REQUEST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cc_import.


    CHECK: iv_trkorr IS NOT INITIAL.


    DATA:
      ev_tp_ret_code TYPE stpa-retcode,
      ev_tp_alog     TYPE stpa-file,
      ev_tp_slog     TYPE stpa-file,
      ev_tp_pid      TYPE stpa-pid,
      ev_tpstat_key  TYPE  tmstpkey,
      es_exception   TYPE stmscalert,
      et_tp_imports  TYPE  stms_tp_imports,
      tt_logptr      TYPE STANDARD TABLE OF  tplogptr,
      tt_stdout      TYPE STANDARD TABLE OF  tpstdout.


    CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system                  = mv_target
*       IV_DOMAIN                  =
        iv_request                 = iv_trkorr
*       IV_CLIENT                  =
*       IV_CTC_ACTIVE              =
*       IV_OVERTAKE                =
*       IV_IMPORT_AGAIN            =
*       IV_IGNORE_ORIGINALITY      =
*       IV_IGNORE_REPAIRS          =
*       IV_IGNORE_TRANSTYPE        =
*       IV_IGNORE_TABLETYPE        =
*       IV_IGNORE_QAFLAG           =
*       IV_IGNORE_PREDEC           =
*       IV_IGNORE_CVERS            =
*       IV_IGNORE_SPAM             =
*       IV_TEST_IMPORT             =
*       IV_CMD_IMPORT              =
*       IV_NO_DELIVERY             =
*       IV_SUBSET                  =
*       IV_OFFLINE                 =
*       IV_FEEDBACK                =
*       IV_MONITOR                 = 'X'
*       IV_FORCE                   =
*       IV_VERBOSE                 =
*       IS_BATCH                   =
*       IT_REQUESTS                =
*       IT_CLIENTS                 =
      IMPORTING
        ev_tp_ret_code             = ev_tp_ret_code
        ev_tp_alog                 = ev_tp_alog
        ev_tp_slog                 = ev_tp_slog
        ev_tp_pid                  = ev_tp_pid
        ev_tpstat_key              = ev_tpstat_key
        es_exception               = es_exception
        et_tp_imports              = et_tp_imports
      TABLES
        tt_logptr                  = tt_logptr
        tt_stdout                  = tt_stdout
      EXCEPTIONS
        read_config_failed         = 1
        table_of_requests_is_empty = 2
        OTHERS                     = 3.
    IF sy-subrc <> 0.
      mo_log->e( |Import of { iv_trkorr } not done, code { sy-subrc } | ).
    ELSE.
      mo_log->s( |Transport Request { iv_trkorr } imported into { mv_target } | ).
    ENDIF.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_INSERT_OBJS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRFROM                      TYPE        E070-TRKORR
* | [--->] IV_TRTO                        TYPE        E070-TRKORR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cc_insert_objs.

    CHECK: iv_trfrom IS NOT INITIAL,
           iv_trto   IS NOT INITIAL.

    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
*       WI_DIALOG                = 'X'
        wi_trkorr_from           = iv_trfrom
        wi_trkorr_to             = iv_trto
        wi_without_documentation = abap_true
      EXCEPTIONS
        db_access_error          = 1
        trkorr_from_not_exist    = 2
        trkorr_to_is_repair      = 3
        trkorr_to_locked         = 4
        trkorr_to_not_exist      = 5
        trkorr_to_released       = 6
        user_not_owner           = 7
        no_authorization         = 8
        wrong_client             = 9
        wrong_category           = 10
        object_not_patchable     = 11
        OTHERS                   = 12.
    IF sy-subrc <> 0.

      mo_log->e( |Objects copy error from { iv_trfrom } code { sy-subrc } | ).

    ELSE.

      mo_log->s( |Objects from { iv_trfrom } added to { iv_trto } | ).

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_RELEASE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRKORR                      TYPE        E070-TRKORR
* | [<-()] RS_RESULT                      TYPE        TRWBO_REQUEST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cc_release.

    DATA:
*      es_request       TYPE  trwbo_request,
      et_deleted_tasks TYPE  trwbo_t_e070,
      et_messages      TYPE  ctsgerrmsgs,
      ev_tr_lock_tst   TYPE  cts_timestamp,
      et_status_msgs   TYPE  ctsstatusmsgs.

    CHECK: iv_trkorr IS NOT INITIAL.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = iv_trkorr
        iv_dialog                   = abap_false
*       IV_AS_BACKGROUND_JOB        = ' '
*       IV_SUCCESS_MESSAGE          = 'X'
*       IV_WITHOUT_OBJECTS_CHECK    = abap_true
*       IV_CALLED_BY_ADT            = ' '
*       IV_CALLED_BY_PERFORCE       = ' '
*       IV_WITHOUT_DOCU             = ' '
        iv_without_locking          = abap_true
*       IV_DISPLAY_EXPORT_LOG       = 'X'
*       IV_IGNORE_WARNINGS          = abap_true
*       IV_SIMULATION               = lv_
*       IV_NO_RELEASE_ON_ATC_ERROR  = ' '
*       IV_ATC_OPTIONS              = ' '
*       IV_FORCE_MODE               =
      IMPORTING
        es_request                  = rs_result
        et_deleted_tasks            = et_deleted_tasks
        et_messages                 = et_messages
        ev_tr_lock_tst              = ev_tr_lock_tst
        et_status_msgs              = et_status_msgs
      EXCEPTIONS
        cts_initialization_failure  = 1
        enqueue_failed              = 2
        no_authorization            = 3
        invalid_request             = 4
        request_already_released    = 5
        repeat_too_early            = 6
        object_lock_error           = 7
        object_check_error          = 8
        docu_missing                = 9
        db_access_error             = 10
        action_aborted_by_user      = 11
        export_failed               = 12
        execute_objects_check       = 13
        release_in_bg_mode          = 14
        release_in_bg_mode_w_objchk = 15
        error_in_export_methods     = 16
        object_lang_error           = 17
        OTHERS                      = 18.
    IF sy-subrc <> 0.

      mo_log->e( |Transport { iv_trkorr } not released, code { sy-subrc } | ).
      mo_log->add_message( it_anymsg = et_messages ).

    ELSE.

      mo_log->s( |Transport { iv_trkorr } released | ).

    ENDIF.






  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STMS=>CHECK_SYSTEM_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SYST_TYPE                   TYPE        T000-CCCATEGORY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_system_type.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
*       SYSTEMEDIT         = " 'N' for pord system
*       SYSTEMNAME         = lv_current_syst_name
*       SYSTEMTYPE         = " 'CUSTOMER'
*       SYSTEM_CLIENT_EDIT = " '2' for pord, '1' for others
*       SYS_CLIINDDEP_EDIT = " '2' for pord, 'space' for others
        system_client_role = rv_syst_type " D,T,P -> main flag for syst determination
*       EV_SFW_BCSET_REC   = " not used
*       EV_C_SYSTEM        = " not used
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      CLEAR rv_syst_type.
    ENDIF.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STMS=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CLASS_CONSTRUCTOR.

    SELECT single *
      INTO @ms_tms_source
      FROM tmscsys
      WHERE sysnam = @sy-sysid.

    IF sy-subrc = 0 AND ms_tms_source IS NOT INITIAL.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE @mt_tms_targets
        FROM tmscsys
        WHERE nfsgrp = @ms_tms_source-nfsgrp
          AND sysnam <> @sy-sysid.

    ENDIF.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TARGET_SYS                  TYPE        SYST-SYSID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    mo_log = zcl_api_log=>init_popup_log( ).

    READ TABLE mt_tms_targets INTO DATA(ls_target) WITH KEY sysnam = iv_target_sys.
    IF sy-subrc = 0.
      " TODO: add checks
      mv_target = ls_target-sysnam.
    ELSE.
      mo_log->e( |Target system { iv_target_sys } unknown| ).
      " error?
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_STMS=>TEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UNAME                       TYPE        SY-UNAME (default =SY-UNAME)
* | [--->] IV_TCODE                       TYPE        SY-TCODE (default ='SM30')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD test. " >> CODE for GUI REP

**    REPORT zbc_stms_app.
**
**    SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-001.
**      PARAMETERS: p_treq TYPE e070-trkorr OBLIGATORY MEMORY ID kor,
**                  p_dest TYPE syst-sysid DEFAULT 'TST' MATCHCODE OBJECT s_realsys,
**                  p_slog AS CHECKBOX DEFAULT abap_true.
**      SELECTION-SCREEN SKIP.
**      PARAMETERS: p_copytr RADIOBUTTON GROUP gstp,
**                  p_releas RADIOBUTTON GROUP gstp,
**                  p_import RADIOBUTTON GROUP gstp DEFAULT 'X'.
**    SELECTION-SCREEN END OF BLOCK 001.
**      SELECTION-SCREEN PUSHBUTTON 2(23) TEXT-S10 USER-COMMAND TORG.
**
**      AT SELECTION-SCREEN.
**        IF sy-ucomm = 'TORG '.
**          CALL TRANSACTION 'SE10' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
**        ENDIF.
**
**
**    AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_treq.
**      CALL FUNCTION 'TR_F4_REQUESTS'
**        EXPORTING
**          iv_trkorr_pattern   = p_treq
**        IMPORTING
**          ev_selected_request = p_treq.
**
**    INITIALIZATION.
**
**    END-OF-SELECTION.
**      PERFORM main.
**
**    FORM main.
**
**      zcl_stms=>transport_with_copy( iv_copy    = p_copytr
**                                          iv_release = p_releas
**                                          iv_import  = p_import
**                                          iv_workbench   = p_treq
**                                          iv_destination = p_dest
**                                          iv_show_log    = p_slog ).
**
**    ENDFORM.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STMS=>TRANSPORT_WITH_COPY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COPY                        TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_RELEASE                     TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_IMPORT                      TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_WORKBENCH                   TYPE        E070-TRKORR (default ='')
* | [--->] IV_DESTINATION                 TYPE        SYST-SYSID (default ='')
* | [--->] IV_SHOW_LOG                    TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD transport_with_copy.
    CHECK: cl_ca_system=>is_productive( ) = abap_false. " HINT: instead, you can use FM 'TR_SYS_PARAMS'
    CHECK: check_system_type( ) = 'D'. " using FM 'TR_SYS_PARAMS'

    DATA(lo_trman) = NEW ZCL_API_STMS( iv_destination ).
    DATA(lv_workbench) = iv_workbench.
    DATA(lv_procstep) = cond byte( WHEN iv_import  = abap_true THEN 3
                                   WHEN iv_release = abap_true THEN 2
                                   WHEN iv_copy    = abap_true THEN 1
                                   ELSE 0 ).

    IF lv_procstep >= 1.
      DATA(lv_trcopy) = lo_trman->cc_create( lv_workbench ).
      lo_trman->cc_insert_objs( iv_trfrom = lv_workbench
                                iv_trto   = lv_trcopy ).
    ELSE.
      lv_trcopy = iv_workbench.
    ENDIF.

    IF lv_trcopy IS NOT INITIAL.

      IF lv_procstep >= 2.
        lo_trman->cc_release( lv_trcopy ).
      ENDIF.

      IF lv_procstep >= 3.
        lo_trman->cc_import( lv_trcopy ).
      ENDIF.
    ENDIF.

    IF iv_show_log = abap_true.
      lo_trman->mo_log->show_and_clear( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
