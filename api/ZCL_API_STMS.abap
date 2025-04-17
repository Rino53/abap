class ZCL_API_STMS definition
  public
  final
  create public .

public section.
  types: TT_E071 TYPE STANDARD TABLE OF E071.

  data MO_LOG type ref to ZCL_API_LOG.

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
      !IX_ADDOBJ type ABAP_BOOL default ABAP_FALSE
      !IV_COPY type ABAP_BOOL default ABAP_FALSE
      !IV_RELEASE type ABAP_BOOL default ABAP_FALSE
      !IV_IMPORT type ABAP_BOOL default ABAP_FALSE
      !IV_WORKBENCH type E070-TRKORR optional
      !IV_DESTINATION type SYST-SYSID optional
      !IV_TO_CLIENT type SYST-MANDT optional
      !IV_SHOW_LOG type ABAP_BOOL default ABAP_TRUE
      !IV_TRDESC type E07T-AS4TEXT default 'Manual transport'
      !IT_ADDOBJS type TABLE optional
      !IV_TREXIST type E070-TRKORR optional .
  methods CC_CREATE
    importing
      !IV_TRKORR type E070-TRKORR optional
      !IV_TRTYPE type E070-TRFUNCTION default 'T'
      !IV_POSTFIX type TEXT6 default '[copy]'
      !IV_NEWREQNAME type E07T-AS4TEXT optional
    preferred parameter IV_TRKORR
    returning
      value(RV_TRNEW) type E070-TRKORR .
  methods CONSTRUCTOR
    importing
      !IV_TARGET_SYS type SYST-SYSID
      !IV_TO_CLIENT type SYST-MANDT optional .
  class-methods CLASS_CONSTRUCTOR .
  methods CC_ADD_OBJ
    importing
      !IV_TRTO type E070-TRKORR default 'DEV123'
      !IV_OBJ type E071-OBJ_NAME default 'Z_SOME_FUNC' .
  methods CC_ADD_OBJS
    importing
      !IV_TRTO type E070-TRKORR
      !IV_OBJ type E071-OBJ_NAME optional
      !IT_OBJS type TABLE .
  methods GET_OBJ_INFO
    importing
      !IV_OBJ type CLIKE optional
    exporting
      !ES_OBJ_INFO type E071
    changing
      !CT_TRINFO type TT_E071 .
protected section.
private section.

  class-data MS_TMS_SOURCE type TMSCSYS .
  class-data:
    MT_TMS_targets type STANDARD TABLE OF TMSCSYS .
  data mv_target type TMSCSYS-SYSNAM .
  data mv_to_client type STPA-CLIENT .

  class-methods TEST
    importing
      !IV_UNAME type SY-UNAME default SY-UNAME
      !IV_TCODE type SY-TCODE default 'SM30' .
ENDCLASS.



CLASS ZCL_API_STMS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_ADD_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRTO                        TYPE        E070-TRKORR (default ='DEV123')
* | [--->] IV_OBJ                         TYPE        E071-OBJ_NAME (default ='Z_SOME_FUNC')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cc_add_obj.

    CHECK:
           iv_trto IS NOT INITIAL,
           iv_obj IS NOT INITIAL.



    DATA:
*      lv_trdir      TYPE progname,
      lt_e071       TYPE STANDARD TABLE OF  e071,
      lt_trkey      TYPE STANDARD TABLE OF  trkey,
      lt_devcl_info TYPE STANDARD TABLE OF  devcl_info,
      lt_tadir      TYPE STANDARD TABLE OF  tadir,
      lt_tfdir      TYPE STANDARD TABLE OF  tfdir,
      lt_messages   TYPE  bapiret2_t.

*    lv_trdir = iv_obj.

    CALL FUNCTION '/SDF/TEAP_GET_TADIR'
      EXPORTING
*       IS_TADIR    =
        iv_trdir    = CONV progname( iv_obj )
*     IMPORTING
*       EV_COMPONENT  =
      TABLES
*       it_e071     = lt_e071
*       et_trkey    = lt_trkey
*       et_devcl_info = lt_devcl_info
        et_tadir    = lt_tadir
        et_messages = lt_messages.

    LOOP AT lt_tadir INTO DATA(ls_tadir) WHERE obj_name IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_e071 ASSIGNING FIELD-SYMBOL(<ls_tobj>).
      <ls_tobj>-trkorr = iv_trto.
*    <ls_tobj>-pgmid = 'LIMU'.
      <ls_tobj>-pgmid = ls_tadir-pgmid.
*    <ls_tobj>-object = 'REPS'.
      <ls_tobj>-object = ls_tadir-object.
      <ls_tobj>-obj_name = ls_tadir-obj_name.
    ENDLOOP.

    IF lt_tadir IS INITIAL.

      CALL FUNCTION '/SNP/BB0X_RFC_READ_TFDIR'
        EXPORTING
          iv_name  = CONV tfdir-funcname( iv_obj )
*         IV_MAIN_PROGRAM       = '*'
*         IV_MAX_COUNT          = 0
        TABLES
          et_tfdir = lt_tfdir.

      LOOP AT lt_tfdir INTO DATA(ls_tfdir) WHERE funcname IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_e071 ASSIGNING <ls_tobj>.
        <ls_tobj>-trkorr = iv_trto.
        <ls_tobj>-pgmid = 'LIMU'.
        <ls_tobj>-object = 'FUNC'.
        <ls_tobj>-obj_name = ls_tfdir-funcname.

      ENDLOOP.

      IF lt_tfdir IS NOT INITIAL.

*        CALL FUNCTION '/SNP/BB0X_RFC_READ_TMDIR'
*         EXPORTING
*           IV_CLASS_NAME        = '*'
*           IV_METHOD_NAME       = '*'
*           IV_MAX_COUNT         = 0
*          TABLES
*            et_tmdir             =
                  .


      ENDIF.

    ENDIF.

    IF lt_e071 IS NOT INITIAL.

      CALL FUNCTION 'CTS_LOCK_TRKORR'        " eclipse compatible locking...
        EXPORTING
          iv_trkorr = iv_trto
        EXCEPTIONS
          OTHERS    = 1.

      IF sy-subrc = 0.

        DATA(lv_simu) = CONV abap_bool( space ).

        CALL FUNCTION 'TRINT_APPEND_TO_COMM_ARRAYS'
          EXPORTING
*           WI_ERROR_TABLE            = ' '
            wi_simulation             = lv_simu
*           WI_SUPPRESS_KEY_CHECK     = ' '
            wi_trkorr                 = iv_trto
*           WI_TRPAR_INT_FILLED       = ' '
*           WI_LOCKKEY_FILLED         = ' '
            iv_append_at_order        = abap_true
*           IV_APPEND_AT_ORDER_WITH_LOCK       = ' '
*           IV_NO_OWNER_CHECK         = ' '
*           IT_E071K_STR              =
*           IV_DIALOG                 = 'X'
*           IV_CHECK_ID               =
          TABLES
            wt_e071                   = lt_e071
*           WT_E071K                  =
*           WT_TRMESS_INT             =
*           WT_TRPAR_INT              =
*           WT_LOCKKEY                =
          EXCEPTIONS
            key_check_keysyntax_error = 1
            ob_check_obj_error        = 2
            tr_lockmod_failed         = 3
            tr_lock_enqueue_failed    = 4
            tr_wrong_order_type       = 5
            tr_order_update_error     = 6
            file_access_error         = 7
            ob_no_systemname          = 8
            OTHERS                    = 9.
        IF sy-subrc <> 0.
          ROLLBACK WORK.
          mo_log->e( |Object { iv_obj } adding error to { iv_trto } code { sy-subrc } | ).

        ELSE.
          COMMIT WORK AND WAIT.
          mo_log->s( |Object { iv_obj } added to { iv_trto } | ).

        ENDIF.


        CALL FUNCTION 'CTS_UNLOCK_TRKORR'
          EXPORTING
            iv_trkorr = iv_trto
          EXCEPTIONS
            OTHERS    = 1.


      ENDIF.
    ENDIF.







  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_ADD_OBJS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRTO                        TYPE        E070-TRKORR
* | [--->] IV_OBJ                         TYPE        E071-OBJ_NAME(optional)
* | [--->] IT_OBJS                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CC_ADD_OBJS.

     CHECK:
           iv_trto IS NOT INITIAL,
           it_objs IS NOT INITIAL.

        DATA:
*      lv_trdir      TYPE progname,
      lt_e071       TYPE STANDARD TABLE OF  e071.
*      lt_trkey      TYPE STANDARD TABLE OF  trkey,
*      lt_devcl_info TYPE STANDARD TABLE OF  devcl_info,
*      lt_tadir      TYPE STANDARD TABLE OF  tadir,
*      lt_tfdir      TYPE STANDARD TABLE OF  tfdir,
*      lt_messages   TYPE  bapiret2_t.


    CHECK: iv_trto IS NOT INITIAL.

    LOOP AT it_objs ASSIGNING FIELD-SYMBOL(<lv_obj>).
      IF <lv_obj> IS NOT INITIAL.
        get_obj_info( EXPORTING iv_obj = <lv_obj>
                      CHANGING  ct_trinfo = lt_e071 ).
      ENDIF.
    ENDLOOP.


    IF lt_e071 IS NOT INITIAL.

      CALL FUNCTION 'CTS_LOCK_TRKORR'        " eclipse compatible locking...
        EXPORTING
          iv_trkorr = iv_trto
        EXCEPTIONS
          OTHERS    = 1.

      IF sy-subrc = 0.

        DATA(lv_simu) = CONV abap_bool( space ).

        CALL FUNCTION 'TRINT_APPEND_TO_COMM_ARRAYS'
          EXPORTING
*           WI_ERROR_TABLE            = ' '
            wi_simulation             = lv_simu
*           WI_SUPPRESS_KEY_CHECK     = ' '
            wi_trkorr                 = iv_trto
*           WI_TRPAR_INT_FILLED       = ' '
*           WI_LOCKKEY_FILLED         = ' '
            iv_append_at_order        = abap_true
*           IV_APPEND_AT_ORDER_WITH_LOCK       = ' '
*           IV_NO_OWNER_CHECK         = ' '
*           IT_E071K_STR              =
*           IV_DIALOG                 = 'X'
*           IV_CHECK_ID               =
          TABLES
            wt_e071                   = lt_e071
*           WT_E071K                  =
*           WT_TRMESS_INT             =
*           WT_TRPAR_INT              =
*           WT_LOCKKEY                =
          EXCEPTIONS
            key_check_keysyntax_error = 1
            ob_check_obj_error        = 2
            tr_lockmod_failed         = 3
            tr_lock_enqueue_failed    = 4
            tr_wrong_order_type       = 5
            tr_order_update_error     = 6
            file_access_error         = 7
            ob_no_systemname          = 8
            OTHERS                    = 9.
        IF sy-subrc <> 0.
          ROLLBACK WORK.
          mo_log->e( |Objects adding error to { iv_trto } code { sy-subrc } | ).

        ELSE.
          COMMIT WORK AND WAIT.
          mo_log->s( |Objects added to { iv_trto } | ).

        ENDIF.


        CALL FUNCTION 'CTS_UNLOCK_TRKORR'
          EXPORTING
            iv_trkorr = iv_trto
          EXCEPTIONS
            OTHERS    = 1.


      ENDIF.
    ENDIF.







  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->CC_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TRKORR                      TYPE        E070-TRKORR(optional)
* | [--->] IV_TRTYPE                      TYPE        E070-TRFUNCTION (default ='T')
* | [--->] IV_POSTFIX                     TYPE        TEXT6 (default ='[copy]')
* | [--->] IV_NEWREQNAME                  TYPE        E07T-AS4TEXT(optional)
* | [<-()] RV_TRNEW                       TYPE        E070-TRKORR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cc_create.

    CONSTANTS: lc_desc_std TYPE E07T-AS4TEXT VALUE 'CC Manual transport'.

    DATA: ls_request TYPE strhi_request_wd.
    ls_request-h-trkorr = iv_trkorr.

    CHECK: mv_target IS NOT INITIAL.

    IF ls_request-h-trkorr IS NOT INITIAL.

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
*         ERROR_OCCURED      = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        mo_log->e( |Request { iv_trkorr } read error| ).
        RETURN.

      ENDIF.

    ELSE.

      ls_request-h-as4text = COND #( WHEN iv_newreqname IS NOT INITIAL THEN iv_newreqname ELSE lc_desc_std ).

    ENDIF.

    " trfunction:
*  K  Workbench Request
*  W  Customizing Request
*  T  Transport of Copies
*
*  Q  Customizing Task
*  S  Development/Correction
*  R  Repair
*
*  X  Unclassified Task
*
*  C  Relocation of Objects Without Package Change
*  O  Relocation of Objects with Package Change
*  E  Relocation of complete package
*
*  G  Piece List for CTS Project
*  M  Client Transport Request
*  P  Piece List for Upgrade
*  D  Piece List for Support Package
*  F  Piece List
*  L  Deletion transport
*  Y  Piece list for commit
*  __________	____________________________________________________________

    DATA(lv_postfix) = |[{ sy-datum+4(4) }.{ sy-timlo(4) }-{ COND char02( WHEN ls_request-h-trfunction = 'K' THEN 'WR' " Workbench Request
                                                                          WHEN ls_request-h-trfunction = 'S' THEN 'WT' " Workbench Task
                                                                          WHEN ls_request-h-trfunction = 'W' THEN 'CR' " Customizing Request
                                                                          WHEN ls_request-h-trfunction = 'Q' THEN 'CT' " Customizing Task
                                                                          WHEN ls_request-h-trfunction = 'R' THEN 'RT' " Repair Task
                                                                          ELSE 'XX' ) }]|. " [0903.1513-WB]

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

    DATA: lv_verbose type STMS_FLAG VALUE abap_false.

    CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system                  = mv_target
*       IV_DOMAIN                  =
        iv_request                 = iv_trkorr
        iv_client                  = mv_to_client
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
       IV_VERBOSE                 = lv_verbose
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
      mo_log->s( |Transport Request { iv_trkorr } imported into { mv_target }/{ mv_to_client } | ).
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
* | [--->] IV_TO_CLIENT                   TYPE        SYST-MANDT(optional)
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

    IF iv_to_client IS NOT INITIAL.
      mv_to_client = iv_to_client.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_STMS->GET_OBJ_INFO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJ                         TYPE        CLIKE(optional)
* | [<---] ES_OBJ_INFO                    TYPE        E071
* | [<-->] CT_TRINFO                      TYPE        TT_E071
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_obj_info.

    CHECK:
           iv_obj IS NOT INITIAL.



    DATA:
*      lv_trdir      TYPE progname,
*      lt_e071       TYPE STANDARD TABLE OF  e071,
*      lt_trkey      TYPE STANDARD TABLE OF  trkey,
*      lt_devcl_info TYPE STANDARD TABLE OF  devcl_info,
      lt_trkey    TYPE STANDARD TABLE OF  trkey,
      lt_tadir    TYPE STANDARD TABLE OF  tadir,
      lt_tfdir    TYPE STANDARD TABLE OF  tfdir,
      lt_messages TYPE  bapiret2_t.

*    lv_trdir = iv_obj.

    CALL FUNCTION '/SDF/TEAP_GET_TADIR'
      EXPORTING
*       IS_TADIR    =
        iv_trdir    = CONV progname( iv_obj )
*     IMPORTING
*       EV_COMPONENT  =
      TABLES
*       it_e071     = lt_e071
        et_trkey    = lt_trkey
*       et_devcl_info = lt_devcl_info
        et_tadir    = lt_tadir
        et_messages = lt_messages.

    LOOP AT lt_tadir INTO DATA(ls_tadir) WHERE obj_name IS NOT INITIAL.

      CLEAR: es_obj_info.

      IF ls_tadir-obj_name = iv_obj.

        es_obj_info-pgmid  = ls_tadir-pgmid.
        es_obj_info-object = ls_tadir-object.
        es_obj_info-obj_name = ls_tadir-obj_name.

      ELSE.
        READ TABLE lt_trkey INTO DATA(ls_trkey) WITH KEY devclass = ls_tadir-devclass
                                                         obj_type = ls_tadir-object
                                                         obj_name = ls_tadir-obj_name.
        IF sy-subrc = 0.
          es_obj_info-pgmid = 'LIMU'. " sub_type = REPS
          es_obj_info-object   = ls_trkey-sub_type.
          es_obj_info-obj_name = ls_trkey-sub_name.
        ENDIF.


      ENDIF.

      IF es_obj_info IS NOT INITIAL.
        APPEND es_obj_info TO ct_trinfo.
        mo_log->s( |Object { es_obj_info-obj_name } added | ).

      ENDIF.

    ENDLOOP.

    IF lt_tadir IS INITIAL.

      CALL FUNCTION '/SNP/BB0X_RFC_READ_TFDIR'
        EXPORTING
          iv_name  = CONV tfdir-funcname( iv_obj )
*         IV_MAIN_PROGRAM       = '*'
*         IV_MAX_COUNT          = 0
        TABLES
          et_tfdir = lt_tfdir.

      LOOP AT lt_tfdir INTO DATA(ls_tfdir) WHERE funcname IS NOT INITIAL.
        APPEND INITIAL LINE TO ct_trinfo ASSIGNING FIELD-SYMBOL(<ls_tobj>).
*        <ls_tobj>-trkorr = iv_trto.
        <ls_tobj>-pgmid = 'LIMU'.
        <ls_tobj>-object = 'FUNC'.
        <ls_tobj>-obj_name = ls_tfdir-funcname.

        mo_log->s( |Object { <ls_tobj>-obj_name } added | ).

      ENDLOOP.

      IF lt_tfdir IS INITIAL.

        DATA:
          lv_class_name TYPE tmdir-classname,
          lv_meth_name  TYPE tmdir-methodname,
          lt_tmdir      TYPE STANDARD TABLE OF tmdir.

        SPLIT iv_obj AT ' ' INTO lv_class_name lv_meth_name.
        IF lv_meth_name IS INITIAL.
          SPLIT iv_obj AT '=>' INTO lv_class_name lv_meth_name.
          IF lv_meth_name IS INITIAL.
            SPLIT iv_obj AT '->' INTO lv_class_name lv_meth_name.
            IF lv_meth_name IS INITIAL.
                SPLIT iv_obj AT '-' INTO lv_class_name lv_meth_name.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lv_class_name IS NOT INITIAL AND
           lv_meth_name IS NOT INITIAL.

          CALL FUNCTION '/SNP/BB0X_RFC_READ_TMDIR'
            EXPORTING
              iv_class_name  = lv_class_name
              iv_method_name = lv_meth_name
*             IV_MAX_COUNT   = 0
            TABLES
              et_tmdir       = lt_tmdir.

          IF lines( lt_tmdir ) = 1.
            LOOP AT lt_tmdir INTO DATA(ls_clsobj).
              APPEND INITIAL LINE TO ct_trinfo ASSIGNING <ls_tobj>.
*        <ls_tobj>-trkorr = iv_trto.
              <ls_tobj>-pgmid = 'LIMU'.
              <ls_tobj>-object = 'METH'.
              <ls_tobj>-obj_name = |{ ls_clsobj-classname ALPHA = OUT }{ ls_clsobj-methodname }|. " classname = exact 30 chars

              mo_log->s( |Object { ls_clsobj-classname }->{ ls_clsobj-methodname } added | ).
            ENDLOOP.

          ENDIF.


        ENDIF.
      ENDIF.
    ENDIF.

    IF <ls_tobj> IS ASSIGNED.
      es_obj_info = <ls_tobj>.
    ENDIF.




  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_STMS=>TEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UNAME                       TYPE        SY-UNAME (default =SY-UNAME)
* | [--->] IV_TCODE                       TYPE        SY-TCODE (default ='SM30')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD test. " >> CODE for GUI REP
*  *&---------------------------------------------------------------------*
*  *& Report ZCL_API_STMS_APP
*  *&---------------------------------------------------------------------*
*  *&  Create ToC and import it into Test system
*  *&---------------------------------------------------------------------*
*  REPORT zbc_stms_app
*
*  *    P_ADDOBJ  0. Add objs to transport
*  *    P_COPYTR  1. Create Transport of Copy
*  *    P_RELEAS  2. Release ToC
*  *    P_IMPORT  3. Import ToC
*  *    P_SLOG  Show Log
*  *    P_DESC  Transport Description
*  *    P_TEXIST  Existing request
*
*  SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-001.
*    PARAMETERS:
*                p_treq TYPE e070-trkorr MEMORY ID kor,
*                p_texist TYPE e070-trkorr MEMORY ID zstex,
*                p_dest TYPE syst-sysid DEFAULT 'PRD' MATCHCODE OBJECT s_realsys,
*                p_clnt TYPE stpa-client MATCHCODE OBJECT zstmc,
*                p_slog AS CHECKBOX DEFAULT abap_true.
*    SELECTION-SCREEN SKIP.
*    PARAMETERS:
*                p_obj1 TYPE E071-OBJ_NAME MEMORY ID sto1,
*                p_obj2 TYPE E071-OBJ_NAME MEMORY ID sto2,
*                p_obj3 TYPE E071-OBJ_NAME MEMORY ID sto3,
*                p_desc TYPE E07T-AS4TEXT DEFAULT 'CC Manual transport'.
*    SELECTION-SCREEN SKIP.
*    PARAMETERS:
*                p_addobj RADIOBUTTON GROUP gstp DEFAULT 'X',
*                p_copytr RADIOBUTTON GROUP gstp,
*                p_releas RADIOBUTTON GROUP gstp,
*                p_import RADIOBUTTON GROUP gstp.
*  SELECTION-SCREEN END OF BLOCK 001.
*
*  SELECTION-SCREEN PUSHBUTTON 2(23) TEXT-S10 USER-COMMAND TORG.
*
*  AT SELECTION-SCREEN.
*    IF sy-ucomm = 'TORG '.
*      CALL TRANSACTION 'SE10' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
*    ENDIF.
*
*
*  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_treq.
*    CALL FUNCTION 'TR_F4_REQUESTS'
*      EXPORTING
*        iv_trkorr_pattern   = p_treq
*      IMPORTING
*        ev_selected_request = p_treq.
*
*  INITIALIZATION.
*
*  END-OF-SELECTION.
*    PERFORM main.
*
*  FORM main.
*
*    ZCL_API_STMS=>transport_with_copy( iv_copy    = p_copytr
*                                        iv_release = p_releas
*                                        iv_import  = p_import
*                                        ix_addobj  = p_addobj
*                                        iv_workbench = p_treq
*                                        iv_trexist   = p_texist
*                                        iv_destination = p_dest
*                                        iv_to_client = p_clnt
*                                        iv_show_log    = p_slog
*                                        iv_trdesc = p_desc
*                                        it_addobjs = VALUE sfw_t_transkey( ( p_obj1 ) ( p_obj2 ) ( p_obj3 ) )
*                                        ).
*
*  ENDFORM.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STMS=>TRANSPORT_WITH_COPY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IX_ADDOBJ                      TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_COPY                        TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_RELEASE                     TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_IMPORT                      TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_WORKBENCH                   TYPE        E070-TRKORR(optional)
* | [--->] IV_DESTINATION                 TYPE        SYST-SYSID(optional)
* | [--->] IV_TO_CLIENT                   TYPE        SYST-MANDT(optional)
* | [--->] IV_SHOW_LOG                    TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_TRDESC                      TYPE        E07T-AS4TEXT (default ='Manual transport')
* | [--->] IT_ADDOBJS                     TYPE        TABLE(optional)
* | [--->] IV_TREXIST                     TYPE        E070-TRKORR(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD transport_with_copy.

    CHECK: cl_ca_system=>is_productive( ) = abap_false. " HINT: instead, you can use FM 'TR_SYS_PARAMS'
    CHECK: check_system_type( ) = 'D'. " using FM 'TR_SYS_PARAMS'

    DATA(lo_trman) = NEW ZCL_API_STMS( iv_target_sys = iv_destination
                                        iv_to_client  = iv_to_client ).
    DATA(lv_workbench) = iv_workbench.
    DATA(lv_procstep) = cond byte( WHEN iv_import  = abap_true THEN 3
                                   WHEN iv_release = abap_true THEN 2
                                   WHEN iv_copy    = abap_true THEN 1
                                   ELSE 0 ).
    DATA(lv_trcopy) = iv_trexist.

    IF lv_procstep >= 1.
      IF lv_trcopy IS INITIAL.
        lv_trcopy = lo_trman->cc_create( iv_trkorr = lv_workbench
                                         iv_newreqname = iv_trdesc ).
      ENDIF.

      IF lv_workbench IS NOT INITIAL.
        lo_trman->cc_insert_objs( iv_trfrom = lv_workbench
                                  iv_trto   = lv_trcopy ).
      ENDIF.



*    ELSE.
*      lv_trcopy = iv_workbench.
    ENDIF.

    IF lv_trcopy IS NOT INITIAL.
      lo_trman->cc_add_objs( iv_trto = lv_trcopy
                             it_objs = it_addobjs[] ).


    ENDIF.


    IF lv_trcopy IS NOT INITIAL.

      IF lv_procstep >= 2.
        DATA(lv_do_release) = lo_trman->mo_log->show( iv_title = |Continue? dest { lo_trman->mv_target }/{ lo_trman->mv_to_client }| ).
        IF lv_do_release = lo_trman->mo_log->c_window_action-okay.  "&ONT.
          lo_trman->cc_release( lv_trcopy ).
        ENDIF.
      ENDIF.

      IF lv_procstep >= 3 AND
         lv_do_release = lo_trman->mo_log->c_window_action-okay.
        lo_trman->cc_import( lv_trcopy ).
      ENDIF.
    ENDIF.

    IF iv_show_log = abap_true.
      lo_trman->mo_log->show_and_clear( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
