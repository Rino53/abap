class ZCL_API_STATUS definition
  public
  final
  create private .

public section.

  types:
    TT_JEST TYPE STANDARD TABLE OF jest .
  types:
    TT_JSTO TYPE STANDARD TABLE OF jsto .
  types:
    TT_Jcdo TYPE STANDARD TABLE OF jcdo .
  types:
    TT_Jcds TYPE STANDARD TABLE OF jcds .
  types:
    TT_tj02t TYPE STANDARD TABLE OF tj02t .

  constants C_VKORG_VKTT type AUART value 'VKTT' ##NO_TEXT.
  constants C_PROFILE_ZSD1 type JSTO-STSMA value 'ZSD00001' ##NO_TEXT.
  constants C_STATUS_E3 type JEST-STAT value 'E0003' ##NO_TEXT.
  constants C_STATUS_E4 type JEST-STAT value 'E0004' ##NO_TEXT.
  constants C_STATUS_E1 type JEST-STAT value 'E0001' ##NO_TEXT.
  constants C_TMP_VBELN type VBELN_VA value 'ZZTMPVBELN' ##NO_TEXT.
  constants C_OBTYP_VBK type JSTO-OBTYP value 'VBK' ##NO_TEXT.
  constants C_OBTYP_VBP type JSTO-OBTYP value 'VBP' ##NO_TEXT.
  constants C_POSNR_INITIAL type POSNR_VA value '000000' ##NO_TEXT.
  class-data GT_VBAK_JEST_OLD type TT_JEST .
  
  class-methods TEXT_CONVERSION
    importing
      !IV_OBJNR type JSTO-OBJNR
      !IV_TXT04 type TJ02T-TXT04
      !IV_LANG type SY-LANGU default SY-LANGU
      !IV_MODE type CHAR1 optional
      !IV_STSMA type JSTO-STSMA optional
    returning
      value(RV_STATUS) type JEST-STAT .
  class-methods CHECK
    importing
      !IV_BYPASS_BUFFER type BOOLE_D default ABAP_FALSE
      !IV_OBJNR type JEST-OBJNR
      !IV_STATUS type JEST-STAT optional
      !IV_TXT04 type J_TXT04 optional
    returning
      value(RV_OK) type SUBRC .
  class-methods CHANGE_INTERN
    importing
      !IV_OBJNR type JSTO-OBJNR
      !IV_FROM type J_STATUS optional
      !IV_TO type J_STATUS optional
    changing
      !CT_JSTAT type TT_JSTAT optional
    returning
      value(RV_OK) type SYST_SUBRC .
  class-methods GET_BY_MASK
    importing
      !IV_OBJNR type J_OBJNR
      !IV_MASK type CHAR10 default 'E*'
      !IT_STATUSES type TT_JEST
    returning
      value(RS_STATUS) type JEST .
  class-methods CHANGE_EXTERN
    importing
      !IV_OBJNR type JSTO-OBJNR
      !IV_STATUS type JEST-STAT
    returning
      value(RV_OK) type SYST_SUBRC .
  class-methods READ_MULTI
    importing
      !IV_ONLY_ACTIVE type BOOLE_D default ABAP_TRUE
      !IV_ALL_IN_BUFFER type BOOLE_D default ABAP_TRUE
      !IV_NO_BUFFER_FILL type BOOLE_D default ABAP_TRUE
      !IV_GET_CHANGE_DOCUMENTS type BOOLE_D default ABAP_TRUE
      !IT_ANYTABLE type ANY TABLE optional
      !IV_OBJNR_FIELD type FIELDNAME default 'OBJNR'
      !IV_OBJNR type J_OBJNR optional
    changing
      !CT_OBJNRS type TT_OBJ_NUM optional
      !CT_JEST type TT_JEST optional
      !CT_JSTO type TT_JSTO optional
      !CT_JCDO type TT_JCDO optional
      !CT_JCDS type TT_JCDS optional
      !CT_TJ02T type TT_TJ02T optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_STATUS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STATUS=>CHANGE_EXTERN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNR                       TYPE        JSTO-OBJNR
* | [--->] IV_STATUS                      TYPE        JEST-STAT
* | [<-()] RV_OK                          TYPE        SYST_SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CHANGE_EXTERN.

    CALL FUNCTION 'STATUS_CHANGE_EXTERN'
      EXPORTING
*       CHECK_ONLY                = ' '
*       CLIENT                    = SY-MANDT
        objnr                     = iv_objnr
        user_status               = iv_status
*       SET_INACT                 = ' '
*       SET_CHGKZ                 =
*       NO_CHECK                  = ' '
*     IMPORTING
*       STONR                     =
     EXCEPTIONS
       OBJECT_NOT_FOUND          = 1
       STATUS_INCONSISTENT       = 2
       STATUS_NOT_ALLOWED        = 3
       OTHERS                    = 4
              .
    IF sy-subrc <> 0.
      rv_ok = sy-subrc.
    ENDIF.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STATUS=>CHANGE_INTERN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNR                       TYPE        JSTO-OBJNR
* | [--->] IV_FROM                        TYPE        J_STATUS(optional)
* | [--->] IV_TO                          TYPE        J_STATUS(optional)
* | [<-->] CT_JSTAT                       TYPE        TT_JSTAT(optional)
* | [<-()] RV_OK                          TYPE        SYST_SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD change_intern.

    IF iv_from IS NOT INITIAL.
      APPEND VALUE #( stat = iv_from inact = abap_true  ) TO ct_jstat.
    ENDIF.

    IF iv_to IS NOT INITIAL.
      APPEND VALUE #( stat = iv_to   inact = abap_false ) TO ct_jstat.
    ENDIF.

    CALL FUNCTION 'STATUS_CHANGE_INTERN'
      EXPORTING
*       CHECK_ONLY          = ' '
*       CLIENT              = SY-MANDT
        objnr               = iv_objnr
*       ZEILE               = ' '
*       SET_CHGKZ           =
*   IMPORTING
*       ERROR_OCCURRED      =
*       OBJECT_NOT_FOUND    =
*       STATUS_INCONSISTENT =
*       STATUS_NOT_ALLOWED  =
      TABLES
        status              = ct_jstat
      EXCEPTIONS
        object_not_found    = 1
        status_inconsistent = 2
        status_not_allowed  = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      rv_ok = sy-subrc.
    ENDIF.


  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STATUS=>CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BYPASS_BUFFER               TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_OBJNR                       TYPE        JEST-OBJNR
* | [--->] IV_STATUS                      TYPE        JEST-STAT(optional)
* | [--->] IV_TXT04                       TYPE        J_TXT04(optional)
* | [<-()] RV_OK                          TYPE        SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check.
    DATA: l_status TYPE jest-stat.

    CLEAR rv_ok.

    IF iv_status IS NOT INITIAL.
      l_status = iv_status.
    ELSEIF iv_txt04 IS NOT INITIAL.
      l_status = text_conversion( iv_objnr = iv_objnr iv_txt04 = iv_txt04 ).
    ELSE.
      rv_ok = 9.
      RETURN.
    ENDIF.

    CALL FUNCTION 'STATUS_CHECK'
      EXPORTING
        bypass_buffer     = iv_bypass_buffer
        objnr             = iv_objnr
        status            = l_status
      EXCEPTIONS
        object_not_found  = 1
        status_not_active = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      rv_ok = sy-subrc.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STATUS=>GET_BY_MASK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNR                       TYPE        J_OBJNR
* | [--->] IV_MASK                        TYPE        CHAR10 (default ='E*')
* | [--->] IT_STATUSES                    TYPE        TT_JEST
* | [<-()] RS_STATUS                      TYPE        JEST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_BY_MASK.
    CLEAR rs_status.
    LOOP AT it_statuses INTO DATA(ls_status) WHERE objnr = iv_objnr AND
                                                   stat CP iv_mask.
      rs_status = ls_status.
      exit.
    ENDLOOP.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STATUS=>READ_MULTI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ONLY_ACTIVE                 TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_ALL_IN_BUFFER               TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_NO_BUFFER_FILL              TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_GET_CHANGE_DOCUMENTS        TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IT_ANYTABLE                    TYPE        ANY TABLE(optional)
* | [--->] IV_OBJNR_FIELD                 TYPE        FIELDNAME (default ='OBJNR')
* | [--->] IV_OBJNR                       TYPE        J_OBJNR(optional)
* | [<-->] CT_OBJNRS                      TYPE        TT_OBJ_NUM(optional)
* | [<-->] CT_JEST                        TYPE        TT_JEST(optional)
* | [<-->] CT_JSTO                        TYPE        TT_JSTO(optional)
* | [<-->] CT_JCDO                        TYPE        TT_JCDO(optional)
* | [<-->] CT_JCDS                        TYPE        TT_JCDS(optional)
* | [<-->] CT_TJ02T                       TYPE        TT_TJ02T(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD READ_MULTI.
    FIELD-SYMBOLS: <ls_structure> TYPE any,
                   <lv_field>     TYPE any.

    IF iv_objnr IS NOT INITIAL.
      APPEND iv_objnr TO ct_objnrs[].
    ENDIF.

    IF it_anytable IS NOT INITIAL.
      LOOP AT it_anytable ASSIGNING <ls_structure>.
        ASSIGN COMPONENT iv_objnr_field OF STRUCTURE <ls_structure> TO <lv_field>.
        IF sy-subrc = 0.
          APPEND <lv_field> TO ct_objnrs[].
        ENDIF.
      ENDLOOP.
    ENDIF.

    CHECK ct_objnrs[] IS NOT INITIAL.

    CALL FUNCTION 'STATUS_READ_MULTI'
      EXPORTING
        only_active          = iv_only_active
        all_in_buffer        = iv_all_in_buffer
        get_change_documents = iv_get_change_documents
        no_buffer_fill       = iv_no_buffer_fill
      TABLES
        objnr_tab            = ct_objnrs
        status               = ct_jest
        jsto_tab             = ct_jsto
        jcdo_tab             = ct_jcdo
        jcds_tab             = ct_jcds.

    DATA: _debug TYPE boole_d VALUE abap_false.
    IF ( ct_tj02t IS REQUESTED AND ct_jcds[] IS NOT INITIAL ) OR
        _debug = abap_true.

      DATA(lt_statuses) = ct_jcds[].
      SORT lt_statuses BY stat.
      DELETE ADJACENT DUPLICATES FROM lt_statuses COMPARING stat.

      SELECT *
        FROM tj02t
        INTO TABLE ct_tj02t
        FOR ALL ENTRIES IN lt_statuses[]
        WHERE istat = lt_statuses-stat
          AND spras = sy-langu.

    ENDIF.



  ENDMETHOD.
  
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_STATUS=>TEXT_CONVERSION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNR                       TYPE        JSTO-OBJNR
* | [--->] IV_TXT04                       TYPE        TJ02T-TXT04
* | [--->] IV_LANG                        TYPE        SY-LANGU (default =SY-LANGU)
* | [--->] IV_MODE                        TYPE        CHAR1(optional)
* | [--->] IV_STSMA                       TYPE        JSTO-STSMA(optional)
* | [<-()] RV_STATUS                      TYPE        JEST-STAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD text_conversion.

  IF iv_mode IS NOT INITIAL.

    CALL FUNCTION 'STATUS_TEXT_CONVERSION'
      EXPORTING
        language           = iv_lang
        mode               = iv_mode
        objnr              = iv_objnr
        stsma              = iv_stsma
        txt04              = iv_txt04
      IMPORTING
        status_number      = rv_status
      EXCEPTIONS
        insufficient_input = 1
        not_found          = 2
        object_not_found   = 3
        wrong_mode         = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      CLEAR rv_status.
    ENDIF.

  ELSE.

    CALL FUNCTION 'STATUS_TEXT_CONVERSION'
      EXPORTING
        language           = iv_lang
        mode               = 'E'
        objnr              = iv_objnr
        stsma              = iv_stsma
        txt04              = iv_txt04
      IMPORTING
        status_number      = rv_status
      EXCEPTIONS
        insufficient_input = 1
        not_found          = 2
        object_not_found   = 3
        wrong_mode         = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      CALL FUNCTION 'STATUS_TEXT_CONVERSION'
        EXPORTING
          language           = iv_lang
          mode               = 'I'
          objnr              = iv_objnr
          stsma              = iv_stsma
          txt04              = iv_txt04
        IMPORTING
          status_number      = rv_status
        EXCEPTIONS
          insufficient_input = 1
          not_found          = 2
          object_not_found   = 3
          wrong_mode         = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        CLEAR rv_status.
      ENDIF.
    ENDIF.


  ENDIF.


ENDMETHOD.
ENDCLASS.
