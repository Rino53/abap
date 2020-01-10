CLASS zcl_tvarvc DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_value
      IMPORTING
        !iv_name TYPE simple
        !iv_value TYPE data
      RETURNING
        value(rv_result) TYPE flag .
    CLASS-METHODS get_range
      IMPORTING
        !iv_name TYPE simple
      EXPORTING
        !er_range TYPE STANDARD TABLE .
    CLASS-METHODS get_value
      IMPORTING
        !iv_name TYPE simple
        !iv_default TYPE data OPTIONAL
      EXPORTING
        !ev_value TYPE data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_data,
          sign TYPE tvarvc-sign,
          option TYPE tvarvc-opti,
          low  TYPE tvarvc-low,
          high TYPE tvarvc-high,
        END OF ts_data .
    TYPES:
      tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_cache,
          name   TYPE tvarvc-name,
          t_data TYPE tt_data,
        END OF ts_cache .
    TYPES:
      tt_cache TYPE HASHED TABLE OF ts_cache WITH UNIQUE KEY name .

    CLASS-DATA st_cache TYPE tt_cache .

    CLASS-METHODS _get_ref
      IMPORTING
        !iv_name TYPE simple
      RETURNING
        value(rps_cache) TYPE REF TO ts_cache .
ENDCLASS.



CLASS ZCL_TVARVC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARVC=>CHECK_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [--->] IV_VALUE                       TYPE        DATA
* | [<-()] RV_RESULT                      TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_value.
    CLEAR rv_result.

    DATA: lps_cache TYPE REF TO ts_cache.

    lps_cache = _get_ref( iv_name ).
    IF lps_cache->t_data[] IS NOT INITIAL AND iv_value IN lps_cache->t_data[].
      rv_result = 'X'.
    ENDIF.

  ENDMETHOD.                    "check_value


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARVC=>GET_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [<---] ER_RANGE                       TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_range.

    FREE er_range.

    DATA: lps_cache TYPE REF TO ts_cache.
    lps_cache = _get_ref( iv_name ).

    DATA: ls_data TYPE ts_data.
    FIELD-SYMBOLS: <ls_any> TYPE any.
    LOOP AT lps_cache->t_data INTO ls_data.
      APPEND INITIAL LINE TO er_range ASSIGNING <ls_any>.
      IF <ls_any> IS NOT INITIAL.
        MOVE-CORRESPONDING ls_data TO <ls_any>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "get_range


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARVC=>GET_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [--->] IV_DEFAULT                     TYPE        DATA(optional)
* | [<---] EV_VALUE                       TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_value.

    ev_value = iv_default.

    DATA: lps_cache TYPE REF TO ts_cache.
    lps_cache = _get_ref( iv_name ).

    FIELD-SYMBOLS: <s_data> TYPE ts_data.
    READ TABLE lps_cache->t_data[] ASSIGNING <s_data> INDEX 1.
    IF sy-subrc = 0.
      ev_value = <s_data>-low.
    ENDIF.

  ENDMETHOD.                    "get_value


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TVARVC=>_GET_REF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [<-()] RPS_CACHE                      TYPE REF TO TS_CACHE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_ref.
    READ TABLE st_cache REFERENCE INTO rps_cache WITH KEY name = iv_name.
    CHECK sy-subrc <> 0.

    DATA: ls_cache TYPE ts_cache.
    ls_cache-name = iv_name.
    INSERT ls_cache INTO TABLE st_cache REFERENCE INTO rps_cache.

    SELECT
      sign
      opti AS option
      low
      high
      FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE rps_cache->t_data[]
      WHERE name = iv_name.

    FIELD-SYMBOLS: <s_data> TYPE ts_data.
    LOOP AT rps_cache->t_data ASSIGNING <s_data>.
      IF <s_data>-sign IS INITIAL.
        <s_data>-sign = 'I'.
      ENDIF.
      IF <s_data>-option IS INITIAL.
        <s_data>-option = 'EQ'.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.                    "_get_ref
ENDCLASS.
