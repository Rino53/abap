class ZCL_API_TXT definition
* SUPPORTS <7.4 *
  public
  create private .

public section.
*"* public components of class ZCL_API_TXT
*"* do not include other source files here!!!
  type-pools ABAP .

  types:
    ty_r_objects TYPE RANGE OF stxh-tdobject .
  types:
    ty_r_names    TYPE RANGE OF stxh-tdname .
  types:
    ty_r_ids      TYPE RANGE OF stxh-tdid .
  types:
    ty_r_langus   TYPE RANGE OF stxh-tdspras .
  types:
    Begin of ty_s_texts_multi,
    tdobject TYPE stxl-tdobject,
    tdname   TYPE stxl-tdname,
    tdid     TYPE stxl-tdid,
    tdspras  TYPE stxl-tdspras,
    tline    type TLINE_TAB,
    string   TYPE string,
  END OF ty_s_texts_multi .
  types:
    ty_t_texts_multi TYPE STANDARD TABLE OF ty_s_texts_multi .

  class-data MT_TEXTS_CACHE type TY_T_TEXTS_MULTI .

  class-methods COPY_TEXTS
    importing
      !SRC_ID type THEAD-TDID optional
      !SRC_SPRAS type THEAD-TDSPRAS default '*'
      !SRC_NAME type THEAD-TDNAME optional
      !SRC_OBJECT type THEAD-TDOBJECT optional
      !DST_ID type THEAD-TDID optional
      !DST_SPRAS type THEAD-TDSPRAS optional
      !DST_NAME type THEAD-TDNAME optional
      !DST_OBJECT type THEAD-TDOBJECT optional
    exporting
      !RV_ERROR type CHAR1
    changing
      !SRC_HEADERS type EMMA_THEAD_T optional .
  class-methods READ_SMART
    importing
      !IV_ID type THEAD-TDID
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type THEAD-TDNAME
      !IV_OBJECT type THEAD-TDOBJECT
      !IV_NAME_PATTERN_FORCED type STRING optional
    returning
      value(RV_TEXT) type STRING .
  class-methods READ_SINGLE
    importing
      !IV_ID type THEAD-TDID default 'ST'
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type CLIKE
      !IV_OBJECT type THEAD-TDOBJECT default 'TEXT'
      !IV_INITIAL_CLEAR type BOOLE_D default ABAP_TRUE
      !IV_FORCE_SPACES type BOOLE_D default ABAP_FALSE
      !IV_SPRAS2 type THEAD-TDSPRAS optional
    exporting
      !RV_TEXT type STRING
    changing
      !CS_THEAD type THEAD optional
      !CT_TEXTS type RE_T_TEXTLINE optional
      !CT_LINES type TLINE_TAB optional
      !CT_SOLI type SOLI_TAB optional .
  class-methods READ_SIMPLE
    importing
      !IV_ID type THEAD-TDID default 'ST'
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type CLIKE
      !IV_OBJECT type THEAD-TDOBJECT default 'TEXT'
      !IV_INITIAL_CLEAR type BOOLE_D default ABAP_TRUE
      !IV_FORCE_SPACES type BOOLE_D default ABAP_FALSE
      !IV_SPRAS2 type THEAD-TDSPRAS optional
    returning
      value(RV_TEXT) type STRING .
  class-methods SHOW
    importing
      !IV_OBJECT type TDOBJECT optional
      !IV_NAME type TDOBNAME optional
      !IV_ID type TDID optional
      !IV_SPRAS type SPRAS default SY-LANGU
      !IV_TEXT_FILTER type TDOBNAME optional
      !IV_MAXIMUM_RESULTS type I default 100
    exporting
      !ET_TEXTS type TY_T_TEXTS_MULTI .
  class-methods READ_MULTI
    importing
      !IR_OBJECT type TY_R_OBJECTS
      !IR_NAME type TY_R_NAMES optional
      !IR_ID type TY_R_IDS optional
      !IR_SPRAS type TY_R_LANGUS optional
      !IV_PACKAGE_SIZE type I default 3000
      !IV_MAXIMUM_RESULTS type I default 0
    returning
      value(RO_TEXTS) type ref to ZCL_API_TXT .
  class-methods REFRESH_CACHE .
  class-methods SAVE_TEXT
    importing
      !IV_TEXT type ANY optional
      !IV_ID type THEAD-TDID optional
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type THEAD-TDNAME optional
      !IV_OBJECT type THEAD-TDOBJECT optional
      !IS_HEADER_IN type THEAD optional
      !IV_SAVEMODE_DIRECT type BOOLE_D default ABAP_TRUE
      !IV_INSERT type ABAP_BOOL default ABAP_FALSE
    exporting
      !RS_HEADER_OUT type THEAD
    changing
      !CT_LINES type TLINE_TAB optional
      !CT_TEXTS type RE_T_TEXTLINE optional .
  class-methods DELETE_TEXT
    importing
      !IV_ID type THEAD-TDID optional
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type THEAD-TDNAME optional
      !IV_OBJECT type THEAD-TDOBJECT optional
      !IS_HEADER_IN type THEAD optional
      !IV_SAVEMODE_DIRECT type BOOLE_D default ABAP_TRUE
    returning
      value(RS_HEADER_OUT) type THEAD .
  class-methods STRING_REVERSE
    importing
      !IV_STRING type ANY
    returning
      value(RV_REVERSED_STRING) type STRING .
  class CL_ABAP_CHAR_UTILITIES definition load .
  class-methods SOLI_COMPRESS
    importing
      !IV_LINE_BREAKER type CLIKE default CL_ABAP_CHAR_UTILITIES=>NEWLINE
    changing
      value(CT_SOLI) type SOLI_TAB .
  class-methods STRING_TO_SOLI
    importing
      !IV_STRING type ANY
    returning
      value(RT_SOLI) type SOLI_TAB .
  class-methods STRING_TO_TLINES
    importing
      !IV_STRING type ANY
    returning
      value(RV_TLINES) type TLINE_TAB .
  class-methods TLINES_TO_STRING
    importing
      !IT_TLINES type TLINE_TAB
      !IV_FORCE_SPACES type ABAP_BOOL optional
    preferred parameter IT_TLINES
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_PARSE_O2O
    importing
      !IV_TEXT type ANY
    returning
      value(RT_TLINES) type TLINE_TAB .
protected section.
private section.

*"* private components of class ZCL_API_TXT
*"* do not include other source files here!!!
  data MT_TEXTS_SELECTED type TY_T_TEXTS_MULTI .

  class-methods ADD_TO_CACHE
    importing
      !IS_DATA type TY_S_TEXTS_MULTI optional
      !IT_DATA type TY_T_TEXTS_MULTI optional .
  class-methods READ_FROM_CACHE
    importing
      !IV_ID type THEAD-TDID
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type THEAD-TDNAME
      !IV_OBJECT type THEAD-TDOBJECT
    returning
      value(RS_DATA) type TY_S_TEXTS_MULTI .
ENDCLASS.



CLASS ZCL_API_TXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_TXT=>ADD_TO_CACHE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_S_TEXTS_MULTI(optional)
* | [--->] IT_DATA                        TYPE        TY_T_TEXTS_MULTI(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_to_cache.
" Last changed: 26.02.2015 12:28:26 by Rinat Salakhov
    DATA: ls_text  TYPE ty_s_texts_multi,
          lv_spras TYPE spras.

    IF is_data IS SUPPLIED AND is_data IS NOT INITIAL.
      IF is_data-tdspras IS INITIAL.
        lv_spras = sy-langu.
      ELSE.
        lv_spras = is_data-tdspras.
      ENDIF.
      ls_text = read_from_cache( iv_object = is_data-tdobject
                                 iv_name   = is_data-tdname
                                 iv_id     = is_data-tdid
                                 iv_spras  = lv_spras ).
      IF ls_text IS INITIAL.
        APPEND is_data TO mt_texts_cache[].
      ENDIF.
    ENDIF.

    IF it_data IS SUPPLIED AND it_data[] IS NOT INITIAL.

      APPEND LINES OF it_data[] TO mt_texts_cache[].
      DELETE ADJACENT DUPLICATES FROM mt_texts_cache COMPARING tdobject tdname tdid tdspras.

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>COPY_TEXTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] SRC_ID                         TYPE        THEAD-TDID(optional)
* | [--->] SRC_SPRAS                      TYPE        THEAD-TDSPRAS (default ='*')
* | [--->] SRC_NAME                       TYPE        THEAD-TDNAME(optional)
* | [--->] SRC_OBJECT                     TYPE        THEAD-TDOBJECT(optional)
* | [--->] DST_ID                         TYPE        THEAD-TDID(optional)
* | [--->] DST_SPRAS                      TYPE        THEAD-TDSPRAS(optional)
* | [--->] DST_NAME                       TYPE        THEAD-TDNAME(optional)
* | [--->] DST_OBJECT                     TYPE        THEAD-TDOBJECT(optional)
* | [<---] RV_ERROR                       TYPE        CHAR1
* | [<-->] SRC_HEADERS                    TYPE        EMMA_THEAD_T(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD copy_texts.

    rv_error = 9.

    DATA: lt_cpkeys TYPE STANDARD TABLE OF itctc,
          ls_cpkey  TYPE itctc.

    " find all texts to copy
    IF src_headers IS INITIAL.
      CALL FUNCTION 'SELECT_TEXT'
        EXPORTING
          object                  = src_object
          name                    = src_name
          id                      = src_id
          language                = src_spras
        TABLES
          selections              = src_headers
        EXCEPTIONS
          wrong_access_to_archive = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        rv_error = sy-subrc.
        RETURN.
      ENDIF.

    ENDIF.

    " build destination keys
    DATA: ls_headin LIKE LINE OF src_headers.
    LOOP AT src_headers INTO ls_headin WHERE tdname IS NOT INITIAL
                                         AND tdobject IS NOT INITIAL
                                         AND tdid IS NOT INITIAL
                                         AND tdspras IS NOT INITIAL.
      CLEAR ls_cpkey.
      ls_cpkey-srcobject = ls_headin-tdobject.
      ls_cpkey-srcname   = ls_headin-tdname.
      ls_cpkey-srcid     = ls_headin-tdid.
      ls_cpkey-srclang   = ls_headin-tdspras.

      IF dst_object IS NOT INITIAL. ls_cpkey-destobject = dst_object. ELSE. ls_cpkey-destobject = ls_cpkey-srcobject. ENDIF.
      IF dst_name   IS NOT INITIAL. ls_cpkey-destname   = dst_name.   ELSE. ls_cpkey-destname   = ls_cpkey-srcname.   ENDIF.
      IF dst_id     IS NOT INITIAL. ls_cpkey-destid     = dst_id.     ELSE. ls_cpkey-destid     = ls_cpkey-srcid.     ENDIF.
      IF dst_spras  IS NOT INITIAL. ls_cpkey-destlang   = dst_spras.  ELSE. ls_cpkey-destlang   = ls_cpkey-srclang.   ENDIF.

      IF ls_cpkey-srcobject  && ls_cpkey-srcname  && ls_cpkey-srcid  && ls_cpkey-srclang <>
         ls_cpkey-destobject && ls_cpkey-destname && ls_cpkey-destid && ls_cpkey-destlang .
        APPEND ls_cpkey TO lt_cpkeys.
      ENDIF.
    ENDLOOP.

    IF lt_cpkeys IS INITIAL.
      rv_error = 8.
      RETURN.
    ENDIF.

    CALL FUNCTION 'COPY_TEXTS'
      IMPORTING
        error = rv_error
      TABLES
        texts = lt_cpkeys.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>DELETE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        THEAD-TDID(optional)
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        THEAD-TDNAME(optional)
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT(optional)
* | [--->] IS_HEADER_IN                   TYPE        THEAD(optional)
* | [--->] IV_SAVEMODE_DIRECT             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<-()] RS_HEADER_OUT                  TYPE        THEAD
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD delete_text.
" Last changed: 25.03.2015 11:10:13 by Rinat Salakhov
  DATA: ls_header TYPE thead.
  CLEAR: rs_header_out.

  IF is_header_in IS SUPPLIED AND is_header_in IS NOT INITIAL.
    ls_header = is_header_in.
  ELSE.
    ls_header-tdobject = iv_object.
    ls_header-tdname   = iv_name.
    ls_header-tdid     = iv_id.
    ls_header-tdspras  = iv_spras.
  ENDIF.

  CHECK ls_header-tdobject IS NOT INITIAL
    AND ls_header-tdname   IS NOT INITIAL
    AND ls_header-tdid     IS NOT INITIAL
    AND ls_header-tdspras  IS NOT INITIAL.

  CALL FUNCTION 'DELETE_TEXT'
    EXPORTING
      id              = ls_header-tdid
      language        = ls_header-tdspras
      name            = ls_header-tdname
      object          = ls_header-tdobject
      savemode_direct = iv_savemode_direct
    EXCEPTIONS
      not_found       = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  rs_header_out = ls_header.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_TXT=>READ_FROM_CACHE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        THEAD-TDID
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        THEAD-TDNAME
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT
* | [<-()] RS_DATA                        TYPE        TY_S_TEXTS_MULTI
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method READ_FROM_CACHE.
" Last changed: 26.02.2015 11:50:14 by Rinat Salakhov

    FIELD-SYMBOLS: <ls_data> TYPE ty_s_texts_multi.

    READ TABLE mt_texts_cache ASSIGNING <ls_data> WITH KEY tdobject = iv_object
                                                           tdname   = iv_name
                                                           tdid     = iv_id
                                                           tdspras  = iv_spras.
    IF sy-subrc = 0.
      rs_data = <ls_data>.
    ENDIF.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>READ_MULTI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_OBJECT                      TYPE        TY_R_OBJECTS
* | [--->] IR_NAME                        TYPE        TY_R_NAMES(optional)
* | [--->] IR_ID                          TYPE        TY_R_IDS(optional)
* | [--->] IR_SPRAS                       TYPE        TY_R_LANGUS(optional)
* | [--->] IV_PACKAGE_SIZE                TYPE        I (default =3000)
* | [--->] IV_MAXIMUM_RESULTS             TYPE        I (default =0)
* | [<-()] RO_TEXTS                       TYPE REF TO ZCL_API_TXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_multi.
" Last changed: 01.04.2015 10:36:41 by Rinat Salakhov

    TYPES: BEGIN OF ty_stxl,
             relid    TYPE stxl-relid,
             tdobject TYPE stxl-tdobject,
             tdname   TYPE stxl-tdname,
             tdid     TYPE stxl-tdid,
             tdspras  TYPE stxl-tdspras,
             srtf2    TYPE stxl-srtf2,
             clustr   TYPE stxl-clustr,
             clustd   TYPE stxl-clustd,
           END OF ty_stxl.
    DATA: t_stxl        TYPE STANDARD TABLE OF ty_stxl,
          t_stxl_buffer TYPE STANDARD TABLE OF ty_stxl.
*          lt_texts TYPE ty_t_texts_multi.
    FIELD-SYMBOLS: <stxl> TYPE ty_stxl.
* compressed text data without text name
    TYPES: BEGIN OF ty_stxl_raw,
             clustr TYPE stxl-clustr,
             clustd TYPE stxl-clustd,
           END OF ty_stxl_raw.
    DATA:  t_stxl_raw TYPE STANDARD TABLE OF ty_stxl_raw.
    DATA:  w_stxl_raw TYPE ty_stxl_raw.
* decompressed text
    DATA:  t_tline TYPE STANDARD TABLE OF tline.
    FIELD-SYMBOLS: <tline> TYPE tline.
    DATA: t_stxh TYPE STANDARD TABLE OF stxh.
*          w_stxh TYPE stxh.
*TABLES stxh.

*    DATA:
*          s_object TYPE RANGE OF stxh-tdobject,
*          s_name   TYPE RANGE OF stxh-tdname,
*          s_id     TYPE RANGE OF stxh-tdid,
*          s_langu  TYPE RANGE OF stxh-tdspras.

    DATA s_stxl         TYPE ty_stxl.
    DATA l_first_tabix  TYPE sy-tabix.
    DATA l_last_tabix   TYPE sy-tabix.
    DATA subrc          TYPE sy-subrc.
    DATA process        TYPE abap_bool.

    DATA cursor TYPE cursor.

    DATA: ls_result TYPE ty_s_texts_multi.

    CREATE OBJECT ro_texts.
    CHECK ro_texts IS NOT INITIAL.

    "try this for debug
*    DATA: ss_object LIKE LINE OF s_object.
*    ss_object = 'IEQBELEG'.
*    APPEND ss_object TO s_object.
*
*    DATA: ss_langu LIKE LINE OF s_langu.
*    ss_langu = 'IEQRU'.
*    APPEND ss_langu TO s_langu.

    REFRESH ro_texts->mt_texts_selected[].

    CHECK ir_object[] IS NOT INITIAL.

    TRY.

        SELECT tdname tdobject tdid tdspras UP TO iv_maximum_results ROWS
            FROM stxh
              INTO CORRESPONDING FIELDS OF TABLE t_stxh
            WHERE tdobject IN ir_object[]
              AND tdname   IN ir_name[]
              AND tdid     IN ir_id[]
              AND tdspras  IN ir_spras[].

        CHECK t_stxh[] IS NOT INITIAL.


* select compressed text lines in blocks of 3000 (adjustable)
        OPEN CURSOR cursor FOR
        SELECT relid tdobject tdname tdid tdspras srtf2 clustr clustd
                FROM stxl
                FOR ALL ENTRIES IN t_stxh "WITH APPLICATION DATA AND TDNAME
                WHERE relid    = 'TX'          "standard text
                  AND tdobject = t_stxh-tdobject
                  AND tdname   = t_stxh-tdname
                  AND tdid     = t_stxh-tdid
                  AND tdspras  = t_stxh-tdspras
                ORDER BY PRIMARY KEY. "<=== new

        DO.
          FETCH NEXT CURSOR cursor
                  APPENDING TABLE t_stxl
                  PACKAGE SIZE iv_package_size.
          subrc = sy-subrc.

          IF subrc = 4.
            IF lines( t_stxl ) > 0.
              process = abap_true.
            ELSE.
              process = abap_false.
            ENDIF.

          ELSEIF subrc = 0.
            IF lines( t_stxl ) < iv_package_size.
              process = abap_true.
            ELSE.

              " put lines of last key aside, as there may be other lines for the same key
              DESCRIBE TABLE t_stxl LINES l_last_tabix.
              READ TABLE t_stxl INDEX l_last_tabix INTO s_stxl.
              READ TABLE t_stxl INDEX 1 ASSIGNING <stxl>.

              IF <stxl>-relid    = s_stxl-relid
                    AND <stxl>-tdobject = s_stxl-tdobject
                    AND <stxl>-tdname   = s_stxl-tdname
                    AND <stxl>-tdid     = s_stxl-tdid
                    AND <stxl>-tdspras  = s_stxl-tdspras.

                " The whole package has same key -> load next lines

                process = abap_false.

              ELSE.

                process = abap_true.

                l_first_tabix = l_last_tabix.
                l_first_tabix = l_last_tabix.
                DO.
                  SUBTRACT 1 FROM l_first_tabix.
                  READ TABLE t_stxl INDEX l_first_tabix ASSIGNING <stxl>.
                  IF sy-subrc <> 0.
                    EXIT.
                  ENDIF.
                  IF NOT ( <stxl>-relid    = s_stxl-relid
                       AND <stxl>-tdobject = s_stxl-tdobject
                       AND <stxl>-tdname   = s_stxl-tdname
                       AND <stxl>-tdid     = s_stxl-tdid
                       AND <stxl>-tdspras  = s_stxl-tdspras ).
                    EXIT.
                  ENDIF.
                ENDDO.

                ADD 1 TO l_first_tabix.
                APPEND LINES OF t_stxl FROM l_first_tabix TO l_last_tabix TO t_stxl_buffer.
                DELETE t_stxl FROM l_first_tabix TO l_last_tabix.

              ENDIF.
            ENDIF.
          ELSE.
            " can't happen
            ASSERT 0 = 1.
          ENDIF.

          IF process = abap_true.
            LOOP AT t_stxl ASSIGNING <stxl>.

              AT NEW tdspras.
                REFRESH t_stxl_raw.
              ENDAT.

              " decompress text
              CLEAR w_stxl_raw.
              w_stxl_raw-clustr = <stxl>-clustr.
              w_stxl_raw-clustd = <stxl>-clustd.
              APPEND w_stxl_raw TO t_stxl_raw.

              AT END OF tdspras.
                IMPORT tline = t_tline FROM INTERNAL TABLE t_stxl_raw.

                CLEAR ls_result.
                MOVE-CORRESPONDING <stxl> TO ls_result.
                ls_result-tline[] = t_tline[].
                LOOP AT t_tline ASSIGNING <tline> WHERE tdformat <> '/*'. "comment line
                  CONCATENATE ls_result-string <tline>-tdline INTO ls_result-string.
                  CONDENSE ls_result-string.
                ENDLOOP.
                APPEND ls_result TO ro_texts->mt_texts_selected[].

                " test output
*            DESCRIBE TABLE t_stxl_raw.
*            FORMAT COLOR 5.
*            WRITE: / 'AA', sy-tfill LEFT-JUSTIFIED, <stxl>-tdobject, <stxl>-tdname, <stxl>-tdid, <stxl>-tdspras.
*            FORMAT RESET.
*            LOOP AT t_tline ASSIGNING <tline>.
*              WRITE: / <tline>-tdline.
*            ENDLOOP.


                REFRESH t_stxl_raw.
              ENDAT.

            ENDLOOP.
          ENDIF.

          t_stxl = t_stxl_buffer.

          IF subrc <> 0.
            EXIT.
          ENDIF.
        ENDDO.

        CLOSE CURSOR cursor.

      CATCH: cx_sy_open_sql_db. "#EC NO_HANDLER

    ENDTRY.

    ASSERT 1 = 1. "(line for helping debug)

  ENDMETHOD.                                            "#EC CI_VALPAR.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>READ_SIMPLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        THEAD-TDID (default ='ST')
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        CLIKE
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT (default ='TEXT')
* | [--->] IV_INITIAL_CLEAR               TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_FORCE_SPACES                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_SPRAS2                      TYPE        THEAD-TDSPRAS(optional)
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_simple.
  " Last changed: 08.2023 by Rinat Salakhov
  read_single( EXPORTING
                 iv_object = iv_object
                 iv_name   = iv_name
                 iv_spras  = iv_spras
                 iv_id     = iv_id
                 iv_force_spaces  = iv_force_spaces
                 iv_initial_clear = iv_initial_clear
                 iv_spras2        = iv_spras2
               IMPORTING
                 rv_text = rv_text ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>READ_SINGLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        THEAD-TDID (default ='ST')
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        CLIKE
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT (default ='TEXT')
* | [--->] IV_INITIAL_CLEAR               TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_FORCE_SPACES                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_SPRAS2                      TYPE        THEAD-TDSPRAS(optional)
* | [<---] RV_TEXT                        TYPE        STRING
* | [<-->] CS_THEAD                       TYPE        THEAD(optional)
* | [<-->] CT_TEXTS                       TYPE        RE_T_TEXTLINE(optional)
* | [<-->] CT_LINES                       TYPE        TLINE_TAB(optional)
* | [<-->] CT_SOLI                        TYPE        SOLI_TAB(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_single.
  " Last changed: 05.2023 by Rinat Salakhov

  DATA:
*        lt_lines TYPE tttext,
        lv_longtext TYPE string.
  FIELD-SYMBOLS: <ls_line> TYPE tline.

  IF iv_initial_clear = abap_true.
    CLEAR: rv_text, ct_lines[], ct_texts[].
  ENDIF.

  IF iv_id     IS NOT INITIAL. cs_thead-tdid     = iv_id.     ENDIF.
  IF iv_object IS NOT INITIAL. cs_thead-tdobject = iv_object. ENDIF.
  IF iv_name   IS NOT INITIAL. cs_thead-tdname   = iv_name.   ENDIF.
  IF iv_spras  IS NOT INITIAL. cs_thead-tdspras  = iv_spras.  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = cs_thead-tdid
      language                = cs_thead-tdspras
      name                    = cs_thead-tdname
      object                  = cs_thead-tdobject
    IMPORTING
      header                  = cs_thead
    TABLES
      lines                   = ct_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    LOOP AT ct_lines ASSIGNING <ls_line> WHERE tdformat <> '/*'. "comment line
      APPEND <ls_line>-tdline TO ct_texts.
      APPEND <ls_line>-tdline TO ct_soli.
    ENDLOOP.

    rv_text = tlines_to_string( ct_lines ).

  ELSEIF iv_spras2 IS NOT INITIAL.
    read_single( EXPORTING iv_id = iv_id
                           iv_name = iv_name
                           iv_object = iv_object
                           iv_spras = iv_spras2
                           iv_initial_clear = iv_initial_clear
                           iv_force_spaces = iv_force_spaces
                 IMPORTING rv_text = rv_text
                 CHANGING cs_thead = cs_thead
                          ct_texts = ct_texts
                          ct_lines = ct_lines ).
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>READ_SMART
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        THEAD-TDID
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        THEAD-TDNAME
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT
* | [--->] IV_NAME_PATTERN_FORCED         TYPE        STRING(optional)
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_smart.
" Last changed: 03.03.2015 12:20:37 by Rinat Salakhov
  " Collecting cache by known patterns, boosting speed of read_text

  DATA:
        lt_texts          TYPE ty_t_texts_multi,
        ls_texts          TYPE ty_s_texts_multi,
        lv_string         TYPE string,
        lv_tdname_pattern TYPE thead-tdname.

  DATA: lr_object TYPE ty_r_objects,
        lr_id     TYPE ty_r_ids,
        lr_spras  TYPE ty_r_langus,
        lr_name   TYPE ty_r_names.

  CLEAR rv_text.
  CLEAR ls_texts.

  " 1. Read from cache
  ls_texts = read_from_cache( iv_object = iv_object
                              iv_name   = iv_name
                              iv_id     = iv_id
                              iv_spras  = iv_spras ).
  rv_text = ls_texts-string.

  IF ls_texts IS INITIAL.
    " 2. Pattern known?

    IF iv_name_pattern_forced IS SUPPLIED AND iv_name_pattern_forced IS NOT INITIAL.
      lv_tdname_pattern = iv_name_pattern_forced.
    ELSE.
      CASE iv_object.
        WHEN 'BELEG'.
          lv_string = iv_name.
          SHIFT lv_string RIGHT CIRCULAR BY 4 PLACES.
          lv_string = iv_name(4) && '*' && lv_string(4). " Name = bukrs + belnr + gjahr
          lv_tdname_pattern = lv_string.
        WHEN OTHERS.
          CLEAR lv_tdname_pattern.
      ENDCASE.
    ENDIF.

    " 2.1 Maybe we already searched with this pattern?
    ls_texts = read_from_cache( iv_object = iv_object
                                iv_name   = lv_tdname_pattern
                                iv_id     = iv_id
                                iv_spras  = iv_spras ).
    rv_text = ls_texts-string.
    CHECK ls_texts IS INITIAL. " Exit if we found pattern entry.

    IF lv_tdname_pattern IS NOT INITIAL. "(we have a pattern)
      " 3. Collect cache by pattern

      CLEAR: lv_string.
      lv_string = 'IEQ' && iv_object.
      APPEND lv_string TO lr_object[].

      lv_string = 'IEQ' && iv_id.
      APPEND lv_string TO lr_id[].

      lv_string = 'IEQ' && iv_spras.
      APPEND lv_string TO lr_spras[].

      lv_string = 'ICP' && lv_tdname_pattern.
      APPEND lv_string TO lr_name[].

      DATA: lo_texts TYPE REF TO ZCL_API_TXT.
      lo_texts = read_multi( ir_object = lr_object[]
                             ir_id     = lr_id[]
                             ir_name   = lr_name[]
                             ir_spras  = lr_spras[] ).
*                  IMPORTING et_texts  = lt_texts ).
      IF lo_texts IS NOT INITIAL.
        lt_texts = lo_texts->mt_texts_selected.
      ENDIF.

      " 3.1 Add cache and return text
      add_to_cache( it_data = lt_texts[] ).
      ls_texts = read_from_cache( iv_object = iv_object
                                  iv_name   = iv_name
                                  iv_id     = iv_id
                                  iv_spras  = iv_spras ).
      rv_text = ls_texts-string.

      " 3.2.1 save pattern to cache (if has) and quit

      CLEAR ls_texts.
      ls_texts-tdobject = iv_object.
      ls_texts-tdname   = lv_tdname_pattern.
      ls_texts-tdspras  = iv_spras.
      ls_texts-tdid     = iv_id.
      ls_texts-string   = rv_text.
      add_to_cache( is_data = ls_texts ).



    ELSE. "(no pattern, single text)
      " 4. New text! Save it to cache and quit
      CLEAR rv_text.
      rv_text = read_simple( iv_object = iv_object
                             iv_name   = iv_name
                             iv_spras  = iv_spras
                             iv_id     = iv_id ).

      CLEAR ls_texts.
      ls_texts-tdobject = iv_object.
      ls_texts-tdname   = iv_name.
      ls_texts-tdspras  = iv_spras.
      ls_texts-tdid     = iv_id.
      ls_texts-string   = rv_text.
      add_to_cache( is_data = ls_texts ).

    ENDIF.

  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>REFRESH_CACHE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD refresh_cache.

  REFRESH mt_texts_cache[].

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>SAVE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        ANY(optional)
* | [--->] IV_ID                          TYPE        THEAD-TDID(optional)
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        THEAD-TDNAME(optional)
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT(optional)
* | [--->] IS_HEADER_IN                   TYPE        THEAD(optional)
* | [--->] IV_SAVEMODE_DIRECT             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_INSERT                      TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<---] RS_HEADER_OUT                  TYPE        THEAD
* | [<-->] CT_LINES                       TYPE        TLINE_TAB(optional)
* | [<-->] CT_TEXTS                       TYPE        RE_T_TEXTLINE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD save_text.
" Last changed: 27.04.2017 14:59:22 by Rinat Salakhov

  DATA:
*        lt_lines TYPE tline_tab,
        ls_header TYPE thead,
        lv_tdline LIKE LINE OF ct_texts.
  FIELD-SYMBOLS: <ls_line> LIKE LINE OF ct_lines.

  CLEAR: rs_header_out.

  IF ct_lines[] IS INITIAL.
    IF iv_text IS NOT INITIAL.
      ct_lines = string_to_tlines( iv_text ).
    ELSEIF ct_texts[] IS NOT INITIAL.
      LOOP AT ct_texts INTO lv_tdline.
        APPEND INITIAL LINE TO ct_lines[] ASSIGNING <ls_line>.
        <ls_line>-tdformat = '*'.
        <ls_line>-tdline   = lv_tdline.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CHECK ct_lines[] IS NOT INITIAL.

  IF is_header_in IS SUPPLIED AND is_header_in IS NOT INITIAL.
    ls_header = is_header_in.
  ELSE.
    ls_header-tdobject = iv_object.
    ls_header-tdname   = iv_name.
    ls_header-tdid     = iv_id.
    ls_header-tdspras  = iv_spras.
  ENDIF.

  CHECK ls_header-tdobject IS NOT INITIAL
    AND ls_header-tdname   IS NOT INITIAL
    AND ls_header-tdid     IS NOT INITIAL
    AND ls_header-tdspras  IS NOT INITIAL.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = ls_header
      insert          = iv_insert
      savemode_direct = iv_savemode_direct
    IMPORTING
      newheader       = rs_header_out
    TABLES
      lines           = ct_lines
    EXCEPTIONS
      OTHERS          = 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>SHOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        TDOBJECT(optional)
* | [--->] IV_NAME                        TYPE        TDOBNAME(optional)
* | [--->] IV_ID                          TYPE        TDID(optional)
* | [--->] IV_SPRAS                       TYPE        SPRAS (default =SY-LANGU)
* | [--->] IV_TEXT_FILTER                 TYPE        TDOBNAME(optional)
* | [--->] IV_MAXIMUM_RESULTS             TYPE        I (default =100)
* | [<---] ET_TEXTS                       TYPE        TY_T_TEXTS_MULTI
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SHOW.
" Last changed: 12.03.2015 13:53:29 by Rinat Salakhov

  DEFINE _add_to_range.
    IF &1 IS NOT INITIAL.

      lv_string = &1.
      IF lv_string CA '*'.
        lv_string = 'ICP' && lv_string.
      ELSE.
        lv_string = 'IEQ' && lv_string.
      ENDIF.

      APPEND lv_string TO &2.

    ENDIF.
  END-OF-DEFINITION.

  DATA: lr_object TYPE ty_r_objects,
        lr_id     TYPE ty_r_ids,
        lr_spras  TYPE ty_r_langus,
        lr_name   TYPE ty_r_names.

  DATA: lv_string TYPE string,
        lv_max    TYPE I.

  FIELD-SYMBOLS: <ls_text> TYPE ty_s_texts_multi.
  FIELD-SYMBOLS: <ls_spras_r> LIKE LINE OF lr_spras.

  _add_to_range iv_object lr_object[].
  _add_to_range iv_id     lr_id[].
  _add_to_range iv_name   lr_name[].
  _add_to_range iv_spras  lr_spras[].

  IF lr_spras[] IS INITIAL.
    APPEND INITIAL LINE TO lr_spras[] ASSIGNING <ls_spras_r>.
    <ls_spras_r>-sign   = 'I'.
    <ls_spras_r>-option = 'EQ'.
    <ls_spras_r>-low    = sy-langu.
  ENDIF.

  IF iv_maximum_results IS INITIAL.
    lv_max = 999.
  ELSE.
    lv_max = iv_maximum_results.
  ENDIF.

  DATA: lo_texts TYPE REF TO ZCL_API_TXT.
  lo_texts = read_multi( ir_object = lr_object[]
                        ir_id     = lr_id[]
                        ir_name   = lr_name[]
                        ir_spras  = lr_spras[]
                        iv_maximum_results = lv_max ).
*              IMPORTING et_texts  = et_texts[] ).

  IF lo_texts IS NOT INITIAL.
    et_texts = lo_texts->mt_texts_selected.
  ENDIF.

  IF iv_text_filter IS NOT INITIAL.
    LOOP AT et_texts ASSIGNING <ls_text>.
      FIND FIRST OCCURRENCE OF iv_text_filter IN <ls_text>-string IGNORING CASE.
      IF sy-subrc <> 0.
        DELETE et_texts USING KEY LOOP_KEY.
      ENDIF.
    ENDLOOP.
  ENDIF.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>SOLI_COMPRESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINE_BREAKER                TYPE        CLIKE (default =CL_ABAP_CHAR_UTILITIES=>NEWLINE)
* | [<-->] CT_SOLI                        TYPE        SOLI_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD soli_compress.
    " Last changed: 11.2023 by Rinat Salakhov

***    Compress soli lines with linebreaker:
***    |Line 1 text           |
***    |Line 2 very long text |
***    |3_LINE                |
***    =>
***    |Line 1 text#Line 2 ver|
***    |y long text#3_LINE    |


    DATA: ls_soli TYPE soli,
          lv_fulltext TYPE string,
          lv_str_line TYPE string.

    LOOP AT ct_soli INTO lv_str_line WHERE line IS NOT INITIAL.
      IF iv_line_breaker IS NOT INITIAL.
        lv_fulltext = |{ lv_fulltext }{ lv_str_line }{ iv_line_breaker }|.
      ELSE.
        lv_fulltext = |{ lv_fulltext }{ lv_str_line }|.
      ENDIF.
    ENDLOOP.

    IF lv_fulltext IS NOT INITIAL.
      ct_soli = string_to_soli( lv_fulltext ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>STRING_PARSE_O2O
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        ANY
* | [<-()] RT_TLINES                      TYPE        TLINE_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD string_parse_o2o.

    CLEAR rt_tlines[].

    CHECK iv_text IS NOT INITIAL.

    DATA: lv_count TYPE i,
          lv_shift TYPE i,
          lv_fdpos TYPE sy-fdpos,
          lv_subrc TYPE sy-subrc,
          ls_tline TYPE tline,
          p_text   TYPE string.

    p_text = iv_text.
    ls_tline-tdformat = '*'.

    REPLACE ALL OCCURRENCES OF '<b>' IN p_text WITH ''.
    REPLACE ALL OCCURRENCES OF '</b>' IN p_text WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN p_text WITH ''.
    CONDENSE p_text.

    DO.
      lv_subrc = 4.
      CLEAR lv_fdpos.
      MOVE strlen( p_text ) TO lv_count.

      SEARCH p_text FOR '\n'.
      IF sy-subrc = 0 AND ( sy-fdpos < lv_fdpos OR lv_fdpos IS INITIAL ).
        lv_fdpos = sy-fdpos.
        lv_shift = 2.
        lv_subrc = 0.
      ENDIF.
      SEARCH p_text FOR '\r'.
      IF sy-subrc = 0 AND ( sy-fdpos < lv_fdpos OR ( lv_fdpos IS INITIAL
      AND lv_subrc <> 0 ) ).
        lv_fdpos = sy-fdpos.
        lv_shift = 2.
        lv_subrc = 0.
      ENDIF.
      SEARCH p_text FOR '<br />'.
      IF sy-subrc = 0 AND ( sy-fdpos < lv_fdpos OR ( lv_fdpos IS INITIAL
      AND lv_subrc <> 0 ) ).
        lv_fdpos = sy-fdpos.
        lv_shift = 6.
        lv_subrc = 0.
      ENDIF.

      IF lv_subrc = 0 AND lv_fdpos < 132.
        ls_tline-tdline = p_text(lv_fdpos).
        APPEND ls_tline TO rt_tlines.
        SHIFT p_text BY lv_fdpos PLACES LEFT.
        SHIFT p_text BY lv_shift PLACES LEFT.
        ls_tline-tdformat = '*'.
      ELSEIF ( lv_subrc = 0 AND lv_fdpos >= 132 ) OR ( lv_subrc <> 0 AND
      lv_count >= 132 ).
        ls_tline-tdline = p_text(132).
        APPEND ls_tline TO rt_tlines.
        SHIFT p_text BY 132 PLACES LEFT.
        ls_tline-tdformat = '='.
      ELSEIF lv_subrc <> 0 AND lv_count < 132.
        ls_tline-tdline = p_text.
        APPEND ls_tline TO rt_tlines.
        EXIT.
      ENDIF.
    ENDDO.
    DELETE rt_tlines WHERE tdline = space.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>STRING_REVERSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        ANY
* | [<-()] RV_REVERSED_STRING             TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD string_reverse.
" Last changed: 07.04.2015 15:51:21 by Rinat Salakhov

  DATA : length TYPE i,
         index  TYPE i,
         buf_str TYPE string.

  length = strlen( iv_string ).
  index = length - 1.
  buf_str = iv_string.

  DO length TIMES.
    SHIFT buf_str BY index PLACES.
    CONCATENATE rv_reversed_string buf_str INTO rv_reversed_string.
    buf_str = iv_string.
    SHIFT buf_str BY sy-index PLACES RIGHT CIRCULAR.
  ENDDO.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>STRING_TO_SOLI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        ANY
* | [<-()] RT_SOLI                        TYPE        SOLI_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD string_to_soli.
    " Last changed: 11.2023 by Rinat Salakhov

    DATA: lv_string TYPE string.

    lv_string = iv_string.

    CALL FUNCTION 'SO_STRING_TO_TAB'
      EXPORTING
        content_str = lv_string
      TABLES
        content_tab = rt_soli.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>STRING_TO_TLINES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        ANY
* | [<-()] RV_TLINES                      TYPE        TLINE_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method STRING_TO_TLINES.
" Last changed: 11.07.2016 15:19:35 by Rinat Salakhov

  DATA: lt_lines TYPE tline_tab,
        lv_text  TYPE string,

*        ls_header TYPE thead,
        ls_line   TYPE tline,

        lv_offset  TYPE i VALUE 0,
        lv_length  TYPE i VALUE 132.
*        lv_name    TYPE thead-tdname.

  TRY.
    CLEAR:  lt_lines,
            ls_line.
    lv_text = iv_string.
     IF lv_length >= strlen( lv_text ).
       " if text can fill in one piece
       lv_length = strlen( lv_text ).
       ls_line-tdformat = '*'.
       ls_line-tdline   = lv_text+lv_offset(lv_length).
       APPEND ls_line TO lt_lines.
     ELSE.
       " if text cant fill in one line and we need to split
       WHILE lv_length + lv_offset <= strlen( lv_text ).
         ls_line-tdformat = '*'.
         ls_line-tdline   = lv_text+lv_offset(lv_length).
         APPEND ls_line TO lt_lines.
         " add reading offset
         lv_offset = lv_offset + lv_length.
       ENDWHILE.
       " we need to take last piece
       " get difference between text and offset
       lv_length = strlen( lv_text ) - lv_offset.
       " get text remaining
       ls_line-tdformat = '*'.
       ls_line-tdline   = lv_text+lv_offset(lv_length).
       APPEND ls_line TO lt_lines.
     ENDIF.
   CATCH cx_sy_range_out_of_bounds.
     " accidently get out of range
     RETURN.
  ENDTRY.

  rv_tlines[] = lt_lines[].

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_TXT=>TLINES_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TLINES                      TYPE        TLINE_TAB
* | [--->] IV_FORCE_SPACES                TYPE        ABAP_BOOL(optional)
* | [<-()] RV_STRING                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD tlines_to_string.
    " Last changed: 09.2023 by Rinat Salakhov

    FIELD-SYMBOLS: <ls_line> TYPE tline.

    CLEAR rv_string.
    LOOP AT it_tlines ASSIGNING <ls_line> WHERE tdformat <> '/*'. "comment line
      IF iv_force_spaces = abap_true
        OR <ls_line>-tdformat = ''
        OR <ls_line>-tdformat = '/'
        OR <ls_line>-tdformat = '/='.
        CONCATENATE rv_string <ls_line>-tdline INTO rv_string SEPARATED BY space.
      ELSE.
        CONCATENATE rv_string <ls_line>-tdline INTO rv_string.
      ENDIF.
      CONDENSE rv_string.
    ENDLOOP.


***    Some format keys are predefined by SAPscript. They have a predefined meaning and can be used in all texts:
***    * default paragraph
***    The format definitions which are specified for the paragraph defined in the assigned style or form as the
***    default paragraph are used for the output formatting of the paragraph involved.
***
***    / new line
***    The subsequent text is written to a new line during output formatting. The formatting specifications of the
***    last paragraph format apply.
***
***    /: Command line
***    The characters contained in the actual text line are not output as text but are regarded as a control command.
***    They are not interpreted or executed until output formatting of the text. Control commands must always fit into
***    a line fully. It is not allowed to spread them over subsequent lines. The SAPscript editor does not format control statement lines.
***
***    /* Comment line
***    When formatting a text for output, the system does not output this line.
***
***    = long line
***    This line is not subject to the line formatting in the SAPscript editor. The text contained in this line is also
***    appended directly to the character of the preceding text line which was output last. If this is not required,
***    there must be at least one blank at the beginning of the extended line.
***
***    /= long line with line feed
***    This line is treated just as = (long line), but when formatting for output, the subsequent text appears in a new line.
***
***    ( raw line
***    The SAPscript composer does not interpret the subsequent line when formatting the text for output. This means that
***    character formats, symbols, tab characters, masking characters, or hypertext links which may be contained in this
***    line are not evaluated and are therefore passed unchanged to the output device. The text contained in this line is also
***    appended directly to the character of the preceding text line which was output last. If this is not required, there must
***    be at least one blank at the beginning of the extended line.
***
***    /( raw line with line feed
***    This line is treated just as ( (raw line), but when formatting for output, the subsequent text appears on a new line.
***
***    >x fix line
***    The line is not ready for input in the SAPscript editor. It can also not be deleted or separated. You can only create
***    fixed lines with a program . You can therefore give a text a fixed structure, for example, which cannot be changed by the user.
***    You can use any number or letter for the 'x'. You can therefore separate different sub-headings, for example.
***    If several fixed lines occur consecutively with the same indicator, they are regarded as a unit by the SAPscript editor.
***    It is not possible to insert anything between these lines in the editor. In the case of fixed lines, SAPscript print
***    formatting interprets the first two characters of the line as a paragraph format for formatting. You therefore need to enter the required paragraph format or blank here.


  ENDMETHOD.
ENDCLASS.
