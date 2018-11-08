class ZCL_API_LONGTEXT definition
  public
  create private .

public section.
*"* public components of class zcl_api_longtext
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
      !IV_ID type THEAD-TDID
      !IV_SPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type THEAD-TDNAME
      !IV_OBJECT type THEAD-TDOBJECT
      !IV_INITIAL_CLEAR type BOOLE_D default ABAP_TRUE
      !IV_FORCE_SPACES type BOOLE_D default ABAP_FALSE
    changing
      !CT_TEXTS type LOP_TDLINE_TAB optional
      !CT_LINES type TTTEXT optional
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
      value(RO_TEXTS) type ref to ZCL_API_LONGTEXT .
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
    changing
      !CT_LINES type TLINE_TAB optional
      !CT_TEXTS type LOP_TDLINE_TAB optional
    returning
      value(RS_HEADER_OUT) type THEAD .
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
  class-methods STRING_TO_TLINES
    importing
      !IV_STRING type ANY
    returning
      value(RV_TLINES) type TLINE_TAB .
protected section.
private section.

*"* private components of class zcl_api_longtext
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



CLASS ZCL_API_LONGTEXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_LONGTEXT=>ADD_TO_CACHE
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
* | Static Public Method ZCL_API_LONGTEXT=>DELETE_TEXT
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
* | Static Private Method ZCL_API_LONGTEXT=>READ_FROM_CACHE
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
* | Static Public Method ZCL_API_LONGTEXT=>READ_MULTI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_OBJECT                      TYPE        TY_R_OBJECTS
* | [--->] IR_NAME                        TYPE        TY_R_NAMES(optional)
* | [--->] IR_ID                          TYPE        TY_R_IDS(optional)
* | [--->] IR_SPRAS                       TYPE        TY_R_LANGUS(optional)
* | [--->] IV_PACKAGE_SIZE                TYPE        I (default =3000)
* | [--->] IV_MAXIMUM_RESULTS             TYPE        I (default =0)
* | [<-()] RO_TEXTS                       TYPE REF TO ZCL_API_LONGTEXT
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
* | Static Public Method ZCL_API_LONGTEXT=>READ_SINGLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        THEAD-TDID
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        THEAD-TDNAME
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT
* | [--->] IV_INITIAL_CLEAR               TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_FORCE_SPACES                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [<-->] CT_TEXTS                       TYPE        LOP_TDLINE_TAB(optional)
* | [<-->] CT_LINES                       TYPE        TTTEXT(optional)
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_single.
" Last changed: 27.04.2017 14:55:34 by Rinat Salakhov

  DATA:
*        lt_lines TYPE tttext,
        lv_longtext TYPE string.
  FIELD-SYMBOLS: <ls_line> TYPE tline.

  IF iv_initial_clear = abap_true.
    CLEAR: rv_text, ct_lines[], CT_TEXTS[].
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                            = iv_id
      language                      = iv_spras
      name                          = iv_name
      object                        = iv_object
    TABLES
      lines                         = ct_lines
    EXCEPTIONS
      id                            = 1
      language                      = 2
      name                          = 3
      not_found                     = 4
      object                        = 5
      reference_check               = 6
      wrong_access_to_archive       = 7
      OTHERS                        = 8
            .
  IF sy-subrc = 0.
    CLEAR lv_longtext.
    LOOP AT ct_lines ASSIGNING <ls_line> WHERE tdformat <> '/*'. "comment line
      IF IV_FORCE_SPACES = abap_true
        OR <ls_line>-tdformat = ''
        OR <ls_line>-tdformat = '/'
        OR <ls_line>-tdformat = '/='.
          CONCATENATE lv_longtext <ls_line>-tdline INTO lv_longtext SEPARATED BY space.
      ELSE.
          CONCATENATE lv_longtext <ls_line>-tdline INTO lv_longtext.
      ENDIF.
      CONDENSE lv_longtext.
      APPEND <ls_line>-tdline TO CT_TEXTS.
    ENDLOOP.
  ENDIF.

  IF lv_longtext IS NOT INITIAL.
    rv_text = lv_longtext.
  ENDIF.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_LONGTEXT=>READ_SMART
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

      DATA: lo_texts TYPE REF TO zcl_api_longtext.
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
      rv_text = read_single( iv_object = iv_object
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
* | Static Public Method ZCL_API_LONGTEXT=>REFRESH_CACHE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD refresh_cache.

  REFRESH mt_texts_cache[].

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_LONGTEXT=>SAVE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        ANY(optional)
* | [--->] IV_ID                          TYPE        THEAD-TDID(optional)
* | [--->] IV_SPRAS                       TYPE        THEAD-TDSPRAS (default =SY-LANGU)
* | [--->] IV_NAME                        TYPE        THEAD-TDNAME(optional)
* | [--->] IV_OBJECT                      TYPE        THEAD-TDOBJECT(optional)
* | [--->] IS_HEADER_IN                   TYPE        THEAD(optional)
* | [--->] IV_SAVEMODE_DIRECT             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<-->] CT_LINES                       TYPE        TLINE_TAB(optional)
* | [<-->] CT_TEXTS                       TYPE        LOP_TDLINE_TAB(optional)
* | [<-()] RS_HEADER_OUT                  TYPE        THEAD
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD save_text.
" Last changed: 27.04.2017 14:59:22 by Rinat Salakhov

  DATA:
*        lt_lines TYPE tline_tab,
        ls_header TYPE thead.

  CLEAR: rs_header_out.

  IF ct_lines[] IS INITIAL.
    IF iv_text IS NOT INITIAL.
      ct_lines = string_to_tlines( iv_text ).
    ELSEIF ct_texts[] IS NOT INITIAL.
      LOOP AT ct_texts INTO DATA(lv_tdline).
        APPEND VALUE #( tdformat = '*' tdline = lv_tdline ) TO ct_lines[].
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
      savemode_direct = iv_savemode_direct
    IMPORTING
      newheader       = rs_header_out
    TABLES
      lines           = ct_lines
    EXCEPTIONS
      OTHERS          = 1.
  IF sy-subrc <> 0.
    "Если не удалось записать
    RETURN.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_LONGTEXT=>SHOW
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

  _add_to_range iv_object lr_object[].
  _add_to_range iv_id     lr_id[].
  _add_to_range iv_name   lr_name[].
  _add_to_range iv_spras  lr_spras[].

  IF lr_spras[] IS INITIAL.
    APPEND 'IEQ' && sy-langu TO lr_spras[].
  ENDIF.

  IF iv_maximum_results IS INITIAL.
    lv_max = 999.
  ELSE.
    lv_max = iv_maximum_results.
  ENDIF.

  DATA: lo_texts TYPE REF TO zcl_api_longtext.
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
* | Static Public Method ZCL_API_LONGTEXT=>STRING_REVERSE
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
* | Static Public Method ZCL_API_LONGTEXT=>STRING_TO_TLINES
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
       "Если текст влезает в одну строку таблицы
       lv_length = strlen( lv_text ).
       ls_line-tdformat = '*'.
       ls_line-tdline   = lv_text+lv_offset(lv_length).
       APPEND ls_line TO lt_lines.
     ELSE.
       "Если текст не влезает на одну строку
       WHILE lv_length + lv_offset <= strlen( lv_text ).
         ls_line-tdformat = '*'.
         ls_line-tdline   = lv_text+lv_offset(lv_length).
         APPEND ls_line TO lt_lines.
         "Увеличиваем позицию с которой читаем
         lv_offset = lv_offset + lv_length.
       ENDWHILE.
       "Нужно забрать последний кусок
       "Получаем разницу между длинной текста и смещением
       lv_length = strlen( lv_text ) - lv_offset.
       "Получаем текст
       ls_line-tdformat = '*'.
       ls_line-tdline   = lv_text+lv_offset(lv_length).
       APPEND ls_line TO lt_lines.
     ENDIF.
   CATCH cx_sy_range_out_of_bounds.
     "Случайно вышли за границу текста
     RETURN.
  ENDTRY.

  rv_tlines[] = lt_lines[].

ENDMETHOD.
ENDCLASS.
