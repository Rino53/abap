class ZCL_EXCEL_READER definition
  public
  final
  create public .

public section.

  types:
*"* public components of class ZCLSRM_EXCEL_READER
*"* do not include other source files here!!!
    BEGIN OF t_zssrm_excel_tab,
        row    TYPE int4,
        column TYPE int4,
        value  TYPE string,
    END OF t_zssrm_excel_tab .
  types:
    tt_zssrm_excel_tab TYPE STANDARD TABLE OF t_zssrm_excel_tab .
  types:
    BEGIN OF t_zssrm_excel_sheet,
        name    TYPE string,
        excel_tab TYPE ZMD_EXCEL_TAB_TT. " <= This is the same type as tt_zssrm_excel_tab
    TYPES: END OF t_zssrm_excel_sheet .
  types:
    tt_zssrm_excel_sheet TYPE STANDARD TABLE OF t_zssrm_excel_sheet .

  data MO_CONVERT_LOG type ref to ZCL_MD_LOG .
  constants C_X000D type STRING value '_x000D_' ##NO_TEXT.

  class-methods CONVERT_STR_TO_DATUM
    importing
      !IV_ANYDATE type CLIKE
    returning
      value(RV_DATUM) type SYST-DATUM .
  class-methods CONVERT_STR_TO_MENGE
    importing
      !IV_ANYSTRING type ANY
      !IV_DECIMAL_C type CHAR1 optional
    returning
      value(RV_DEC) type MENGE_D .
  class-methods READ_FILE_TXT
    importing
      !IV_FNAME type CLIKE
      !IV_FROM_SERVER_FLAG type ABAP_BOOL default ABAP_FALSE
      !IV_SERVER_WHITE_PREFIX type STRING optional
    exporting
      !CT_FILEDATA type TABLE .
  class-methods READ_FILE_XLSX
    importing
      !IV_FNAME type CLIKE
      !IT_SHEET_NAME type /BOBF/T_BUF_STRING_RANGE optional
      !IT_SHEET_NUMBER type /SDF/IVIS_DC_SXMSPMSTAT_TAB optional
      !IV_DEL_EPMTY type FLAG default ''
      !IV_CONTENT type XSTRING optional
      !IV_FROM_SERVER_FLAG type ABAP_BOOL default ABAP_FALSE
    exporting
      !ET_EXCEL_SHEET type TT_ZSSRM_EXCEL_SHEET .
  methods GET_COLUMN_NAME
    importing
      !IV_COLUMN_IDX type ZMD_EXCEL_TAB-ROW
    returning
      value(RV_COLUMN_NAME) type FIELDNAME .
  methods CONVERT_SHEET
    importing
      !IV_FNAME type CLIKE optional
      !IS_SHEET type T_ZSSRM_EXCEL_SHEET optional
      !IV_CONTENT type XSTRING optional
      !IV_FROM_SERVER_FLAG type ABAP_BOOL default ABAP_FALSE
    changing
      !CT_DYNTABLE type TABLE
    exceptions
      PSP_NOT_FOUND .
  methods CONSTRUCTOR .
  class-methods FILE_OPEN_DIALOG_LOCAL
    importing
      !IV_TITLE type STRING default 'Select a file'
      !IV_DEF_EXT type STRING optional
      !IV_DEF_NAME type STRING optional
      !IV_DEF_DIR type STRING optional
      !IV_FILE_FILTER type STRING optional
    returning
      value(RV_FILEPATH) type FILE_TABLE-FILENAME .
  class-methods FILE_SAVE_DIALOG_FOLDER
    importing
      !IV_TITLE type STRING default 'Choose destination'
      !IV_DEF_DIR type STRING optional
    returning
      value(RV_FOLDER) type STRING .
  class-methods FILE_OPEN_DIALOG_SERVER
    importing
      !IV_TITLE type STRING default 'Select a file'
      !IV_FILEMASK type STRING optional
      !IV_DEF_NAME type STRING optional
      !IV_DEF_DIR type CLIKE optional "default '/usr/sap/D01/D01/data'
      !IV_FILE_FILTER type STRING optional
    returning
      value(RV_FILEPATH) type FILE_TABLE-FILENAME .
protected section.
private section.

  data MT_COLUMN_NAMES type ZMD_EXCEL_TAB_TT .

*"* private components of class ZCLSRM_EXCEL_READER
*"* do not include other source files here!!!
  class-methods _CONVERT_SHEET_DATA
    importing
      !IV_SHEET_NAME type STRING
      !I_SHEET_DATA type ref to DATA
    returning
      value(RS_EXCEL_SHEET) type T_ZSSRM_EXCEL_SHEET .
  class-methods _LOAD_FILE_FROM_LOCALPC
    importing
      !IV_FILEPATH type STRING
    returning
      value(RV_CONTENT) type XSTRING .
  class-methods _LOAD_FILE_FROM_SERVER
    importing
      !IV_SERVERFILE type CLIKE
    returning
      value(RV_CONTENT) type XSTRING .
ENDCLASS.



CLASS ZCL_EXCEL_READER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EXCEL_READER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.

    super->constructor( ).

    mo_convert_log = zcl_md_log=>create_log( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EXCEL_READER->CONVERT_SHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FNAME                       TYPE        CLIKE(optional)
* | [--->] IS_SHEET                       TYPE        T_ZSSRM_EXCEL_SHEET(optional)
* | [--->] IV_CONTENT                     TYPE        XSTRING(optional)
* | [--->] IV_FROM_SERVER_FLAG            TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-->] CT_DYNTABLE                    TYPE        TABLE
* | [EXC!] PSP_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_sheet.
    DATA: lv_last_row TYPE zmd_excel_tab-row,
          lo_datatype TYPE REF TO cl_abap_datadescr,
          lo_elemtype TYPE REF TO cl_abap_elemdescr,
          lt_sheets   TYPE tt_zssrm_excel_sheet.

    FIELD-SYMBOLS: <lv_tabline> TYPE any,
                   <lv_any>     TYPE any.

    DATA(ls_sheet) = is_sheet.

    IF ls_sheet IS INITIAL AND iv_fname IS NOT INITIAL.

      read_file_xlsx( EXPORTING iv_fname   = iv_fname
                                iv_content = iv_content
                                iv_from_server_flag = iv_from_server_flag
                      IMPORTING et_excel_sheet = lt_sheets ).
      READ TABLE lt_sheets INTO ls_sheet INDEX 1.

    ENDIF.

    CHECK ls_sheet IS NOT INITIAL.

    CLEAR: mt_column_names[].

    LOOP AT ls_sheet-excel_tab INTO DATA(ls_exline).
      CHECK: ls_exline-value IS NOT INITIAL.

      IF ls_exline-row = 1.
        APPEND ls_exline TO mt_column_names.
      ELSE.

        IF ls_exline-row <> lv_last_row.
          APPEND INITIAL LINE TO ct_dyntable ASSIGNING <lv_tabline>.
        ENDIF.


        DATA(lv_colname) = me->get_column_name( ls_exline-column ).
        IF lv_colname IS NOT INITIAL AND <lv_tabline> IS ASSIGNED.
          ASSIGN COMPONENT lv_colname OF STRUCTURE <lv_tabline> TO <lv_any>.
          IF sy-subrc = 0.

            lo_datatype ?= cl_abap_typedescr=>describe_by_data( <lv_any> ).
            lo_elemtype ?= lo_datatype.

            CASE lo_datatype->type_kind.
              WHEN 'D'.
                <lv_any> = convert_str_to_datum( ls_exline-value ).
              WHEN 'P'.
                <lv_any> = convert_str_to_menge( ls_exline-value ).
              WHEN OTHERS. " C, N
                CASE lo_elemtype->edit_mask.
                  WHEN '==ABPSP'.
*                    WRITE ls_exline-value TO <lv_any> USING EDIT MASK '==ABPSP'. " <- calls *_output instead!
                    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
                      EXPORTING
                        input  = ls_exline-value
                      IMPORTING
                        output = <lv_any>
                      EXCEPTIONS
                        NOT_FOUND = 1
                        OTHERS = 2.
                    IF sy-subrc <> 0.
*                      MESSAGE e049 WITH ls_exline-value INTO mo_convert_log->mv_message. mo_convert_log->add_message( ).
*                      <lv_any> = space.
                    ENDIF.
                    <lv_any> = space. " reading from file removed!

                  WHEN OTHERS.
                    <lv_any> = ls_exline-value.

                ENDCASE.

            ENDCASE.

          ENDIF.
        ENDIF.
      ENDIF.

      lv_last_row = ls_exline-row.
    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>CONVERT_STR_TO_DATUM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANYDATE                     TYPE        CLIKE
* | [<-()] RV_DATUM                       TYPE        SYST-DATUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONVERT_STR_TO_DATUM.
    DATA: lv_date10 TYPE char10.

    CLEAR: rv_datum.

    lv_date10 = iv_anydate.

    CHECK: lv_date10 IS NOT INITIAL.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external       = lv_date10
        accept_initial_date = abap_true
      IMPORTING
        date_internal       = rv_datum
      EXCEPTIONS
        OTHERS              = 2.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

* Get the format.

* DD.MM.YYYY
    IF lv_date10+2(1) = '.'.
      CONCATENATE lv_date10+6(4) lv_date10+3(2) lv_date10(2) INTO rv_datum.
    ENDIF.

* MM/DD/YYYY
    IF lv_date10+2(1) = '/'.
      CONCATENATE lv_date10+6(4) lv_date10(2) lv_date10+3(2) INTO rv_datum.
    ENDIF.

* MM-DD-YYYY
    IF lv_date10+2(1) = '-'.
      CONCATENATE lv_date10+6(4) lv_date10(2) lv_date10+3(2) INTO rv_datum.
    ENDIF.

* YYYY.MM.DD
    IF lv_date10+4(1) = '.'.
      CONCATENATE lv_date10(4) lv_date10+5(2) lv_date10+8(2)  INTO rv_datum.
    ENDIF.

* YYYY/MM/DD
    IF lv_date10+4(1) = '/'.
      CONCATENATE lv_date10(4) lv_date10+5(2) lv_date10+8(2)  INTO rv_datum.
    ENDIF.

* YYYY-MM-DD
    IF lv_date10+4(1) = '-'.
      CONCATENATE lv_date10(4) lv_date10+5(2) lv_date10+8(2)  INTO rv_datum.
    ENDIF.

* Check the valid date.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = rv_datum
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

* Check if the output parameter is populated.exception.
    IF sy-subrc <> 0.
      CLEAR rv_datum.
    ENDIF.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>CONVERT_STR_TO_MENGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANYSTRING                   TYPE        ANY
* | [--->] IV_DECIMAL_C                   TYPE        CHAR1(optional)
* | [<-()] RV_DEC                         TYPE        MENGE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_str_to_menge.

    DATA: lv_string                   TYPE char30,
          lv_float                    TYPE f,
          lv_decimals_i                 TYPE i,
          lv_raise_thousand_separator TYPE abap_bool VALUE abap_false.

    CLEAR rv_dec.

    WRITE iv_anystring TO lv_string.
    CONDENSE lv_string.

    IF lv_string = '0'.
      RETURN.
    ENDIF.

    IF ( lv_string CA 'E' ). " Rinat Salakhov: Convert 7.0000000000000007E-2 directly
      MOVE lv_string TO lv_float.
      MOVE lv_float TO rv_dec.
      RETURN.
    ENDIF.

    "  MIGRATED FROM FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(lv_string) TYPE  CHAR30
*"  EXPORTING
*"     VALUE(lv_float) TYPE  F
*"     VALUE(E_DEC) TYPE  ESECOMPAVG
*"     VALUE(lv_decimals_i) TYPE  I
*"  EXCEPTIONS
*"      WRONG_CHARACTERS
*"      FIRST_CHARACTER_WRONG
*"      ARITHMETIC_SIGN
*"      MULTIPLE_DECIMAL_SEPARATOR
*"      THOUSANDSEP_IN_DECIMAL
*"      THOUSAND_SEPARATOR
*"      NUMBER_TOO_BIG
*"----------------------------------------------------------------------

* ---------------------------------------------------------------------
*   Specified Syntax for DB internal value lv_decimals_i
*   -2 = NO decimals
*   -1 = NULL value (no input)
*    0 = initial value (definition of E_DEC/E_FLOAT)
*    1 = ONE decimal
*    2 = TWO decimals
*    ...
*    4 and 15 are converted to 0!
* ---------------------------------------------------------------------


* ----------------------------------------------------------------------
* Local data
* ----------------------------------------------------------------------
    CONSTANTS: lc_1000      TYPE i VALUE 1000.
    DATA: lv_char(30)       TYPE c VALUE IS INITIAL.
*    DATA: l_char_1         TYPE c VALUE IS INITIAL.
    DATA: lv_num            TYPE f VALUE IS INITIAL.
* character for decimal separator
    DATA: lv_dchar         TYPE c VALUE IS INITIAL.
* character for thousands separator
    DATA: lv_tchar         TYPE c VALUE IS INITIAL.
* value in characters
    DATA: lv_value_char     TYPE cawn-atwrt.
    DATA: lv_length_before  TYPE i.
    DATA: lv_length_after   TYPE i.
    DATA: lv_delta          TYPE i.
    DATA: lv_flg_negative   TYPE esp1_boolean.
* part left side of the decimal separator
    DATA: lv_front(30)      TYPE c.
    DATA: lv_front_save(30) TYPE c.
* part right side of the decimal separator
    DATA: lv_decimals_char(30)   TYPE c.
    DATA: lv_subrc          TYPE sy-subrc VALUE IS INITIAL.
    DATA: lv_search(3)      TYPE c.

    FIELD-SYMBOLS: <lv_char> TYPE any.

* ----------------------------------------------------------------------
* Function body
* ----------------------------------------------------------------------
* init
    CLEAR lv_float.
    CLEAR rv_dec.
    CLEAR lv_decimals_i.

* ---------------------------------------
* STEP 1: check system configuration.
* ---------------------------------------
* check decimal character

    IF iv_decimal_c IS INITIAL.
      lv_num = 11 / 10.
      WRITE lv_num TO lv_char.
      SEARCH lv_char FOR ','.
      IF sy-subrc = 0.
        lv_dchar = ','.
      ELSE.
        lv_dchar = '.'.
      ENDIF.
    ELSE.
      lv_dchar = iv_decimal_c. " Rinat Salakhov: force decimal char
    ENDIF.

* ckeck thousands character

    WRITE lc_1000 TO lv_char.
    lv_tchar = lv_char+25(1).

* -----------------------------------
* STEP 2: check th input string
* -----------------------------------
    IF ( lv_string IS INITIAL ).
*   set the number of decimals sign to -1 and exit
      lv_decimals_i = -1.
      EXIT.
    ENDIF.

    lv_value_char = lv_string.

    ASSIGN lv_value_char(1) TO <lv_char>.  "#EC CI_FLDEXT_OK[2215424]

* determine if wrong characters were entered
    IF ( lv_value_char CN '1234567890+-,. ' ).
*   string contains wrong characters!
*    RAISE wrong_characters.
      CLEAR rv_dec. RETURN.
    ENDIF.

* delete leading blanks
    SHIFT lv_value_char LEFT DELETING LEADING space.

* check first character
    IF ( <lv_char>(1) = lv_tchar(1) ). "#EC CI_FLDEXT_OK[2215424]
*   first character is wrong!
*    RAISE first_character_wrong.
      CLEAR rv_dec. RETURN.
    ENDIF.

* determine the if first character is + or -
    IF ( <lv_char> = '-' ).
      lv_flg_negative = abap_true.
      SHIFT lv_value_char.
    ELSEIF ( <lv_char> = '+' ).
      lv_flg_negative = abap_false.
      SHIFT lv_value_char.
    ENDIF.
    IF ( lv_value_char CA '+' ) OR ( lv_value_char CA '-' ).
*   more than one arithmetic sign has been entered
*    RAISE arithmetic_sign.
      CLEAR rv_dec. RETURN.
    ENDIF.

* shift the entry until the first character is a number
    SHIFT lv_value_char LEFT DELETING LEADING space.
    IF ( <lv_char>(1) = lv_dchar(1) ). "#EC CI_FLDEXT_OK[2215424]
      SHIFT lv_value_char RIGHT.
      REPLACE ' ' WITH '0' INTO lv_value_char.
    ENDIF.

* check the part right of the decimal separator
* search decimals part for ',' and '.'
    SPLIT lv_value_char AT lv_dchar INTO lv_front lv_decimals_char. "#EC CI_FLDEXT_OK[2215424]
    IF ( lv_decimals_char CA lv_dchar ).
*   more than one decimal separator!
*    RAISE multiple_decimal_separator.
      CLEAR rv_dec. RETURN.
*   EXIT.
    ENDIF.
    IF ( NOT lv_tchar = space ).
      IF ( lv_decimals_char CA lv_tchar ).
*     thousands separator in decimal part!
*      RAISE thousandsep_in_decimal.
        CLEAR rv_dec. RETURN.
*     EXIT.
      ENDIF.
    ELSE.
*   just eliminate space from string
      CONDENSE lv_decimals_char NO-GAPS.
    ENDIF.
* remember the number of the decimals
    lv_decimals_i = strlen( lv_decimals_char ).
    CASE lv_decimals_i.
      WHEN 0.
*     we define -2 as no decimals
        lv_decimals_i = -2.
      WHEN 15.
*     we define 0 as export value of the number of decimals equals
*     the number of decimals of the definition of the numeric values
*      IF ( lv_float IS REQUESTED ).
        lv_decimals_i = 0.
*      ENDIF.
      WHEN 4.
*      IF ( rv_dec IS REQUESTED ).
        lv_decimals_i = 0.
*      ENDIF.
    ENDCASE.

* now, we test the part left of the decimal separator
    lv_front_save = lv_front.
    IF ( lv_front CA lv_tchar ) AND ( lv_tchar NE space ).
      lv_length_before = strlen( lv_front ).
      SHIFT lv_front LEFT UP TO lv_tchar.
      SHIFT lv_front LEFT.
      lv_length_after = strlen( lv_front ).
      lv_delta = lv_length_before - lv_length_after.
      IF ( lv_delta > 4 ).
*     wrong number of characters in front of first thousands separator
*      RAISE thousand_separator.
        lv_raise_thousand_separator = abap_true.


      ENDIF.

      lv_length_before = strlen( lv_front ).
      WHILE ( lv_length_before > 3 ).
        IF ( lv_front CA lv_tchar ).
          lv_length_before = strlen( lv_front ).
          SHIFT lv_front LEFT UP TO lv_tchar.
          SHIFT lv_front LEFT.
          lv_length_after = strlen( lv_front ).
          lv_delta = lv_length_before - lv_length_after.
          IF ( lv_delta <> 4 ).
*         wrong number of characters between thousands separator
*          RAISE thousand_separator.
            lv_raise_thousand_separator = abap_true.
            EXIT. " From WHILE
*         EXIT.
          ENDIF.
          lv_length_before = strlen( lv_front ).
        ELSE.
*       too many numbers after last thousands separator
*        RAISE thousand_separator.
          lv_raise_thousand_separator = abap_true.
          EXIT. " From WHILE

        ENDIF.
      ENDWHILE.                          " lv_length_before > 3

      IF ( lv_length_before < 3 ).
*     too less numbers after last thousands separator
*      RAISE thousand_separator.
        lv_raise_thousand_separator = abap_true.
      ENDIF.                             " lv_length_before < 3
    ELSE.
      CONDENSE lv_front_save NO-GAPS.
    ENDIF.                " lv_front ca lv_tchar and lv_tchar ne space

    IF lv_raise_thousand_separator IS INITIAL.


* -----------------------------------
* STEP 3: convert string to numbers
* -----------------------------------
* remove the thousands separator and set '.' as decimal separator.
* (this is needed for the move statement! Internally, the decimal
* separator is always a '.')
      CLEAR lv_value_char.
      lv_subrc = 0.
      CONCATENATE '.' lv_tchar '.' INTO lv_search.
      WHILE lv_subrc = 0.
        SEARCH lv_front_save FOR lv_search.
        lv_subrc = sy-subrc.
        REPLACE lv_tchar WITH '' INTO lv_front_save.
      ENDWHILE.
      CONDENSE lv_front_save NO-GAPS.
      IF ( NOT lv_decimals_char IS INITIAL ).
        CONCATENATE lv_front_save '.' lv_decimals_char INTO lv_value_char.
      ELSE.
        MOVE lv_front_save TO lv_value_char.
      ENDIF.
      CONDENSE lv_value_char NO-GAPS.

* Begin Correction 26.10.2009 1399738 ****************************
* Begin Correction 18.01.2010 1399738 ****************************
      TRY.
          MOVE lv_value_char TO rv_dec.  "#EC CI_FLDEXT_OK[2215424]

        CATCH cx_sy_conversion_overflow.
*      RAISE number_too_big.
          CLEAR rv_dec. RETURN.
      ENDTRY.
* End Correction 18.01.2010 1399738 ******************************
***  IF ( rv_dec >= 999999999 ). " Rinat Salakhov - There is no Maximum anymore
****   number too big
***    RAISE number_too_big.
****    EXIT.
***  ENDIF.

* set the algebraic sign
* End Correction 26.10.2009 1399738 ******************************
      IF ( lv_flg_negative = abap_true ).
        rv_dec = - rv_dec.
      ENDIF.

* fill the floating field
* add zeros to value
      MOVE lv_value_char TO lv_float.  "#EC CI_FLDEXT_OK[2215424]
      IF ( lv_flg_negative = abap_true ).
        lv_float = - lv_float.
      ENDIF.

* Set the number of decimals to initial value (0) if number of
* input decimals exceeds the possible number of decimals
      IF ( lv_decimals_i > 15 ). " AND ( lv_float IS REQUESTED ) AND
        lv_decimals_i = 0.
      ELSEIF
        "( NOT lv_float IS REQUESTED ) AND
             "( rv_dec IS REQUESTED ) AND
        ( lv_decimals_i > 3 ).
        lv_decimals_i = 0.
      ENDIF.

    ENDIF.

    " Rinat Salakhov @ 09.2020 try to convert with another separator.
    IF lv_raise_thousand_separator = abap_true
       OR rv_dec IS INITIAL.

      IF iv_decimal_c IS SUPPLIED AND iv_decimal_c IS NOT INITIAL.
        CLEAR rv_dec.
      ELSE.

        lv_dchar = COND #( WHEN lv_dchar = '.' THEN ',' ELSE '.' ).
        rv_dec = convert_str_to_menge( EXPORTING iv_anystring = iv_anystring
                                                 iv_decimal_c = lv_dchar ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>FILE_OPEN_DIALOG_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TITLE                       TYPE        STRING (default ='Select a file')
* | [--->] IV_DEF_EXT                     TYPE        STRING(optional)
* | [--->] IV_DEF_NAME                    TYPE        STRING(optional)
* | [--->] IV_DEF_DIR                     TYPE        STRING(optional)
* | [--->] IV_FILE_FILTER                 TYPE        STRING(optional)
* | [<-()] RV_FILEPATH                    TYPE        FILE_TABLE-FILENAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD file_open_dialog_local.


*  value( WINDOW_TITLE )  TYPE STRING OPTIONAL
*  value( DEFAULT_EXTENSION )	TYPE STRING OPTIONAL
*  value( DEFAULT_FILENAME )  TYPE STRING OPTIONAL
*  value( FILE_FILTER )	TYPE STRING OPTIONAL
*  value( WITH_ENCODING )	TYPE ABAP_BOOL OPTIONAL
*  value( INITIAL_DIRECTORY )	TYPE STRING OPTIONAL
*  value( MULTISELECTION )  TYPE ABAP_BOOL OPTIONAL
*  FILE_TABLE	TYPE FILETABLE
*  RC	TYPE I
*  USER_ACTION  TYPE I OPTIONAL
*  FILE_ENCODING  TYPE ABAP_ENCODING OPTIONAL

    DATA: lt_file_table TYPE filetable,
          lv_rc         TYPE sy-subrc.


    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title      = iv_title
        default_extension = iv_def_ext
        default_filename  = iv_def_name
        initial_directory = iv_def_dir
        file_filter       = iv_file_filter
      CHANGING
        file_table        = lt_file_table
        rc                = lv_rc.
    IF sy-subrc = 0.
      READ TABLE lt_file_table INTO DATA(ls_file_table) INDEX 1.
      rv_filepath = ls_file_table-filename.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>FILE_OPEN_DIALOG_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TITLE                       TYPE        STRING (default ='Select a file')
* | [--->] IV_FILEMASK                    TYPE        STRING(optional)
* | [--->] IV_DEF_NAME                    TYPE        STRING(optional)
* | [--->] IV_DEF_DIR                     TYPE        CLIKE(optional)
* | [--->] IV_FILE_FILTER                 TYPE        STRING(optional)
* | [<-()] RV_FILEPATH                    TYPE        FILE_TABLE-FILENAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD file_open_dialog_server.

    DATA: lv_test1 TYPE i.
    DATA: lv_directory(75)  TYPE c. " Will not work with dynamic-sized char!

    lv_directory = iv_def_dir.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = lv_directory
        filemask         = iv_filemask
      IMPORTING
        serverfile       = rv_filepath
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      CLEAR rv_filepath.
    ELSE.
      DATA: lv_filename TYPE AUTHB-FILENAME.
      CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
        EXPORTING
          activity         = sabc_act_read
          filename         = lv_filename
        EXCEPTIONS
          no_authority     = 1
          activity_unknown = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        CLEAR rv_filepath.
      ENDIF.

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>FILE_SAVE_DIALOG_FOLDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TITLE                       TYPE        STRING (default ='Choose destination')
* | [--->] IV_DEF_DIR                     TYPE        STRING(optional)
* | [<-()] RV_FOLDER                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD FILE_SAVE_DIALOG_FOLDER.

*value( WINDOW_TITLE )  TYPE STRING OPTIONAL
*value( INITIAL_FOLDER )  TYPE STRING OPTIONAL
*SELECTED_FOLDER  TYPE STRING
*CNTL_ERROR
*ERROR_NO_GUI
*NOT_SUPPORTED_BY_GUI

    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title    = iv_title
        initial_folder  = iv_def_dir
      CHANGING
        selected_folder = rv_folder
      EXCEPTIONS
        OTHERS          = 4.
    IF sy-subrc <> 0.
      CLEAR rv_folder.
    ENDIF.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EXCEL_READER->GET_COLUMN_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COLUMN_IDX                  TYPE        ZMD_EXCEL_TAB-ROW
* | [<-()] RV_COLUMN_NAME                 TYPE        FIELDNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_column_name.
    CLEAR: rv_column_name.

    READ TABLE mt_column_names INTO DATA(ls_column_name) WITH KEY row = 1 column = iv_column_idx.
    IF sy-subrc = 0.
      rv_column_name = to_upper( ls_column_name-value ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>READ_FILE_TXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FNAME                       TYPE        CLIKE
* | [--->] IV_FROM_SERVER_FLAG            TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_SERVER_WHITE_PREFIX         TYPE        STRING(optional)
* | [<---] CT_FILEDATA                    TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_file_txt.
  DATA: lv_xstring TYPE xstring.

  IF iv_from_server_flag IS NOT INITIAL.
*      lv_xstring = _load_file_from_server( iv_fname ).


    DATA(lo_path) = cl_fs_path=>create_smart_path( iv_fname ).
    CHECK lo_path IS BOUND.

    DATA(lv_srvfile) = to_lower( lo_path->get_path_name( ) ).

    DATA(lv_prefix_len) = strlen( iv_server_white_prefix ).
    IF iv_server_white_prefix IS NOT INITIAL AND
       lv_prefix_len > 0.

      IF lo_path->is_absolute( ) = abap_false OR
        lv_srvfile(lv_prefix_len) <> to_lower( iv_server_white_prefix ).
        MESSAGE i398(00) WITH 'Server path must start from:' iv_server_white_prefix DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.


    " lv_srvfile is lowercase
    OPEN DATASET lv_srvfile FOR INPUT
                            IN TEXT MODE
                            ENCODING DEFAULT
                            WITH WINDOWS LINEFEED.
    CHECK sy-subrc = 0.

    DATA:
      lv_strline TYPE string,
      lv_len     TYPE i.

    DO.
      CLEAR lv_len.
      CLEAR lv_strline.
      READ DATASET lv_srvfile INTO lv_strline LENGTH lv_len.
      IF sy-subrc <> 0.
        IF lv_len > 0.
          APPEND lv_strline TO ct_filedata.
        ENDIF.
        EXIT.
      ENDIF.
      APPEND lv_strline TO ct_filedata.
    ENDDO.

    IF sy-subrc > 10.
      CLOSE DATASET lv_srvfile.
      RETURN.
    ENDIF.

    CLOSE DATASET lv_srvfile.



  ELSE.
*      lv_xstring = _load_file_from_localpc( iv_fname ).

    cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename   = iv_fname
        CHANGING
          data_tab   = ct_filedata
        EXCEPTIONS
          OTHERS     = 20 ).


  ENDIF.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>READ_FILE_XLSX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FNAME                       TYPE        CLIKE
* | [--->] IT_SHEET_NAME                  TYPE        /BOBF/T_BUF_STRING_RANGE(optional)
* | [--->] IT_SHEET_NUMBER                TYPE        /SDF/IVIS_DC_SXMSPMSTAT_TAB(optional)
* | [--->] IV_DEL_EPMTY                   TYPE        FLAG (default ='')
* | [--->] IV_CONTENT                     TYPE        XSTRING(optional)
* | [--->] IV_FROM_SERVER_FLAG            TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<---] ET_EXCEL_SHEET                 TYPE        TT_ZSSRM_EXCEL_SHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_file_xlsx.
*  DATA: lv_file_len TYPE i.
*  DATA: lt_binary  TYPE TABLE OF sdokcntbin.
  DATA: lv_xstring TYPE xstring.
  DATA: lv_message TYPE string.
  DATA: lo_excel  TYPE REF TO cl_fdt_xl_spreadsheet.
  DATA: lx_excel_core TYPE REF TO cx_fdt_excel_core.
  DATA: lr_sheet_data  TYPE REF TO data.
  DATA: ls_excel_sheet TYPE t_zssrm_excel_sheet.
  DATA: lt_worksheets_number TYPE STANDARD TABLE OF string .
  DATA: lt_worksheets TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.
  DATA: lv_fname TYPE string.
  FIELD-SYMBOLS: <lv_worksheets> LIKE LINE OF lt_worksheets.

  lv_fname = iv_fname.
  IF iv_content IS NOT INITIAL.
    lv_xstring = iv_content.
  ELSEIF lv_fname IS NOT INITIAL.
    IF iv_from_server_flag IS NOT INITIAL.
      lv_xstring = _load_file_from_server( lv_fname ).

    ELSE.
      lv_xstring = _load_file_from_localpc( lv_fname ).

    ENDIF.
  ENDIF.

  TRY.

*     Create object of class to read .xlsx file contents
      CREATE OBJECT lo_excel
        EXPORTING
          document_name = lv_fname
          xdocument     = lv_xstring.

    CATCH cx_fdt_excel_core INTO lx_excel_core.

*     Call method to get error message text
      CALL METHOD lx_excel_core->if_message~get_text
        RECEIVING
          result = lv_message.
*        MESSAGE e000(zsrm_prpl) WITH lv_message  INTO zclsrm_ex_helper=>gv_dummy.
*        zclsrm_ex_helper=>raise_message_syst( ).
      RETURN.
  ENDTRY.
  lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = lt_worksheets ).
  IF lt_worksheets IS NOT INITIAL.

    IF it_sheet_number[] IS NOT INITIAL.
      LOOP AT lt_worksheets ASSIGNING <lv_worksheets>.
        READ TABLE it_sheet_number TRANSPORTING NO FIELDS
          WITH KEY table_line = sy-tabix.
        IF sy-subrc IS INITIAL.
          APPEND <lv_worksheets> TO lt_worksheets_number.
        ENDIF.
      ENDLOOP.
      lt_worksheets[] = lt_worksheets_number[].
    ENDIF.
*   Read active worksheet
    LOOP AT lt_worksheets ASSIGNING <lv_worksheets>
      WHERE table_line IN it_sheet_name.
      CLEAR ls_excel_sheet.
      lr_sheet_data = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <lv_worksheets> ).
      IF lr_sheet_data IS NOT INITIAL.
        ls_excel_sheet = _convert_sheet_data( i_sheet_data = lr_sheet_data iv_sheet_name = <lv_worksheets> ).
        IF iv_del_epmty = abap_true.
          DELETE ls_excel_sheet-excel_tab[] WHERE value IS INITIAL.
        ENDIF.
      ELSE.
        ls_excel_sheet-name = <lv_worksheets>.
      ENDIF.
      APPEND ls_excel_sheet TO et_excel_sheet[].
    ENDLOOP.
  ELSE.
*      MESSAGE e000(zsrm_prpl) WITH 'message!'  INTO zclsrm_ex_helper=>gv_dummy.
*      zclsrm_ex_helper=>raise_message_syst( ).
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_EXCEL_READER=>_CONVERT_SHEET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SHEET_NAME                  TYPE        STRING
* | [--->] I_SHEET_DATA                   TYPE REF TO DATA
* | [<-()] RS_EXCEL_SHEET                 TYPE        T_ZSSRM_EXCEL_SHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD _convert_sheet_data.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    DATA: lo_rtti_table TYPE REF TO cl_abap_tabledescr.
    DATA: lo_rtti_struc TYPE REF TO cl_abap_structdescr.
    DATA: lt_comp TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS: <lv_field> TYPE any.
    FIELD-SYMBOLS: <lv_table> TYPE any.
    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lt_comp.
    FIELD-SYMBOLS: <ls_row> LIKE LINE OF rs_excel_sheet-excel_tab.
    DATA: lv_row TYPE i VALUE 1.
    DATA: lv_column TYPE i.

    rs_excel_sheet-name = iv_sheet_name.

    ASSIGN i_sheet_data->* TO <lt_table>.
    lo_rtti_table ?= cl_abap_tabledescr=>describe_by_data( <lt_table> ).
    lo_rtti_struc ?= lo_rtti_table->get_table_line_type( ).
    lt_comp = lo_rtti_struc->get_components( ).
    LOOP AT <lt_table> ASSIGNING <lv_table>.
      lv_column = 1.
      LOOP AT lt_comp ASSIGNING <ls_comp>.
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <lv_table> TO <lv_field>.
        IF <lv_field> IS ASSIGNED.
          APPEND INITIAL LINE TO rs_excel_sheet-excel_tab ASSIGNING <ls_row>.
          <ls_row>-row = lv_row.
          <ls_row>-column = lv_column.
          <ls_row>-value = <lv_field>.
          REPLACE ALL OCCURRENCES OF c_x000d IN <ls_row>-value WITH space.
          lv_column = lv_column + 1.
        ENDIF.
      ENDLOOP.
      lv_row = lv_row + 1.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_EXCEL_READER=>_LOAD_FILE_FROM_LOCALPC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILEPATH                    TYPE        STRING
* | [<-()] RV_CONTENT                     TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _load_file_from_localpc.
    DATA: lv_file_len TYPE i,
          lt_binary   TYPE TABLE OF sdokcntbin.

    cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename   = iv_filepath
          filetype   = 'BIN'
        IMPORTING
          filelength = lv_file_len
        CHANGING
          data_tab   = lt_binary
        EXCEPTIONS
          OTHERS     = 20 ).
    IF sy-subrc <> 0.
*        MESSAGE e188(zsrm_xls) INTO zclgl_utils=>gv_dummy.
*        zclsrm_ex_helper=>raise_message_syst( ).
    ELSE.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_file_len
        IMPORTING
          buffer       = rv_content
        TABLES
          binary_tab   = lt_binary
        EXCEPTIONS
          OTHERS       = 2.
      IF sy-subrc <> 0 .
        CLEAR rv_content.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_EXCEL_READER=>_LOAD_FILE_FROM_SERVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SERVERFILE                  TYPE        CLIKE
* | [<-()] RV_CONTENT                     TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _load_file_from_server.

    DATA: lv_datafile   TYPE localfile,
          lv_rawline    TYPE cps_x255,
          lt_rawcontent TYPE cpt_x255,
          lv_len        TYPE sy-tabix,
          lv_lines      TYPE i.

    lv_datafile = iv_serverfile.

    OPEN DATASET lv_datafile FOR INPUT IN BINARY MODE.
    CHECK sy-subrc = 0.

    DO.
      CLEAR lv_len.
      CLEAR lv_rawline.
      READ DATASET lv_datafile INTO lv_rawline LENGTH lv_len.
      IF sy-subrc <> 0.
        IF lv_len > 0.
          APPEND lv_rawline TO lt_rawcontent.
        ENDIF.
        EXIT.
      ENDIF.
      APPEND lv_rawline TO lt_rawcontent.
    ENDDO.

    IF sy-subrc > 10.
      CLOSE DATASET lv_datafile.
      RETURN.
    ENDIF.

    CLOSE DATASET lv_datafile.

    " Convert data to xstring
    cl_scp_change_db=>xtab_to_xstr( EXPORTING im_xtab = lt_rawcontent[]
                                              im_size = lv_lines
                                    IMPORTING ex_xstring = rv_content ).

  ENDMETHOD.
ENDCLASS.
