CLASS zcl_trm_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS char_to_number
      IMPORTING
        !iv_anystring TYPE any
        !iv_decimal_c TYPE char1 OPTIONAL
      RETURNING
        VALUE(rv_dec) TYPE menge_d
      EXCEPTIONS
        wrong_characters
        first_character_wrong
        arithmetic_sign
        multiple_decimal_separator
        thousandsep_in_decimal
        thousand_separator
        number_too_big .
    CLASS-METHODS char_to_number2
      IMPORTING
        !iv_anystring TYPE any
        !iv_decimal_c TYPE char1 OPTIONAL
        !iv_bad_value TYPE menge_d DEFAULT 0
      RETURNING
        VALUE(rv_dec) TYPE menge_d .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TRM_UTIL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TRM_UTIL=>CHAR_TO_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANYSTRING                   TYPE        ANY
* | [--->] IV_DECIMAL_C                   TYPE        CHAR1(optional)
* | [<-()] RV_DEC                         TYPE        MENGE_D
* | [EXC!] WRONG_CHARACTERS
* | [EXC!] FIRST_CHARACTER_WRONG
* | [EXC!] ARITHMETIC_SIGN
* | [EXC!] MULTIPLE_DECIMAL_SEPARATOR
* | [EXC!] THOUSANDSEP_IN_DECIMAL
* | [EXC!] THOUSAND_SEPARATOR
* | [EXC!] NUMBER_TOO_BIG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD char_to_number.

    DATA: iv_string  TYPE char30,
          e_float    TYPE f,
          e_decimals TYPE i.

    WRITE iv_anystring TO iv_string.

    IF ( iv_string CA 'E' ). " Rinat Salakhov: Convert 7.0000000000000007E-2 directly
      MOVE iv_string TO e_float.
      MOVE e_float TO rv_dec.
      RETURN.
    ENDIF.

    "  MIGRATED FROM FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_STRING) TYPE  CHAR30
*"  EXPORTING
*"     VALUE(E_FLOAT) TYPE  F
*"     VALUE(E_DEC) TYPE  ESECOMPAVG
*"     VALUE(E_DECIMALS) TYPE  I
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
*   Specified Syntax for DB internal value E_DECIMALS
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
    CONSTANTS: c_1000      TYPE i VALUE 1000.
    DATA: lv_char(30)       TYPE c VALUE IS INITIAL.
    DATA: lv_char_1         TYPE c VALUE IS INITIAL.
    DATA: lv_num            TYPE f VALUE IS INITIAL.
* character for decimal separator
    DATA: ls_dchar         TYPE c VALUE IS INITIAL.
* character for thousands separator
    DATA: ls_tchar         TYPE c VALUE IS INITIAL.
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
    DATA: lv_decimals(30)   TYPE c.
    DATA: lv_subrc          TYPE sy-subrc VALUE IS INITIAL.
    DATA: lv_search(3)      TYPE c.

    FIELD-SYMBOLS: <char> TYPE any.

* ----------------------------------------------------------------------
* Function body
* ----------------------------------------------------------------------
* init
    CLEAR e_float.
    CLEAR rv_dec.
    CLEAR e_decimals.

* ---------------------------------------
* STEP 1: check system configuration.
* ---------------------------------------
* check decimal character

    IF iv_decimal_c IS INITIAL.
      lv_num = 11 / 10.
      WRITE lv_num TO lv_char.
      SEARCH lv_char FOR ','.
      IF sy-subrc = 0.
        ls_dchar = ','.
      ELSE.
        ls_dchar = '.'.
      ENDIF.
    ELSE.
      ls_dchar = iv_decimal_c. " Rinat Salakhov: force decimal char
    ENDIF.

* ckeck thousands character

    WRITE c_1000 TO lv_char.
    ls_tchar = lv_char+25(1).

* -----------------------------------
* STEP 2: check th input string
* -----------------------------------
    IF ( iv_string IS INITIAL ).
*   set the number of decimals sign to -1 and exit
      e_decimals = -1.
      EXIT.
    ENDIF.

    lv_value_char = iv_string.

    ASSIGN lv_value_char(1) TO <char>.

* determine if wrong characters were entered
    IF ( lv_value_char CN '1234567890+-,. ' ).
*   string contains wrong characters!
      RAISE wrong_characters.
    ENDIF.

* delete leading blanks
    SHIFT lv_value_char LEFT DELETING LEADING space.

* check first character
    IF ( <char> = ls_tchar ).
*   first character is wrong!
      RAISE first_character_wrong.
    ENDIF.

* determine the if first character is + or -
    IF ( <char> = '-' ).
      lv_flg_negative = abap_true.
      SHIFT lv_value_char.
    ELSEIF ( <char> = '+' ).
      lv_flg_negative = abap_false.
      SHIFT lv_value_char.
    ENDIF.
    IF ( lv_value_char CA '+' ) OR ( lv_value_char CA '-' ).
*   more than one arithmetic sign has been entered
      RAISE arithmetic_sign.
    ENDIF.

* shift the entry until the first character is a number
    SHIFT lv_value_char LEFT DELETING LEADING space.
    IF ( <char> = ls_dchar ).
      SHIFT lv_value_char RIGHT.
      REPLACE ' ' WITH '0' INTO lv_value_char.
    ENDIF.

* check the part right of the decimal separator
* search decimals part for ',' and '.'
    SPLIT lv_value_char AT ls_dchar INTO lv_front lv_decimals.
    IF ( lv_decimals CA ls_dchar ).
*   more than one decimal separator!
      RAISE multiple_decimal_separator.
*   EXIT.
    ENDIF.
    IF ( NOT ls_tchar = space ).
      IF ( lv_decimals CA ls_tchar ).
*     thousands separator in decimal part!
        RAISE thousandsep_in_decimal.
*     EXIT.
      ENDIF.
    ELSE.
*   just eliminate space from string
      CONDENSE lv_decimals NO-GAPS.
    ENDIF.
* remember the number of the decimals
    e_decimals = strlen( lv_decimals ).
    CASE e_decimals.
      WHEN 0.
*     we define -2 as no decimals
        e_decimals = -2.
      WHEN 15.
*     we define 0 as export value of the number of decimals equals
*     the number of decimals of the definition of the numeric values
*      IF ( E_FLOAT IS REQUESTED ).
        e_decimals = 0.
*      ENDIF.
      WHEN 4.
*      IF ( rv_dec IS REQUESTED ).
        e_decimals = 0.
*      ENDIF.
    ENDCASE.

* now, we test the part left of the decimal separator
    lv_front_save = lv_front.
    IF ( lv_front CA ls_tchar ) AND ( ls_tchar NE space ).
      lv_length_before = strlen( lv_front ).
      SHIFT lv_front LEFT UP TO ls_tchar.
      SHIFT lv_front LEFT.
      lv_length_after = strlen( lv_front ).
      lv_delta = lv_length_before - lv_length_after.
      IF ( lv_delta > 4 ).
*     wrong number of characters in front of first thousands separator
        RAISE thousand_separator.
      ENDIF.

      lv_length_before = strlen( lv_front ).
      WHILE ( lv_length_before > 3 ).
        IF ( lv_front CA ls_tchar ).
          lv_length_before = strlen( lv_front ).
          SHIFT lv_front LEFT UP TO ls_tchar.
          SHIFT lv_front LEFT.
          lv_length_after = strlen( lv_front ).
          lv_delta = lv_length_before - lv_length_after.
          IF ( lv_delta <> 4 ).
*         wrong number of characters between thousands separator
            RAISE thousand_separator.
*         EXIT.
          ENDIF.
          lv_length_before = strlen( lv_front ).
        ELSE.
*       too many numbers after last thousands separator
          RAISE thousand_separator.
        ENDIF.
      ENDWHILE.                          " lV_length_before > 3

      IF ( lv_length_before < 3 ).
*     too less numbers after last thousands separator
        RAISE thousand_separator.
      ENDIF.                             " lV_length_before < 3
    ELSE.
      CONDENSE lv_front_save NO-GAPS.
    ENDIF.                " lV_front ca ls_tchar and ls_tchar ne space


* -----------------------------------
* STEP 3: convert string to numbers
* -----------------------------------
* remove the thousands separator and set '.' as decimal separator.
* (this is needed for the move statement! Internally, the decimal
* separator is always a '.')
    CLEAR lv_value_char.
    lv_subrc = 0.
    CONCATENATE '.' ls_tchar '.' INTO lv_search.
    WHILE lv_subrc = 0.
      SEARCH lv_front_save FOR lv_search.
      lv_subrc = sy-subrc.
      REPLACE ls_tchar WITH '' INTO lv_front_save.
    ENDWHILE.
    CONDENSE lv_front_save NO-GAPS.
    IF ( NOT lv_decimals IS INITIAL ).
      CONCATENATE lv_front_save '.' lv_decimals INTO lv_value_char.
    ELSE.
      MOVE lv_front_save TO lv_value_char.
    ENDIF.
    CONDENSE lv_value_char NO-GAPS.

* Begin Correction 26.10.2009 1399738 ****************************
* Begin Correction 18.01.2010 1399738 ****************************
    TRY.
        MOVE lv_value_char TO rv_dec.

      CATCH cx_sy_conversion_overflow.
        RAISE number_too_big.
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
    MOVE lv_value_char TO e_float.
    IF ( lv_flg_negative = abap_true ).
      e_float = - e_float.
    ENDIF.

* Set the number of decimals to initial value (0) if number of
* input decimals exceeds the possible number of decimals
    IF ( e_decimals > 15 ). " AND ( E_FLOAT IS REQUESTED ) AND
      e_decimals = 0.
    ELSEIF
      "( NOT E_FLOAT IS REQUESTED ) AND
           "( rv_dec IS REQUESTED ) AND
      ( e_decimals > 3 ).
      e_decimals = 0.
    ENDIF.

    rv_dec = rv_dec.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TRM_UTIL=>CHAR_TO_NUMBER2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANYSTRING                   TYPE        ANY
* | [--->] IV_DECIMAL_C                   TYPE        CHAR1(optional)
* | [--->] IV_BAD_VALUE                   TYPE        MENGE_D (default =0)
* | [<-()] RV_DEC                         TYPE        MENGE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD char_to_number2.

    CALL METHOD zcl_trm_util=>char_to_number
      EXPORTING
        iv_anystring = iv_anystring
        iv_decimal_c = iv_decimal_c
      RECEIVING
        rv_dec       = rv_dec
      EXCEPTIONS
        OTHERS       = 8.
    IF sy-subrc <> 0.
      rv_dec = iv_bad_value.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
