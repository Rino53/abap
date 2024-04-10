class ZCL_XLS_SUMM definition
  public
  create private .

*"* public components of class ZCL_XLS_SUMM
*"* do not include other source files here!!!
public section.

  types TY_V_KEYFIELD type MMPUR_DDICFIELD .
  types:
    BEGIN OF ts_key_fields,
      number TYPE ty_v_keyfield,
      total TYPE ty_v_keyfield,
      lvl2key TYPE ty_v_keyfield,
      lvl3key TYPE ty_v_keyfield,
      special_char TYPE CHAR1,
    END OF ts_key_fields .
  types:
    BEGIN OF ts_delete_field,
      delfield TYPE ty_v_keyfield,
    END OF ts_delete_field .
  types:
    tt_delete_fields TYPE STANDARD TABLE OF ts_delete_field .

  class-data SET_KEYS type TS_KEY_FIELDS .
  constants MC_MARKER_TOTAL type TY_V_KEYFIELD value '<TOTAL>'. "#EC NOTEXT
  constants MC_MARKER_EMPTY type TY_V_KEYFIELD value '<EMPTY>'. "#EC NOTEXT

  class-methods COLLECT
    importing
      !IS_KEY_FIELDS type TS_KEY_FIELDS optional
      !IT_DONT_COLLECT_FIELDS type TT_DELETE_FIELDS optional
      !IT_DATA type ANY TABLE .
  type-pools ABAP .
  class-methods WRITE_BY_LVL
    importing
      !IS_TOTALS_TEXT type TS_KEY_FIELDS optional
      !IV_WRITE_TOTAL type BOOLE_D default ABAP_FALSE
    changing
      !CT_DATA type TABLE .
  class-methods WRITE_BY_PAGES
    importing
      !IV_GAP_FIRST type I default 15
      !IV_GAP_NEXT type I default 25
      !IV_TEXT_ITOGO type STRING optional
    changing
      !CT_DATA type ANY TABLE .
  class-methods WRITE_TOTAL
    importing
      !IV_TEXT_ITOGO type ANY default 'ИТОГО'
    exporting
      !ES_TOTALS type ANY
    changing
      !CT_DATA type TABLE optional .
  class-methods GENERATE_TEST_DATA
    importing
      !IV_LINES_NUMBER type I default 100
      !IS_KEY_FIELDS type TS_KEY_FIELDS optional
    changing
      !CT_DATA type TABLE .
  class-methods GENERATE_LINE_NUMS
    importing
      !IS_KEY_FIELDS type TS_KEY_FIELDS optional
    changing
      !CT_DATA type TABLE .
  PROTECTED SECTION.
*"* private components of class ZCL_XLS_SUMM
*"* do not include other source files here!!!
private section.

  data MT_PODSUMMS type ref to DATA .
  data MS_PODSUMM_TOTAL type ref to DATA .
  class-data MO_INSTANCE type ref to ZCL_XLS_SUMM .
  data MS_KEY_FIELDS type TS_KEY_FIELDS .
  data MS_KEY_MULTIPLE type TS_KEY_FIELDS .
  data MT_DELETE_FIELDS type TT_DELETE_FIELDS .

  methods PARSE_TOTAL_TEXT
    importing
      !IV_TEXT_ITOGO type ANY
      !IS_DATA type ANY
      !IV_SPECIAL_CHAR type C default '|'
    returning
      value(RV_TEXT_ITOGO_PARSED) type STRING .
  methods CONSTRUCTOR
    importing
      !IT_DATA type ANY TABLE
      !IS_KEY_FIELDS type TS_KEY_FIELDS
      !IT_DEL_FIELDS type TT_DELETE_FIELDS optional .
  methods COLLECT_BY_LVL
    importing
      value(IS_DATA) type ANY
      !IV_LVL type I default 1 .
  methods READ_BY_LVL
    importing
      !IV_TOTAL type TY_V_KEYFIELD default MC_MARKER_TOTAL
      !IV_KEY2 type ANY optional
      !IV_KEY3 type ANY optional
    exporting
      !EF_DATA type ANY .
  methods INSERT_BY_LVL
    importing
      !IS_KEYS type ANY
      !IV_LVL type I
      !IV_INDEX type SYST-TABIX default 0
      !IV_TEXT_ITOGO type ANY optional
    changing
      !CT_DATA type TABLE .
  type-pools ABAP .
  methods INSERT_1LVL_TOTAL
    importing
      !IV_TEXT_ITOGO type ANY default 'ИТОГО'
      !IV_WRITE_CURRENT_TOTAL type BOOLE_D default ABAP_TRUE
    changing
      !CT_DATA type TABLE optional .
  class-methods COMPARE_FS_FIELDS
    importing
      !LINE1 type ANY
      !LINE2 type ANY
      !COMPONENT type TY_V_KEYFIELD
    returning
      value(RV_EQUAL) type BOOLE_D .
ENDCLASS.




CLASS ZCL_XLS_SUMM IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_XLS_SUMM=>COLLECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_KEY_FIELDS                  TYPE        TS_KEY_FIELDS(optional)
* | [--->] IT_DONT_COLLECT_FIELDS         TYPE        TT_DELETE_FIELDS(optional)
* | [--->] IT_DATA                        TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD COLLECT. " Last changed: 15.09.2015 15:39:11 by Rinat Salakhov

  DATA: ls_keys TYPE ts_key_fields.

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lv_any_field> TYPE any.

  IF mo_instance IS BOUND.
    FREE mo_instance.
  ENDIF.

  IF is_key_fields IS NOT SUPPLIED OR is_key_fields IS INITIAL.
    ls_keys = SET_KEYS.
  ELSE.
    ls_keys = is_key_fields.
  ENDIF.

  CREATE OBJECT mo_instance EXPORTING it_data = it_data
                                      is_key_fields = ls_keys
                                      it_del_fields = it_dont_collect_fields.

  ls_keys = mo_instance->ms_key_fields.

  LOOP AT it_data ASSIGNING <ls_data>.
    UNASSIGN <lv_any_field>.
    ASSIGN COMPONENT ls_keys-number OF STRUCTURE <ls_data> TO <lv_any_field>.
    IF <lv_any_field> IS ASSIGNED.
      IF <lv_any_field> IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    mo_instance->collect_by_lvl( is_data = <ls_data>
                                 iv_lvl  = 1 ).

    IF ls_keys-lvl2key IS NOT INITIAL.
       mo_instance->collect_by_lvl( is_data = <ls_data>
                                    iv_lvl  = 2 ).
    ENDIF.

    IF ls_keys-lvl3key IS NOT INITIAL.
       mo_instance->collect_by_lvl( is_data = <ls_data>
                                    iv_lvl  = 3 ).
    ENDIF.

  ENDLOOP.

  mo_instance->INSERT_1LVL_TOTAL( ). "buffer this totals


ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLS_SUMM->COLLECT_BY_LVL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ANY
* | [--->] IV_LVL                         TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD collect_by_lvl. " Last changed: 24.09.2015 16:40:53 by Rinat Salakhov

  DATA: lr_current_keys TYPE RANGE OF ts_delete_field,
        ls_current_key LIKE LINE OF lr_current_keys,
        lt_multi_keys TYPE STANDARD TABLE OF ty_v_keyfield,
        lv_keystring TYPE string.

  FIELD-SYMBOLS: <lv_any_field> TYPE any,
                 <lt_podsumms> TYPE STANDARD TABLE,
                 <lv_delete_field> TYPE ts_delete_field.
  IF iv_lvl = 1.
    CHECK ms_key_fields-total IS NOT INITIAL.
    APPEND 'IEQ' TO lr_current_keys.
  ENDIF.
  IF iv_lvl >= 2.
    CHECK ms_key_fields-lvl2key IS NOT INITIAL.
    IF ms_key_multiple-lvl2key IS NOT INITIAL.
      SPLIT ms_key_multiple-lvl2key AT ms_key_multiple-special_char INTO TABLE lt_multi_keys IN CHARACTER MODE.
      LOOP AT lt_multi_keys INTO ls_current_key-low.
        CONCATENATE 'IEQ' ls_current_key-low INTO ls_current_key.
        APPEND ls_current_key TO lr_current_keys.
      ENDLOOP.
    ELSE.
      CONCATENATE 'IEQ' ms_key_fields-lvl2key INTO ls_current_key.
      APPEND ls_current_key TO lr_current_keys.
    ENDIF.
  ENDIF.
  IF iv_lvl >= 3.
    CHECK ms_key_fields-lvl3key IS NOT INITIAL.
    IF ms_key_multiple-lvl3key IS NOT INITIAL.
      SPLIT ms_key_multiple-lvl3key AT ms_key_multiple-special_char INTO TABLE lt_multi_keys IN CHARACTER MODE.
      LOOP AT lt_multi_keys INTO ls_current_key-low.
        CONCATENATE 'IEQ' ls_current_key-low INTO ls_current_key.
        APPEND ls_current_key TO lr_current_keys.
      ENDLOOP.
    ELSE.
      CONCATENATE 'IEQ' ms_key_fields-lvl3key INTO ls_current_key.
      APPEND ls_current_key TO lr_current_keys.
    ENDIF.
  ENDIF.

  UNASSIGN <lv_any_field>.
  LOOP AT mt_delete_fields ASSIGNING <lv_delete_field> WHERE delfield NOT IN lr_current_keys.
    ASSIGN COMPONENT <lv_delete_field> OF STRUCTURE is_data TO <lv_any_field>.
    IF <lv_any_field> IS ASSIGNED.
      CLEAR: <lv_any_field>.
    ENDIF.
    UNASSIGN <lv_any_field>.
  ENDLOOP.

  ASSIGN COMPONENT ms_key_fields-total OF STRUCTURE is_data TO <lv_any_field>.
  IF <lv_any_field> IS ASSIGNED.
    IF <lv_any_field> IS INITIAL.
      <lv_any_field> = mc_marker_total.
    ENDIF.
  ENDIF.
  UNASSIGN <lv_any_field>.

  " rsalakhov 24.09.2015 16:41:16 mark empty keyfield for proper collecting
  IF iv_lvl >= 2.
    LOOP AT lr_current_keys INTO ls_current_key.
      ASSIGN COMPONENT ls_current_key-low OF STRUCTURE is_data TO <lv_any_field>.
      IF sy-subrc = 0.
        IF <lv_any_field> IS INITIAL.
          <lv_any_field> = mc_marker_empty.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  ASSIGN mo_instance->mt_podsumms->* TO <lt_podsumms>.
  COLLECT is_data INTO <lt_podsumms>.


ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_XLS_SUMM=>COMPARE_FS_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] LINE1                          TYPE        ANY
* | [--->] LINE2                          TYPE        ANY
* | [--->] COMPONENT                      TYPE        TY_V_KEYFIELD
* | [<-()] RV_EQUAL                       TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD compare_fs_fields.

  FIELD-SYMBOLS: <lv_comp1> TYPE any,
                 <lv_comp2> TYPE any.

  rv_equal = abap_false.

  ASSIGN COMPONENT component OF STRUCTURE line1 TO <lv_comp1>.
  CHECK <lv_comp1> IS ASSIGNED.

  ASSIGN COMPONENT component OF STRUCTURE line2 TO <lv_comp2>.
  CHECK <lv_comp2> IS ASSIGNED.

  IF <lv_comp1> = <lv_comp2>.
    rv_equal = abap_true.
  ENDIF.


ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLS_SUMM->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        ANY TABLE
* | [--->] IS_KEY_FIELDS                  TYPE        TS_KEY_FIELDS
* | [--->] IT_DEL_FIELDS                  TYPE        TT_DELETE_FIELDS(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD CONSTRUCTOR.
  DATA: dummy.
  DATA : lcl_structdescr TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS: <ls_compdescr> TYPE abap_compdescr,
                 <ls_delfield> TYPE ts_delete_field.

  CREATE DATA mt_podsumms LIKE it_data.
  CREATE DATA ms_podsumm_total LIKE LINE OF it_data.

  ms_key_fields = is_key_fields.

  " Add auto not collectable fields
  APPEND ms_key_fields-number TO mt_delete_fields.
  lcl_structdescr ?= cl_abap_typedescr=>describe_by_data_ref( ms_podsumm_total ).
  LOOP AT lcl_structdescr->components[] ASSIGNING <ls_compdescr> WHERE type_kind = cl_abap_typedescr=>TYPEKIND_CHAR OR
                                                                       type_kind = cl_abap_typedescr=>TYPEKIND_DATE OR
                                                                       type_kind = cl_abap_typedescr=>TYPEKIND_STRING.
    APPEND <ls_compdescr>-name TO mt_delete_fields.
  ENDLOOP.

  " Add user not collectable fields
  LOOP AT it_del_fields ASSIGNING <ls_delfield>.
    APPEND <ls_delfield> TO mt_delete_fields.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM mt_delete_fields.
  DELETE mt_delete_fields WHERE delfield IS INITIAL.

  " To upper case
  TRANSLATE: ms_key_fields-number  TO UPPER CASE,
             ms_key_fields-total   TO UPPER CASE,
             ms_key_fields-lvl2key TO UPPER CASE,
             ms_key_fields-lvl3key TO UPPER CASE.
  LOOP AT mt_delete_fields ASSIGNING <ls_delfield>.
    TRANSLATE <ls_delfield> TO UPPER CASE.
  ENDLOOP.

  " If user provided multiple keys
  IF ms_key_fields-special_char IS NOT INITIAL.
    ms_key_multiple-special_char = ms_key_fields-special_char.
    ms_key_multiple-number       = ms_key_fields-number.
    ms_key_multiple-total        = ms_key_fields-total.

    IF ms_key_fields-lvl2key CA ms_key_fields-special_char.
      ms_key_multiple-lvl2key = ms_key_fields-lvl2key.
      SPLIT ms_key_fields-lvl2key AT ms_key_fields-special_char INTO ms_key_fields-lvl2key dummy IN CHARACTER MODE.
    ENDIF.
    IF ms_key_fields-lvl3key CA ms_key_fields-special_char.
      ms_key_multiple-lvl3key = ms_key_fields-lvl3key.
      SPLIT ms_key_fields-lvl3key AT ms_key_fields-special_char INTO ms_key_fields-lvl3key dummy IN CHARACTER MODE.
    ENDIF.

  ENDIF.




ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_XLS_SUMM=>GENERATE_LINE_NUMS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_KEY_FIELDS                  TYPE        TS_KEY_FIELDS(optional)
* | [<-->] CT_DATA                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GENERATE_LINE_NUMS. " Last changed: 22.09.2015 12:29:37 by Rinat Salakhov

  DATA: ls_keys TYPE ts_key_fields,
        sy_tabix TYPE sytabix.

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lv_any_field> TYPE any.

  IF is_key_fields IS NOT SUPPLIED OR is_key_fields IS INITIAL.
    ls_keys = SET_KEYS.
  ELSE.
    ls_keys = is_key_fields.
  ENDIF.

  CHECK ls_keys-number IS NOT INITIAL.
  TRANSLATE: ls_keys-number  TO UPPER CASE.

  LOOP AT ct_data ASSIGNING <ls_data>.
    sy_tabix = sy-tabix.

    UNASSIGN <lv_any_field>.
    ASSIGN COMPONENT ls_keys-number OF STRUCTURE <ls_data> TO <lv_any_field>.
    IF <lv_any_field> IS ASSIGNED.

      MOVE sy_tabix TO <lv_any_field>.

    ENDIF.

  ENDLOOP.


endmethod.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_XLS_SUMM=>GENERATE_TEST_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINES_NUMBER                TYPE        I (default =100)
* | [--->] IS_KEY_FIELDS                  TYPE        TS_KEY_FIELDS(optional)
* | [<-->] CT_DATA                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GENERATE_TEST_DATA. " Last changed: 16.09.2015 17:15:09 by Rinat Salakhov

  "TODO: rework this to make pure generations

  DATA: lv_last_idx TYPE i,
        ls_data TYPE REF TO data,
        ls_keys TYPE ts_key_fields,
        lr_developers TYPE RANGE OF sy-uname.

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lv_field> TYPE any.

  APPEND 'IEQRSALAKHOV' TO lr_developers.

  CHECK sy-uname IN lr_developers or sy-subrc = 99.

  IF is_key_fields IS NOT SUPPLIED OR is_key_fields IS INITIAL.
    ls_keys = SET_KEYS.
  ELSE.
    ls_keys = is_key_fields.
  ENDIF.

  CHECK ls_keys-number IS NOT INITIAL.
  TRANSLATE: ls_keys-number  TO UPPER CASE.

  CREATE DATA ls_data LIKE LINE OF ct_data.
  ASSIGN ls_data->* TO <ls_data>.
  READ TABLE ct_data INTO <ls_data> INDEX lines( ct_data ).
  ASSIGN COMPONENT ls_keys-number OF STRUCTURE <ls_data> TO <lv_field>.
  IF sy-subrc = 0.
    lv_last_idx = <lv_field>.

    DO iv_lines_number TIMES.

      ADD 1 TO lv_last_idx.
      <lv_field> = lv_last_idx.
      APPEND <ls_data> TO ct_data.

    ENDDO.

  ENDIF.

endmethod.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLS_SUMM->INSERT_1LVL_TOTAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT_ITOGO                  TYPE        ANY (default ='ИТОГО')
* | [--->] IV_WRITE_CURRENT_TOTAL         TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<-->] CT_DATA                        TYPE        TABLE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD INSERT_1LVL_TOTAL.

  DATA: ls_data TYPE REF TO data. 

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lt_podsumms> TYPE STANDARD TABLE,
                 <lv_any_field> TYPE any.

  IF iv_write_current_total = abap_true. " Take TOTAL from current podsumms table

    ASSIGN mt_podsumms->* TO <lt_podsumms>.
    CREATE DATA ls_data LIKE LINE OF <lt_podsumms>.
    ASSIGN ls_data->* TO <ls_data>.

    read_by_lvl( EXPORTING iv_total = mc_marker_total
                 IMPORTING ef_data = <ls_data> ).
  ELSE.
    CHECK ct_data IS SUPPLIED. 
    CHECK ms_podsumm_total IS NOT INITIAL.

    ASSIGN ms_podsumm_total->* TO <ls_data>.
  ENDIF.

  CHECK <ls_data> IS NOT INITIAL. " This is structure with total

*  key1 = 'KOSTL'.
  ASSIGN COMPONENT ms_key_fields-total OF STRUCTURE <ls_data> TO <lv_any_field>.
  IF <lv_any_field> IS ASSIGNED.
    <lv_any_field> = iv_text_itogo.
    UNASSIGN <lv_any_field>.
  ENDIF.

  IF ct_data IS SUPPLIED.
    APPEND <ls_data> TO ct_data.
    IF iv_write_current_total = abap_true.
      REFRESH <lt_podsumms>.
    ENDIF.
  ELSE.
    IF ms_podsumm_total IS NOT INITIAL.
      FREE ms_podsumm_total.
    ENDIF.
    CREATE DATA ms_podsumm_total LIKE LINE OF <lt_podsumms>.
    ms_podsumm_total = ls_data.
  ENDIF.
  " TODO: WE NEED REFRESH HERE?
ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLS_SUMM->INSERT_BY_LVL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_KEYS                        TYPE        ANY
* | [--->] IV_LVL                         TYPE        I
* | [--->] IV_INDEX                       TYPE        SYST-TABIX (default =0)
* | [--->] IV_TEXT_ITOGO                  TYPE        ANY(optional)
* | [<-->] CT_DATA                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD insert_by_lvl. " Last changed: 24.09.2015 16:39:46 by Rinat Salakhov

  DATA: ls_data TYPE REF TO data,
        lv_key2_value TYPE REF TO data,
        lv_key3_value TYPE REF TO data,
        lv_keyvalue TYPE string,
        ls_keys TYPE TS_KEY_FIELDS,
        dummy.

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lt_podsumms> TYPE STANDARD TABLE,
                 <lv_key2_value> TYPE any,
                 <lv_key3_value> TYPE any,
                 <lv_any_field> TYPE any.

  ASSIGN mt_podsumms->* TO <lt_podsumms>.
  CREATE DATA ls_data LIKE LINE OF <lt_podsumms>.
  ASSIGN ls_data->* TO <ls_data>.

  ls_keys = ms_key_fields.

  IF iv_lvl >= 2.
    ASSIGN COMPONENT ls_keys-lvl2key OF STRUCTURE is_keys TO <lv_any_field>.
    CHECK sy-subrc = 0.
    CREATE DATA lv_key2_value LIKE <lv_any_field>.
    ASSIGN lv_key2_value->* TO <lv_key2_value>.
    <lv_key2_value> = <lv_any_field>.
    IF <lv_key2_value> IS INITIAL. WRITE MC_MARKER_EMPTY TO <lv_key2_value>. ENDIF.
  ENDIF.
  IF iv_lvl >= 3.
    ASSIGN COMPONENT ls_keys-lvl3key OF STRUCTURE is_keys TO <lv_any_field>.
    CHECK sy-subrc = 0.
    CREATE DATA lv_key3_value LIKE <lv_any_field>.
    ASSIGN lv_key3_value->* TO <lv_key3_value>.
    <lv_key3_value> = <lv_any_field>.
    IF <lv_key3_value> IS INITIAL. WRITE MC_MARKER_EMPTY TO <lv_key3_value>. ENDIF.
  ENDIF.

  CASE iv_lvl.
    WHEN 2.
      read_by_lvl( EXPORTING iv_key2 = <lv_key2_value>
                   IMPORTING ef_data = <ls_data> ).
      lv_keyvalue = <lv_key2_value>.
    WHEN 3.
      read_by_lvl( EXPORTING iv_key2 = <lv_key2_value>
                             iv_key3 = <lv_key3_value>
                   IMPORTING ef_data = <ls_data> ).
      lv_keyvalue = <lv_key3_value>.
  ENDCASE.

  CHECK <ls_data> IS NOT INITIAL.

  IF lv_keyvalue = MC_MARKER_EMPTY. CLEAR lv_keyvalue. ENDIF.

  ASSIGN COMPONENT ls_keys-total OF STRUCTURE <ls_data> TO <lv_any_field>.
  CHECK sy-subrc = 0.
  IF iv_text_itogo IS SUPPLIED AND iv_text_itogo IS NOT INITIAL.
    IF ls_keys-special_char IS NOT INITIAL.
      <lv_any_field> = parse_total_text( iv_text_itogo   = iv_text_itogo
                                         iv_special_char = ls_keys-special_char
                                         is_data         = <ls_data> ).
    ELSE.
      CONCATENATE iv_text_itogo lv_keyvalue INTO <lv_any_field> SEPARATED BY space.
    ENDIF.
  ELSE.
    CONCATENATE mc_marker_total lv_keyvalue INTO <lv_any_field> SEPARATED BY space.
  ENDIF.
  UNASSIGN <lv_any_field>.

  IF iv_index > 0.
    INSERT <ls_data> INTO ct_data INDEX iv_index.
  ELSE.
    APPEND <ls_data> TO ct_data.
  ENDIF.


ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLS_SUMM->PARSE_TOTAL_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT_ITOGO                  TYPE        ANY
* | [--->] IS_DATA                        TYPE        ANY
* | [--->] IV_SPECIAL_CHAR                TYPE        C (default ='|')
* | [<-()] RV_TEXT_ITOGO_PARSED           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD parse_total_text. " Last changed: 19.03.2015 17:31:00 by Rinat Salakhov
*"==test
*  TYPES: BEGIN OF ty_test,
*    n1 TYPE char40,
*    n2 TYPE char40,
*    nhkont TYPE char40,
*  END OF ty_test.
*"==test

  DATA: lt_find_results TYPE match_result_tab.

  DATA: lv_offset TYPE i,
        lv_length TYPE i,
        lv_field  TYPE ty_v_keyfield,
        lv_tabix  TYPE sy-tabix,
        ls_any_field_data TYPE string,
        lv_length_spchar   TYPE i,
        lv_offset_replaced TYPE i,
        lv_length_replaced TYPE i.

  FIELD-SYMBOLS: <ls_any_field> TYPE any,
                 <ls_result>    TYPE match_result.

*"==test
*  DATA: lv_test TYPE string,
*        ls_test_struct TYPE ty_test.
*  lv_test = 'ИТОГО по счету |n1| - (|nhkont|)   "|n2| "'.
*  ls_test_struct-n1 = '89023232'.
*  ls_test_struct-n2 = '00000'.
*  ls_test_struct-nhkont = '!!<<Описание счета>>!!'.
*"==test

  rv_text_itogo_parsed = iv_text_itogo.

  FIND ALL OCCURRENCES OF iv_special_char IN rv_text_itogo_parsed IN CHARACTER MODE RESULTS lt_find_results.
  CHECK sy-subrc = 0.

  lv_length_spchar = strlen( iv_special_char ).

  "==test
*1  0 15  1
*2  0 18  1
*3  0 21  1
*4  0 28  1
*  lv_offset = 15 + 1.
*  lv_length = 18 - lv_offset.
  "==test
  TRY .

      WHILE lv_tabix < lines( lt_find_results ).
        ADD 1 TO lv_tabix.
        READ TABLE lt_find_results ASSIGNING <ls_result> INDEX lv_tabix.
        CHECK sy-subrc = 0.
        lv_offset = <ls_result>-offset + lv_length_spchar. " first special char position

        ADD 1 TO lv_tabix.
        READ TABLE lt_find_results ASSIGNING <ls_result> INDEX lv_tabix.
        CHECK sy-subrc = 0.
        lv_length = <ls_result>-offset - lv_offset. " length of fieldname, before second special char

        lv_field = to_upper( iv_text_itogo+lv_offset(lv_length) ).

        ASSIGN COMPONENT lv_field OF STRUCTURE is_data TO <ls_any_field>.
        IF sy-subrc = 0.
          ls_any_field_data = <ls_any_field>.
          lv_length_replaced = strlen( ls_any_field_data ).
          REPLACE SECTION OFFSET lv_offset + lv_offset_replaced
            LENGTH lv_length
            OF rv_text_itogo_parsed
            WITH ls_any_field_data IN CHARACTER MODE.

          IF sy-subrc = 0.
            lv_offset_replaced = lv_offset_replaced + lv_length_replaced - lv_length.
          ENDIF.

        ENDIF.

      ENDWHILE.

    CATCH cx_root.
  ENDTRY.

  REPLACE ALL OCCURRENCES OF iv_special_char IN rv_text_itogo_parsed WITH space.

ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLS_SUMM->READ_BY_LVL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TOTAL                       TYPE        TY_V_KEYFIELD (default =MC_MARKER_TOTAL)
* | [--->] IV_KEY2                        TYPE        ANY(optional)
* | [--->] IV_KEY3                        TYPE        ANY(optional)
* | [<---] EF_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD READ_BY_LVL. " Last changed: 25.09.2015 13:04:04 by Rinat Salakhov

  FIELD-SYMBOLS: <lt_podsumms> TYPE STANDARD TABLE,
                 <ls_line> TYPE any.
  DATA: keys TYPE ts_key_fields.

  keys = ms_key_fields.

  ASSIGN mt_podsumms->* TO <lt_podsumms>.
  CHECK <lt_podsumms> IS ASSIGNED.

  TRY .

    IF iv_key3 IS SUPPLIED AND iv_key3 IS NOT INITIAL.
      IF keys-total <> keys-lvl3key AND keys-total <> keys-lvl2key.
             READ TABLE <lt_podsumms> ASSIGNING <ls_line> WITH KEY (keys-lvl3key) = iv_key3  (keys-lvl2key) = iv_key2 (keys-total) = iv_total .
      ELSE.  READ TABLE <lt_podsumms> ASSIGNING <ls_line> WITH KEY (keys-lvl3key) = iv_key3  (keys-lvl2key) = iv_key2.
      ENDIF.

    ELSEIF iv_key2 IS SUPPLIED AND iv_key2 IS NOT INITIAL.
      IF keys-total <> keys-lvl2key.
            READ TABLE <lt_podsumms> ASSIGNING <ls_line> WITH KEY (keys-lvl2key) = iv_key2 (keys-total) = iv_total .
      ELSE. READ TABLE <lt_podsumms> ASSIGNING <ls_line> WITH KEY (keys-lvl2key) = iv_key2.
      ENDIF.

    ELSE.
            READ TABLE <lt_podsumms> ASSIGNING <ls_line> WITH KEY (keys-total) = iv_total.
    ENDIF.
    IF sy-subrc = 0.

      ef_data = <ls_line>.

    ENDIF.

  CATCH cx_root.
  ENDTRY.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_XLS_SUMM=>WRITE_BY_LVL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_TOTALS_TEXT                 TYPE        TS_KEY_FIELDS(optional)
* | [--->] IV_WRITE_TOTAL                 TYPE        BOOLE_D (default =ABAP_FALSE)
* | [<-->] CT_DATA                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD write_by_lvl. " Last changed: 15.09.2015 15:37:37 by Rinat Salakhov


  DATA: lv_idx TYPE syst-tabix.

  FIELD-SYMBOLS: <lt_podsumms> TYPE STANDARD TABLE,
                 <ls_line> TYPE any,
                 <ls_excel_last> TYPE any,
                 <lv_any_field> TYPE any.
  DATA: keys TYPE ts_key_fields,
        ls_totals_text TYPE ts_key_fields,
        ls_excel_last TYPE REF TO data,
        lv_key3_exist TYPE boole_d,
        lv_old_lines TYPE i.

  IF is_totals_text IS NOT SUPPLIED OR is_totals_text IS INITIAL.
    ls_totals_text = SET_KEYS.
  ELSE.
    ls_totals_text = is_totals_text.
  ENDIF.

  keys = mo_instance->ms_key_fields.

  CREATE DATA ls_excel_last LIKE LINE OF ct_data.
  ASSIGN ls_excel_last->* TO <ls_excel_last>.
  READ TABLE ct_data INTO <ls_excel_last> INDEX 1.

  " key 3 existance
  lv_key3_exist = abap_false.
  IF keys-lvl3key IS NOT INITIAL.
    ASSIGN COMPONENT keys-lvl3key OF STRUCTURE <ls_excel_last> TO <lv_any_field>.
    IF sy-subrc = 0.
      lv_key3_exist = abap_true.
    ENDIF.
  ENDIF.

  LOOP AT ct_data ASSIGNING <ls_line>.
    lv_idx = sy-tabix.

    IF compare_fs_fields( line1 = <ls_line>
                          line2 = <ls_excel_last>
                          component = keys-lvl2key ) = abap_false.

      IF lv_key3_exist = abap_true.
        lv_old_lines = lines( ct_data ).
        mo_instance->insert_by_lvl( EXPORTING is_keys  = <ls_excel_last>
                                              iv_lvl   = 3
                                              iv_index = lv_idx
                                              iv_text_itogo = ls_totals_text-lvl3key
                                    CHANGING ct_data = ct_data ).
        IF lv_old_lines <> lines( ct_data ).
          lv_idx = lv_idx + 1.
        ENDIF.
      ENDIF.

      mo_instance->insert_by_lvl( EXPORTING is_keys  = <ls_excel_last>
                                            iv_lvl   = 2
                                            iv_index = lv_idx
                                            iv_text_itogo = ls_totals_text-lvl2key
                                  CHANGING ct_data = ct_data ).

    ELSEIF compare_fs_fields( line1 = <ls_line>
                              line2 = <ls_excel_last>
                              component = keys-lvl3key ) = abap_false
           AND lv_key3_exist = abap_true.

      mo_instance->insert_by_lvl( EXPORTING is_keys  = <ls_excel_last>
                                            iv_lvl   = 3
                                            iv_index = lv_idx
                                            iv_text_itogo = ls_totals_text-lvl3key
                                  CHANGING ct_data = ct_data ).

    ENDIF.

    <ls_excel_last> = <ls_line>.

  ENDLOOP.

  IF lv_key3_exist = abap_true.
    mo_instance->insert_by_lvl( EXPORTING is_keys  = <ls_excel_last>
                                          iv_lvl   = 3
                                          iv_text_itogo = ls_totals_text-lvl3key
                                CHANGING ct_data = ct_data ).
  ENDIF.

  mo_instance->insert_by_lvl( EXPORTING is_keys  = <ls_excel_last>
                                        iv_lvl   = 2
                                        iv_text_itogo = ls_totals_text-lvl2key
                              CHANGING ct_data = ct_data ).

  IF iv_write_total = abap_true.
    write_total( EXPORTING iv_text_itogo = ls_totals_text-total
                 CHANGING  ct_data = ct_data ).
  ENDIF.

ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_XLS_SUMM=>WRITE_BY_PAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_GAP_FIRST                   TYPE        I (default =15)
* | [--->] IV_GAP_NEXT                    TYPE        I (default =25)
* | [--->] IV_TEXT_ITOGO                  TYPE        STRING(optional)
* | [<-->] CT_DATA                        TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD write_by_pages. " rsalakhov 13.03.2015 15:59:07


    " Modify via sscr:
***  SELECTION-SCREEN SKIP.
***  SELECTION-SCREEN BEGIN OF LINE.
***  SELECTION-SCREEN COMMENT 1(43) text-006. "Строка с концом первой страницы/последующих
***  PARAMETERS: p_line1 TYPE i DEFAULT 13 VISIBLE LENGTH 2.
***
***  SELECTION-SCREEN COMMENT 59(1) text-dum. "/
***  PARAMETERS: p_line99 TYPE i DEFAULT 23.
***  SELECTION-SCREEN END OF LINE.


  DATA: lt_temp TYPE REF TO data,
        lv_tabix     TYPE sytabix,
        lv_text_itogo TYPE string.

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lt_podsumms> TYPE STANDARD TABLE,
                 <lt_temp> TYPE STANDARD TABLE,
                 <lv_any_field> TYPE any.

  CHECK mo_instance IS BOUND.

  " refresh collected
  ASSIGN mo_instance->mt_podsumms->* TO <lt_podsumms>.
  CHECK <lt_podsumms> IS ASSIGNED.
  REFRESH <lt_podsumms>.

  CREATE DATA lt_temp LIKE <lt_podsumms>.
  ASSIGN lt_temp->* TO <lt_temp>.

  "text
  IF iv_text_itogo IS SUPPLIED AND iv_text_itogo IS NOT INITIAL.
    lv_text_itogo = iv_text_itogo.
  ELSE.
    lv_text_itogo = 'Итого по странице'.
  ENDIF.

  LOOP AT ct_data ASSIGNING <ls_data>.
    lv_tabix = sy-tabix.

    IF lv_tabix = iv_gap_first  OR ( ( lv_tabix - iv_gap_first ) MOD iv_gap_next = 0 ).
      mo_instance->insert_1lvl_total( EXPORTING iv_text_itogo = lv_text_itogo CHANGING ct_data = <lt_temp> ).
    ENDIF.

    APPEND <ls_data> TO <lt_temp>.

    ASSIGN COMPONENT mo_instance->ms_key_fields-number OF STRUCTURE <ls_data> TO <lv_any_field>.
    IF <lv_any_field> IS ASSIGNED.
      IF <lv_any_field> IS NOT INITIAL.
        mo_instance->collect_by_lvl( EXPORTING is_data = <ls_data> ).  "1 lvl total
      ENDIF.
      UNASSIGN <lv_any_field>.
    ENDIF.


    IF lv_tabix = lines( ct_data[] ).
      mo_instance->insert_1lvl_total( EXPORTING iv_text_itogo = lv_text_itogo CHANGING ct_data = <lt_temp> ).
    ENDIF.


  ENDLOOP.

  ct_data[] = <lt_temp>.

ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_XLS_SUMM=>WRITE_TOTAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT_ITOGO                  TYPE        ANY (default ='ИТОГО')
* | [<---] ES_TOTALS                      TYPE        ANY
* | [<-->] CT_DATA                        TYPE        TABLE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD write_total. " rsalakhov 05.03.2015 15:20:50

  FIELD-SYMBOLS: <ls_data> TYPE any.

  CHECK mo_instance IS BOUND.
  CHECK mo_instance->ms_podsumm_total IS BOUND.
  CHECK mo_instance->ms_podsumm_total IS NOT INITIAL.

  IF es_totals IS REQUESTED.
    CLEAR es_totals.
    ASSIGN mo_instance->ms_podsumm_total->* TO <ls_data>.
    es_totals = <ls_data>.
  ENDIF.

  IF ct_data IS SUPPLIED.
    mo_instance->insert_1lvl_total( EXPORTING iv_text_itogo = iv_text_itogo
                                              iv_write_current_total = abap_false
                                    CHANGING  ct_data = ct_data ).
  ENDIF.

ENDMETHOD.
ENDCLASS.
