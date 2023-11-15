class ZCL_API_CONST definition
* SUPPORTS <7.4 *
  public
  final
  create private .

public section.
*"* public components of class ZCL_API_CONST
*"* do not include other source files here!!!

  types:
    BEGIN OF T_PARAMS,
           list_delimeter TYPE delim,
         END OF T_PARAMS .
  types:
    BEGIN OF T_ACCEPTS_CREATION,
           group TYPE ztools_const_group,
           type TYPE ztools_const_type,
         END OF T_ACCEPTS_CREATION .
  types TS_CONST type ZTOOLS_CM_T001 .
  types:
    Tt_CONST type STANDARD TABLE OF TS_CONST .
  types TT_VALUE_LIST type HRTIM00_REQ_WF_VALUE_TAB .

  constants:
    BEGIN OF E_CONSTANT_TYPES,
               FIELD TYPE ZTOOLS_CONST_TYPE VALUE '00',
               BOOLEAN TYPE ZTOOLS_CONST_TYPE VALUE '01',
               SAP_RANGE TYPE ZTOOLS_CONST_TYPE VALUE '02',
               tab_list TYPE ZTOOLS_CONST_TYPE VALUE '03',
               NOT_EXISTS TYPE ZTOOLS_CONST_TYPE VALUE '99',
             END OF E_CONSTANT_TYPES .

  type-pools ABAP .
  class-methods MAINTAIN_GUI
    importing
      !I_SCOPE type ANY optional
      !IV_EDIT_MODE type ABAP_BOOL default ABAP_TRUE
    preferred parameter I_SCOPE .
  methods GET
    importing
      !IV_NAME type TS_CONST-NAME
      !IV_KEY1 type TS_CONST-KEY1 optional
      !IV_KEY2 type TS_CONST-KEY2 optional
    returning
      value(RV_VALUE) type TS_CONST-VALUE .
  methods GET_BOOL
    importing
      !IV_NAME type TS_CONST-NAME
      !IV_KEY1 type TS_CONST-KEY1 optional
      !IV_KEY2 type TS_CONST-KEY2 optional
    returning
      value(RV_XFELD) type TS_CONST-ACTIVE .
  methods GET_LIST
    importing
      !IV_NAME type TS_CONST-NAME
      !IV_KEY1 type TS_CONST-KEY1 optional
      !IV_KEY2 type TS_CONST-KEY2 optional
    returning
      value(RT_LIST) type TT_VALUE_LIST .
  class-methods READ_CONSTANT
    importing
      !PI_NAME type ZTOOLS_CM_T001-NAME
      !PI_SCOPE type ZTOOLS_CM_T001-SCOPE default SY-REPID
      !PI_KEY1 type ZTOOLS_CM_T001-KEY1 optional
      !PI_KEY2 type ZTOOLS_CM_T001-KEY2 optional
    exporting
      !PE_TYPE type ZTOOLS_CM_T001-TYPE
      !PE_VALUE type ANY .
  class-methods WRITE_CONSTANT
    importing
      !PI_NAME type ZTOOLS_CM_T001-NAME
      !PI_SCOPE type ZTOOLS_CM_T001-SCOPE default SY-REPID
      !PI_TYPE type ZTOOLS_CM_T001-TYPE default E_CONSTANT_TYPES-FIELD
      !PI_KEY1 type ZTOOLS_CM_T001-KEY1 optional
      !PI_KEY2 type ZTOOLS_CM_T001-KEY2 optional
      !PI_DESCR type ZTOOLS_CM_T001-DESCR optional
      !PI_VALUE type ANY .
  class-methods CONFIGURE_CONSTANT
    importing
      !PI_NAME type ZTOOLS_CM_T001-NAME
      !PI_ACCEPTS_CREATION type T_ACCEPTS_CREATION optional
    exporting
      !PE_SUBRC type SYSUBRC
      !PE_VALUE type ANY .
  class-methods DEFINE_SCOPE
    importing
      !I_SCOPE type ANY
    preferred parameter I_SCOPE
    returning
      value(RV_SCOPE) type TS_CONST-SCOPE .
  class-methods INIT
    importing
      !I_SCOPE type ANY optional
      !IV_DELIM_LIST type DELIM default ','
    preferred parameter I_SCOPE
    returning
      value(RO_INST) type ref to ZCL_API_CONST .
  class-methods DESCRIBE_CLASS_NAME
    importing
      !IO_INSTANCE type ANY
    returning
      value(RV_CLASS_NAME) type SYST-REPID .
protected section.
*"* protected components of class ZCL_API_CONST
*"* do not include other source files here!!!

  data MT_BUF type TT_CONST .
  data MS_KEYS type TS_CONST .
private section.
*"* private components of class ZCL_API_CONST
*"* do not include other source files here!!!

  data MS_PARAMS type T_PARAMS .
  constants C_VERSION_1_0 type STRING value '1.0'. "#EC NOTEXT
  constants:
    BEGIN OF E_XML_TAGS,
               range      TYPE string VALUE 'range',
               line       TYPE string VALUE 'line',
               sign       TYPE string VALUE 'sign',
               option     TYPE string VALUE 'option',
               low        TYPE string VALUE 'low',
               high       TYPE string VALUE 'high',
               version    TYPE string VALUE 'version',
             END OF E_XML_TAGS .

  methods CONSTRUCTOR
    importing
      !IV_SCOPE type TS_CONST-SCOPE default SY-REPID
      !IV_DELIM_LIST type DELIM default ',' .
  methods SELECT_BUFF
    importing
      !IV_SCOPE type TS_CONST-SCOPE default SY-REPID .
  class-methods HELPER_WRITE_CONST_FIELD
    importing
      !IS_CONST type ZTOOLS_CM_T001 .
  class-methods HELPER_CONF_CONST_FIELD
    importing
      !PI_NAME type TS_CONST-NAME
    exporting
      !PE_SUBRC type SYSUBRC
    changing
      !PC_VALUE type ANY .
  class-methods HELPER_READ_CONST_SAP_RANGE
    importing
      !PI_VALUE type TS_CONST-VALUE
    exporting
      !PE_VALUE type ANY .
  class-methods HELPER_WRITE_CONST_SAP_RANGE
    importing
      !PI_NAME type TS_CONST-NAME
      !PI_SCOPE type TS_CONST-SCOPE optional
      !PI_VALUE type ANY .
ENDCLASS.



CLASS ZCL_API_CONST IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>CONFIGURE_CONSTANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        ZTOOLS_CM_T001-NAME
* | [--->] PI_ACCEPTS_CREATION            TYPE        T_ACCEPTS_CREATION(optional)
* | [<---] PE_SUBRC                       TYPE        SYSUBRC
* | [<---] PE_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONFIGURE_CONSTANT.
    DATA: l_group TYPE ztools_const_group,
          l_type TYPE ztools_const_type.

    CLEAR pe_subrc.
    CLEAR pe_value.

    CALL METHOD read_constant
      EXPORTING
        pi_name  = pi_name
        pi_scope = pi_accepts_creation-group
      IMPORTING
        pe_type  = l_type
        pe_value = pe_value.

    IF l_type = e_constant_types-not_exists.
      IF pi_accepts_creation IS SUPPLIED.
        l_type = pi_accepts_creation-type.
        l_group = pi_accepts_creation-group.
      ELSE.
        pe_subrc = 1.
        EXIT.
      ENDIF.
    ENDIF.

    CASE l_type.
      WHEN e_constant_types-field.
        CALL METHOD helper_conf_const_field
          EXPORTING
            pi_name  = pi_name
          IMPORTING
            pe_subrc = pe_subrc
          CHANGING
            pc_value = pe_value.
      WHEN OTHERS.
        pe_subrc = 1.
    ENDCASE.

    IF pe_subrc = 0.
      CALL METHOD write_constant
        EXPORTING
          pi_name  = pi_name
          pi_scope = l_group
          pi_type  = l_type
          pi_value = pe_value.
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_API_CONST->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SCOPE                       TYPE        TS_CONST-SCOPE (default =SY-REPID)
* | [--->] IV_DELIM_LIST                  TYPE        DELIM (default =',')
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONSTRUCTOR.

  ms_keys-scope = iv_scope.

  ms_params-list_delimeter = iv_delim_list.

  select_buff( iv_scope ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>DEFINE_SCOPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SCOPE                        TYPE        ANY
* | [<-()] RV_SCOPE                       TYPE        TS_CONST-SCOPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DEFINE_SCOPE.

  CHECK: i_scope IS NOT INITIAL.

  " 1. Is it object? return CLASSNAME
  rv_scope = describe_class_name( i_scope ).

  CHECK: rv_scope IS INITIAL.

  " 2. Is it sy-repid from class-method (ZCL_CLASS====CP)? Return CLASSNAME = ZCL_CLASS
  DATA: lv_progname TYPE RS38L-INCLUDE,
        lv_isclass TYPE abap_bool,
        lv_class_name TYPE seoclsname.
  MOVE i_scope TO lv_progname.
  CALL FUNCTION 'RS_PROGNAME_SPLIT'
    EXPORTING
      progname_with_namespace = lv_progname
    IMPORTING
      class_is_name           = lv_isclass
      class_name              = lv_class_name
    EXCEPTIONS
      OTHERS                  = 2.
  IF sy-subrc = 0 AND lv_isclass = abap_true AND lv_class_name IS NOT INITIAL.
    rv_scope = lv_class_name.
  ENDIF.

  CHECK: rv_scope IS INITIAL.

  " 3. Its just sy-repid.
  MOVE i_scope TO rv_scope.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>DESCRIBE_CLASS_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_INSTANCE                    TYPE        ANY
* | [<-()] RV_CLASS_NAME                  TYPE        SYST-REPID
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD describe_class_name.


  DATA: ref_descr TYPE REF TO cl_abap_refdescr,
        typedescr TYPE REF TO cl_abap_typedescr,
        absolute_class_name TYPE abap_abstypename,
        dummy TYPE c.

  typedescr = cl_abap_typedescr=>describe_by_data( io_instance ).
  CHECK: typedescr IS BOUND.

  TRY.
      ref_descr ?= typedescr.
      CHECK: sy-subrc = 0.
    CATCH cx_sy_move_cast_error.
      RETURN.
  ENDTRY.

  absolute_class_name = ref_descr->get_referenced_type( )->absolute_name. " \CLASS=ZCL_VMD_MANAGER
  SPLIT absolute_class_name AT '\CLASS=' INTO dummy rv_class_name.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_CONST->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RV_VALUE                       TYPE        TS_CONST-VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get.
  DATA:
        ls_buf TYPE ts_const,
        lt_key1_rng TYPE RANGE OF ts_const-key1,
        lt_key2_rng TYPE RANGE OF ts_const-key2.

  FIELD-SYMBOLS: <ls_key1> LIKE LINE OF lt_key1_rng,
                 <ls_key2> LIKE LINE OF lt_key2_rng.

  CLEAR: rv_value.

  IF iv_key1 IS NOT INITIAL AND iv_key1 <> '*'.
    APPEND INITIAL LINE TO lt_key1_rng ASSIGNING <ls_key1>.
    MOVE 'IEQ' TO <ls_key1>.
    <ls_key1>-low = to_upper( iv_key1 ).
  ENDIF.
  IF iv_key2 IS NOT INITIAL AND iv_key1 <> '*'.
    APPEND INITIAL LINE TO lt_key2_rng ASSIGNING <ls_key2>.
    MOVE 'IEQ' TO <ls_key2>.
    <ls_key2>-low = to_upper( iv_key2 ).
  ENDIF.

  LOOP AT mt_buf INTO ls_buf WHERE name = to_upper( iv_name )
                               AND key1 IN lt_key1_rng
                               AND key2 IN lt_key2_rng.
    EXIT.
  ENDLOOP.

  IF ls_buf IS NOT INITIAL.

    CASE ls_buf-type.

      WHEN e_constant_types-boolean.
        MOVE ls_buf-active TO rv_value.

      WHEN e_constant_types-sap_range.
        helper_read_const_sap_range( EXPORTING pi_value = ls_buf-value
                                     IMPORTING pe_value = rv_value ).

      WHEN OTHERS. " e_constant_types-field.
        MOVE ls_buf-value TO rv_value.

    ENDCASE.
*    ELSE.

  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_CONST->GET_BOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RV_XFELD                       TYPE        TS_CONST-ACTIVE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_BOOL.

  DATA: lv_value_raw TYPE ts_const-value.

  CLEAR: rv_xfeld.

  lv_value_raw = me->get( iv_name = iv_name
                          iv_key1 = iv_key1
                          iv_key2 = iv_key2 ).

  IF lv_value_raw IS NOT INITIAL.
    rv_xfeld = abap_true.
  ENDIF.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_CONST->GET_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RT_LIST                        TYPE        TT_VALUE_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_LIST.

  DATA: lv_value_raw TYPE ts_const-value.

  CLEAR: rt_list.

  lv_value_raw = me->get( iv_name = iv_name
                          iv_key1 = iv_key1
                          iv_key2 = iv_key2 ).

  IF lv_value_raw IS NOT INITIAL.
    SPLIT lv_value_raw AT ms_params-list_delimeter INTO TABLE rt_list.
    IF rt_list IS INITIAL.
      APPEND lv_value_raw TO rt_list.
    ENDIF.
  ENDIF.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_CONST=>HELPER_CONF_CONST_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        TS_CONST-NAME
* | [<---] PE_SUBRC                       TYPE        SYSUBRC
* | [<-->] PC_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method HELPER_CONF_CONST_FIELD.
    DATA: l_title TYPE string,
          l_value TYPE string,

          l_answer TYPE C LENGTH 1.

    CLEAR pe_subrc.
    l_value = pc_value.

    l_title = |Configuring constant { pi_name }...|.

    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING
        fieldname           = 'NAME'
        tabname             = 'ZTOOLS_CM_T001'
        titel               = l_title
        valuein             = l_value
      IMPORTING
        answer              = l_answer
        valueout            = l_value
      EXCEPTIONS
        fieldname_not_found = 1
        others              = 2.

    IF sy-subrc = 0 AND l_answer IS INITIAL.
      pc_value = l_value.
    ELSE.
      pe_subrc = 1.
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_CONST=>HELPER_READ_CONST_SAP_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_VALUE                       TYPE        TS_CONST-VALUE
* | [<---] PE_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method HELPER_READ_CONST_SAP_RANGE.
    FIELD-SYMBOLS: <li_table> TYPE STANDARD TABLE,
                   <lwa_line> TYPE ANY,
                   <l_field> TYPE ANY.

**********************************************************************
*  Variables needed for XML macros.
   DATA: lo_xml_document TYPE REF TO cl_xml_document,

         lo_element_tmp TYPE REF TO if_ixml_element,
         lo_node_tmp TYPE REF TO if_ixml_node,
         lo_iterator_tmp TYPE REF TO if_ixml_node_iterator,

**********************************************************************
*  Other variables
         l_retcode TYPE sysubrc,

         lo_element_root TYPE REF TO if_ixml_element,
         lo_collection_lines TYPE REF TO if_ixml_node_collection,
         lo_element_line TYPE REF TO if_ixml_element.
**********************************************************************
    DATA: lv_string_input TYPE string.
    lv_string_input = pi_value.

    PARSE_XML_STRING lv_string_input l_retcode.

    IF l_retcode = 0.
      GET_ROOT_ELEMENT lo_element_root.

      IF lo_element_root->get_name( ) = zcl_api_const=>e_xml_tags-range.
        ASSIGN pe_value TO <li_table>.

        FIND_ELEMENTS lo_element_root zcl_api_const=>e_xml_tags-line lo_collection_lines.

        ITERATE_XML_COLLECTION lo_collection_lines lo_element_line.
          IF lo_element_line IS BOUND.
            APPEND INITIAL LINE TO <li_table> ASSIGNING <lwa_line>.

            ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line zcl_api_const=>e_xml_tags-sign <l_field>.
            ENDIF.

            ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line zcl_api_const=>e_xml_tags-option <l_field>.
            ENDIF.

            ASSIGN COMPONENT 'LOW' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line zcl_api_const=>e_xml_tags-low <l_field>.
            ENDIF.

            ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line zcl_api_const=>e_xml_tags-high <l_field>.
            ENDIF.
          ENDIF.
        END_ITERATE_XML_COLLECTION.
      ENDIF.
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_CONST=>HELPER_WRITE_CONST_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_CONST                       TYPE        ZTOOLS_CM_T001
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method HELPER_WRITE_CONST_FIELD.
    DATA: lwa_ztools_cm_t001 TYPE ztools_cm_t001.

    lwa_ztools_cm_t001 = is_const.

    CHECK: lwa_ztools_cm_t001-name IS NOT INITIAL.

    MODIFY ztools_cm_t001 FROM lwa_ztools_cm_t001.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_CONST=>HELPER_WRITE_CONST_SAP_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        TS_CONST-NAME
* | [--->] PI_SCOPE                       TYPE        TS_CONST-SCOPE(optional)
* | [--->] PI_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method HELPER_WRITE_CONST_SAP_RANGE.
    FIELD-SYMBOLS: <li_table> TYPE STANDARD TABLE,
                   <lwa_line> TYPE ANY,
                   <l_field> TYPE ANY.

**********************************************************************
*  Variables needed for XML macros.
   DATA: lo_ixml TYPE REF TO if_ixml,
         lo_document TYPE REF TO if_ixml_document,
         lo_element_tmp TYPE REF TO if_ixml_element,

         lo_xml_document TYPE REF TO cl_xml_document,

         l_string_tmp TYPE string,
**********************************************************************
*  Other variables
         lwa_ztools_cm_t001 TYPE ztools_cm_t001,

         lo_root TYPE REF TO if_ixml_element,
         lo_line TYPE REF TO if_ixml_element.
**********************************************************************

    lwa_ztools_cm_t001-name = pi_name.
    lwa_ztools_cm_t001-scope = pi_scope.
    lwa_ztools_cm_t001-type = e_constant_types-sap_range.

    CREATE_XML_DOCUMENT.

    CREATE_XML_ELEMENT   lo_document    zcl_api_const=>e_xml_tags-range    lo_root.
    CREATE_XML_ATTRIBUTE lo_root        zcl_api_const=>e_xml_tags-version  zcl_api_const=>c_version_1_0.

    ASSIGN pi_value TO <li_table>.
    IF sy-subrc = 0.
      LOOP AT <li_table> ASSIGNING <lwa_line>.
        CREATE_XML_ELEMENT lo_root        zcl_api_const=>e_xml_tags-line     lo_line.

        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        zcl_api_const=>e_xml_tags-sign     <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        zcl_api_const=>e_xml_tags-option   <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'LOW' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        zcl_api_const=>e_xml_tags-low      <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        zcl_api_const=>e_xml_tags-high     <l_field>.
        ENDIF.
      ENDLOOP.

      DATA: lv_string_out TYPE string.
      CREATE_XML_STRING lv_string_out.
      lwa_ztools_cm_t001-value = lv_string_out.
    ENDIF.

    MODIFY ztools_cm_t001 FROM lwa_ztools_cm_t001.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SCOPE                        TYPE        ANY(optional)
* | [--->] IV_DELIM_LIST                  TYPE        DELIM (default =',')
* | [<-()] RO_INST                        TYPE REF TO ZCL_API_CONST
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD init.

  CREATE OBJECT ro_inst
    EXPORTING
      iv_scope      = define_scope( i_scope )
      iv_delim_list = iv_delim_list.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>MAINTAIN_GUI
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SCOPE                        TYPE        ANY(optional)
* | [--->] IV_EDIT_MODE                   TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD maintain_gui.

  DATA: lt_dba TYPE STANDARD TABLE OF vimsellist,
        ls_dba TYPE vimsellist,
        lv_scope TYPE ts_const-scope,
        lv_action TYPE char1.

  lv_scope = define_scope( i_scope ).

  IF lv_scope IS NOT INITIAL.

    ls_dba-viewfield = 'SCOPE'.
    ls_dba-operator = 'EQ'.
    ls_dba-value = lv_scope.
    APPEND ls_dba TO lt_dba.

  ENDIF.

  " S = Display, U = Change, T = Transport
  IF iv_edit_mode = abap_true.
    lv_action = 'U'.
  ELSE.
    lv_action = 'S'.
  ENDIF.

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action      = lv_action
      view_name   = 'ZTOOLS_CM_T001'
    TABLES
      dba_sellist = lt_dba
    EXCEPTIONS
      OTHERS      = 15.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " Add at the end of sscr:
*  SELECTION-SCREEN PUSHBUTTON 14(18) text-con USER-COMMAND ucon. " 'Maintain Constants'
*  AT SELECTION-SCREEN.
*  IF sy-ucomm = 'UCON'. zcl_api_const=>maintain_gui( sy-repid ). ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>READ_CONSTANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        ZTOOLS_CM_T001-NAME
* | [--->] PI_SCOPE                       TYPE        ZTOOLS_CM_T001-SCOPE (default =SY-REPID)
* | [--->] PI_KEY1                        TYPE        ZTOOLS_CM_T001-KEY1(optional)
* | [--->] PI_KEY2                        TYPE        ZTOOLS_CM_T001-KEY2(optional)
* | [<---] PE_TYPE                        TYPE        ZTOOLS_CM_T001-TYPE
* | [<---] PE_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method READ_CONSTANT.
    DATA: lwa_ztools_cm_t001 TYPE ztools_cm_t001,
          lt_key1_rng TYPE RANGE OF ZTOOLS_CM_T001-KEY1,
          lt_key2_rng TYPE RANGE OF ZTOOLS_CM_T001-KEY2.

    FIELD-SYMBOLS: <ls_key1> LIKE LINE OF lt_key1_rng,
                   <ls_key2> LIKE LINE OF lt_key2_rng.

*    CLEAR pe_group.
    CLEAR pe_type.
    CLEAR pe_value.

    IF pi_key1 IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_key1_rng ASSIGNING <ls_key1>.
      MOVE 'IEQ' TO <ls_key1>.
      <ls_key1>-low = pi_key1.
    ENDIF.
    IF pi_key2 IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_key2_rng ASSIGNING <ls_key2>.
      MOVE 'IEQ' TO <ls_key2>.
      <ls_key2>-low = pi_key2.
    ENDIF.

    SELECT SINGLE *
    FROM ztools_cm_t001
    INTO lwa_ztools_cm_t001
    WHERE name = pi_name
      AND scope = pi_scope
      AND key1 IN lt_key1_rng[]
      AND key2 IN lt_key2_rng[].

    IF sy-subrc = 0.
      pe_type = lwa_ztools_cm_t001-type.

      CASE lwa_ztools_cm_t001-type.
        WHEN e_constant_types-field.
          IF lwa_ztools_cm_t001-active = abap_true.
            MOVE lwa_ztools_cm_t001-value TO pe_value.
          ENDIF.

        WHEN e_constant_types-boolean.
          pe_value = lwa_ztools_cm_t001-active.

        WHEN e_constant_types-sap_range.
          helper_read_const_sap_range( EXPORTING pi_value = lwa_ztools_cm_t001-value
                                       IMPORTING pe_value = pe_value ).
      ENDCASE.
    ELSE.
      pe_type = e_constant_types-not_exists.
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_API_CONST->SELECT_BUFF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SCOPE                       TYPE        TS_CONST-SCOPE (default =SY-REPID)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SELECT_BUFF.
    DATA:
          lv_scope_in TYPE ts_const-scope,
          lt_scope_rng TYPE RANGE OF ts_const-scope.

    FIELD-SYMBOLS: <ls_scope> LIKE LINE OF lt_scope_rng,
                   <ls_buf> TYPE ts_const.

    CLEAR: mt_buf[].

    lv_scope_in = iv_scope.
    IF lv_scope_in IS INITIAL.
      lv_scope_in = ms_keys-scope.
    ENDIF.

    IF lv_scope_in IS NOT INITIAL AND lv_scope_in <> '*'.
      APPEND INITIAL LINE TO lt_scope_rng ASSIGNING <ls_scope>.
      MOVE 'IEQ' TO <ls_scope>.
      <ls_scope>-low = lv_scope_in.
    ENDIF.

    SELECT *
      FROM ztools_cm_t001
      INTO CORRESPONDING FIELDS OF TABLE mt_buf
      WHERE scope IN lt_scope_rng[]
        AND ( type =  e_constant_types-boolean OR
              type <> e_constant_types-boolean AND
              active = abap_true ).

    SORT mt_buf BY scope name key1 key2.

***    MANDT    MANDT               CLNT  3   0 Client
***    SCOPE    ZTOOLS_CONST_GROUP  CHAR  40  0 Constant group -> Domain PROGNAME
***    NAME     SXMS_DYN_CONF_NAME  CHAR  50  0 Parameter Name
***    KEY1     E_ADDKEY            CHAR  10  0 Additional Key
***    KEY2     ERGRN_VK            CHAR  17  0 Supplementary Classification Key
***
***    ACTIVE   EXTACT_KK           CHAR  1   0 Item Active
***    VALUE    AXT_PARAMVALUE      CHAR  255 0 Parameter Value
***    DESCR    MEMGMT_AMP_TEXT     CHAR  128 0 Description
***    TYPE     ZTOOLS_CONST_TYPE   NUMC  2   0 Constant type
******    FIELD      TYPE ZTOOLS_CONST_TYPE VALUE '00',
******    BOOLEAN    TYPE ZTOOLS_CONST_TYPE VALUE '01',
******    SAP_RANGE  TYPE ZTOOLS_CONST_TYPE VALUE '02',
******    TAB_LIST   TYPE ZTOOLS_CONST_TYPE VALUE '03',
******    NOT_EXISTS TYPE ZTOOLS_CONST_TYPE VALUE '99',

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_CONST=>WRITE_CONSTANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        ZTOOLS_CM_T001-NAME
* | [--->] PI_SCOPE                       TYPE        ZTOOLS_CM_T001-SCOPE (default =SY-REPID)
* | [--->] PI_TYPE                        TYPE        ZTOOLS_CM_T001-TYPE (default =E_CONSTANT_TYPES-FIELD)
* | [--->] PI_KEY1                        TYPE        ZTOOLS_CM_T001-KEY1(optional)
* | [--->] PI_KEY2                        TYPE        ZTOOLS_CM_T001-KEY2(optional)
* | [--->] PI_DESCR                       TYPE        ZTOOLS_CM_T001-DESCR(optional)
* | [--->] PI_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method WRITE_CONSTANT.
    DATA: ls_const_t001 TYPE ztools_cm_t001.

    ls_const_t001-name  = pi_name.
    ls_const_t001-scope = pi_scope.
    ls_const_t001-type  = pi_type.
    ls_const_t001-value = pi_value.
    ls_const_t001-key1  = pi_key1.
    ls_const_t001-key2  = pi_key2.
    ls_const_t001-descr = pi_descr.

    CASE pi_type.
      WHEN e_constant_types-field.

        ls_const_t001-active = abap_true.

        CALL METHOD helper_write_const_field
          EXPORTING
            is_const = ls_const_t001.

      WHEN e_constant_types-boolean.

        IF ls_const_t001-value IS NOT INITIAL.
          ls_const_t001-active = abap_true.
          CLEAR: ls_const_t001-value.
        ENDIF.

        CALL METHOD helper_write_const_field
          EXPORTING
            is_const = ls_const_t001.

      WHEN e_constant_types-sap_range.
        CALL METHOD helper_write_const_sap_range
          EXPORTING
            pi_name  = pi_name
            pi_scope = pi_scope
            pi_value = pi_value.
    ENDCASE.
  endmethod.
ENDCLASS.