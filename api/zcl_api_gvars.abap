*----------------------------------------------------------------------*
*       CLASS ZCL_API_GVARS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
* SUPPORTS <7.4 *
* based on https://blogs.sap.com/2020/04/09/constants-management/ *
CLASS ZCL_API_GVARS DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    TYPES:
*"* public components of class ZCL_API_GVARS
*"* do not include other source files here!!!
      BEGIN OF t_params,
        list_delimeter TYPE delim,
      END OF t_params .
    TYPES:
      BEGIN OF t_accepts_creation,
        group TYPE ZD_GVARS_GROUP,
        type  TYPE ZD_GVARS_TYPE,
      END OF t_accepts_creation .
    TYPES ts_const TYPE ZT_GVARS01 .
    TYPES:
      tt_const TYPE STANDARD TABLE OF ts_const .
    TYPES tt_value_list TYPE hrtim00_req_wf_value_tab .

    CONSTANTS:
      BEGIN OF e_constant_types,
        field      TYPE ZD_GVARS_TYPE VALUE '00',
        boolean    TYPE ZD_GVARS_TYPE VALUE '01',
        sap_range  TYPE ZD_GVARS_TYPE VALUE '02',
        tab_list   TYPE ZD_GVARS_TYPE VALUE '03',
        not_exists TYPE ZD_GVARS_TYPE VALUE '99',
      END OF e_constant_types .

    CLASS-METHODS maintain_gui
      IMPORTING
        !i_scope       TYPE any OPTIONAL
        !i_scope_field TYPE viewfield DEFAULT 'SCOPE'
        !iv_viewname   TYPE dd02v-tabname DEFAULT 'ZT_GVARS01'
        !iv_edit_mode  TYPE abap_bool DEFAULT abap_true
          PREFERRED PARAMETER i_scope .
    METHODS get
      IMPORTING
        !iv_name        TYPE ts_const-name
        !iv_key1        TYPE ts_const-key1 OPTIONAL
        !iv_key2        TYPE ts_const-key2 OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE ts_const-value .
    CLASS-METHODS read_var
      IMPORTING
        !iv_name        TYPE ts_const-name
        !iv_scope       TYPE ts_const-scope DEFAULT sy-repid
        !iv_key1        TYPE ts_const-key1 OPTIONAL
        !iv_key2        TYPE ts_const-key2 OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE ts_const-value .
    METHODS get_first
      IMPORTING
        !iv_name        TYPE ts_const-name
        !iv_key1        TYPE ts_const-key1 OPTIONAL
        !iv_key2        TYPE ts_const-key2 OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE ts_const-value .
    METHODS get_bool
      IMPORTING
        !iv_name        TYPE ts_const-name
        !iv_key1        TYPE ts_const-key1 OPTIONAL
        !iv_key2        TYPE ts_const-key2 OPTIONAL
      RETURNING
        VALUE(rv_xfeld) TYPE ts_const-active .
    METHODS get_list
      IMPORTING
        !iv_name       TYPE ts_const-name
        !iv_key1       TYPE ts_const-key1 OPTIONAL
        !iv_key2       TYPE ts_const-key2 OPTIONAL
      RETURNING
        VALUE(rt_list) TYPE tt_value_list .
    METHODS get_vals
      IMPORTING
        !iv_name   TYPE ts_const-name
        !iv_key1   TYPE ts_const-key1 OPTIONAL
        !iv_key2   TYPE ts_const-key2 OPTIONAL
      EXPORTING
        !ev_value1 TYPE clike
        !ev_value2 TYPE clike
        !ev_value3 TYPE clike
        !ev_value4 TYPE clike
        !ev_value5 TYPE clike
        !ev_value6 TYPE clike
        !ev_value7 TYPE clike
        !ev_value8 TYPE clike
        !ev_value9 TYPE clike .
    CLASS-METHODS read_constant
      IMPORTING
        !pi_name  TYPE ZT_GVARS01-name
        !pi_scope TYPE ZT_GVARS01-scope DEFAULT sy-repid
        !pi_key1  TYPE ZT_GVARS01-key1 OPTIONAL
        !pi_key2  TYPE ZT_GVARS01-key2 OPTIONAL
      EXPORTING
        !pe_type  TYPE ZT_GVARS01-type
        !pe_value TYPE any .
    CLASS-METHODS write_constant
      IMPORTING
        !pi_name  TYPE ZT_GVARS01-name
        !pi_scope TYPE ZT_GVARS01-scope DEFAULT sy-repid
        !pi_type  TYPE ZT_GVARS01-type DEFAULT e_constant_types-field
        !pi_key1  TYPE ZT_GVARS01-key1 OPTIONAL
        !pi_key2  TYPE ZT_GVARS01-key2 OPTIONAL
        !pi_descr TYPE ZT_GVARS01-descr OPTIONAL
        !pi_value TYPE any .
    CLASS-METHODS configure_constant
      IMPORTING
        !pi_name             TYPE ZT_GVARS01-name
        !pi_accepts_creation TYPE t_accepts_creation OPTIONAL
      EXPORTING
        !pe_subrc            TYPE sysubrc
        !pe_value            TYPE any .
    CLASS-METHODS define_scope
      IMPORTING
        !i_scope        TYPE any
          PREFERRED PARAMETER i_scope
      RETURNING
        VALUE(rv_scope) TYPE ts_const-scope .
    CLASS-METHODS init
      IMPORTING
        !i_scope       TYPE any OPTIONAL
        !iv_delim_list TYPE delim DEFAULT ','
          PREFERRED PARAMETER i_scope
      RETURNING
        VALUE(ro_inst) TYPE REF TO ZCL_API_GVARS .
    CLASS-METHODS describe_class_name
      IMPORTING
        !io_instance         TYPE any
      RETURNING
        VALUE(rv_class_name) TYPE syst-repid .
  PROTECTED SECTION.
*"* protected components of class ZCL_API_GVARS
*"* do not include other source files here!!!

    DATA mt_buf TYPE tt_const .
    DATA ms_keys TYPE ts_const .
  PRIVATE SECTION.
*"* private components of class ZCL_API_GVARS
*"* do not include other source files here!!!

    DATA ms_params TYPE t_params .
    CONSTANTS c_xml_version TYPE string VALUE '1.0'.        "#EC NOTEXT
    CONSTANTS:
      BEGIN OF e_xml_tags,
        range   TYPE string VALUE 'range',
        line    TYPE string VALUE 'line',
        sign    TYPE string VALUE 'sign',
        option  TYPE string VALUE 'option',
        low     TYPE string VALUE 'low',
        high    TYPE string VALUE 'high',
        version TYPE string VALUE 'version',
      END OF e_xml_tags .

    METHODS _example
      IMPORTING
        !iv_repid TYPE syst-repid OPTIONAL .
    METHODS constructor
      IMPORTING
        !iv_scope      TYPE ts_const-scope DEFAULT sy-repid
        !iv_delim_list TYPE delim DEFAULT ',' .
    METHODS select_buff
      IMPORTING
        !iv_scope TYPE ts_const-scope DEFAULT sy-repid .
    CLASS-METHODS helper_write_const_field
      IMPORTING
        !is_const TYPE ZT_GVARS01 .
    CLASS-METHODS helper_conf_const_field
      IMPORTING
        !pi_name  TYPE ts_const-name
      EXPORTING
        !pe_subrc TYPE sysubrc
      CHANGING
        !pc_value TYPE any .
    CLASS-METHODS helper_read_const_sap_range
      IMPORTING
        !pi_value TYPE ts_const-value
      EXPORTING
        !pe_value TYPE any .
    CLASS-METHODS helper_write_const_sap_range
      IMPORTING
        !pi_name  TYPE ts_const-name
        !pi_scope TYPE ts_const-scope OPTIONAL
        !pi_value TYPE any .
ENDCLASS.



CLASS ZCL_API_GVARS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>CONFIGURE_CONSTANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        ZT_GVARS01-NAME
* | [--->] PI_ACCEPTS_CREATION            TYPE        T_ACCEPTS_CREATION(optional)
* | [<---] PE_SUBRC                       TYPE        SYSUBRC
* | [<---] PE_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD configure_constant.
    DATA: l_group TYPE ZD_GVARS_GROUP,
          l_type  TYPE ZD_GVARS_TYPE.

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
  ENDMETHOD.                    "CONFIGURE_CONSTANT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_API_GVARS->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SCOPE                       TYPE        TS_CONST-SCOPE (default =SY-REPID)
* | [--->] IV_DELIM_LIST                  TYPE        DELIM (default =',')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    ms_keys-scope = iv_scope.

    ms_params-list_delimeter = iv_delim_list.

    select_buff( iv_scope ).

  ENDMETHOD.                    "CONSTRUCTOR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>DEFINE_SCOPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SCOPE                        TYPE        ANY
* | [<-()] RV_SCOPE                       TYPE        TS_CONST-SCOPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD define_scope.

    CHECK: i_scope IS NOT INITIAL.

    " 1. Is it object? return CLASSNAME
    rv_scope = describe_class_name( i_scope ).

    CHECK: rv_scope IS INITIAL.

    " 2. Is it sy-repid from class-method (ZCL_CLASS====CP)? Return CLASSNAME = ZCL_CLASS
    DATA: lv_progname   TYPE rs38l-include,
          lv_isclass    TYPE abap_bool,
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

  ENDMETHOD.                    "DEFINE_SCOPE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>DESCRIBE_CLASS_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_INSTANCE                    TYPE        ANY
* | [<-()] RV_CLASS_NAME                  TYPE        SYST-REPID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD describe_class_name.


    DATA: ref_descr           TYPE REF TO cl_abap_refdescr,
          typedescr           TYPE REF TO cl_abap_typedescr,
          absolute_class_name TYPE abap_abstypename,
          dummy               TYPE c.

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

  ENDMETHOD.                    "describe_class_name


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_GVARS->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RV_VALUE                       TYPE        TS_CONST-VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.
    DATA:
      ls_buf      TYPE ts_const,
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

  ENDMETHOD.                    "get


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_GVARS->GET_BOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RV_XFELD                       TYPE        TS_CONST-ACTIVE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_bool.

    DATA: lv_value_raw TYPE ts_const-value.

    CLEAR: rv_xfeld.

    lv_value_raw = me->get( iv_name = iv_name
                            iv_key1 = iv_key1
                            iv_key2 = iv_key2 ).

    IF lv_value_raw IS NOT INITIAL.
      rv_xfeld = abap_true.
    ENDIF.


  ENDMETHOD.                    "GET_BOOL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_GVARS->GET_FIRST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RV_VALUE                       TYPE        TS_CONST-VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_first.

    me->get_vals( EXPORTING iv_key1 = iv_key1
                            iv_key2 = iv_key2
                            iv_name = iv_name
                  IMPORTING ev_value1 = rv_value ).

  ENDMETHOD.                    "GET_FIRST


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_GVARS->GET_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RT_LIST                        TYPE        TT_VALUE_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_list.

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


  ENDMETHOD.                    "GET_LIST


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_API_GVARS->GET_VALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<---] EV_VALUE1                      TYPE        CLIKE
* | [<---] EV_VALUE2                      TYPE        CLIKE
* | [<---] EV_VALUE3                      TYPE        CLIKE
* | [<---] EV_VALUE4                      TYPE        CLIKE
* | [<---] EV_VALUE5                      TYPE        CLIKE
* | [<---] EV_VALUE6                      TYPE        CLIKE
* | [<---] EV_VALUE7                      TYPE        CLIKE
* | [<---] EV_VALUE8                      TYPE        CLIKE
* | [<---] EV_VALUE9                      TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_vals.

    DATA: lv_output_field TYPE fieldname.
    FIELD-SYMBOLS: <lv_output> TYPE any.

    DATA(lt_val_list) = me->get_list( iv_name = iv_name
                                      iv_key1 = iv_key1
                                      iv_key2 = iv_key2 ).

    LOOP AT lt_val_list INTO DATA(lv_value_single).

      lv_output_field = CONV #( |EV_VALUE{ sy-tabix WIDTH = 1 }| ).
      ASSIGN (lv_output_field) TO <lv_output>.
      IF sy-subrc = 0.
        MOVE lv_value_single TO <lv_output>.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "GET_LIST


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_GVARS=>HELPER_CONF_CONST_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        TS_CONST-NAME
* | [<---] PE_SUBRC                       TYPE        SYSUBRC
* | [<-->] PC_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD helper_conf_const_field.
    DATA: l_title  TYPE string,
          l_value  TYPE string,

          l_answer TYPE c LENGTH 1.

    CLEAR pe_subrc.
    l_value = pc_value.

    l_title = |Configuring constant { pi_name }...|.

    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING
        fieldname           = 'NAME'
        tabname             = 'ZT_GVARS01'
        titel               = l_title
        valuein             = l_value
      IMPORTING
        answer              = l_answer
        valueout            = l_value
      EXCEPTIONS
        fieldname_not_found = 1
        OTHERS              = 2.

    IF sy-subrc = 0 AND l_answer IS INITIAL.
      pc_value = l_value.
    ELSE.
      pe_subrc = 1.
    ENDIF.
  ENDMETHOD.                    "HELPER_CONF_CONST_FIELD


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_GVARS=>HELPER_READ_CONST_SAP_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_VALUE                       TYPE        TS_CONST-VALUE
* | [<---] PE_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD helper_read_const_sap_range.
    FIELD-SYMBOLS: <li_table> TYPE STANDARD TABLE,
                   <lwa_line> TYPE any,
                   <l_field>  TYPE any.

**********************************************************************
*  Variables needed for XML macros.
    DATA: lo_xml_document     TYPE REF TO cl_xml_document,

          lo_element_tmp      TYPE REF TO if_ixml_element,
          lo_node_tmp         TYPE REF TO if_ixml_node,
          lo_iterator_tmp     TYPE REF TO if_ixml_node_iterator,

**********************************************************************
*  Other variables
          l_retcode           TYPE sysubrc,

          lo_element_root     TYPE REF TO if_ixml_element,
          lo_collection_lines TYPE REF TO if_ixml_node_collection,
          lo_element_line     TYPE REF TO if_ixml_element.
**********************************************************************
    DATA: lv_string_input TYPE string.
    lv_string_input = pi_value.

*    parse_xml_string lv_string_input l_retcode.

    IF l_retcode = 0.
*      get_root_element lo_element_root.

      IF lo_element_root->get_name( ) = ZCL_API_GVARS=>e_xml_tags-range.
        ASSIGN pe_value TO <li_table>.

*        find_elements lo_element_root ZCL_API_GVARS=>e_xml_tags-line lo_collection_lines.

*        iterate_xml_collection lo_collection_lines lo_element_line.
        IF lo_element_line IS BOUND.
          APPEND INITIAL LINE TO <li_table> ASSIGNING <lwa_line>.

          ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lwa_line> TO <l_field>.
          IF sy-subrc = 0.
*            read_xml_field lo_element_line ZCL_API_GVARS=>e_xml_tags-sign <l_field>.
          ENDIF.

          ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lwa_line> TO <l_field>.
          IF sy-subrc = 0.
*            read_xml_field lo_element_line ZCL_API_GVARS=>e_xml_tags-option <l_field>.
          ENDIF.

          ASSIGN COMPONENT 'LOW' OF STRUCTURE <lwa_line> TO <l_field>.
          IF sy-subrc = 0.
*            read_xml_field lo_element_line ZCL_API_GVARS=>e_xml_tags-low <l_field>.
          ENDIF.

          ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lwa_line> TO <l_field>.
          IF sy-subrc = 0.
*            read_xml_field lo_element_line ZCL_API_GVARS=>e_xml_tags-high <l_field>.
          ENDIF.
        ENDIF.
*        end_iterate_xml_collection.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "HELPER_READ_CONST_SAP_RANGE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_GVARS=>HELPER_WRITE_CONST_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_CONST                       TYPE        ZT_GVARS01
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD helper_write_const_field.
    DATA: lwa_zt_gvars01 TYPE ZT_GVARS01.

    lwa_zt_gvars01 = is_const.

    CHECK: lwa_zt_gvars01-name IS NOT INITIAL.

    MODIFY ZT_GVARS01 FROM lwa_zt_gvars01.
  ENDMETHOD.                    "HELPER_WRITE_CONST_FIELD


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_API_GVARS=>HELPER_WRITE_CONST_SAP_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        TS_CONST-NAME
* | [--->] PI_SCOPE                       TYPE        TS_CONST-SCOPE(optional)
* | [--->] PI_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD helper_write_const_sap_range.
    FIELD-SYMBOLS: <li_table> TYPE STANDARD TABLE,
                   <lwa_line> TYPE any,
                   <l_field>  TYPE any.

**********************************************************************
*  Variables needed for XML macros.
    DATA: lo_ixml              TYPE REF TO if_ixml,
          lo_document          TYPE REF TO if_ixml_document,
          lo_element_tmp       TYPE REF TO if_ixml_element,

          lo_xml_document      TYPE REF TO cl_xml_document,

          l_string_tmp         TYPE string,
**********************************************************************
*  Other variables
          lwa_zt_gvars01 TYPE ZT_GVARS01,

          lo_root              TYPE REF TO if_ixml_element,
          lo_line              TYPE REF TO if_ixml_element.
**********************************************************************

    lwa_zt_gvars01-name = pi_name.
    lwa_zt_gvars01-scope = pi_scope.
    lwa_zt_gvars01-type = e_constant_types-sap_range.

*    create_xml_document.

*    create_xml_element   lo_document    ZCL_API_GVARS=>e_xml_tags-range    lo_root.
*    create_xml_attribute lo_root        ZCL_API_GVARS=>e_xml_tags-version  ZCL_API_GVARS=>c_xml_version. " 1.0

    ASSIGN pi_value TO <li_table>.
    IF sy-subrc = 0.
      LOOP AT <li_table> ASSIGNING <lwa_line>.
*        create_xml_element lo_root        ZCL_API_GVARS=>e_xml_tags-line     lo_line.

        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
*          write_xml_field  lo_line        ZCL_API_GVARS=>e_xml_tags-sign     <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
*          write_xml_field  lo_line        ZCL_API_GVARS=>e_xml_tags-option   <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'LOW' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
*          write_xml_field  lo_line        ZCL_API_GVARS=>e_xml_tags-low      <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
*          write_xml_field  lo_line        ZCL_API_GVARS=>e_xml_tags-high     <l_field>.
        ENDIF.
      ENDLOOP.

      DATA: lv_string_out TYPE string.
*      create_xml_string lv_string_out.
      lwa_zt_gvars01-value = lv_string_out.
    ENDIF.

    MODIFY ZT_GVARS01 FROM lwa_zt_gvars01.
  ENDMETHOD.                    "HELPER_WRITE_CONST_SAP_RANGE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SCOPE                        TYPE        ANY(optional)
* | [--->] IV_DELIM_LIST                  TYPE        DELIM (default =',')
* | [<-()] RO_INST                        TYPE REF TO ZCL_API_GVARS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init.

    CREATE OBJECT ro_inst
      EXPORTING
        iv_scope      = define_scope( i_scope )
        iv_delim_list = iv_delim_list.

  ENDMETHOD.                    "init


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>MAINTAIN_GUI
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SCOPE                        TYPE        ANY(optional)
* | [--->] I_SCOPE_FIELD                  TYPE        VIEWFIELD (default ='SCOPE')
* | [--->] IV_VIEWNAME                    TYPE        DD02V-TABNAME (default ='ZT_GVARS01')
* | [--->] IV_EDIT_MODE                   TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD maintain_gui.

    DATA: lt_dba    TYPE STANDARD TABLE OF vimsellist,
          ls_dba    TYPE vimsellist,
          lv_scope  TYPE ts_const-scope,
          lv_action TYPE char1.

    lv_scope = define_scope( i_scope ).

    IF lv_scope IS NOT INITIAL.

      ls_dba-viewfield = i_scope_field.
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
        view_name   = iv_viewname
      TABLES
        dba_sellist = lt_dba
      EXCEPTIONS
        OTHERS      = 15.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*  === Snippets: ===
*
*  1. Add at the end of sscr:
*  SELECTION-SCREEN PUSHBUTTON 14(18) text-con USER-COMMAND ucon. " 'Maintain Constants'
*  AT SELECTION-SCREEN.
*  IF sy-ucomm = 'UCON'. ZCL_API_GVARS=>maintain_gui( sy-repid ). ENDIF.
*
*  2. To call another view:
*  IF sy-ucomm = 'UCAK'. ZCL_API_GVARS=>maintain_gui( iv_viewname = 'ZVMDM_BP_AKONT' ). ENDIF.

  ENDMETHOD.                    "maintain_gui


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>READ_CONSTANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        ZT_GVARS01-NAME
* | [--->] PI_SCOPE                       TYPE        ZT_GVARS01-SCOPE (default =SY-REPID)
* | [--->] PI_KEY1                        TYPE        ZT_GVARS01-KEY1(optional)
* | [--->] PI_KEY2                        TYPE        ZT_GVARS01-KEY2(optional)
* | [<---] PE_TYPE                        TYPE        ZT_GVARS01-TYPE
* | [<---] PE_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_constant.
    DATA: lwa_zt_gvars01 TYPE ZT_GVARS01,
          lt_key1_rng          TYPE RANGE OF ZT_GVARS01-key1,
          lt_key2_rng          TYPE RANGE OF ZT_GVARS01-key2.

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
    FROM ZT_GVARS01
    INTO lwa_zt_gvars01
    WHERE name = pi_name
      AND scope = pi_scope
      AND key1 IN lt_key1_rng[]
      AND key2 IN lt_key2_rng[].

    IF sy-subrc = 0.
      pe_type = lwa_zt_gvars01-type.

      CASE lwa_zt_gvars01-type.
        WHEN e_constant_types-field.
          IF lwa_zt_gvars01-active = abap_true.
            MOVE lwa_zt_gvars01-value TO pe_value.
          ENDIF.

        WHEN e_constant_types-boolean.
          pe_value = lwa_zt_gvars01-active.

        WHEN e_constant_types-sap_range.
          helper_read_const_sap_range( EXPORTING pi_value = lwa_zt_gvars01-value
                                       IMPORTING pe_value = pe_value ).
      ENDCASE.
    ELSE.
      pe_type = e_constant_types-not_exists.
    ENDIF.
  ENDMETHOD.                    "READ_CONSTANT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>READ_VAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        TS_CONST-NAME
* | [--->] IV_SCOPE                       TYPE        TS_CONST-SCOPE (default =SY-REPID)
* | [--->] IV_KEY1                        TYPE        TS_CONST-KEY1(optional)
* | [--->] IV_KEY2                        TYPE        TS_CONST-KEY2(optional)
* | [<-()] RV_VALUE                       TYPE        TS_CONST-VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_var.
    DATA: lo_gvars TYPE REF TO ZCL_API_GVARS.

    lo_gvars = ZCL_API_GVARS=>init( iv_scope ).
    CHECK lo_gvars IS NOT INITIAL.

    rv_value = lo_gvars->get( iv_name = iv_name
                              iv_key1 = iv_key1
                              iv_key2 = iv_key2 ).

  ENDMETHOD.                    "READ_VAR


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_API_GVARS->SELECT_BUFF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SCOPE                       TYPE        TS_CONST-SCOPE (default =SY-REPID)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select_buff.
    DATA:
      lv_scope_in  TYPE ts_const-scope,
      lt_scope_rng TYPE RANGE OF ts_const-scope.

    FIELD-SYMBOLS: <ls_scope> LIKE LINE OF lt_scope_rng,
                   <ls_buf>   TYPE ts_const.

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
      FROM ZT_GVARS01
      INTO CORRESPONDING FIELDS OF TABLE mt_buf
      WHERE scope IN lt_scope_rng[]
        AND ( type =  e_constant_types-boolean OR
              type <> e_constant_types-boolean AND
              active = abap_true ).

    SORT mt_buf BY scope name key1 key2.

***    MANDT    MANDT               CLNT  3   0 Client
***    SCOPE    ZD_GVARS_GROUP  CHAR  40  0 Constant group -> Domain PROGNAME
***    NAME     SXMS_DYN_CONF_NAME  CHAR  50  0 Parameter Name
***    KEY1     E_ADDKEY            CHAR  10  0 Additional Key
***    KEY2     ERGRN_VK            CHAR  17  0 Supplementary Classification Key
***
***    ACTIVE   EXTACT_KK           CHAR  1   0 Item Active
***    VALUE    AXT_PARAMVALUE      CHAR  255 0 Parameter Value
***    DESCR    MEMGMT_AMP_TEXT     CHAR  128 0 Description
***    TYPE     ZD_GVARS_TYPE   NUMC  2   0 Constant type
******    FIELD      TYPE ZD_GVARS_TYPE VALUE '00',
******    BOOLEAN    TYPE ZD_GVARS_TYPE VALUE '01',
******    SAP_RANGE  TYPE ZD_GVARS_TYPE VALUE '02',
******    TAB_LIST   TYPE ZD_GVARS_TYPE VALUE '03',
******    NOT_EXISTS TYPE ZD_GVARS_TYPE VALUE '99',

  ENDMETHOD.                    "SELECT_BUFF


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_API_GVARS=>WRITE_CONSTANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] PI_NAME                        TYPE        ZT_GVARS01-NAME
* | [--->] PI_SCOPE                       TYPE        ZT_GVARS01-SCOPE (default =SY-REPID)
* | [--->] PI_TYPE                        TYPE        ZT_GVARS01-TYPE (default =E_CONSTANT_TYPES-FIELD)
* | [--->] PI_KEY1                        TYPE        ZT_GVARS01-KEY1(optional)
* | [--->] PI_KEY2                        TYPE        ZT_GVARS01-KEY2(optional)
* | [--->] PI_DESCR                       TYPE        ZT_GVARS01-DESCR(optional)
* | [--->] PI_VALUE                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_constant.
    DATA: ls_const_t001 TYPE ZT_GVARS01.

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
  ENDMETHOD.                    "WRITE_CONSTANT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_API_GVARS->_EXAMPLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPID                       TYPE        SYST-REPID(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _example.
    DATA: lo_gvars      TYPE REF TO ZCL_API_GVARS,
          lv_value      TYPE me->ts_const-value,
          ls_adrc       TYPE adrc,
          lt_bukrs_list TYPE t_bukrs.

    " 1. Initialization
    " # Simple init
    lo_gvars = ZCL_API_GVARS=>init( iv_repid ).
    lo_gvars = ZCL_API_GVARS=>init( sy-repid ).

    " # Child Class: ZCL_CHILD->CONSTRUCTOR, parent class will create GVARS of childs scope
*     super->constructor( iv_progname = ZCL_API_GVARS=>describe_class_name( me ) ). " passed scope = ZCL_CHILD
    " AT method super->constructor:
*     mo_gvars = ZCL_API_GVARS=>init( iv_progname ).

    " # Init scope of another object and read value
*    zcl_dev_obj1=>mo_gvars->get( iv_name = 'VAR1' iv_key1 = 'POST' ).
    " AT method zcl_dev_obj1=>class_constructor:
*    mo_gvars = ZCL_API_GVARS=>init( sy-repid ).

    " 2. Value Read
    " # Read with keys
    ls_adrc-name1 = lo_gvars->get( iv_name = 'AdRC-name1' iv_key1 = 'JUR' ).

    " # Simple read and Move
    MOVE: lo_gvars->get( 'adrc-addr_group' ) TO ls_adrc-addr_group.

    " # Read list -> recieving typed ITAB
    lt_bukrs_list = lo_gvars->get_list( 'BUKRS_LIST' ).

    " # Using as key in ITAB Read
    READ TABLE lt_bukrs_list TRANSPORTING NO FIELDS WITH KEY table_line = lo_gvars->get( 'B100' ).

    " # Get first value of the list
    ls_adrc-street = lo_gvars->get_first( 'STREET_LIST' ).

    " # Read single value directly, outside of the scope
    lv_value = lo_gvars->read_var( iv_name  = 'VAR1'
                                   iv_scope = 'ZPROG').

    " # Read value by bool switch
    CONSTANTS: c_text_name TYPE me->ts_const-name VALUE 'EMAIL_LONGTEXT_ID',
               c_tkey_crt  TYPE me->ts_const-key1 VALUE 'CREATION',
               c_tkey_upd  TYPE me->ts_const-key1 VALUE 'UPDATING'.
    DATA:
      lv_tkaction    TYPE me->ts_const-key1 VALUE c_tkey_upd,
      lv_longtext_id TYPE me->ts_const-value.

    IF lo_gvars->get_bool( 'CREATE_VEND' ) = abap_true.
      lv_tkaction = c_tkey_crt.
    ENDIF.

    lv_longtext_id = lo_gvars->get( iv_name = c_text_name iv_key1 = lv_tkaction ).



  ENDMETHOD.                    "_EXAMPLE
ENDCLASS.
