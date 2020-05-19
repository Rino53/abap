class ZCL_SALV_REPORT definition
  public
  create public .

public section.

  types TS_MLST_BILLPLAN type ZPS_MLST_BILLPLAN .
  types:
    tt_mlst_billplan TYPE STANDARD TABLE OF ts_mlst_billplan .

  constants C_AKTYP_DISPLAY type TC10-TRTYP value 'A' ##NO_TEXT.
  data MT_DATA1 type TT_MLST_BILLPLAN .
  data MO_SALV1 type ref to CL_SALV_TABLE .
  constants C_LANGU_DEUTSCH type LANGU value 'D' ##NO_TEXT.

  methods REFRESH_DATA
    importing
      !IS_PRPS type PRPS optional
    exporting
      !ET_DATA type TT_MLST_BILLPLAN .
  methods CONSTRUCTOR
    importing
      !IO_PARENT_CONT type ref to CL_GUI_CONTAINER optional .
  methods SHOW_DATA
    importing
      !IV_MODE_TYPE type TC10-TRTYP optional .
  methods ON_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
protected section.
private section.

  data MV_DISPLAY_ONLY type ABAP_BOOL .
  data MO_PARENT_CONT type ref to CL_GUI_CONTAINER .
  data MS_PRPS type PRPS .

  methods GET_PROJECT
    importing
      !IV_POSID type PRPS-POSID optional
    returning
      value(RV_RET) type PRPS-POSID .
  methods MILESTONE_CREATE .
ENDCLASS.



CLASS ZCL_SALV_REPORT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SALV_REPORT->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_PARENT_CONT                 TYPE REF TO CL_GUI_CONTAINER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.

    mo_parent_cont = io_parent_cont.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SALV_REPORT->GET_PROJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_POSID                       TYPE        PRPS-POSID(optional)
* | [<-()] RV_RET                         TYPE        PRPS-POSID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_PROJECT.
    DATA(l_posid) = COND #( WHEN iv_posid IS NOT INITIAL THEN iv_posid ELSE ms_prps-posid ).
    TRY.
        rv_ret = CONV bapipr-project_definition( l_posid(8) ).
      CATCH cx_sy_itab_line_not_found.
        CLEAR rv_ret.
    ENDTRY.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SALV_REPORT->MILESTONE_CREATE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD milestone_create.

*    SUBMIT ZPS_ESTIMATOR. " <- great reference

    DATA:
**          ls_bapi_project_definition     TYPE bapi_project_definition,
*          ls_bapi_project_definition_upd TYPE bapi_project_definition_up,
      ls_methods            TYPE bapi_method_project,
      lt_methods            TYPE TABLE OF bapi_method_project,
      ls_wbs_mlst_table     TYPE bapi_wbs_milestone,
      lt_wbs_mlst_table     TYPE TABLE OF  bapi_wbs_milestone,
      ls_wbs_mlst_table_upd TYPE bapi_wbs_milestone_upd,
      lt_wbs_mlst_table_upd TYPE TABLE OF  bapi_wbs_milestone_upd,
      lt_message            TYPE STANDARD TABLE OF bapi_meth_message,
      ls_bapiret1           TYPE BAPIRETURN1.

    CONSTANTS:
      lc_create        TYPE bapi_method_project-method VALUE 'Create' ##NO_TEXT,
      lc_save          TYPE bapi_method_project-method VALUE 'Save' ##NO_TEXT,
      lc_wbs_element   TYPE bapi_method_project-objecttype VALUE 'WBS-Element' ##NO_TEXT,
      lc_wbs_hierarchy TYPE bapi_method_project-objecttype VALUE 'WBS-Hierarchy' ##NO_TEXT,
      lc_wbs_milestone TYPE bapi_method_project-objecttype VALUE 'WBS-Milestone' ##NO_TEXT.

*    " fill BAPI header data
    DATA(l_proj_def) = get_project( ms_prps-posid ).
    DATA(ls_project_definition)     = VALUE bapi_project_definition( project_definition = l_proj_def ).
    DATA(ls_project_definition_upd) = VALUE bapi_project_definition_up( project_definition = abap_true ).

    APPEND VALUE #( objecttype = lc_wbs_milestone
                    method     = lc_create
                    refnumber  = '000001' ) TO lt_methods.

    APPEND VALUE #( method     = lc_save ) TO lt_methods.

    APPEND VALUE #( wbs_element = ms_prps-posid
                    sched_milestone_date_basic = '20191217'
                    fixed_milestone_date_basic = '20191219'
                    milestone_usage = 'DOWN'
                    description = '18-12 1' ) TO lt_wbs_mlst_table.

    APPEND VALUE #( sched_milestone_date_basic = abap_true
                    fixed_milestone_date_basic = abap_true
                    milestone_usage = abap_true
                    description = abap_true ) TO lt_wbs_mlst_table_upd.


    CALL FUNCTION 'DEQUEUE_EC_PROJ'
      EXPORTING
*       MODE_PROJ_ENQ       = 'E'
*       MANDT = SY-MANDT
*       TYP   =
        pspid = l_proj_def
*       X_TYP = ' '
*       X_PSPID             = ' '
*       _SCOPE              = '3'
*       _SYNCHRON           = ' '
*       _COLLECT            = ' '
      .


    CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
      EXPORTING
        i_project_definition         = ls_project_definition
        i_project_definition_upd     = ls_project_definition_upd
      IMPORTING
        return                       = ls_bapiret1
      TABLES
        i_method_project             = lt_methods
        i_wbs_milestone_table        = lt_wbs_mlst_table
        i_wbs_milestone_table_update = lt_wbs_mlst_table_upd
        e_message_table              = lt_message.

    READ TABLE lt_message INTO DATA(ls_message) WITH KEY message_type = 'I'
                                                         message_number = '197'
                                                         message_id = 'CJ'. " Milestone 000000001567 was created for WBS element R20.00710.20
    IF sy-subrc = 0.

      DATA(l_mlst_num) = CONV mlst_zaehl( space ).
      DATA(l_dummy) = space.
      SPLIT ls_message-message_text AT space INTO l_dummy l_mlst_num l_dummy.

      DATA: ls_bill_plan_add TYPE zmm_fplth.
      ls_bill_plan_add-fplnr = l_proj_def.
      ls_bill_plan_add-fpltr = l_mlst_num.
      ls_bill_plan_add-gar_active  = abap_true.
      ls_bill_plan_add-gar_startdt = '20191217'.

      MODIFY zmm_fplth FROM ls_bill_plan_add.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SALV_REPORT->ON_DOUBLE_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            LIKE
* | [--->] COLUMN                         LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_double_click.
    DATA:
*          lt_notes   TYPE STANDARD TABLE OF txw_note,
      lt_notes   TYPE catsxt_longtext_itab,
      lt_tline   TYPE STANDARD TABLE OF tline,
      ls_txthead TYPE thead.

*    data(ls_cell) = me->mo_salv1->get_selections( )->get_current_cell( ).
*    data(ls_cells) = me->mo_salv1->get_selections( )->get_selected_cells( ).
*    data(ls_rows) = me->mo_salv1->get_selections( )->get_selected_rows( ).



    CASE column.
      WHEN 'KTEXT' OR 'LTXSP'.
        READ TABLE mt_data1 INTO DATA(ls_data1) INDEX row.
        IF ls_data1-ltxsp IS NOT INITIAL.

          ls_txthead-tdname   = ls_data1-mlst_zaehl.
          ls_txthead-tdspras  = ls_data1-ltxsp.
          ls_txthead-tdid     = 'MSTT'.
          ls_txthead-tdobject = 'PMS'.


          CALL FUNCTION 'READ_TEXT' " TODO: redo this with API
            EXPORTING
              id       = ls_txthead-tdid
              language = ls_txthead-tdspras
              name     = ls_txthead-tdname
              object   = ls_txthead-tdobject
            IMPORTING
              header   = ls_txthead
            TABLES
              lines    = lt_tline
            EXCEPTIONS
              OTHERS   = 8.
          IF sy-subrc = 0.

            LOOP AT lt_tline INTO DATA(ls_tline) WHERE tdformat = '*'.
              APPEND ls_tline-tdline TO lt_notes.
            ENDLOOP.

            DATA(lt_ltext_old) = lt_notes[].
            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title        = 'Milestone Longtext'
                im_display_mode = mv_display_only
              CHANGING
                ch_text         = lt_notes.

            IF sy-subrc = 0 AND
               sy-ucomm = 'CX_CONT' AND " Ok pressed
               mv_display_only = abap_false AND
               lt_ltext_old[] <> lt_notes[].

              CLEAR: lt_tline[].
              LOOP AT lt_notes INTO DATA(lv_note).
                APPEND VALUE #( tdformat = '*' tdline = lv_note ) TO lt_tline.
              ENDLOOP.

              CALL FUNCTION 'SAVE_TEXT'
                EXPORTING
                  header          = ls_txthead
                  savemode_direct = abap_true
                TABLES
                  lines           = lt_tline
                EXCEPTIONS
                  OTHERS          = 5.

            ENDIF.



          ENDIF.

        ENDIF.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SALV_REPORT->ON_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_SALV_FUNCTION                LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ON_USER_COMMAND.

    data(ls_cell) = me->mo_salv1->get_selections( )->get_current_cell( ).
    data(ls_cells) = me->mo_salv1->get_selections( )->get_selected_cells( ).
    data(ls_rows) = me->mo_salv1->get_selections( )->get_selected_rows( ).

    CHECK mt_data1[] IS NOT INITIAL.

    CASE e_salv_function.
      WHEN 'VA_BILLPLAN'.
        IF ls_rows[] IS NOT INITIAL.
          DATA(lv_vbeln) = mt_data1[ ls_rows[ 1 ] ]-vbeln.
          SET PARAMETER ID 'AUN' FIELD lv_vbeln.
          CASE mv_display_only.
            WHEN abap_true.
              CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. " TODO: replace with batch input!
            WHEN abap_false.
              CALL TRANSACTION 'VA02' AND SKIP FIRST SCREEN.
          ENDCASE.
        ENDIF.

      WHEN 'BTN_TEST1'.
        milestone_create( ).

      WHEN OTHERS.
    ENDCASE.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SALV_REPORT->REFRESH_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_PRPS                        TYPE        PRPS(optional)
* | [<---] ET_DATA                        TYPE        TT_MLST_BILLPLAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD refresh_data.


    CLEAR: mt_data1[].

    IF is_prps IS NOT INITIAL.
      ms_prps = is_prps.
    ENDIF.

    CHECK ms_prps-pspnr IS NOT INITIAL.

    SELECT *
      FROM mlst
      INTO CORRESPONDING FIELDS OF TABLE et_data
      WHERE pspnr = ms_prps-pspnr.

    CHECK et_data[] IS NOT INITIAL.

    " Get descriptions
    SELECT *
      FROM mltx
      INTO TABLE @DATA(lt_mlst_desc)
      FOR ALL ENTRIES IN @et_data[]
      WHERE mltx_zaehl = @et_data-mlst_zaehl
        AND aend_zaehl = @et_data-zaehl
        AND langu IN (@sy-langu, @c_langu_deutsch).

    IF lt_mlst_desc[] IS NOT INITIAL.
      DATA: ls_desc TYPE mltx.
      LOOP AT et_data ASSIGNING FIELD-SYMBOL(<ls_line>).
        CLEAR ls_desc.
        READ TABLE lt_mlst_desc INTO ls_desc WITH KEY mltx_zaehl = <ls_line>-mlst_zaehl
                                                      aend_zaehl = <ls_line>-zaehl
                                                      langu      = sy-langu.
        IF sy-subrc <> 0.
          READ TABLE lt_mlst_desc INTO ls_desc WITH KEY mltx_zaehl = <ls_line>-mlst_zaehl
                                                        aend_zaehl = <ls_line>-zaehl
                                                        langu      = c_langu_deutsch.
        ENDIF.
        IF ls_desc IS NOT INITIAL.
          <ls_line>-ktext = ls_desc-ktext.
          <ls_line>-ltxsp = ls_desc-ltxsp.
        ENDIF.
      ENDLOOP.
    ENDIF.


    " Get vbeln
    SELECT
      fplt~mlstn,
      fplt~fplnr,
      fplt~fpltr,
      fpla~vbeln
    FROM fplt INNER JOIN fpla ON fplt~fplnr = fpla~fplnr
    INTO TABLE @DATA(lt_fplt)
    FOR ALL ENTRIES IN @et_data
    WHERE fplt~mlstn = @et_data-mlst_zaehl
      AND fplt~fksaf = ''.

    LOOP AT et_data ASSIGNING <ls_line>.
      READ TABLE lt_fplt INTO DATA(ls_fplt) WITH KEY mlstn = <ls_line>-mlst_zaehl.
      IF sy-subrc = 0.
        <ls_line>-vbeln = ls_fplt-vbeln.

      ENDIF.
    ENDLOOP.

    " Get bill plan values " TODO recode with this API: ZCL_MM_INVOICE_PLAN
    DATA(l_proj_def) = get_project( ).
    SELECT *
      FROM zmm_fplth
      INTO TABLE @DATA(lt_billplan)
      WHERE fplnr = @l_proj_def.
    LOOP AT et_data ASSIGNING <ls_line> WHERE mlst_zaehl IS NOT INITIAL.

      READ TABLE lt_billplan INTO DATA(ls_billplan) WITH KEY fpltr = <ls_line>-mlst_zaehl.
      IF sy-subrc = 0.
        <ls_line>-gs_gar = ls_billplan-gs_gar.
      ENDIF.
    ENDLOOP.


    " Post-processing
    LOOP AT et_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      IF <ls_data>-pdatu IS INITIAL AND
         <ls_data>-edatu IS NOT INITIAL.

        <ls_data>-pdatu = <ls_data>-edatu.

      ENDIF.
    ENDLOOP.


    mt_data1[] = et_data[].





    " TEST

*    FIELD-SYMBOLS: <gs_proj> TYPE PROJ.
*    ASSIGN ('(SAPLCJWB)PROJ') TO <gs_proj>.
*
*    IF <gs_proj> IS ASSIGNED.



*    CALL FUNCTION 'BAPI_PROJECT_GETINFO'
*     EXPORTING
*       PROJECT_DEFINITION           = <gs_proj>-pspid
*       WITH_ACTIVITIES              = abap_true
*       WITH_MILESTONES              = abap_true
*       WITH_SUBTREE                 = abap_true
*     IMPORTING
*       E_PROJECT_DEFINITION         =
*       RETURN                       =
*     TABLES
*       I_WBS_ELEMENT_TABLE          =
*       E_WBS_ELEMENT_TABLE          =
*       E_WBS_MILESTONE_TABLE        =
*       E_WBS_HIERARCHIE_TABLE       =
*       E_ACTIVITY_TABLE             =
*       E_MESSAGE_TABLE              =
*              .
*    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SALV_REPORT->SHOW_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MODE_TYPE                   TYPE        TC10-TRTYP(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SHOW_DATA.

  mv_display_only = COND #( WHEN IV_MODE_TYPE = c_aktyp_display THEN abap_true ELSE abap_false ).

  IF mo_salv1 IS NOT INITIAL.
    mo_salv1->refresh( ).

  ELSE.

    CHECK mo_parent_cont IS NOT INITIAL.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
           r_container = mo_parent_cont
          IMPORTING
            r_salv_table = mo_salv1
          CHANGING
            t_table      = mt_data1 ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.


    "set the columns technical
    DATA(lr_columns) = mo_salv1->get_columns( ).
    lr_columns->set_optimize( cl_salv_columns=>true ).

    DATA: lo_column TYPE REF TO cl_salv_column.
    LOOP AT lr_columns->get( ) INTO DATA(ls_column_ref).
      CASE ls_column_ref-columnname.
        WHEN 'MATNR' OR 'VKORG'.
        WHEN '1'.
          lo_column ?= ls_column_ref-r_column.    "Narrow casting
          IF sy-subrc = 0 AND lo_column IS NOT INITIAL.
            lo_column->set_visible( abap_false ).
*        lo_column->set_technical( abap_true ).
          ENDIF.
      ENDCASE.

    ENDLOOP.

    " Functions
    DATA(lr_functions) = mo_salv1->get_functions( ).
    lr_functions->set_all( cl_salv_functions_list=>true ).

    lr_functions->add_function( name = 'VA_BILLPLAN' text = 'SO Billing Plan' icon = '@16@' tooltip = 'Jump to SO Billing Plan' " TODO: add tranlation!
                                position = if_salv_c_function_position=>right_of_salv_functions ).

    lr_functions->add_function( name = 'BTN_TEST1' text = 'Test1' icon = '@16@' tooltip = 'Test'
                                position = if_salv_c_function_position=>right_of_salv_functions ).


    DATA(lo_selections) = mo_salv1->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*  go_salv->set_screen_status( pfstatus = 'SALV_STANDARD' report = sy-repid set_functions = go_salv->c_functions_all ).
*  DATA(lo_events) = go_salv->get_event( ).
*  CREATE OBJECT go_hevents.
*  SET HANDLER go_hevents->handle_ucomm FOR lo_events.

    DATA(lo_layout) = mo_salv1->get_layout( ).
    lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).

*    IF p_lay1 IS NOT INITIAL.
*      lo_layout->set_initial_layout( p_lay1 ).
*    ELSE.
    lo_layout->set_default( abap_true ).
*    ENDIF.
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

*    DATA: lo_event_handler TYPE REF TO ZCL_SALV_REPORT.
    DATA(lo_events) = mo_salv1->get_event( ).
*    CREATE OBJECT lo_event_handler.
    SET HANDLER me->on_user_command FOR lo_events.
    SET HANDLER me->on_double_click FOR lo_events.


    " display the table
    mo_salv1->display( ).

  ENDIF.



  endmethod.
ENDCLASS.
