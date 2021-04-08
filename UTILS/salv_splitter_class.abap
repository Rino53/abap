class ZCL_CHILD_ALV1 definition
  public
  inheriting from ZCL_PARENT_REPORT
  create public .

public section.

  constants:
    BEGIN OF c_tb_pos,
        right TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
        left  TYPE salv_de_function_pos VALUE if_salv_c_function_position=>left_of_salv_functions,
      END OF c_tb_pos .
  data MO_SALV1 type ref to CL_SALV_TABLE .
  data MO_SALV2 type ref to CL_SALV_TABLE .

  methods CONSTRUCTOR
    importing
      !IO_PARENT_CONT type ref to CL_GUI_CONTAINER optional .
  methods ALV_DISPLAY
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

  data MO_PARENT_CONT type ref to CL_GUI_CONTAINER .

  methods USER_COMMAND_LINE_PICK .
private section.

  data:
    BEGIN OF ms_params,
      batch_debug    TYPE abap_bool,
      background_run TYPE abap_bool,
      autotax TYPE abap_bool VALUE abap_true,

      BEGIN OF rights,
        locking TYPE abap_bool,
        posting TYPE abap_bool,
        display TYPE abap_bool,
        invoice_clerk TYPE abap_bool,
      END OF rights ,

      BEGIN OF batch,
        debug_yes TYPE abap_bool,
        mode      TYPE ctu_params-dismode VALUE 'N', " Default: do not display batch input
      END OF batch,

      BEGIN OF sscr,
        blart TYPE bkpf-blart,
        budat TYPE bkpf-budat,
        zumsk TYPE bseg-zumsk,
*        hkont TYPE bseg-hkont,
*        koart TYPE bseg-koart,
*        bschl TYPE bseg-bschl,
*        bukrs TYPE bkpf-bukrs,
*        gjahr_fiscal TYPE bseg-gjahr,
      END OF sscr,

      run_status TYPE ZMM_FPLTS,
      run_cnt TYPE i VALUE 1,

    END OF ms_params .
  data:
    BEGIN OF ms_sel_filters,
      BEGIN OF fname,
        heads TYPE string,
        items TYPE string,
        load_from_server TYPE abap_bool VALUE abap_false,
      END OF fname,
      BEGIN OF sscr,
*        blart TYPE bkpf-blart,
*        budat TYPE bkpf-budat,
*        zumsk TYPE bseg-zumsk,
        hkont TYPE bseg-hkont,
        koart TYPE bseg-koart,
        bschl TYPE bseg-bschl,
        bukrs TYPE bkpf-bukrs,
        gjahr_fiscal TYPE bseg-gjahr,
*        fplnr_rng TYPE TT_FPLNR_RNG,
        delta TYPE fakwr,
        test_skip TYPE abap_bool,
        do_retry TYPE abap_bool,
        reverse_need TYPE abap_bool,
        cleardb_need TYPE abap_bool,
        checkdiff TYPE abap_bool,
      END OF sscr,
*      bukrs_rng TYPE icl_bukrs_range,
*      ebeln_rng TYPE mmpur_t_ebeln,
*      bsart_rng TYPE mmpur_t_bsart,
*      ekgrp_rng TYPE mmpur_t_ekgrp,
*      afdat_rng TYPE /atl/date_range_t,
    END OF ms_sel_filters .
  data MV_DISPLAY_ONLY type ABAP_BOOL .
  data MO_ALV_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_ALV_CONTAIN1 type ref to CL_GUI_CONTAINER .
  data MO_ALV_CONTAIN2 type ref to CL_GUI_CONTAINER .

  methods ALV_INIT_HEAD
    changing
      !CO_SALV type ref to CL_SALV_TABLE optional
      !CO_PARENT type ref to CL_GUI_CONTAINER optional .
  methods ALV_INIT_ITEM
    changing
      !CO_SALV type ref to CL_SALV_TABLE optional
      !CO_PARENT type ref to CL_GUI_CONTAINER optional .
  methods SELECTED_ROWS_UPDATE
    importing
      !IT_SELECTED_INDEXES type SALV_T_ROW optional .
  methods ALV_FIELDCAT_INIT
    importing
      !IO_SALV_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  class-methods ALV_TOOLBAR_INIT
    importing
      !IO_TB_FUNCT type ref to CL_SALV_FUNCTIONS_LIST .
ENDCLASS.



CLASS ZCL_CHILD_ALV1 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CHILD_ALV1->ALV_DISPLAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MODE_TYPE                   TYPE        TC10-TRTYP(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ALV_DISPLAY.

    mv_display_only = abap_false . "COND #( WHEN iv_mode_type = c_aktyp_display THEN abap_true ELSE abap_false ).

    IF mo_salv1 IS NOT INITIAL AND
       mo_salv2 IS NOT INITIAL.

      mo_salv1->refresh( ).
      mo_salv2->refresh( ).

    ELSEIF mo_parent_cont IS NOT INITIAL.

      mo_alv_splitter = new #( parent  = mo_parent_cont rows = 2 columns = 1 ).
      mo_alv_contain1 = mo_alv_splitter->get_container( row = 1 column = 1 ).
      mo_alv_contain2 = mo_alv_splitter->get_container( row = 2 column = 1 ).

      mo_alv_splitter->set_row_height( id = 1 height = 10 ).
*      mo_alv_contain1->set_height( 10 ).
*      mo_alv_contain1->SET_ROW_SASH( id = 1 type = 1 value = 1 ).


      me->alv_init_head( CHANGING co_salv = mo_salv1 co_parent = mo_alv_contain1 ).
      me->alv_init_item( CHANGING co_salv = mo_salv2 co_parent = mo_alv_contain2 ).

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CHILD_ALV1->ALV_FIELDCAT_INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SALV_COLUMNS                TYPE REF TO CL_SALV_COLUMNS_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ALV_FIELDCAT_INIT.
    DATA: lv_str TYPE string.

    "set the columns technical
    io_salv_columns->set_optimize( cl_salv_columns=>true ).

    TRY.
        io_salv_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    DATA: lo_column TYPE REF TO cl_salv_column.

    LOOP AT io_salv_columns->get( ) INTO DATA(ls_column_ref).
      CASE ls_column_ref-columnname.
*        WHEN 'MATNR' OR 'VKORG'.
*        WHEN 'FAKWR'.
*          ls_column_ref-r_column->
*        WHEN 'ZFI_CHECK_1A'.
*          ls_column_ref-r_column->set_medium_text( '1A' ).
*          ls_column_ref-r_column->set_short_text(  '1A' ).
*          ls_column_ref-r_column->set_long_text(   '1A' ).
*        WHEN 'ZFI_CHECK_1B'.
*          ls_column_ref-r_column->set_medium_text( '1B' ).
*          ls_column_ref-r_column->set_short_text(  '1B' ).
*          ls_column_ref-r_column->set_long_text(   '1B' ).

*        WHEN 'WRBTR_BSIK'.
*          ls_column_ref-r_column->set_medium_text( CONV #( TEXT-c01 ) ).
*          ls_column_ref-r_column->set_short_text(  CONV #( TEXT-c01 ) ).
*          ls_column_ref-r_column->set_long_text(   CONV #( TEXT-c01 ) ).
*        WHEN 'WRBTR_BSAK'.
*          ls_column_ref-r_column->set_medium_text( CONV #( TEXT-c02 ) ).
*          ls_column_ref-r_column->set_short_text(  CONV #( TEXT-c02 ) ).
*          ls_column_ref-r_column->set_long_text(   CONV #( TEXT-c02 ) ).
        WHEN 'MANDT' OR 'CLIENT' OR 'SELECTED'. " OR 'EBELP'. " OR 'WRBTR_BSIK' OR 'WRBTR_BSAK'.
          lo_column ?= ls_column_ref-r_column.    "Narrow casting
          IF sy-subrc = 0 AND lo_column IS BOUND.
            lo_column->set_visible( abap_false ).
            lo_column->set_technical( abap_true ).
          ENDIF.

        WHEN OTHERS.
          IF ls_column_ref-columnname CP '*GUID_*'.
            ls_column_ref-r_column->set_technical( abap_true ).
          ENDIF.

      ENDCASE.

      IF ls_column_ref-columnname(9) = 'ZFI_CHECK'.
        CLEAR lv_str.
        lv_str = ls_column_ref-columnname.
        CONDENSE lv_str.
        SHIFT lv_str RIGHT CIRCULAR BY 2 PLACES.
        lv_str = lv_str(2).

        IF lv_str IS NOT INITIAL.

          ls_column_ref-r_column->set_medium_text( CONV #( lv_str ) ).
          ls_column_ref-r_column->set_short_text(  CONV #( lv_str ) ).
          ls_column_ref-r_column->set_long_text(   CONV #( lv_str ) ).

        ENDIF.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CHILD_ALV1->ALV_INIT_HEAD
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CO_SALV                        TYPE REF TO CL_SALV_TABLE(optional)
* | [<-->] CO_PARENT                      TYPE REF TO CL_GUI_CONTAINER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alv_init_head.


    IF co_salv IS NOT INITIAL.
      co_salv->refresh( ).

    ELSEIF mo_parent_cont IS NOT INITIAL.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
             r_container = co_parent
            IMPORTING
              r_salv_table = co_salv
            CHANGING
              t_table      = mt_alv_head ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
      ENDTRY.

      alv_fieldcat_init( co_salv->get_columns( ) ).  " Fieldcatalog
      alv_toolbar_init( co_salv->get_functions( ) ). " Functions

      DATA(lo_selections) = co_salv->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*  go_salv->set_screen_status( pfstatus = 'SALV_STANDARD' report = sy-repid set_functions = go_salv->c_functions_all ).
*  DATA(lo_events) = go_salv->get_event( ).
*  CREATE OBJECT go_hevents.
*  SET HANDLER go_hevents->handle_ucomm FOR lo_events.

      DATA(lo_layout) = co_salv->get_layout( ).
      lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid handle = 'HEAD' ) ).

*    IF p_lay1 IS NOT INITIAL.
*      lo_layout->set_initial_layout( p_lay1 ).
*    ELSE.
      lo_layout->set_default( abap_true ).
*    ENDIF.
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

*    DATA: lo_event_handler TYPE REF TO zcl_ps_milestone_billplan.
      DATA(lo_events) = co_salv->get_event( ).
*    CREATE OBJECT lo_event_handler.
      SET HANDLER me->on_user_command FOR lo_events.
      SET HANDLER me->on_double_click FOR lo_events.


      " display the table
      co_salv->display( ).

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CHILD_ALV1->ALV_INIT_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CO_SALV                        TYPE REF TO CL_SALV_TABLE(optional)
* | [<-->] CO_PARENT                      TYPE REF TO CL_GUI_CONTAINER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ALV_INIT_ITEM.


    IF co_salv IS NOT INITIAL.
      co_salv->refresh( ).

    ELSEIF mo_parent_cont IS NOT INITIAL.

      TRY.
          cl_salv_table=>factory(
            EXPORTING
             r_container = co_parent
            IMPORTING
              r_salv_table = co_salv
            CHANGING
              t_table      = mt_alv_item ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
      ENDTRY.

      alv_fieldcat_init( co_salv->get_columns( ) ).  " Fieldcatalog
      alv_toolbar_init( co_salv->get_functions( ) ). " Functions

      DATA(lo_selections) = co_salv->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*  go_salv->set_screen_status( pfstatus = 'SALV_STANDARD' report = sy-repid set_functions = go_salv->c_functions_all ).
*  DATA(lo_events) = go_salv->get_event( ).
*  CREATE OBJECT go_hevents.
*  SET HANDLER go_hevents->handle_ucomm FOR lo_events.

      DATA(lo_layout) = co_salv->get_layout( ).
      lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid handle = 'ITEM' ) ).

*    IF p_lay1 IS NOT INITIAL.
*      lo_layout->set_initial_layout( p_lay1 ).
*    ELSE.
      lo_layout->set_default( abap_true ).
*    ENDIF.
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

*    DATA: lo_event_handler TYPE REF TO zcl_ps_milestone_billplan.
      DATA(lo_events) = co_salv->get_event( ).
*    CREATE OBJECT lo_event_handler.
      SET HANDLER me->on_user_command FOR lo_events.
      SET HANDLER me->on_double_click FOR lo_events.


      " display the table
      co_salv->display( ).

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CHILD_ALV1=>ALV_TOOLBAR_INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TB_FUNCT                    TYPE REF TO CL_SALV_FUNCTIONS_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ALV_TOOLBAR_INIT.

    io_tb_funct->set_all( cl_salv_functions_list=>true ).

    TRY.

      io_tb_funct->add_function( name = 'BTN_REFRESH' icon = '@42@' position = c_tb_pos-left
                                 tooltip = CONV #( 'Refresh data'(t07) ) ).


              catch cx_salv_existing into data(lx_existing).
      CATCH cx_salv_wrong_call INTO DATA(lx_wrong_call).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CHILD_ALV1->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_PARENT_CONT                 TYPE REF TO CL_GUI_CONTAINER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.

    super->constructor( io_parent_cont ).

    mo_parent_cont = io_parent_cont.

*    mo_parent_cont = io_parent_cont.
*    mo_log = zcl_md_log=>init_popup_log( ).

*    get_user_settings( IMPORTING ev_invoice_clerk = ms_params-rights-invoice_clerk ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CHILD_ALV1->ON_DOUBLE_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            LIKE
* | [--->] COLUMN                         LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ON_DOUBLE_CLICK.

    DATA(ls_cell) = me->mo_salv1->get_selections( )->get_current_cell( ). " TYPE SALV_S_CELL - row, columnname, value
    READ TABLE mt_alv_head ASSIGNING FIELD-SYMBOL(<ls_head_cur>) INDEX ls_cell-row.
    CHECK <ls_head_cur> IS ASSIGNED.

*    IF ls_cell-value IS INITIAL AND
*       column <> 'MWSKZ' AND
*       column <> 'XBLNR' .
*
*      alv2_display_items( CHANGING cs_head = <ls_head_cur> ).
*    ELSE.

      CASE column.
*        WHEN 'PARTNER'.
*          SET PARAMETER ID 'BPA' FIELD <ls_head_cur>-partner.
*          CALL TRANSACTION 'BP' AND SKIP FIRST SCREEN.
*          billplan_pick_tax_code( CHANGING cs_head = <ls_head_cur> ).
*          alv_display( ).
*
*        WHEN 'XBLNR'. " Pick XBLNR.
*          billplan_pick_xblnr( CHANGING cs_head = <ls_head_cur> ).
*          alv_display( ).

*        WHEN 'BELNR' OR 'GJAHR'.
*          SET PARAMETER ID 'BLN' FIELD <ls_head_cur>-belnr.
*          SET PARAMETER ID 'BUK' FIELD <ls_head_cur>-bukrs.
*          SET PARAMETER ID 'GJR' FIELD <ls_head_cur>-gjahr.
*          CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
*
*        WHEN 'AUGBL' OR 'AUGDT' OR 'AUGJA'.
*          SET PARAMETER ID 'BLN' FIELD <ls_head_cur>-augbl.
*          SET PARAMETER ID 'BUK' FIELD <ls_head_cur>-bukrs.
*          SET PARAMETER ID 'GJR' FIELD <ls_head_cur>-augja.
*          CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
*
*        WHEN 'FPLNR'.
*          SET PARAMETER ID 'BES' FIELD <ls_head_cur>-fplnr.
*          CALL TRANSACTION 'ME23N' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
*
        WHEN OTHERS.
*          alv2_display_items( CHANGING cs_head = <ls_head_cur> ).

      ENDCASE.
*    ENDIF.

*    on_db_update( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CHILD_ALV1->ON_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_SALV_FUNCTION                LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ON_USER_COMMAND.

*    selected_rows_update( me->mo_salv1->get_selections( )->get_selected_rows( ) ).

    CASE e_salv_function.

      WHEN 'ALV1_BP_SEND'.
*        me->outbound_ale( ). " redifined to use selected rows

      WHEN 'BTN_REFRESH'.
        refresh_data( ).

    ENDCASE.

*    selected_rows_update( ).

    alv_display( ).
    mo_log->show_and_clear( iv_save_handle = abap_true ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CHILD_ALV1->SELECTED_ROWS_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SELECTED_INDEXES            TYPE        SALV_T_ROW(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SELECTED_ROWS_UPDATE.

    LOOP AT mt_alv_head[] ASSIGNING FIELD-SYMBOL(<ls_alv>).
      READ TABLE it_selected_indexes INTO DATA(ls_row) WITH KEY table_line = sy-tabix.
      IF sy-subrc = 0.
        <ls_alv>-selected = abap_true.
      ELSE.
        <ls_alv>-selected = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CHILD_ALV1->USER_COMMAND_LINE_PICK
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method USER_COMMAND_LINE_PICK.

*    DATA: lt_bupa_inst TYPE bus_ei_instance_t.
*
*    DATA(lt_idx) = me->mo_salv1->get_selections( )->get_selected_rows( ).
*    LOOP AT lt_idx INTO DATA(lv_idx).
*
*      READ TABLE mt_alv_head INTO DATA(ls_alv) INDEX lv_idx.
*      IF sy-subrc = 0.
*        APPEND INITIAL LINE TO lt_bupa_inst ASSIGNING FIELD-SYMBOL(<ls_bupa_inst>).
*        <ls_bupa_inst>-bpartner     = ls_alv-partner.
*        <ls_bupa_inst>-bpartnerguid = ls_alv-partner_guid.
*      ENDIF.
*
*    ENDLOOP.
*
*    IF lt_bupa_inst[] IS NOT INITIAL.
*      super->outbound_ale( it_bupa_inst = lt_bupa_inst ).
*    ELSE.
*      MESSAGE s398(00) WITH 'Please pick some lines' DISPLAY LIKE 'W'.
*    ENDIF.

  endmethod.
ENDCLASS.
