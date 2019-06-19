FORM alv_show.


  IF go_salv IS BOUND.
    CLEAR go_salv.
  ENDIF.

  TRY .

      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_salv
        CHANGING
          t_table      = gt_uploaded.

    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

* set columns
  DATA(lo_columns) = go_salv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

  TRY.
      DATA: l_column TYPE REF TO cl_salv_column_table.
      l_column ?= lo_columns->get_column( 'ICON' ).
      l_column->set_icon( if_salv_c_bool_sap=>true ).
      l_column->set_long_text( 'Status'(T02) ).
    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

  DATA(lo_functions) = go_salv->get_functions( ).
  lo_functions->set_all( abap_true ).

  go_salv->set_screen_status(
    pfstatus      =  'SALV_STANDARD'
    report        =  sy-repid
    set_functions = go_salv->c_functions_all ).

*  DATA: lr_layout TYPE REF TO cl_salv_layout,
*        ls_key    TYPE salv_s_layout_key.

  DATA(lo_layout) = go_salv->get_layout( ).

  lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).

  IF p_lay1 IS NOT INITIAL.
    lo_layout->set_initial_layout( p_lay1 ).
  ELSE.
    lo_layout->set_default( abap_true ).
  ENDIF.

  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  DATA: lo_event_handler TYPE REF TO lcl_handle_events.
  DATA(lo_events) = go_salv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.
*  gr_selections = go_salv->get_selections( ).
*  gr_selections->set_selection_mode( 1 ).
  go_salv->display( ).



ENDFORM.

CLASS lcl_handle_events DEFINITION .
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'ACT_MODIFY'.
        PERFORM modify_orgf CHANGING gt_uploaded.
        go_salv->refresh( ).

      WHEN 'ACT_REFR'.
        PERFORM data_checks CHANGING gt_uploaded.
        go_salv->refresh( ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
