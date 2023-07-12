REPORT zsalv_quickview.

TABLES: but000, but0id.

CONSTANTS: gc_tcode TYPE syst-tcode VALUE 'ZBP_REP1'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
  s_partn FOR but000-partner,
  s_type  FOR but0id-type,
  s_idnum FOR but0id-idnumber,
  s_entry FOR but0id-entry_date,
  s_from  FOR but0id-valid_date_from,
  s_valto FOR but0id-valid_date_to.

SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_application DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      gc_payload_type_items TYPE string VALUE 'items'.

    TYPES: BEGIN OF t_workload,
             datatype    TYPE string,
             solix_len   TYPE i,
             solix_tab   TYPE solix_tab,
             xstring     TYPE xstring,
             filename    TYPE string,

             alv_obj     TYPE REF TO cl_salv_table,
             alv_tab_ref TYPE REF TO data,

           END OF t_workload,
           tt_workload TYPE STANDARD TABLE OF t_workload.

    DATA: mt_workload TYPE tt_workload.

    METHODS:
      run,
      show_alv_trades CHANGING ct_data TYPE table,
      constructor IMPORTING iv_repid TYPE sy-repid,
      on_double_click_trades FOR EVENT double_click OF cl_salv_events_table
                             IMPORTING row column,
      set_init_dates CHANGING cv_from TYPE sy-datum
                              cv_to   TYPE sy-datum.
ENDCLASS.



INITIALIZATION.
  DATA(lo_app) = NEW lcl_application( sy-repid ).
*  lo_app->set_init_dates( CHANGING cv_from = p_from cv_to = p_to ).
  s_valto-low  = '20000101'.
  s_valto-high = sy-datum.
  APPEND s_valto.

START-OF-SELECTION.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD gc_tcode.
***  IF sy-subrc <> 0 AND
***     sy-uname <> 'USER1' AND
***     sy-uname <> 'USER2' AND
***    MESSAGE i149(00) WITH gc_tcode DISPLAY LIKE 'E'.
***    EXIT.
***  ENDIF.

END-OF-SELECTION.
  lo_app->run( ).

CLASS lcl_application IMPLEMENTATION.
  METHOD run.

    SELECT
        but000~partner,
        but0id~type,
        but0id~idnumber,
        but0id~entry_date,
        but0id~valid_date_from,
        but0id~valid_date_to,
        but000~bu_sort1,
        but000~bu_sort2
      FROM but000 JOIN but0id ON but000~partner = but0id~partner
      INTO TABLE @DATA(lt_partners)
      WHERE but000~xblck = ''
        AND but000~xdele = ''
        AND but000~partner  IN @s_partn
        AND but0id~type     IN @s_type
        AND but0id~idnumber IN @s_idnum
        AND but0id~entry_date      IN @s_entry
        AND but0id~valid_date_from IN @s_from
        AND but0id~valid_date_to   IN @s_valto.

    SORT lt_partners BY partner.

    IF lt_partners IS NOT INITIAL.
      show_alv_trades( CHANGING ct_data = lt_partners ).
    ELSE.
      MESSAGE s398(00) WITH 'No data found' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.


  METHOD show_alv_trades.

    IF ct_data[] IS NOT INITIAL.

      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = DATA(lo_alv)
      CHANGING
        t_table      = ct_data ).

      lo_alv->get_functions( )->set_default( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).

*      change_fcat_xlsx_output( CHANGING ct_fcat = lt_fcat ).

*       data: go_events type ref to lcl_handle_events.
*       data: lr_events type ref to cl_salv_events_table.
      DATA(lo_events) = lo_alv->get_event( ).
*      CREATE OBJECT gr_events.
      SET HANDLER me->on_double_click_trades FOR lo_events.

      APPEND INITIAL LINE TO mt_workload ASSIGNING FIELD-SYMBOL(<ls_workload>).
      <ls_workload>-alv_tab_ref = REF #( ct_data[] ).
      <ls_workload>-alv_obj = lo_alv.
      <ls_workload>-datatype = gc_payload_type_items.

      lo_alv->display( ).

    ENDIF.

  ENDMETHOD.
  METHOD on_double_click_trades.

    READ TABLE mt_workload INTO DATA(ls_payload) WITH KEY datatype = gc_payload_type_items.
    CHECK: sy-subrc = 0 AND ls_payload-alv_tab_ref IS NOT INITIAL.

    FIELD-SYMBOLS: <ls_items> TYPE table. "tt_ex_trades.
    ASSIGN ls_payload-alv_tab_ref->* TO <ls_items>[].
    CHECK: sy-subrc = 0 AND <ls_items>[] IS NOT INITIAL.

    CHECK: row <= lines( <ls_items>[] ) AND row > 0.

    READ TABLE <ls_items>[] ASSIGNING FIELD-SYMBOL(<ls_line>) INDEX row.
    CHECK: sy-subrc = 0 AND <ls_line> IS NOT INITIAL.

    CASE column.
      WHEN OTHERS.
        ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_cell>).
        IF sy-subrc = 0.
          SET PARAMETER ID 'BPA' FIELD <lv_cell>.
          CALL TRANSACTION 'BP' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD set_init_dates.

    CALL FUNCTION 'HR_GB_DAY_RELATIVE_TO_DATE'
      IMPORTING
        new_date = cv_to.

    CALL FUNCTION 'HR_GB_DAY_RELATIVE_TO_DATE'
      EXPORTING
        date        = cv_to
        day_in_week = '1'
      IMPORTING
        new_date    = cv_from.

  ENDMETHOD.

  METHOD constructor.
*    super->constructor( ).
*    ms_app_settings-repid = iv_repid.
*    mo_log = zcl_md_log=>init_popup_log( iv_repid ).
*    set_const( ).
  ENDMETHOD.

ENDCLASS.