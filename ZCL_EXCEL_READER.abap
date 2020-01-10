class ZCLSRM_EXCEL_READER definition
  public
  final
  create public .

public section.
*"* public components of class ZCLSRM_EXCEL_READER
*"* do not include other source files here!!!

  types:
    BEGIN OF ts_zssrm_excel_tab,
        row    TYPE int4,
        column TYPE int4,
        value  TYPE string.
    TYPES: END OF ts_zssrm_excel_tab .
  types:
    tt_zssrm_excel_tab TYPE STANDARD TABLE OF ts_zssrm_excel_tab .
  types:
    BEGIN OF ts_zssrm_excel_sheet,
        name    TYPE string,
        excel_tab TYPE ZISRM_EXCEL_TAB. " <= This is the same type as tt_zssrm_excel_tab
    TYPES: END OF ts_zssrm_excel_sheet .
  types:
    tt_zssrm_excel_sheet TYPE STANDARD TABLE OF ts_zssrm_excel_sheet .

  constants GC_X000D type STRING value '_x000D_'. "#EC NOTEXT

  class-methods READ_FILE_XLSX
    importing
      !IV_FNAME type CLIKE
      !ITD_SHEET_NAME type /BOBF/T_BUF_STRING_RANGE optional
      !ITD_SHEET_NUMBER type /SDF/IVIS_DC_SXMSPMSTAT_TAB optional
      !IV_DEL_EPMTY type FLAG default ''
      !IV_CONTENT type XSTRING optional
    exporting
      !ETD_EXCEL_SHEET type TT_ZSSRM_EXCEL_SHEET .
protected section.
private section.
*"* private components of class ZCLSRM_EXCEL_READER
*"* do not include other source files here!!!

  class-methods _CONVERT_SHEET_DATA
    importing
      !IV_SHEET_NAME type STRING
      !ID_SHEET_DATA type ref to DATA
    returning
      value(RS_EXCEL_SHEET) type TS_ZSSRM_EXCEL_SHEET .
ENDCLASS.



CLASS ZCLSRM_EXCEL_READER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCLSRM_EXCEL_READER=>READ_FILE_XLSX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FNAME                       TYPE        CLIKE
* | [--->] ITD_SHEET_NAME                 TYPE        /BOBF/T_BUF_STRING_RANGE(optional)
* | [--->] ITD_SHEET_NUMBER               TYPE        /SDF/IVIS_DC_SXMSPMSTAT_TAB(optional)
* | [--->] IV_DEL_EPMTY                   TYPE        FLAG (default ='')
* | [--->] IV_CONTENT                     TYPE        XSTRING(optional)
* | [<---] ETD_EXCEL_SHEET                TYPE        TT_ZSSRM_EXCEL_SHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_file_xlsx.
    DATA: lv_file_len TYPE i.
    DATA: ltd_binary  TYPE TABLE OF sdokcntbin.
    DATA: lv_xstring TYPE xstring.
    DATA: lv_message TYPE string.
    DATA: lo_excel  TYPE REF TO cl_fdt_xl_spreadsheet.
    DATA: lx_excel_core TYPE REF TO cx_fdt_excel_core.
    DATA: ld_sheet_data  TYPE REF TO data.
    DATA: ls_excel_sheet TYPE Ts_ZSSRM_EXCEL_SHEET.
    DATA: ltd_worksheets_number TYPE STANDARD TABLE OF string .
    DATA: ltd_worksheets TYPE IF_FDT_DOC_SPREADSHEET=>T_WORKSHEET_NAMES.
    DATA: l_fname TYPE string.
    FIELD-SYMBOLS: <lv_worksheets> LIKE LINE OF ltd_worksheets.

    l_fname = iv_fname.
    IF iv_content IS NOT INITIAL.
      lv_xstring = iv_content.
    ELSEIF l_fname IS NOT INITIAL.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename   = l_fname
          filetype   = 'BIN'
        IMPORTING
          filelength = lv_file_len
        CHANGING
          data_tab   = ltd_binary
        EXCEPTIONS
          OTHERS     = 20 ).
      IF sy-subrc IS NOT INITIAL.
*        MESSAGE e188(zsrm_xls) INTO zclgl_utils=>gv_dummy.
*        zclsrm_ex_helper=>raise_message_syst( ).
      ENDIF.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_file_len
        IMPORTING
          buffer       = lv_xstring
        TABLES
          binary_tab   = ltd_binary
        EXCEPTIONS
          OTHERS       = 2.
    ENDIF.
    TRY.

*     Create object of class to read .xlsx file contents
        CREATE OBJECT lo_excel
          EXPORTING
            document_name = l_fname
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
        worksheet_names = ltd_worksheets ).
    IF ltd_worksheets IS NOT INITIAL.

      IF itd_sheet_number[] IS NOT INITIAL.
        LOOP AT ltd_worksheets ASSIGNING <lv_worksheets>.
          READ TABLE itd_sheet_number TRANSPORTING NO FIELDS
            WITH KEY table_line = sy-tabix.
          IF sy-subrc IS INITIAL.
            APPEND <lv_worksheets> TO ltd_worksheets_number.
          ENDIF.
        ENDLOOP.
        ltd_worksheets[] = ltd_worksheets_number[].
      ENDIF.
*   Read active worksheet
      LOOP AT ltd_worksheets ASSIGNING <lv_worksheets>
        WHERE table_line IN itd_sheet_name.
        CLEAR ls_excel_sheet.
        ld_sheet_data = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( <lv_worksheets> ).
        IF ld_sheet_data IS NOT INITIAL.
          ls_excel_sheet = _convert_sheet_data( id_sheet_data = ld_sheet_data iv_sheet_name = <lv_worksheets> ).
          IF iv_del_epmty = abap_true.
            DELETE ls_excel_sheet-excel_tab[] WHERE value IS INITIAL.
          ENDIF.
        ELSE.
          ls_excel_sheet-name = <lv_worksheets>.
        ENDIF.
        APPEND ls_excel_sheet TO etd_excel_sheet[].
      ENDLOOP.
    ELSE.
*      MESSAGE e000(zsrm_prpl) WITH 'message!'  INTO zclsrm_ex_helper=>gv_dummy.
*      zclsrm_ex_helper=>raise_message_syst( ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCLSRM_EXCEL_READER=>_CONVERT_SHEET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SHEET_NAME                  TYPE        STRING
* | [--->] ID_SHEET_DATA                  TYPE REF TO DATA
* | [<-()] RS_EXCEL_SHEET                 TYPE        TS_ZSSRM_EXCEL_SHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD _convert_sheet_data.
    FIELD-SYMBOLS: <ltd_table> TYPE STANDARD TABLE.
    DATA: lo_rtti_table TYPE REF TO cl_abap_tabledescr.
    DATA: lo_rtti_struc TYPE REF TO cl_abap_structdescr.
    DATA: ltd_comp TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS: <la_field> TYPE any.
    FIELD-SYMBOLS: <la_table> TYPE any.
    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF ltd_comp.
    FIELD-SYMBOLS: <ls_row> LIKE LINE OF rs_excel_sheet-excel_tab.
    DATA: lv_row TYPE i VALUE 1.
    DATA: lv_column TYPE i.

    rs_excel_sheet-name = iv_sheet_name.

    ASSIGN id_sheet_data->* TO <ltd_table>.
    lo_rtti_table ?= cl_abap_tabledescr=>describe_by_data( <ltd_table> ).
    lo_rtti_struc ?= lo_rtti_table->get_table_line_type( ).
    ltd_comp = lo_rtti_struc->get_components( ).
    LOOP AT <ltd_table> ASSIGNING <la_table>.
      lv_column = 1.
      LOOP AT ltd_comp ASSIGNING <ls_comp>.
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <la_table> TO <la_field>.
        IF <la_field> IS ASSIGNED.
          APPEND INITIAL LINE TO rs_excel_sheet-excel_tab ASSIGNING <ls_row>.
          <ls_row>-row = lv_row.
          <ls_row>-column = lv_column.
          <ls_row>-value = <la_field>.
          REPLACE ALL OCCURRENCES OF gc_x000d IN <ls_row>-value WITH space.
          lv_column = lv_column + 1.
        ENDIF.
      ENDLOOP.
      lv_row = lv_row + 1.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
