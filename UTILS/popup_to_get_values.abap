FORM fhm_change CHANGING cs_alv TYPE ty_alv.

  DATA: lt_fields             TYPE TABLE OF sval,
        lv_returncode(1)      TYPE c,
        lv_popup_title(30)    TYPE c,
        ls_alv_save           TYPE ty_alv,
        lv_ok_pushbuttontext  TYPE char20, 
        lv_icon_ok_push       TYPE iconname,
        lv_quickinfo_ok_push  TYPE gui_text,
        lv_first_pushbutton   TYPE char20,
        lv_icon_button_1      TYPE iconname,
        lv_quickinfo_button_1 TYPE gui_text,
        lv_rc                 TYPE sysubrc.

  IF cs_alv-fhmar <> 'S'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_E_FHSON'
    EXPORTING
      sfhnr          = cs_alv-sfhnr
      objty          = cs_alv-objty
      objid          = cs_alv-objid
    EXCEPTIONS
      foreign_lock   = 01
      system_failure = 02.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE i195(cf).
      RETURN.
    WHEN 2.
      MESSAGE i899(cf).
      RETURN.
  ENDCASE.

  ls_alv_save = cs_alv.

  lv_popup_title       = 'title'(003).
  lv_icon_ok_push       = icon_check. 
  lv_quickinfo_ok_push  = 'Ok'. 

  lv_icon_button_1      = icon_system_save.
  lv_quickinfo_button_1 = 'Save'(004).

  APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<lfs_fields>).
  <lfs_fields>-tabname = GC_STRUNAME.
  <lfs_fields>-fieldname = 'F_USER'.
  <lfs_fields>-value = cs_alv-f_user.

  APPEND INITIAL LINE TO lt_fields ASSIGNING <lfs_fields>.
  <lfs_fields>-tabname = GC_STRUNAME.
  <lfs_fields>-fieldname = 'F_FERT'.
  <lfs_fields>-value = cs_alv-f_fert.
  <lfs_fields>-novaluehlp = 'S'.

  CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
    EXPORTING
*     F1_FORMNAME        = ' '
*     F1_PROGRAMNAME     = ' '
      f4_formname        = 'FHM_CHANGE_MM_F4_HELP'
      f4_programname     = sy-repid
      formname           = 'FHM_CHANGE_MM_HANDLE_CODE'
      programname        = sy-repid
      popup_title        = lv_popup_title
      ok_pushbuttontext  = lv_ok_pushbuttontext
      icon_ok_push       = lv_icon_ok_push
      quickinfo_ok_push  = lv_quickinfo_ok_push
      first_pushbutton   = lv_first_pushbutton
      icon_button_1      = lv_icon_button_1
      quickinfo_button_1 = lv_quickinfo_button_1
*     SECOND_PUSHBUTTON  = ' '
*     ICON_BUTTON_2      =
*     QUICKINFO_BUTTON_2 = ' '
*     START_COLUMN       = '5'
*     START_ROW          = '5'
*     NO_CHECK_FOR_FIXED_VALUES       = ' '
    IMPORTING
      returncode         = lv_returncode
    TABLES
      fields             = lt_fields
    EXCEPTIONS
      error_in_fields    = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF lv_returncode = 'A'.
    " do nothing
  ELSE.
    READ TABLE lt_fields ASSIGNING <lfs_fields>
      WITH KEY tabname = GC_STRUNAME
               fieldname = 'F_USER'.
    IF sy-subrc = 0.
      cs_alv-fhm_user = <lfs_fields>-value.
    ENDIF.
    READ TABLE lt_fields ASSIGNING <lfs_fields>
      WITH KEY tabname = GC_STRUNAME
               fieldname = 'F_FERT'.
    IF sy-subrc = 0.
      cs_alv-f_fert = <lfs_fields>-value.
    ENDIF.
    IF ls_alv_save <> cs_alv.
      CLEAR lv_rc.
      PERFORM fhm_change_save USING cs_alv CHANGING lv_rc.
      IF lv_rc <> 0.
        cs_alv = ls_alv_save.
      ENDIF.
    ENDIF.
  ENDIF.
  
  CALL FUNCTION 'DEQUEUE_E_FHSON'
    EXPORTING
      sfhnr = cs_alv-sfhnr
      objty = cs_alv-objty
      objid = cs_alv-objid.


ENDFORM.
