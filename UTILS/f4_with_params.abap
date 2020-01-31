  class-methods F4_WITH_PARAMS
    importing
      !I_DISPLAY_ONLY type ABAP_BOOL default ABAP_FALSE
      !I_MAX_RECORDS type I default SPACE
      !I_SHLP_NAME type SHLPNAME
      !IT_DDSHIFACES type DDSHIFACES
    returning
      value(RT_VALUES) type TFW_DDSHRETVAL_TAB .
  class-methods F4_WITH_PARAMS_SIMPLE
    importing
      !I_SHLP_NAME type SHLPNAME
      !I_SHFIELD1 type DDSHIFACE-SHLPFIELD
      !I_SHISVAL1 type DDSHIFACE-VALFIELD default 'X'
      !I_SHVALUE1 type CLIKE optional
      !I_SHFIELD2 type DDSHIFACE-SHLPFIELD optional
      !I_SHISVAL2 type DDSHIFACE-VALFIELD default 'X'
      !I_SHVALUE2 type CLIKE optional
      !I_SHFIELD3 type DDSHIFACE-SHLPFIELD optional
      !I_SHISVAL3 type DDSHIFACE-VALFIELD default 'X'
      !I_SHVALUE3 type CLIKE optional
      !IV_RET_FIELD type DDSHRETVAL-RETFIELD optional
    returning
      value(RV_VALUE) type DDSHRETVAL-FIELDVAL .

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /SIE/SSA_SD_UTILS=>F4_WITH_PARAMS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DISPLAY_ONLY                 TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] I_MAX_RECORDS                  TYPE        I (default =SPACE)
* | [--->] I_SHLP_NAME                    TYPE        SHLPNAME
* | [--->] IT_DDSHIFACES                  TYPE        DDSHIFACES
* | [<-()] RT_VALUES                      TYPE        TFW_DDSHRETVAL_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD f4_with_params.
    DATA: lt_shlp TYPE shlp_descr.
    DATA: lv_rc TYPE sy-subrc.

    FIELD-SYMBOLS: <fs_im_face> TYPE ddshiface,
                   <fs_face>    TYPE ddshiface.

    CLEAR rt_values[].

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = i_shlp_name
        shlptype = 'SH'
      IMPORTING
        shlp     = lt_shlp.

    LOOP AT it_ddshifaces ASSIGNING <fs_im_face>.
      READ TABLE lt_shlp-interface[] WITH KEY shlpfield = <fs_im_face>-shlpfield ASSIGNING <fs_face>.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_im_face> TO <fs_face>.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = lt_shlp
        disponly      = i_display_only
        maxrecords    = i_max_records
        multisel      = space
*       CUCOL         = SY-CUCOL
*       CUROW         = SY-CUROW
      IMPORTING
        rc            = lv_rc
      TABLES
        return_values = rt_values.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /SIE/SSA_SD_UTILS=>F4_WITH_PARAMS_SIMPLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SHLP_NAME                    TYPE        SHLPNAME
* | [--->] I_SHFIELD1                     TYPE        DDSHIFACE-SHLPFIELD
* | [--->] I_SHISVAL1                     TYPE        DDSHIFACE-VALFIELD (default ='X')
* | [--->] I_SHVALUE1                     TYPE        CLIKE(optional)
* | [--->] I_SHFIELD2                     TYPE        DDSHIFACE-SHLPFIELD(optional)
* | [--->] I_SHISVAL2                     TYPE        DDSHIFACE-VALFIELD (default ='X')
* | [--->] I_SHVALUE2                     TYPE        CLIKE(optional)
* | [--->] I_SHFIELD3                     TYPE        DDSHIFACE-SHLPFIELD(optional)
* | [--->] I_SHISVAL3                     TYPE        DDSHIFACE-VALFIELD (default ='X')
* | [--->] I_SHVALUE3                     TYPE        CLIKE(optional)
* | [--->] IV_RET_FIELD                   TYPE        DDSHRETVAL-RETFIELD(optional)
* | [<-()] RV_VALUE                       TYPE        DDSHRETVAL-FIELDVAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD F4_WITH_PARAMS_SIMPLE.
    DATA: l_ret_field TYPE DDSHRETVAL-RETFIELD.


    DATA lt_interfaces TYPE ddshifaces.
    DATA ls_interface TYPE ddshiface.
    DATA lt_values TYPE TABLE OF ddshretval.

    FIELD-SYMBOLS: <val> TYPE ddshretval.

    CLEAR lt_interfaces.
    CLEAR ls_interface.
    ls_interface-shlpfield = i_shfield1.
    ls_interface-valfield  = i_shisval1.
    ls_interface-value     = i_shvalue1.
    APPEND ls_interface TO lt_interfaces.

    IF i_shfield2 IS NOT INITIAL.
      CLEAR ls_interface.
      ls_interface-shlpfield = i_shfield2.
      ls_interface-valfield  = i_shisval2.
      ls_interface-value     = i_shvalue2.
      APPEND ls_interface TO lt_interfaces.
    ENDIF.

    IF i_shfield3 IS NOT INITIAL.
      CLEAR ls_interface.
      ls_interface-shlpfield = i_shfield3.
      ls_interface-valfield  = i_shisval3.
      ls_interface-value     = i_shvalue3.
      APPEND ls_interface TO lt_interfaces.
    ENDIF.

    lt_values[] = f4_with_params( i_shlp_name = i_shlp_name it_ddshifaces = lt_interfaces[] ).

    IF lt_values[] IS NOT INITIAL.

      IF iv_ret_field IS NOT INITIAL.
        l_ret_field = iv_ret_field.
      ELSE.
        l_ret_field = i_shfield1.
      ENDIF.

      READ TABLE lt_values WITH KEY fieldname = l_ret_field ASSIGNING <val>.
      IF sy-subrc EQ 0.
        MOVE <val>-fieldval TO rv_value.
      ENDIF.

    ENDIF.

  ENDMETHOD.
