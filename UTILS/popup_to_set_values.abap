  METHOD pick_garantie.
    DATA:
      lt_fields  TYPE STANDARD TABLE OF sval,
      lv_ret     TYPE char1.

    lt_fields = VALUE #(
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_TYPE'    value = cs_head-gar_type )
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_HBKID'   value = cs_head-GAR_HBKID ) 
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_NUM'     value = cs_head-gar_num )
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_HAFTB'   value = cs_head-GAR_HAFTB ) 
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_STARTDT' value = cs_head-gar_startdt )
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_ENDDT'   value = cs_head-gar_enddt )
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_VALUE'   value = cs_head-gar_value )
      ( tabname = 'T001'      fieldname = 'WAERS'       value = 'EUR' field_attr = '04' )
      ( tabname = 'ZMM_GAR_S' fieldname = 'GAR_ACTIVE'  value = cs_head-gar_active ) ).

    DATA(lt_fields_old) = lt_fields[].

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = 'Change guarantee'
        start_column = '30'
      IMPORTING
        returncode   = lv_ret
      TABLES
        fields       = lt_fields
      EXCEPTIONS
        OTHERS       = 2.
    IF sy-subrc = 0 AND lv_ret IS INITIAL AND lt_fields_old[] <> lt_fields[].

      LOOP AT lt_fields INTO DATA(ls_field).
        CASE ls_field-fieldname.
          WHEN 'GAR_TYPE'.
            cs_head-gar_type = ls_field-value.
          WHEN 'GAR_NUM'.
            cs_head-gar_num = ls_field-value.
          WHEN 'GAR_STARTDT'.
            cs_head-gar_startdt = ls_field-value.
          WHEN 'GAR_ENDDT'.
            cs_head-gar_enddt = ls_field-value.
          WHEN 'GAR_VALUE'.
            cs_head-gar_value = ls_field-value.
          WHEN 'GAR_ACTIVE'.
            cs_head-gar_active = ls_field-value.

          WHEN 'GAR_HBKID'.
            cs_head-GAR_HBKID = ls_field-value.
          WHEN 'GAR_HAFTB'.
            cs_head-GAR_HAFTB = ls_field-value.
        ENDCASE.
      ENDLOOP.

    ENDIF.


  ENDMETHOD.
