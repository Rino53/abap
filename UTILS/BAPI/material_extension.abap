methods EXTEND_MATERIAL_VALTYPE
    importing
      !IV_MATNR type MARA-MATNR
      !IV_PLANT type MARC-WERKS
      !IV_BWTAR type MBEW-BWTAR
      !IX_ZERO_PRICE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_OK) type SYST-SUBRC . 


METHOD extend_material_valtype.
  " Checks that material exists for BWTAR. If itdoesn't - created it.
  DATA: e_mara TYPE bapi_mara_ga,
        e_mbew TYPE bapi_mbew_ga.

  DATA: t_makt TYPE STANDARD TABLE OF bapi_makt_ga,
        t_marm TYPE STANDARD TABLE OF bapi_marm_ga,
        t_mean TYPE STANDARD TABLE OF bapi_mean_ga,
        t_mltx TYPE STANDARD TABLE OF bapi_mltx_ga,
        t_mlan TYPE STANDARD TABLE OF bapi_mlan_ga,
        t_ext TYPE STANDARD TABLE OF  bapiparex,
        t_return TYPE STANDARD TABLE OF  bapireturn.

  DATA: i_headdata TYPE bapimathead,
        lt_bapi_matreturn2 TYPE STANDARD TABLE OF bapi_matreturn2,
        ls_bapiret2 TYPE bapiret2,
        i_marc     TYPE bapi_marc,
        i_marcx    TYPE bapi_marcx,
        i_mbew     TYPE bapi_mbew,
        i_mbewx    TYPE bapi_mbewx.

  DATA:
        ls_mbew TYPE mbew.

  rv_ok = 0.

  CHECK: iv_bwtar IS NOT INITIAL,
         iv_matnr IS NOT INITIAL,
         iv_plant IS NOT INITIAL.

  SELECT SINGLE * FROM mbew
    INTO ls_mbew
    WHERE  matnr = iv_matnr
      AND  bwkey = iv_plant
      AND  bwtar = iv_bwtar.

  IF sy-subrc = 0.
    " Message - Val Type already exist.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_MATERIAL_GET_ALL'
    EXPORTING
      material      = iv_matnr
      val_area      = iv_plant
      plant         = iv_plant
    IMPORTING
      clientdata    = e_mara
      valuationdata = e_mbew
    TABLES
      return        = t_return.
  IF sy-subrc <> 0.
    log_current->add_message( iv_textmsg1 = 'Material not found' iv_textmsg2 = iv_matnr iv_textmsg3 = iv_plant iv_textmsgty = 'E' ).
    rv_ok = 1.
    RETURN. " ERROR
  ENDIF.

  i_headdata-material     = e_mara-material.
  i_headdata-account_view = true.

  i_marc-plant  = iv_plant.
  i_marcx-plant = iv_plant.

  MOVE-CORRESPONDING e_mbew TO i_mbew.

  i_mbew-val_type   = iv_bwtar.
  i_mbew-val_area   = iv_plant.
  i_mbewx-val_type  = iv_bwtar.
  i_mbewx-val_area  = iv_plant.

  i_mbewx-val_class  = true.
  i_mbewx-val_cat    = true.
  i_mbewx-price_ctrl = true.
  i_mbewx-price_unit = true.
  IF ix_zero_price = false.
    i_mbewx-moving_pr  = true.
    i_mbewx-mov_pr_pp  = true.
    i_mbewx-mov_pr_py  = true.
  ENDIF.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata       = i_headdata
      plantdata      = i_marc
      plantdatax     = i_marcx
      valuationdata  = i_mbew
      valuationdatax = i_mbewx
    IMPORTING
      return         = ls_bapiret2
    TABLES
      returnmessages = lt_bapi_matreturn2.

  log_current->add_message( is_bapiret = ls_bapiret2 ).

  IF log_current->has_errors( ) = true.
    log_current->add_message( iv_textmsg1 = 'Unable to create valuated material for'
                              iv_textmsg2 = iv_matnr
                              iv_textmsg3 = iv_plant
                              iv_textmsg4 = iv_bwtar
                              iv_textmsgty = 'E' ).
    rv_ok = 2.
  ELSE.
    log_current->add_message( iv_textmsg1 = 'Material extended to new valuation type'
                              iv_textmsg2 = iv_matnr
                              iv_textmsg3 = iv_plant
                              iv_textmsg4 = iv_bwtar ).
  ENDIF.

ENDMETHOD.
