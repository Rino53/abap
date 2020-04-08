* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /SIE/SSA_SD_ENH_VL06=>GET_PERNR_NAME_BY_VBELN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VBELN                       TYPE        VBAK-VBELN
* | [--->] IV_POSNR                       TYPE        VBAP-POSNR(optional)
* | [--->] IV_PARVW                       TYPE        VBPA-PARVW (default ='ZM')
* | [<---] EV_NAME                        TYPE        CLIKE
* | [<---] EV_PERNR                       TYPE        VBPA-PERNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_PERNR_NAME_BY_VBELN.

    DATA: ls_vbpa  TYPE vbpa,
          ls_vbadr TYPE vbadr,
          lv_posnr TYPE vbap-posnr.

    IF iv_posnr IS INITIAL.
      lv_posnr = c_posnr_initial.
    ELSE.
      lv_posnr = iv_posnr.
    ENDIF.

    CALL FUNCTION 'SD_VBPA_SINGLE_READ'
      EXPORTING
        i_vbeln          = iv_vbeln
        i_posnr          = lv_posnr
        i_parvw          = iv_parvw
      IMPORTING
        e_vbpa           = ls_vbpa
      EXCEPTIONS
        record_not_found = 1
        OTHERS           = 2.
    IF sy-subrc = 0 AND ls_vbpa-pernr IS NOT INITIAL.
      ev_pernr = ls_vbpa-pernr.
      CALL FUNCTION 'VIEW_VBADR'
        EXPORTING
          input   = ls_vbpa
        IMPORTING
          adresse = ls_vbadr
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
      IF sy-subrc = 0.
        WRITE ls_vbadr-name_list TO ev_name.
      ENDIF.
    ENDIF.



  ENDMETHOD.
