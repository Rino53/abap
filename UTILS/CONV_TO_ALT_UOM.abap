class ZCL_BC_CONVERSION_TOOLS definition
  public
  final
  create public .

public section.

  class-methods CONV_TO_ALT_UOM
    importing
      !IV_ALT_UOM type MARM-MEINH
      !IV_CURR_UOM type MARM-MEINH
      !IV_CUR_QTY type P
      !IV_MATNR type MARA-MATNR
    exporting
      !EV_ALT_QTY type P
      !EV_UMREZ type UMREZ
      !EV_UMREN type UMREN
      !EV_UMREZ_CURR_TO_ALT type UMREZ
    exceptions
      ALT_UOM_NOT_FOUND
      CURR_UOM_NOT_FOUND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BC_CONVERSION_TOOLS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_CONVERSION_TOOLS=>CONV_TO_ALT_UOM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ALT_UOM                     TYPE        MARM-MEINH
* | [--->] IV_CURR_UOM                    TYPE        MARM-MEINH
* | [--->] IV_CUR_QTY                     TYPE        P
* | [--->] IV_MATNR                       TYPE        MARA-MATNR
* | [<---] EV_ALT_QTY                     TYPE        P
* | [<---] EV_UMREZ                       TYPE        UMREZ
* | [<---] EV_UMREN                       TYPE        UMREN
* | [<---] EV_UMREZ_CURR_TO_ALT           TYPE        UMREZ
* | [EXC!] ALT_UOM_NOT_FOUND
* | [EXC!] CURR_UOM_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_to_alt_uom.
    DATA : lv_c_cf    TYPE   decfloat34,
           lv_c_cf1   TYPE  marm,
           lv_c_cf2   TYPE  marm,
           lv_c_meins TYPE  mara.

    DATA : lv_base_cf    TYPE   p DECIMALS 5,
           lv_alt_cf     TYPE   p DECIMALS 5,
           lv_cur_qty    TYPE   p DECIMALS 5,
           ls_base_cf1   TYPE  marm,
           ls_curr_cf2   TYPE  marm,
           ls_alt_cf3    TYPE  marm,
           lv_base_meins TYPE mara-meins,
           lv_alt_uom    TYPE mara-meins,
           lv_curr_uom   TYPE mara-meins.

    CLEAR: lv_cur_qty,
            lv_base_meins,
            lv_c_meins.

*** sales unit of measure not equal to Alt unit of measure

    IF iv_alt_uom <> iv_curr_uom.

      lv_curr_uom = iv_curr_uom.
      lv_alt_uom  = iv_alt_uom.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = lv_alt_uom
        IMPORTING
          output         = lv_alt_uom
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = lv_curr_uom
        IMPORTING
          output         = lv_curr_uom
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*---  1ST GET THE BASE UOM
      SELECT SINGLE meins
        FROM mara
        INTO lv_base_meins
        WHERE matnr = iv_matnr.

*--- Conversion Factor for Base UOM
      CLEAR : ls_base_cf1.       " Conversion Factor for Base unit measure
      SELECT matnr,
             meinh,
                    umrez,     "Numerator for Conversion to Base UnitsMeasure
                    umren      "Denominator for conversion to base unitsmeasure
               FROM marm UP TO 1 ROWS
               INTO CORRESPONDING FIELDS OF @ls_base_cf1
              WHERE matnr = @iv_matnr
                AND meinh = @lv_base_meins.
      ENDSELECT.
*--- conversion Factor for Curr UOM
      CLEAR : ls_curr_cf2.       " Conversion Factor for Current unit measure
      SELECT matnr,
             meinh,
             umrez,     "Numerator for Conversion to Base UnitsMeasure
             umren     "Denominator for conversion to base unitsmeasure
               FROM marm UP TO 1 ROWS
               INTO CORRESPONDING FIELDS OF @ls_curr_cf2
              WHERE matnr = @iv_matnr
                AND meinh = @lv_curr_uom.
      ENDSELECT.
      IF sy-subrc NE 0.
        RAISE curr_uom_not_found.
      ENDIF.

*--- Conversion Factor for Alt UOM
      CLEAR : ls_alt_cf3.  " Conversion Factor for Alternate unit measure
      SELECT matnr,
             meinh,
             umrez,
             umren
               FROM marm UP TO 1 ROWS
               INTO CORRESPONDING FIELDS OF @ls_alt_cf3
              WHERE matnr = @iv_matnr
              AND   meinh = @lv_alt_uom.
      ENDSELECT.
      IF sy-subrc NE 0.
        RAISE alt_uom_not_found.
      ENDIF.

*** What is the Base Qty, convert from Alternate to Base.
*** Calculate the conversion factor from Current UOM to the Base UOM
      lv_base_cf =   (  ls_curr_cf2-umrez / ls_curr_cf2-umren )
                   * (  ls_base_cf1-umrez / ls_base_cf1-umren ).
*--- Qty value form Current UOM to Base UOM
      lv_cur_qty = iv_cur_qty * lv_base_cf.

*** What is the new UOM Qty, Convert from Base to new Alternate
*** Calculate the conversin factor from Base UOM to Alternate UOM.
      lv_c_cf =   ( ls_alt_cf3-umren  / ls_alt_cf3-umrez )
                * ( ls_base_cf1-umren / ls_base_cf1-umrez ).

*--- Qty value from Base UOM to Alt UOM
      lv_cur_qty = lv_cur_qty * lv_c_cf.

*--- Quotient zwischen Einheiten
      lv_alt_cf =   (  ls_alt_cf3-umrez / ls_alt_cf3-umren )
                   * (  ls_base_cf1-umrez / ls_base_cf1-umren ).
      IF lv_alt_cf > lv_base_cf.
        ev_umrez_curr_to_alt = lv_alt_cf / lv_base_cf.
      ELSE.
        ev_umrez_curr_to_alt = lv_base_cf / lv_alt_cf.
      ENDIF.

    ELSE.
      lv_cur_qty = iv_cur_qty.
    ENDIF.

    ev_alt_qty = lv_cur_qty.
    ev_umrez     = ls_alt_cf3-umrez.
    ev_umren     = ls_alt_cf3-umren.
  ENDMETHOD.
ENDCLASS.
