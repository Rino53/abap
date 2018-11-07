DATA: lo_cor2 TYPE REF TO zcl_api_bdc.
        CREATE OBJECT lo_cor2 EXPORTING tcode = 'COR2'.
        lo_cor2->options-updmode = 'S'.
        lo_cor2->screen( 'SAPLCOKO|5110|=OMLA' ).
        lo_cor2->field( EXPORTING fnam = 'CAUFVD-AUFNR' fval = caufvd-aufnr ).
        lo_cor2->call( check_auth = yes ).



FORM po_change_charg USING iv_aufnr TYPE aufnr
                           iv_charg TYPE charg_d.
  " change orders batch (charg) with batch input

  IF iv_aufnr IS NOT INITIAL AND iv_charg IS NOT INITIAL.

    DATA: lo_cor2 TYPE REF TO zcl_api_bdc.
    CREATE OBJECT lo_cor2 EXPORTING tcode = 'COR2'.
    lo_cor2->options-updmode = 'S'.
    lo_cor2->options-nobinpt = no.
    lo_cor2->options-dismode = 'N'.
*    lo_cor2->options-updmode = 'A'.
    lo_cor2->screen( 'SAPLCOKO|5110|/00' ).
    lo_cor2->field( EXPORTING fnam = 'CAUFVD-AUFNR' fval = iv_aufnr ).
    lo_cor2->screen( 'SAPLCOKO|5115|=KOWE' ).
    lo_cor2->screen( 'SAPLCOKO|5115|/00' ).
    lo_cor2->field( EXPORTING fnam = 'AFPOD-CHARG' fval = iv_charg ).

    lo_cor2->screen( 'SAPLCOKO|5115|=OMLA' ).
    lo_cor2->screen( 'SAPLCOMK|5120|=BACK' ).
    lo_cor2->field( EXPORTING fnam = 'RESBD-CHARG(01)' fval = iv_charg ).
    lo_cor2->screen( 'SAPLCOMD|5100|=BU' ).

    lo_cor2->screen( 'SAPLCOKO|5115|=BU' ).

    go_sequence->dequeue_sequence( ). " before cor2
    lo_cor2->call( check_auth = yes ).
  ENDIF.
ENDFORM.
 
 
FORM inspect_bulk_200.

  CHECK caufvd-plnbez IS NOT INITIAL.

  DATA: lo_qe51n TYPE REF TO zcl_api_bdc.
  CREATE OBJECT lo_qe51n EXPORTING tcode = 'QE51N'.
  lo_qe51n->options-updmode = 'S'.
  lo_qe51n->options-nobinpt = yes.
  lo_qe51n->options-defsize = no.

  " Exclude work centers
  lo_qe51n->screen( 'SAPLQEES|0500|=%022' ).
  lo_qe51n->screen( 'SAPLALDB|3000|=NOSV' ).
  lo_qe51n->screen( 'SAPLALDB|3000|=ACPT' ).
  lo_qe51n->field( EXPORTING fnam = 'RSCSEL_255-SLOW_E(01)' fval = 'EXTERN' ).
  lo_qe51n->field( EXPORTING fnam = 'RSCSEL_255-SLOW_E(01)' fval = 'F&E' ).
  lo_qe51n->field( EXPORTING fnam = 'RSCSEL_255-SLOW_E(01)' fval = 'LABOR' ).

  " Pass other params
  lo_qe51n->screen( 'SAPLQEES|0500|=CRET' ).
  lo_qe51n->field( EXPORTING fnam = 'QL_WERKS-LOW' fval = caufvd-werks ).
  lo_qe51n->field( EXPORTING fnam = 'QL_MATNR-LOW' fval = caufvd-plnbez ).
  lo_qe51n->field( EXPORTING fnam = 'QL_CHARG-LOW' fval = gs_header200-charg ).
  lo_qe51n->field( EXPORTING fnam = 'QL_HERKT-LOW' fval = '03' ). " insp lot origin
  lo_qe51n->field( EXPORTING fnam = 'QL_HERKT-HIGH' fval = '04' ).
  lo_qe51n->field( EXPORTING fnam = 'QL_MAX_R' fval = '99999' ).

  lo_qe51n->call( check_auth = yes ).

ENDFORM.
