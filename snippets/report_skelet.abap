*&---------------------------------------------------------------------*
*& Report Z_REP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_REP

SELECTION-SCREEN BEGIN OF BLOCK b001 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_tst AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b001.

CLASS lcl_app DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS:
      run.
ENDCLASS.

INITIALIZATION.
  PERFORM init.

END-OF-SELECTION.
  PERFORM main.

FORM init.
ENDFORM.

FORM main.
  lcl_app=>run( ).
ENDFORM.

CLASS lcl_app IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
