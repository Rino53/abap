class-methods CHECK_LAUNCHED_PROGRAM
    importing
      !IV_LEVEL type BYTE default 2
      !IV_PROGNAME type DBGLPROG
      !IV_TYPE type DBGLEVTYPE default 'FORM'
      !IV_NAME type DBGLEVENT
    returning
      value(RV_FOUND) type BOOLEAN .     
      
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /SIE/SSA_SD_UTILS=>CHECK_LAUNCHED_PROGRAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LEVEL                       TYPE        BYTE (default =2)
* | [--->] IV_PROGNAME                    TYPE        DBGLPROG
* | [--->] IV_TYPE                        TYPE        DBGLEVTYPE (default ='FORM')
* | [--->] IV_NAME                        TYPE        DBGLEVENT
* | [<-()] RV_FOUND                       TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_launched_program.
    DATA: lt_sys_callst TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
*       callstack    = lt_abap_callstack
        et_callstack = lt_sys_callst.

    DATA(lv_level) = iv_level + 1.

    IF lines( lt_sys_callst[] ) >= lv_level.
      " If this calculation launched from xkomv_bewerten - skip calc
      READ TABLE lt_sys_callst[] INTO DATA(ls_callst) INDEX lv_level.
      IF ls_callst-progname  = iv_progname AND
         ls_callst-eventtype = iv_type AND
         ls_callst-eventname = iv_name.
        rv_found = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
