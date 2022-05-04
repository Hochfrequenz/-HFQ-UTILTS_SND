*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/HFQ/UTILTS_TRIG
*   generation date: 16.12.2020 at 16:23:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/HFQ/UTILTS_TRIG   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
