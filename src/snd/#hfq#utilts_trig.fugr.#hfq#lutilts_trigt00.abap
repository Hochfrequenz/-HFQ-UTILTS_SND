*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.12.2020 at 16:23:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /HFQ/UTILTS_TRIG................................*
DATA:  BEGIN OF STATUS_/HFQ/UTILTS_TRIG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/HFQ/UTILTS_TRIG              .
CONTROLS: TCTRL_/HFQ/UTILTS_TRIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */HFQ/UTILTS_TRIG              .
TABLES: /HFQ/UTILTS_TRIG               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
