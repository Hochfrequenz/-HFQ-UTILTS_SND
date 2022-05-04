interface /HFQ/IF_BADI_UTILTS_TRIG
  public .


  interfaces IF_BADI_INTERFACE .

  constants CO_UTILTS_MSS_PROC_ID type /IDXGC/DE_PROC_ID value '/HFQ/UTILTS_MSS' ##NO_TEXT.
  constants CO_UTILTS_SND_PROC_ID type /IDXGC/DE_PROC_ID value '/HFQ/UTILTS_SND' ##NO_TEXT.

  methods FILL_NEW_PROCESS_DATA
    importing
      !IV_INT_UI type INT_UI
      !IV_PROC_DATE type /IDXGC/DE_PROC_DATE
      !IV_UTILTS_PROC_ID type /IDXGC/DE_PROC_ID
      !IS_PROCESS_STEP_DATA_OLD type /IDXGC/S_PROC_STEP_DATA_ALL
    changing
      !CS_PROC_DATA_NEW type /IDXGC/S_PROC_DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  methods VALIDATE_UTILTS_PROCESS
    importing
      !IS_HFQ_UTILTS_TRIG type /HFQ/UTILTS_TRIG
      !IS_PROCESS_STEP_DATA_ALL type /IDXGC/S_PROC_STEP_DATA_ALL
    changing
      !CV_IS_INVALID type KENNZX .
endinterface.
