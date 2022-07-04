interface /HFQ/IF_BADI_UTILTS_RCV
  public .


  interfaces IF_BADI_INTERFACE .

  class-methods SAVE_CALC_FORM
    importing
      !IS_PROC_STEP_DATA_ALL type /IDXGC/S_PROC_STEP_DATA_ALL
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods SELECT_RELEVANT_DATA
    importing
      !IV_MALO type EXT_UI optional
      !IV_KEYDATE type DATUM default SY-DATUM
      !IV_KEYTIME type UZEIT default '000000'
    exporting
      !ET_CALC_FORM_H type /HFQ/T_CALC_FORM_H
      !ET_CALC_FORM type /HFQ/T_CALC_FORM
    exceptions
      NO_CONSTRUCT_FOUND
      NO_CALC_FORM_FOUND .
  methods SELECT_CF_DETAILS
    importing
      !IV_MALO type EXT_UI optional
      !IV_KEYDATE type DATUM default SY-DATUM
      !IV_KEYTIME type UZEIT default '000000'
    exporting
      !ET_POD_DATA type /IDXGL/T_POD_DATA_DETAILS
      !EV_DATETO type DATUM
      !EV_DATEFROM type DATUM
      !EV_TIMETO type UZEIT
      !EV_TIMEFROM type UZEIT
      !EV_CALC_FORM_STAT type /IDXGC/DE_CALC_FORM_STAT
      !EV_CALC_STEP_AMOUNT type /IDXGC/DE_CACL_STP_AMOUNT_MALO
    exceptions
      NO_CONSTRUCT_FOUND
      NO_CALC_FORM_FOUND .
  methods CHECK_DATA_STORAGE
    importing
      !IV_MALO type EXT_UI
      !IV_KEYDATE type DATUM default SY-DATUM
      !IV_KEYTIME type UZEIT default '000000'
    exporting
      !EV_RESULT type /IDXGC/DE_CHECK_RESULT .
endinterface.
