interface /HFQ/IF_BADI_UTILTS_DISP
  public .


  interfaces IF_BADI_INTERFACE .

  class-methods FILL_SOURCETAB
    importing
      !IT_MALO type ISU_RANGES_TAB
      !IV_STARTDATE type /HFQ/CALC_FORM_H-VALIDSTART_DATE
      !IV_ENDDATE type /HFQ/CALC_FORM_H-VALIDEND_DATE
    exporting
      !ET_SOURCETAB type /HFQ/T_CF_DISP_SOURCETAB .
  class-methods FILL_TIMESLICES
    importing
      !IT_MALO type ISU_RANGES_TAB
      !IV_STARTDATE type /HFQ/CALC_FORM_H-VALIDSTART_DATE
      !IV_ENDDATE type /HFQ/CALC_FORM_H-VALIDEND_DATE
    exporting
      !ET_TIMESLICES type /HFQ/T_CF_DISP_TIMESLICE .
  class-methods FILL_CALC_STEPS
    importing
      !IT_MALO type ISU_RANGES_TAB
      !IV_STARTDATE type /HFQ/CALC_FORM_H-VALIDSTART_DATE
      !IV_ENDDATE type /HFQ/CALC_FORM_H-VALIDEND_DATE
    exporting
      !ET_TOP_CALC_STEP type /HFQ/T_CF_DISP_CALC_STEP .
  class-methods FILL_CALC_SUB_STEPS
    importing
      !IT_MALO type ISU_RANGES_TAB
      !IV_STARTDATE type /HFQ/CALC_FORM_H-VALIDSTART_DATE
      !IV_ENDDATE type /HFQ/CALC_FORM_H-VALIDEND_DATE
    exporting
      !ET_SUB_CALC_STEP type /HFQ/T_CF_DISP_CALC_STEP .
endinterface.
