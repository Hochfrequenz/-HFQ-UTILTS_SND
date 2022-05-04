interface /HFQ/IF_BADI_UTILTS_SND
  public .


  interfaces IF_BADI_INTERFACE .

  constants GC_INTCODE_MSB type INTCODE value 'M1' ##NO_TEXT.
  constants GC_FORMEL_VERSAND_COMPLEX type /HFQ/DE_CALC_FORM_STAT value 'Z33' ##NO_TEXT.
  constants GC_CONTACT_QUAL_EMAIL type /IDXGC/DE_COMM_ADDR_QUAL value 'EM' ##NO_TEXT.
  constants GC_BASICPROC_E_UTILTS type E_DEXBASICPROC value 'E_UTILTS' ##NO_TEXT.
  constants GC_INTCODE_LIEF type INTCODE value '02' ##NO_TEXT.
  constants GC_FORMEL_VERSAND_BILATERAL type /HFQ/DE_CALC_FORM_STAT value 'Z34' ##NO_TEXT.
  constants GC_FORMEL_VERSAND_SINGLE type /HFQ/DE_CALC_FORM_STAT value 'Z40' ##NO_TEXT.
  constants GC_FORMEL_VERSAND_PAUSCHAL type /HFQ/DE_CALC_FORM_STAT value 'Z41' ##NO_TEXT.
  constants GC_PROC_TYPE_UTILTS_SENDER type /IDXGC/DE_PROC_TYPE value '77' ##NO_TEXT.

  methods HAS_TRANSFERABLE_FORMULA
    importing
      !IV_MALO type EXT_UI
      !IV_KEYDATE type DATUM default SY-DATUM
      !IV_KEYTIME type TIMEFROM default '000000'
    exporting
      !EV_HAS_FORMULA type KENNZX
      !EV_IS_TRANSFERABLE type KENNZX .
  methods HAS_WAIT_STEP
    importing
      !IS_STEP type /IDXGC/S_PROC_STEP_DATA_ALL
    returning
      value(RV_HAS_WAIT_STEP) type KENNZX .
  methods IS_ON_BLACKLIST
    importing
      !IV_SERVPROV type SERVICE_PROV
    returning
      value(RV_INVALID) type KENNZX .
  methods PROVIDE_CONTACT
    importing
      !IS_STEP_DATA type /IDXGC/S_PROC_STEP_DATA_ALL
      !IV_MESSAGE_STAT type /IDXGC/DE_CALC_FORM_STAT
    exporting
      !EV_CONTACT_NAME type /IDXGC/DE_DEPEMPLOY_NAME
      !EV_CONTACT_ADDR type /IDXGC/DE_COMM_ADDR_ID
      !EV_CONTACT_QUAL type /IDXGC/DE_COMM_ADDR_QUAL
    exceptions
      NO_COMM_ADDR_FOUND
      NO_SERVICE_ID .
  methods PROVIDE_DATATABLE_NAMES
    exporting
      !EV_FORMEL_H_TABLENAME type SYST_MSGV
      !EV_FORMELSTEPS_TABLENAME type SYST_MSGV .
  methods PROVIDE_MR_RELEVANCE_AND_USE
    importing
      !IV_INT_UI_MALO type INT_UI
      !IV_PROC_DATE type /IDXGC/DE_PROC_DATE
      !IV_MR_SERVICEID type /IDXGC/S_MARKPAR_DETAILS-SERVICEID
      !IV_ITEMID type /IDXGC/DE_ITEM_ID
      !IV_CONTEXT_SEQ type CHAR3
      !IV_DATA_MRREL type /IDXGL/DE_DATA_MRREL
    exporting
      !ET_DATA_RELEVANCE type /IDXGL/T_DATA_REL_DETAILS .
  methods SELECT_CORRESPONDING_MALOS
    importing
      !IV_MALO_INT_UI type INT_UI
      !IV_KEYDATE type DATUM
    exporting
      !ET_MALO_INFO type /IDXGC/T_POD_INFO_DETAILS .
  methods SELECT_CORRESPONDING_MELOS
    importing
      !IV_MALO_INT_UI type INT_UI
      !IV_KEYDATE type DATUM
    exporting
      !ET_MELO_INFO type /IDXGC/T_POD_INFO_DETAILS .
  methods SELECT_RELEVANT_DATA
    importing
      !IV_MALO type EXT_UI
      !IV_KEYDATE type DATUM default SY-DATUM
      !IV_KEYTIME type UZEIT default '000000'
      !IV_RECEIVER type E_DEXSERVPROV
    exporting
      !ET_CALC_FORM_H type /HFQ/T_CALC_FORM_H
      !ET_CALC_FORM type /HFQ/T_CALC_FORM
    exceptions
      NO_CONSTRUCT_FOUND
      NO_CALC_FORM_FOUND
      TECHNICAL_ERROR .
  methods SELECT_RELEVANT_RECEIVER_CAT
    importing
      !IS_PROC_STEP_DATA_ALL type /IDXGC/S_PROC_STEP_DATA_ALL
    exporting
      !EV_NOT_TO_LIEF type KENNZX
      !EV_NOT_TO_MSB type KENNZX .
  methods SELECT_SERVICE_TYPES
    importing
      !IV_SERVICE_TYP type INTCODE
    returning
      value(RT_SERV_TYPE_RANGE) type ISU_RANGES_TAB
    exceptions
      NO_SERVTYPE_FOUND .
  methods VALIDATE_SERVICES_AT_MALO
    importing
      !IV_INT_UI_MALO type INT_UI
      !IV_KEY_DATE type DATEFROM
    returning
      value(RV_HAS_VALID_SERVICE) type KENNZX
    exceptions
      NO_MALO
      NO_SERVICES
      MULTIPLE_SERVPROV .
  methods VALIDATE_SERVICES_AT_MELO
    importing
      !IV_INT_UI_MELO type INT_UI
      !IV_KEY_DATE type DATEFROM
    returning
      value(RV_HAS_VALID_SERVICE) type KENNZX
    exceptions
      NO_MELO
      NO_SERVICES
      MULTIPLE_SERVPROV .
endinterface.
