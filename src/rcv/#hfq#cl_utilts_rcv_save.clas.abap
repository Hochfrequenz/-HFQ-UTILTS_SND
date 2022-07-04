class /HFQ/CL_UTILTS_RCV_SAVE definition
  public
  inheriting from /IDXGC/CL_PRSTP_DATA_01
  create public .

public section.

  class-data GR_PREVIOUS type ref to /IDXGC/CX_GENERAL .
protected section.

  methods PROCESS
    redefinition .
private section.
ENDCLASS.



CLASS /HFQ/CL_UTILTS_RCV_SAVE IMPLEMENTATION.


  METHOD process.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Verbucht die eingegangene Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    DATA: lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

    TRY.
        GET BADI lr_badi_utilts_rcv.

        CALL BADI lr_badi_utilts_rcv->save_calc_form
          EXPORTING
            is_proc_step_data_all = cs_process_step_data.

        CATCH /IDXGC/CX_PROCESS_ERROR INTO gr_previous.
        CALL METHOD /IDXGC/CX_PROCESS_ERROR=>raise_proc_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
