*&---------------------------------------------------------------------*
*&  Include           /HFQ/UTILTS_RCV_RELNOTE
*&---------------------------------------------------------------------*
** Autor        Julian Cambeis
** Firma        Hochfrequenz Unternehmensberatung GmbH
** Erstelldatum 30.09.2019
*&---------------------------------------------------------------------*
**
**********************************************************************
**              BASISRELASE
**********************************************************************
** Kurzdokumentation:
**        Im Rahmen der gesetzlichen Änderungen zum 01.12.2019 ist der
**        Datenaustausch-Prozess UTILTS zum Versand der Berechnungsformel
**        für komplexe Konstrukte eingeführt worden.
**        Dieses Paket enthält die Entwicklungen und Konfigurationen der
**        Hochfrequenz zur Implementierung des Datenaustauschprozesses auf
**        Empfängerseite (Messstellenbetreiber / Lieferant).
**
** Funktionsumfang:
**        -	Prüfklasse zum Prozess /HFQ/UTILTS_RCV zur Prüfung der eingehenden Berechnungsformel
**        -	Erweiterungsmöglichkeit der Datenverbuchung über einen BAdI
**
**
** Technische Objekte:
**      Klassen
**        /HFQ/CL_BADI_UTILTS_RCV_DEF Klasse zur BAdI-Impl.: /HFQ/BADI_UTILTS_RCV_DEF
**        /HFQ/CL_UTILTS_RCV Klasse zum Empfangen der Berechnungsformel
**      Interfaces
**        /HFQ/IF_BADI_UTILTS_RCV Interface zum BAdI: /HFQ/BADI_UTILTS_RCV
**      Erweiterungsspots
**        /HFQ/ES_UTILTS_RCV Erweiterungsspot für den Empfang der UTILTS
**          /HFQ/BADI_UTILTS_RCV BAdI für den Empfang der Berechnungsformel
**
**********************************************************************
**              Release DD.MM.CCYY
************************************************************************
**
**
** Korrekturen:
**
** Neue Funktionen:
**
**
