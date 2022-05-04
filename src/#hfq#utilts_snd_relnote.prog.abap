*&---------------------------------------------------------------------*
*&  Include           /HFQ/UTILTS_SND_RELNOTE
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
**    Im Rahmen der gesetzlichen Änderungen zum 01.12.2019 ist der
**    Datenaustausch-Prozess UTILTS zum Versand der Berechnungsformel
**    für komplexe Konstrukte eingeführt worden.
**    Dieses Paket enthält die Entwicklungen und Konfigurationen der
**    Hochfrequenz zur Implementierung des Datenaustauschprozesses für den Verteilnetzbetreiber.
**
** Funktionsumfang:
**      -	Programm zum manuellen auslösen des UTILTS-Versands
**      -	Prüfklasse zum Prozess /HFQ/UTILTS_MSS (Sammelprozess für alle Marktpartner zum Versand der Berechnungsformel)
**      -	Prüfklasse zum Prozess /HFQ/UTILTS_SND (Versandprozess der Berechnungsformel an einen Marktpartner)
**      -	Datenbereitstellungsklasse für die Erstellung des ausgehenden UTILTS-IDOCS
**      -	Erweiterungsmöglichkeit der Datenermittlung über BAdIs
**
**
**
** Technische Objekte:
**      Klassen
**        /HFQ/CL_BADI_UTILTS_SND_DEF Klasse zur BAdI-Impl.: /HFQ/BADI_UTILTS_SND_DEF
**        /HFQ/CL_MESSAGE_UTILTS_IN UTILTS-Klasse für den Nachrichteneingang
**        /HFQ/CL_UTILTS_MSS Klasse zur Befüllung der UTILTS-Prozesse
**        /HFQ/CL_UTILTS_SND Klasse zum Senden der Berechnungsformel
**      Interfaces
**        /HFQ/IF_BADI_UTILTS_SND Interface zum BAdI: /HFQ/BADI_UTILTS_SND
**      Programme
**        /HFQ/RP_DISP_CALC_FORM Programm /HFQ/RP_DISP_CALC_FORM
**        /HFQ/RP_UTILTS_SND Programm zum Versand der Berechnungsformel einer MaLo
**      Nachrichtenklassen
**        /HFQ/UTILTS_SND Nachrichtenklasse für den UTILTS-Versand
**      Erweiterungsspots
**        /HFQ/ES_UTILTS_SND Erweiterungsspot für den Versand der UTILTS
**          /HFQ/BADI_UTILTS_SND BAdI zur Ausprägung des UTILTS-Versands
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
