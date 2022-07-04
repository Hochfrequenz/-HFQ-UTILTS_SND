*&---------------------------------------------------------------------*
*&  Include           /HFQ/UTILTS_RELNOTE
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
**    Im Rahmen der gesetzlichen Änderungen zum 01.12.2019 ist der Datenaustausch-Prozess
**    UTILTS zum Versand der Berechnungsformel für komplexe Konstrukte eingeführt worden.
**    Dieses Paket enthält die Marktrollenübergreifenden Entwicklungen und
**    Konfigurationen der Hochfrequenz zur Implementierung des Datenaustauschprozesses.
**    Zudem werden Möglichkeiten zur Pflege und Anzeige komplexer Konstrukte mit
**    Berechnungsformel zur Verfügung gestellt.
**    Mit der Transaktion /HFQ/CALC_FORM (Report /HFQ/RP_DISP_CALC_FORM) können im
**    System vorhandene Berechnungsformel-Konstrukte graphisch angezeigt werden.
**    Die Daten für die Anzeige werden über einen BAdI (/HFQ/BADI_UTILTS_SND) ermittelt.
**    In der Standardauslieferung wird dieser mithilfe der Berechnungsformel-Tabellen
**    /HFQ/CALC_FORM_H und /HFQ/CALC_FORM befüllt.
**    Die Anzeige der Daten findet in einer Baumansicht statt, sodass pro MaLo die Zeitscheiben
**    mit den Berechnungsschritten übersichtlich dargestellt werden können.
**    Das Programm /HFQ/RP_GEN_CALC_FORM bietet die Möglichkeit eine neue Berechnungsformel
**    (mit Zeitscheibe), Schritt-für-Schritt anzulegen. Dabei werden über die Klasse
**    /HFQ/CL_GEN_CALC_FORM die eingetragenen Daten direkt auf Konsistenz und Konflikte geprüft.
**
** Funktionsumfang:
**        -	Anzeige der Konstrukte mit Berechnungsformel
**        -	Pflege-Programm zur Erstellung neuer Berechnungsformel-Konstrukte
**        -	Marktrollen-Übergreifende Objekte für die UTILTS-Prozesse
**
**
**    zugehörige Unterpakete:
**        /HFQ/UTILTS_SND Entwicklungen und Konfiguration für den Versand der UTILTS (VNB)
**        /HFQ/UTILTS_RCV Entwicklungen und Konfiguration für den Empfang der UTILTS (MSB und Lieferant)
**
** Technische Objekte:
**      Datenbank-Tabellen:
**          /HFQ/CALC_FORM_H 	Tabelle mit Zeitscheibendaten zu Marktlokationen, die eine Berechnungsformel haben.
**          /HFQ/CALC_FORM  Tabelle mit den Berechnungsschritten für die Zeitscheiben aus /HFQ/CALC_FORM_H
**      Tabellentypen
**          /HFQ/T_CALC_FORM Tabellentyp zu Tabelle /HFQ/CALC_FORM
**          /HFQ/T_CALC_FORM_H Tabellentyp zu /HFQ/CALC_FORM_H
**          /HFQ/T_CF_DISP_CALC_STEP Tabellentyp zur Visualisierungs-Schrittstruktur
**          /HFQ/T_CF_DISP_SOURCETAB Tabellentyp zur Visualisierungs-Quellstruktur
**          /HFQ/T_CF_DISP_TIMESLICE Tabellentyp zur Visualisierungs-Zeitscheibe
**      Strukturen
**          /HFQ/S_CF_DISP_CALC_STEP Berechnungsformel: Berechnungsschritte f. d. Anzeige
**          /HFQ/S_CF_DISP_SOURCETAB Berechnungsformel: Quellstruktur f. d. Anzeige
**          /HFQ/S_CF_DISP_TIMESLICE Berechnungsformel: Zeitscheiben f. d. Anzeige
**      Datenelemente
**          /HFQ/DE_BILATERAL Kennzeichen für Bilaterale Klärung/Übermittlung
**          /HFQ/DE_CF_GUID GUID für die Berechnungsformel
**          /HFQ/DE_EXT_UI_MALO MaLo Zählpunktbezeichnung
**          /HFQ/DE_EXT_UI_MELO Melo Zählpunktbezeichnung
**      Klassen
**          /HFQ/CL_BADI_UTILTS_DISP_DEF Klasse zur BAdI-Impl.: /HFQ/BADI_UTILTS_DISP_DEF
**          /HFQ/CL_DP_UTILTS Data-Provision Class
**          /HFQ/CL_GEN_CALC_FORM Gen. d. Berechnungsformel
**      Interfaces
**          /HFQ/IF_BADI_UTILTS_DISP Interface zum BAdI: /HFQ/BADI_UTILTS_DISP
**      Programme
**          /HFQ/RP_GEN_CALC_FORM Anlegen der Berechnungsformel-Einträge
**      Transaktionen
**        /HFQ/CALC_FORM Darstellung der Berechnungsformel
**      Nachrichtenklassen
**          /HFQ/CALC_FORM
**      Erweiterungsspots
**         /HFQ/ES_UTILTS Erweiterungsspot für allgemeine Entwicklungen zur Berechnungsformel
**            /HFQ/BADI_UTILTS_DISP BAdI für die Anzeige der Berechnungsformel
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
